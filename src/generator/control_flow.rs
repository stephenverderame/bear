use rand::{
    distributions::Uniform, prelude::Distribution, seq::SliceRandom, thread_rng,
};

use crate::{
    bare_c::{AExpr, Block, DuffsInfo, LoopStatement, Pretty, Statement},
    pcfg::{BlockPCFG, LoopPCFG, StatementPCFG, TopPCFG},
};

use super::{
    context::{Context, FuncList, StackFrame},
    gen_aexpr, gen_bexpr, gen_stmt,
    interval::Interval,
    Distribs, ExprInfo, StatementTy, StepType, Type, EXPR_FUEL,
};

const LOOP_MAX_ITER: i64 = 10_000;

/// Generates an if statement
pub(super) fn gen_if<T: Pretty + StatementTy, P: StatementPCFG>(
    pcfg: &BlockPCFG<P>,
    tp: &TopPCFG,
    ctx: &mut Context,
    distribs: &mut Distribs,
    funcs: &mut FuncList,
    fuel: usize,
) -> Block<T> {
    let (cond, _) =
        gen_bexpr(&pcfg.if_pcfg.guard, ctx, distribs, funcs, EXPR_FUEL);
    let mut true_frame = ctx.child_frame();
    let then_block =
        gen_blocks(pcfg, tp, &mut true_frame, distribs, funcs, fuel - 1);
    let mut false_frame = ctx.child_frame();
    let else_block =
        gen_blocks(pcfg, tp, &mut false_frame, distribs, funcs, fuel - 1);
    let sf = Context::meet(true_frame, false_frame);
    ctx.update(sf);
    Block::If {
        guard: cond,
        then: then_block,
        otherwise: else_block,
    }
}

/// Generates a switch statement
fn gen_switch<T: Pretty + StatementTy, P: StatementPCFG>(
    pcfg: &BlockPCFG<P>,
    tp: &TopPCFG,
    ctx: &mut Context,
    distribs: &mut Distribs,
    funcs: &mut FuncList,
    fuel: usize,
) -> Block<T> {
    let (switch, sw_info) = gen_switch_guard(tp, ctx, distribs, funcs);
    let mut cases = vec![];
    while (cases.len() < 2 && sw_info.interval.len() > 3)
        || distribs.uniform.sample(&mut thread_rng()) < pcfg.case_seq.exp()
    {
        let case_rng =
            sw_info.interval.lower_bound()..=sw_info.interval.upper_bound();
        let case_num = Uniform::from(case_rng).sample(&mut thread_rng());
        let case = AExpr::Num(case_num);
        let mut case_frame = ctx.child_frame();
        let case_block =
            gen_blocks(pcfg, tp, &mut case_frame, distribs, funcs, fuel - 1);
        cases.push((case, case_block, case_frame));
    }
    let mut default_frame = ctx.child_frame();
    let default =
        gen_blocks(pcfg, tp, &mut default_frame, distribs, funcs, fuel - 1);
    let mut sf = default_frame.stack_frame();
    let mut new_cases = vec![];
    for (case, case_block, fr) in cases {
        sf = Context::meet_sf(sf, fr);
        new_cases.push((case, case_block));
    }
    ctx.update(sf);
    Block::Switch {
        guard: switch,
        cases: new_cases,
        default,
    }
}

fn gen_try_catch<T: Pretty + StatementTy, P: StatementPCFG>(
    pcfg: &BlockPCFG<P>,
    tp: &TopPCFG,
    ctx: &mut Context,
    distribs: &mut Distribs,
    funcs: &mut FuncList,
    fuel: usize,
) -> Block<T> {
    let catch_type = distribs.type_idx.sample(&mut thread_rng());
    let (catch_name, catch_type, mut try_frame) = match catch_type {
        Type::INT_IDX => {
            (Some(ctx.new_var()), Type::Int, ctx.try_child(Type::Int))
        }
        Type::BOOL_IDX => {
            (Some(ctx.new_var()), Type::Bool, ctx.try_child(Type::Bool))
        }
        Type::VOID_IDX => (None, Type::Void, ctx.try_child(Type::Void)),
        _ => unreachable!(),
    };
    let mut catch_frame = ctx.child_frame();
    if catch_type == Type::Int {
        catch_frame
            .new_avar(catch_name.as_ref().unwrap(), &ExprInfo::make_unknown());
    } else if catch_type == Type::Bool {
        catch_frame.new_bvar(catch_name.as_ref().unwrap(), vec![]);
    }
    let r = Block::TryCatch {
        try_block: gen_blocks(
            pcfg,
            tp,
            &mut try_frame,
            distribs,
            funcs,
            fuel - 1,
        ),
        catch_block: gen_blocks(
            pcfg,
            tp,
            &mut catch_frame,
            distribs,
            funcs,
            fuel - 1,
        ),
        catch_name,
    };
    ctx.update(Context::meet(try_frame, catch_frame));
    r
}

/// Generates a sequence of blocks
pub(super) fn gen_blocks<T: Pretty + StatementTy, P: StatementPCFG>(
    pcfg: &BlockPCFG<P>,
    tp: &TopPCFG,
    ctx: &mut Context,
    distribs: &mut Distribs,
    funcs: &mut FuncList,
    fuel: usize,
) -> Vec<Block<T>> {
    let mut res = vec![];
    while res.is_empty()
        || distribs.uniform.sample(&mut thread_rng()) < pcfg.seq.exp()
            && fuel > 0
            && (ctx.can_follow() || ctx.is_dead())
    {
        res.extend(gen_block(pcfg, tp, ctx, distribs, funcs, fuel));
    }
    res
}

struct LoopInit {
    init: AExpr,
    iter_range: Interval,
    limit: AExpr,
    limit_info: ExprInfo,
    step: AExpr,
    step_info: ExprInfo,
}

/// Corrects the loop step to ensure that the loop can be run and that it terminates
pub(super) fn correct_loop_step(
    step_ty: StepType,
    mut step: AExpr,
    mut step_info: ExprInfo,
) -> (AExpr, ExprInfo) {
    if step_ty == StepType::Inc && step_info.interval.contains(0) {
        let lower_bound = step_info.interval.lower_bound();
        let addend = lower_bound.saturating_abs();
        let inc = if -addend == lower_bound { 1 } else { 2 };
        step = AExpr::Add(
            Box::new(AExpr::Add(Box::new(step), Box::new(AExpr::Num(addend)))),
            Box::new(AExpr::Num(inc)),
        );
        step_info.interval = step_info.interval
            + Interval::from_const(addend)
            + Interval::from_const(inc);
    } else if step_ty == StepType::Dec && step_info.interval.contains(0) {
        let upper_bound = step_info.interval.upper_bound();
        let subtractend = upper_bound.saturating_abs();
        let inc = if -subtractend == upper_bound { 1 } else { 2 };
        step = AExpr::Sub(
            Box::new(AExpr::Sub(
                Box::new(step),
                Box::new(AExpr::Num(subtractend)),
            )),
            Box::new(AExpr::Num(inc)),
        );
        step_info.interval = step_info.interval
            - Interval::from_const(subtractend)
            - Interval::from_const(inc);
    }
    (step, step_info)
}

/// Corrects the loop limit to ensure that the loop can be run
/// at least once by adding or subtracting the step from the limit
/// so that its minimum value is greater than or equal to the
/// minimum value of the loop initializer for an increasing loop
/// or so that its maximum value is less than or equal to the
/// maximum value of the loop initializer for a decreasing loop
/// # Returns
/// A tuple of the corrected limit and its interval
fn correct_loop_limit(
    step_ty: StepType,
    init_info: &ExprInfo,
    mut limit: AExpr,
    mut limit_info: ExprInfo,
) -> (AExpr, ExprInfo) {
    if step_ty == StepType::Inc
        && limit_info.interval.lower_bound() < init_info.interval.lower_bound()
    {
        let diff = init_info
            .interval
            .lower_bound()
            .saturating_sub(limit_info.interval.lower_bound());
        let addend = diff.saturating_add(1);
        limit = AExpr::Add(Box::new(limit), Box::new(AExpr::Num(addend)));
        limit_info.interval =
            limit_info.interval + Interval::from_const(addend);
    } else if step_ty == StepType::Dec
        && limit_info.interval.upper_bound() > init_info.interval.upper_bound()
    {
        let diff = limit_info
            .interval
            .upper_bound()
            .saturating_sub(init_info.interval.upper_bound());
        let subtractend = diff.saturating_add(1);
        limit = AExpr::Sub(Box::new(limit), Box::new(AExpr::Num(subtractend)));
        limit_info.interval =
            limit_info.interval - Interval::from_const(subtractend);
    }

    (limit, limit_info)
}

/// Gets the range an iterator could take on in a loop body.
fn get_iterator_range(
    step_ty: StepType,
    init_info: &ExprInfo,
    limit_info: &ExprInfo,
) -> Interval {
    if step_ty == StepType::Inc {
        Interval::new(
            init_info.interval.lower_bound(),
            limit_info.interval.upper_bound(),
        )
    } else {
        Interval::new(
            limit_info.interval.lower_bound(),
            init_info.interval.upper_bound(),
        )
    }
}

/// Returns true if the combination of the loop bounds and step will cause the
/// loop to potentially run too long
fn is_invalid_loop_bounds(
    limit_info: &ExprInfo,
    init_info: &ExprInfo,
    step_info: &ExprInfo,
    step_ty: StepType,
) -> bool {
    (step_ty == StepType::Inc
        && (step_info.interval.lower_bound() == 0
            || limit_info
                .interval
                .upper_bound()
                .saturating_sub(init_info.interval.lower_bound())
                .saturating_div(step_info.interval.lower_bound())
                .abs()
                > LOOP_MAX_ITER))
        || (step_ty == StepType::Dec
            && (step_info.interval.upper_bound() == 0
                || init_info
                    .interval
                    .upper_bound()
                    .saturating_sub(limit_info.interval.lower_bound())
                    .saturating_div(step_info.interval.upper_bound())
                    .abs()
                    > LOOP_MAX_ITER))
}

/// Generates the loop initializer, limiter, and step
fn gen_loop_init(
    loop_pcfg: &LoopPCFG,
    ctx: &mut Context,
    distribs: &mut Distribs,
    funcs: &mut FuncList,
    step_ty: StepType,
) -> LoopInit {
    const LOOP_EXPR_FUEL: usize = 4;
    const LOOP_STEP_FUEL: usize = 2;
    let (mut init, mut init_info) =
        gen_aexpr(&loop_pcfg.init, ctx, distribs, funcs, LOOP_EXPR_FUEL);
    let (mut limit, mut limit_info) =
        gen_aexpr(&loop_pcfg.limit, ctx, distribs, funcs, LOOP_EXPR_FUEL);
    let (mut step, mut step_info) =
        gen_aexpr(&loop_pcfg.step, ctx, distribs, funcs, LOOP_STEP_FUEL);
    (limit, limit_info) =
        correct_loop_limit(step_ty, &init_info, limit, limit_info);
    (step, step_info) = correct_loop_step(step_ty, step, step_info);
    let mut tries = 0;
    while tries < 3
        && is_invalid_loop_bounds(&limit_info, &init_info, &step_info, step_ty)
    {
        (init, init_info) =
            gen_aexpr(&loop_pcfg.init, ctx, distribs, funcs, LOOP_EXPR_FUEL);
        (limit, limit_info) =
            gen_aexpr(&loop_pcfg.limit, ctx, distribs, funcs, LOOP_EXPR_FUEL);
        (limit, limit_info) =
            correct_loop_limit(step_ty, &init_info, limit, limit_info);
        (step, step_info) =
            gen_aexpr(&loop_pcfg.step, ctx, distribs, funcs, LOOP_STEP_FUEL);
        (step, step_info) = correct_loop_step(step_ty, step, step_info);
        tries += 1;
    }
    (limit, limit_info) = force_correct_while_limit(
        limit_info, limit, &init_info, &step_info, step_ty,
    );
    assert!(!is_invalid_loop_bounds(
        &limit_info,
        &init_info,
        &step_info,
        step_ty
    ));
    let iter_range = get_iterator_range(step_ty, &init_info, &limit_info);
    LoopInit {
        init,
        iter_range,
        limit,
        limit_info,
        step,
        step_info,
    }
}

fn gen_for<T: Pretty + StatementTy, P: StatementPCFG>(
    pcfg: &BlockPCFG<P>,
    tp: &TopPCFG,
    ctx: &mut Context,
    distribs: &mut Distribs,
    funcs: &mut FuncList,
    fuel: usize,
) -> Block<T> {
    let var = ctx.new_var();
    let step_ty = if distribs.uniform.sample(&mut thread_rng())
        < pcfg.loop_pcfg.inc_or_dec.exp()
    {
        StepType::Inc
    } else {
        StepType::Dec
    };
    let init = gen_loop_init(&pcfg.loop_pcfg, ctx, distribs, funcs, step_ty);
    let mut body_frame =
        ctx.loop_child_frame(step_ty, 0.5_f64.ln(), &var, false);
    body_frame.new_avar(&var, &ExprInfo::from_interval(init.iter_range));
    body_frame.pin_vars(&init.limit_info.vars);
    body_frame.pin_vars(&init.step_info.vars);
    body_frame.pin_vars(&[var.clone()]);
    let body = gen_blocks(
        &tp.loop_block,
        tp,
        &mut body_frame,
        distribs,
        funcs,
        fuel - 1,
    );
    ctx.update_from_loop(&body_frame.stack_frame());
    Block::For {
        var,
        init: init.init,
        limit: init.limit,
        step: init.step,
        body,
        is_inc: step_ty == StepType::Inc,
    }
}

/// Gets the loop variable for a while loop, creating one if necessary
/// # Returns
/// A tuple of an optional block containing the initialization of the loop
/// variable, the name of the loop variable, and the interval of the loop
fn gen_while_var<T: Pretty + StatementTy>(
    tp: &TopPCFG,
    ctx: &mut Context,
    distribs: &mut Distribs,
    funcs: &mut FuncList,
    fuel: usize,
) -> (Option<Block<T>>, String, Interval) {
    if let Some(v) = ctx.get_mutable_avars().choose(&mut thread_rng()) {
        (None, v.clone(), ctx.get_avar_interval(v).unwrap())
    } else {
        let v = ctx.new_var();
        let (init, init_info) =
            gen_aexpr(&tp.expr.a_expr, ctx, distribs, funcs, fuel);
        (
            Some(Block::Stmt(T::from(super::StatementEnum::Statement(
                Statement::Assign {
                    dest: v.clone(),
                    src: crate::bare_c::Expr::AExpr(init),
                },
            )))),
            v,
            init_info.interval,
        )
    }
}

/// If a while loop has an invalid limit, this function will force the limit
/// to be correct by adding or subtracting `MAX_LOOP_ITER` from the limit
/// so that the loop can run at most `MAX_LOOP_ITER` times
/// # Returns
/// A tuple of the corrected limit and its interval or the original limit and its interval
/// if the limit was already correct
fn force_correct_while_limit(
    mut limit_info: ExprInfo,
    mut limit: AExpr,
    init_info: &ExprInfo,
    worst_case_step_info: &ExprInfo,
    step_ty: StepType,
) -> (AExpr, ExprInfo) {
    if is_invalid_loop_bounds(
        &limit_info,
        init_info,
        worst_case_step_info,
        step_ty,
    ) {
        if step_ty == StepType::Inc {
            limit = AExpr::Add(
                Box::new(AExpr::Num(init_info.interval.lower_bound())),
                Box::new(AExpr::Num(LOOP_MAX_ITER)),
            );
            limit_info = ExprInfo::from_const(
                init_info.interval.lower_bound() + LOOP_MAX_ITER,
            );
        } else {
            limit = AExpr::Sub(
                Box::new(AExpr::Num(init_info.interval.upper_bound())),
                Box::new(AExpr::Num(LOOP_MAX_ITER)),
            );
            limit_info = ExprInfo::from_const(
                init_info.interval.upper_bound() - LOOP_MAX_ITER,
            );
        }
    }
    (limit, limit_info)
}

/// Generates the loop limit for a while loop
fn gen_while_limit<P: StatementPCFG>(
    pcfg: &BlockPCFG<P>,
    ctx: &mut Context,
    distribs: &mut Distribs,
    funcs: &mut FuncList,
    step_ty: StepType,
    init_info: &ExprInfo,
) -> (AExpr, ExprInfo) {
    let (mut limit, mut limit_info) =
        gen_aexpr(&pcfg.loop_pcfg.limit, ctx, distribs, funcs, EXPR_FUEL - 2);
    let worst_case_step_info = if step_ty == StepType::Inc {
        ExprInfo::from_const(1)
    } else {
        ExprInfo::from_const(-1)
    };
    (limit, limit_info) =
        correct_loop_limit(step_ty, init_info, limit, limit_info);
    let mut tries = 0;
    while tries < 3
        && is_invalid_loop_bounds(
            &limit_info,
            init_info,
            &worst_case_step_info,
            step_ty,
        )
    {
        (limit, limit_info) = gen_aexpr(
            &pcfg.loop_pcfg.limit,
            ctx,
            distribs,
            funcs,
            EXPR_FUEL - 2,
        );
        (limit, limit_info) =
            correct_loop_limit(step_ty, init_info, limit, limit_info);
        tries += 1;
    }
    force_correct_while_limit(
        limit_info,
        limit,
        init_info,
        &worst_case_step_info,
        step_ty,
    )
}

fn gen_while_body(
    tp: &TopPCFG,
    body_frame: &mut Context,
    distribs: &mut Distribs,
    funcs: &mut FuncList,
    fuel: usize,
) -> Vec<Block<LoopStatement>> {
    let mut body = vec![];
    while body_frame.get_pending_step() != StepType::None {
        body.extend(gen_blocks(
            &tp.loop_block,
            tp,
            body_frame,
            distribs,
            funcs,
            fuel - 1,
        ));
    }
    body
}

fn gen_while<T: Pretty + StatementTy, P: StatementPCFG>(
    pcfg: &BlockPCFG<P>,
    tp: &TopPCFG,
    ctx: &mut Context,
    distribs: &mut Distribs,
    funcs: &mut FuncList,
    fuel: usize,
) -> Vec<Block<T>> {
    if fuel < 2 {
        return gen_block(pcfg, tp, ctx, distribs, funcs, fuel);
    }
    if ctx.is_dead() {
        return gen_block(pcfg, tp, ctx, distribs, funcs, fuel);
    }
    let step_ty = if distribs.uniform.sample(&mut thread_rng())
        < pcfg.loop_pcfg.inc_or_dec.exp()
    {
        StepType::Inc
    } else {
        StepType::Dec
    };
    let do_while = distribs.uniform.sample(&mut thread_rng()) < 0.5;
    let (init_block, var, var_interval) =
        gen_while_var(tp, ctx, distribs, funcs, fuel);
    let init_info = ExprInfo::from_interval(var_interval);
    let (limit, limit_info) =
        gen_while_limit(pcfg, ctx, distribs, funcs, step_ty, &init_info);
    let iter_range = get_iterator_range(step_ty, &init_info, &limit_info);
    let mut body_frame =
        ctx.loop_child_frame(step_ty, 0.5_f64.ln(), &var, true);
    body_frame.pin_vars(&limit_info.vars);
    body_frame.pin_vars(&[var.clone()]);
    if do_while {
        body_frame.new_avar(
            &var,
            &ExprInfo::from_interval(iter_range.union(init_info.interval)),
        );
    } else {
        body_frame.new_avar(&var, &iter_range.into());
    }
    let body = gen_while_body(tp, &mut body_frame, distribs, funcs, fuel - 1);
    let sf = body_frame.stack_frame();
    if do_while {
        ctx.update_from_do_while_loop(&sf);
    } else {
        ctx.update_from_loop(&sf);
    }
    init_block
        .into_iter()
        .chain(std::iter::once(Block::While {
            var,
            limit,
            body,
            is_inc: step_ty == StepType::Inc,
            do_while,
        }))
        .collect()
}

fn gen_switch_guard(
    tp: &TopPCFG,
    ctx: &mut Context,
    distribs: &mut Distribs,
    funcs: &mut FuncList,
) -> (AExpr, ExprInfo) {
    let (mut switch, mut sw_info) =
        gen_aexpr(&tp.expr.a_expr, ctx, distribs, funcs, 3);
    let mut tries = 0;
    while tries < 3 && sw_info.interval.len() < 3 {
        (switch, sw_info) = gen_aexpr(&tp.expr.a_expr, ctx, distribs, funcs, 3);
        tries += 1;
    }
    (switch, sw_info)
}

/// Generates the default case for a duff's device
/// and updates the context with the information from the
/// child frames of each case
#[allow(clippy::too_many_arguments)]
fn gen_duffs_default_and_update_ctx<
    T: Pretty + StatementTy,
    P: StatementPCFG,
>(
    pcfg: &BlockPCFG<P>,
    tp: &TopPCFG,
    ctx: &mut Context,
    distribs: &mut Distribs,
    funcs: &mut FuncList,
    fuel: usize,
    step_ty: StepType,
    var: &str,
    init: &LoopInit,
    sw_info: &ExprInfo,
    frames: Vec<StackFrame>,
) -> Vec<Block<T>> {
    let mut default_frame =
        ctx.loop_child_frame(step_ty, 0.5_f64.ln(), var, false);
    default_frame.pin_vars(&init.limit_info.vars);
    default_frame.pin_vars(&[var.to_string()]);
    default_frame.pin_vars(&sw_info.vars);
    default_frame.new_avar(var, &ExprInfo::from(init.iter_range));
    let default =
        gen_blocks(pcfg, tp, &mut default_frame, distribs, funcs, fuel - 1);
    let mut sf = default_frame.stack_frame();
    for frame in frames {
        sf = sf.meet(frame);
    }
    ctx.update_from_do_while_loop(&sf);
    default
}

fn gen_duffs<T: Pretty + StatementTy, P: StatementPCFG>(
    pcfg: &BlockPCFG<P>,
    tp: &TopPCFG,
    ctx: &mut Context,
    distribs: &mut Distribs,
    funcs: &mut FuncList,
    fuel: usize,
) -> Block<T> {
    let step_ty = if distribs.uniform.sample(&mut thread_rng())
        < pcfg.loop_pcfg.inc_or_dec.exp()
    {
        StepType::Inc
    } else {
        StepType::Dec
    };
    let init = gen_loop_init(&pcfg.loop_pcfg, ctx, distribs, funcs, step_ty);
    let var = ctx.new_var();
    let (switch, sw_info) = gen_switch_guard(tp, ctx, distribs, funcs);
    let mut bodies = vec![];
    let mut frames = vec![];
    while (bodies.len() < 2 && sw_info.interval.len() > 3)
        || distribs.uniform.sample(&mut thread_rng()) < pcfg.case_seq.exp()
    {
        let mut body_frame =
            ctx.loop_child_frame(step_ty, 0.5_f64.ln(), &var, false);
        body_frame.pin_vars(&init.limit_info.vars);
        body_frame.pin_vars(&[var.clone()]);
        body_frame.pin_vars(&sw_info.vars);
        body_frame.new_avar(&var, &ExprInfo::from(init.iter_range));
        let body =
            gen_blocks(pcfg, tp, &mut body_frame, distribs, funcs, fuel - 1);
        let case = AExpr::Num(
            Uniform::from(
                sw_info.interval.lower_bound()..=sw_info.interval.upper_bound(),
            )
            .sample(&mut thread_rng()),
        );
        bodies.push((case, body));
        frames.push(body_frame.stack_frame());
    }
    let default = gen_duffs_default_and_update_ctx(
        pcfg, tp, ctx, distribs, funcs, fuel, step_ty, &var, &init, &sw_info,
        frames,
    );
    Block::Duffs(DuffsInfo {
        var,
        limit: init.limit,
        init: init.init,
        step: init.step,
        bodies,
        is_inc: step_ty == StepType::Inc,
        guard: switch,
        default,
    })
}

pub(super) fn gen_block<T: Pretty + StatementTy, P: StatementPCFG>(
    pcfg: &BlockPCFG<P>,
    tp: &TopPCFG,
    ctx: &mut Context,
    distribs: &mut Distribs,
    funcs: &mut FuncList,
    fuel: usize,
) -> Vec<Block<T>> {
    let mut idx = if fuel == 0 {
        Block::<Statement>::STMT_IDX
    } else {
        distribs.block_idx.sample(&mut rand::thread_rng())
    };
    while ctx.is_dead() && idx == Block::<Statement>::DEAD_IDX {
        // No nested dead blocks
        idx = distribs.block_idx.sample(&mut rand::thread_rng());
    }
    match idx {
        Block::<Statement>::DEAD_IDX => vec![Block::Dead(gen_blocks(
            pcfg,
            tp,
            &mut ctx.dead_child_frame(),
            distribs,
            funcs,
            fuel - 1,
        ))],
        Block::<Statement>::STMT_IDX => {
            let stmt =
                gen_stmt(&pcfg.stmt, &tp.expr, ctx, distribs, funcs, None);
            vec![Block::Stmt(T::from(stmt))]
        }
        Block::<Statement>::IF_IDX => {
            vec![gen_if(pcfg, tp, ctx, distribs, funcs, fuel)]
        }
        Block::<Statement>::SWITCH_IDX => {
            vec![gen_switch(pcfg, tp, ctx, distribs, funcs, fuel)]
        }
        Block::<Statement>::TRYCATCH_IDX => {
            vec![gen_try_catch(pcfg, tp, ctx, distribs, funcs, fuel)]
        }
        Block::<Statement>::FOR_IDX => {
            vec![gen_for(pcfg, tp, ctx, distribs, funcs, fuel)]
        }
        Block::<Statement>::WHILE_IDX => {
            gen_while(pcfg, tp, ctx, distribs, funcs, fuel)
        }
        Block::<Statement>::DUFFS_IDX => {
            vec![gen_duffs(pcfg, tp, ctx, distribs, funcs, fuel)]
        }
        _ => unreachable!(),
    }
}
