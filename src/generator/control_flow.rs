use rand::{
    distributions::Uniform, prelude::Distribution, seq::SliceRandom, Rng,
};

use crate::{
    bare_c::{AExpr, Block, DuffsInfo, LoopStatement, Pretty, Statement},
    generator::LOOP_MAX_ITER,
    pcfg::{BlockPCFG, LoopPCFG, StatementPCFG, TopPCFG},
};

use super::{
    context::{Context, FuncList, StackFrame},
    expr::{gen_aexpr, gen_bexpr},
    interval::Interval,
    rnd,
    stmt::StatementTy,
    stmt::{gen_print, gen_stmt},
    Distribs, ExprInfo, StepType, Type, EXPR_FUEL,
};

const LOOP_EXPR_FUEL: usize = 3;
const LOOP_STEP_FUEL: usize = 2;

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
        || distribs.uniform.sample(&mut *rnd::get_rng()) < pcfg.case_seq.exp()
    {
        let case_rng =
            sw_info.interval.lower_bound()..=sw_info.interval.upper_bound();
        let case_num = Uniform::from(case_rng).sample(&mut *rnd::get_rng());
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
    let catch_type = distribs.type_idx.sample(&mut *rnd::get_rng());
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
    let mut has_print = false;
    if !ctx.is_dead() && ctx.can_follow() && rnd::get_rng().gen_bool(0.2) {
        // small chance of throwing in an extra print for good measure
        if let Some(x) = gen_print(ctx) {
            res.push(Block::Stmt(T::from(x)));
            has_print = true;
        }
    }
    while res.is_empty()
        || distribs.uniform.sample(&mut *rnd::get_rng()) < pcfg.seq.exp()
            && fuel > 0
            && (ctx.can_follow() || ctx.is_dead())
    {
        let blk = gen_block::<T, P>(pcfg, tp, ctx, distribs, funcs, fuel);
        if blk.len() == 1 && !has_print {
            if let Block::Stmt(stmt) = &blk[0] {
                if stmt.is_print() {
                    has_print = true;
                }
            }
        }
        res.extend(blk);
    }
    if !ctx.is_dead()
        && ctx.can_follow()
        && !has_print
        && rnd::get_rng().gen_bool(0.7)
    {
        if let Some(x) = gen_print(ctx) {
            res.push(Block::Stmt(T::from(x)));
        }
    }
    res
}

/// Information for a loop initializer
struct LoopInit {
    /// The loop initializer
    init: AExpr,
    /// The interval of the loop initializer
    init_info: ExprInfo,
    /// The interval of values the loop counter could take on
    iter_range: Interval,
    /// The loop counter limit
    limit: AExpr,
    /// The interval of the loop counter limit
    limit_info: ExprInfo,
    /// The loop step
    step: AExpr,
    /// The interval of the loop step
    step_info: ExprInfo,
    /// The numter of iterations a given loop with this initializer, limit, and step
    /// will run for
    max_iters: i64,
}

/// Corrects the loop step to ensure that the loop can be run and that it terminates
pub(super) fn correct_loop_step(
    step_ty: StepType,
    mut step: AExpr,
    mut step_info: ExprInfo,
) -> (AExpr, ExprInfo) {
    assert_ne!(step_ty, StepType::None);
    if step_ty == StepType::Inc && step_info.interval.contains_neg_or_zero() {
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
    } else if step_ty == StepType::Dec
        && step_info.interval.contains_pos_or_zero()
    {
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
    assert!(
        step_ty == StepType::Inc && step_info.interval.is_positive()
            || step_ty == StepType::Dec && step_info.interval.is_negative()
    );
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

/// Gets the range a loop counter could take on in a loop body.
fn get_iterator_range(
    step_ty: StepType,
    init_info: &ExprInfo,
    limit_info: &ExprInfo,
) -> Interval {
    assert_ne!(step_ty, StepType::None);
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

/// Gets the maximum number of iterations a loop with the given
/// initializer, limit, and step will run for
///
/// Requires step to be positive for an increasing loop
/// and negative for a decreasing loop
fn get_loop_max_iters(
    init_info: &ExprInfo,
    limit_info: &ExprInfo,
    step_info: &ExprInfo,
    step_ty: StepType,
) -> i64 {
    assert!(
        step_ty == StepType::Inc && step_info.interval.is_positive()
            || step_ty == StepType::Dec && step_info.interval.is_negative()
    );
    match step_ty {
        StepType::Inc => limit_info
            .interval
            .upper_bound()
            .saturating_sub(init_info.interval.lower_bound())
            .saturating_div(step_info.interval.lower_bound())
            .abs(),
        StepType::Dec => init_info
            .interval
            .upper_bound()
            .saturating_sub(limit_info.interval.lower_bound())
            .saturating_div(step_info.interval.upper_bound())
            .abs(),
        StepType::None => unreachable!(),
    }
}

/// Returns true if the combination of the loop bounds and step will cause the
/// loop to potentially run too long
/// # Arguments
/// * `limit_info` - The interval of the loop limit
/// * `init_info` - The interval of the loop initializer
/// * `step_info` - The interval of the loop step
/// * `step_ty` - The type of the loop step
/// * `max_iter` - The maximum number of iterations the loop is allowed to
/// run for
fn is_invalid_loop_bounds(
    limit_info: &ExprInfo,
    init_info: &ExprInfo,
    step_info: &ExprInfo,
    step_ty: StepType,
    max_iter: u64,
) -> bool {
    let max_iter = max_iter.try_into().unwrap();
    assert!(max_iter > 1);
    (step_ty == StepType::Inc
        && (step_info.interval.contains_neg_or_zero()
            || get_loop_max_iters(init_info, limit_info, step_info, step_ty)
                > max_iter))
        || (step_ty == StepType::Dec
            && (step_info.interval.contains_pos_or_zero()
                || get_loop_max_iters(
                    init_info, limit_info, step_info, step_ty,
                ) > max_iter))
}

/// Generates the loop initializer, limiter, and step
#[allow(clippy::too_many_lines)]
fn gen_loop_init(
    loop_pcfg: &LoopPCFG,
    ctx: &mut Context,
    distribs: &mut Distribs,
    funcs: &mut FuncList,
    step_ty: StepType,
) -> LoopInit {
    let mut tries = 0;
    let mut init;
    let mut init_info;
    let mut limit;
    let mut limit_info;
    let mut step;
    let mut step_info;
    loop {
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
        if tries > 3
            || !is_invalid_loop_bounds(
                &limit_info,
                &init_info,
                &step_info,
                step_ty,
                ctx.max_loop_iter(),
            )
        {
            break;
        }
    }
    (limit, limit_info) = force_correct_loop_limit(
        limit_info,
        limit,
        &init_info,
        &step_info,
        step_ty,
        ctx.max_loop_iter(),
    );
    let iter_range = get_iterator_range(step_ty, &init_info, &limit_info);
    let max_iters =
        get_loop_max_iters(&init_info, &limit_info, &step_info, step_ty);
    assert!(
        max_iters <= LOOP_MAX_ITER.try_into().unwrap()
            && max_iters <= ctx.max_loop_iter().try_into().unwrap()
    );
    LoopInit {
        init,
        init_info,
        iter_range,
        limit,
        limit_info,
        step,
        step_info,
        max_iters,
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
    let step_ty = gen_step_ty(distribs, pcfg);
    let init = gen_loop_init(&pcfg.loop_pcfg, ctx, distribs, funcs, step_ty);
    let mut body_frame = ctx.loop_child_frame(
        step_ty,
        0.5_f64.ln(),
        &var,
        false,
        init.max_iters,
    );
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
    if let Some(v) = ctx.get_mutable_avars().choose(&mut *rnd::get_rng()) {
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

/// If a loop has an invalid limit, this function will force the limit
/// to be correct by adding or subtracting `MAX_LOOP_ITER` from the limit
/// so that the loop can run at most `MAX_LOOP_ITER` times.
///
/// If the limit is already correct, this function will return the original
/// # Returns
/// A tuple of the corrected limit and its interval or the original limit and its interval
/// if the limit was already correct
/// # Arguments
/// * `limit_info` - The interval of the loop limit
/// * `limit` - The loop limit
/// * `init_info` - The interval of the loop initializer
/// * `step_info` - The interval of the loop step
/// * `step_ty` - The type of the loop step
/// * `max_iter` - The maximum number of iterations the loop is allowed to
/// run for
fn force_correct_loop_limit(
    mut limit_info: ExprInfo,
    mut limit: AExpr,
    init_info: &ExprInfo,
    step_info: &ExprInfo,
    step_ty: StepType,
    max_iter: u64,
) -> (AExpr, ExprInfo) {
    if is_invalid_loop_bounds(
        &limit_info,
        init_info,
        step_info,
        step_ty,
        max_iter,
    ) {
        let max_iter: i64 = max_iter.try_into().unwrap();
        assert!(max_iter > 1);
        if step_ty == StepType::Inc {
            assert!(step_info.interval.is_positive());
            limit = AExpr::Add(
                Box::new(AExpr::Num(init_info.interval.lower_bound())),
                Box::new(AExpr::Num(
                    max_iter * step_info.interval.lower_bound() - 1,
                )),
            );
            limit_info = ExprInfo::from_const(
                init_info.interval.lower_bound() + max_iter - 1,
            );
        } else if step_ty == StepType::Dec {
            assert!(step_info.interval.is_negative());
            limit = AExpr::Add(
                Box::new(AExpr::Num(init_info.interval.upper_bound())),
                Box::new(AExpr::Num(
                    // max_iter is positive, step_info.interval is negative
                    max_iter * step_info.interval.upper_bound() + 1,
                )),
            );
            limit_info = ExprInfo::from_const(
                init_info.interval.upper_bound()
                    + max_iter * step_info.interval.upper_bound()
                    + 1,
            );
        } else {
            panic!("Invalid step type");
        }
    }
    assert!(!is_invalid_loop_bounds(
        &limit_info,
        init_info,
        step_info,
        step_ty,
        max_iter,
    ));
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
            ctx.max_loop_iter(),
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
    force_correct_loop_limit(
        limit_info,
        limit,
        init_info,
        &worst_case_step_info,
        step_ty,
        ctx.max_loop_iter(),
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

fn worst_case_step_info(step_ty: StepType) -> Interval {
    if step_ty == StepType::Inc {
        Interval::from_const(1)
    } else {
        Interval::from_const(0)
    }
}

fn gen_step_ty<P: StatementPCFG>(
    distribs: &mut Distribs,
    pcfg: &BlockPCFG<P>,
) -> StepType {
    if distribs.uniform.sample(&mut *rnd::get_rng())
        < pcfg.loop_pcfg.inc_or_dec.exp()
    {
        StepType::Inc
    } else {
        StepType::Dec
    }
}

fn gen_while<T: Pretty + StatementTy, P: StatementPCFG>(
    pcfg: &BlockPCFG<P>,
    tp: &TopPCFG,
    ctx: &mut Context,
    distribs: &mut Distribs,
    funcs: &mut FuncList,
    fuel: usize,
) -> Vec<Block<T>> {
    if fuel < 2 || ctx.is_dead() {
        return gen_block(pcfg, tp, ctx, distribs, funcs, fuel);
    }
    let step_ty = gen_step_ty(distribs, pcfg);
    let do_while = distribs.uniform.sample(&mut *rnd::get_rng()) < 0.5;
    let (init_block, var, var_interval) =
        gen_while_var(tp, ctx, distribs, funcs, fuel);
    let init_info = ExprInfo::from_interval(var_interval);
    let (limit, limit_info) =
        gen_while_limit(pcfg, ctx, distribs, funcs, step_ty, &init_info);
    let iter_range = get_iterator_range(step_ty, &init_info, &limit_info);
    let cur_loop_iters = get_loop_max_iters(
        &init_info,
        &limit_info,
        &ExprInfo::from_interval(worst_case_step_info(step_ty)),
        step_ty,
    );
    let mut body_frame =
        ctx.loop_child_frame(step_ty, 0.5_f64.ln(), &var, true, cur_loop_iters);
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
        ctx.loop_child_frame(step_ty, 0.5_f64.ln(), var, false, init.max_iters);
    default_frame.pin_vars(&init.limit_info.vars);
    default_frame.pin_vars(&[var.to_string()]);
    default_frame.pin_vars(&sw_info.vars);
    // we can skip the first guard, so every possible initial value must be considered
    default_frame.new_avar(
        var,
        &ExprInfo::from_interval(
            init.init_info.interval.union(init.limit_info.interval),
        ),
    );
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
    let step_ty = gen_step_ty(distribs, pcfg);
    let init = gen_loop_init(&pcfg.loop_pcfg, ctx, distribs, funcs, step_ty);
    let var = ctx.new_var();
    let (switch, sw_info) = gen_switch_guard(tp, ctx, distribs, funcs);
    let mut bodies = vec![];
    let mut frames = vec![];
    while (bodies.len() < 2 && sw_info.interval.len() > 3)
        || distribs.uniform.sample(&mut *rnd::get_rng()) < pcfg.case_seq.exp()
    {
        let mut body_frame = ctx.loop_child_frame(
            step_ty,
            0.5_f64.ln(),
            &var,
            false,
            init.max_iters,
        );
        body_frame.pin_vars(&init.limit_info.vars);
        body_frame.pin_vars(&[var.clone()]);
        body_frame.pin_vars(&sw_info.vars);
        // we can skip the first guard, so every possible initial value must be considered
        body_frame.new_avar(
            &var,
            &ExprInfo::from(
                init.init_info.interval.union(init.limit_info.interval),
            ),
        );
        let body =
            gen_blocks(pcfg, tp, &mut body_frame, distribs, funcs, fuel - 1);
        let case = AExpr::Num(
            Uniform::from(
                sw_info.interval.lower_bound()..=sw_info.interval.upper_bound(),
            )
            .sample(&mut *rnd::get_rng()),
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
        distribs.block_idx.sample(&mut *rnd::get_rng())
    };
    while ctx.is_dead() && idx == Block::<Statement>::DEAD_IDX {
        // No nested dead blocks
        idx = distribs.block_idx.sample(&mut *rnd::get_rng());
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
        Block::<Statement>::FOR_IDX if ctx.max_loop_iter() > 10 => {
            vec![gen_for(pcfg, tp, ctx, distribs, funcs, fuel)]
        }
        // Block::<Statement>::WHILE_IDX => {
        //     gen_while(pcfg, tp, ctx, distribs, funcs, fuel)
        // }
        Block::<Statement>::DUFFS_IDX if ctx.max_loop_iter() > 2 => {
            vec![gen_duffs(pcfg, tp, ctx, distribs, funcs, fuel)]
        }
        _ => gen_block(pcfg, tp, ctx, distribs, funcs, fuel),
        // _ => unreachable!(),
    }
}
