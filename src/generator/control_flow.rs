use rand::{distributions::Uniform, prelude::Distribution, thread_rng};

use crate::{
    bare_c::{AExpr, Block, Pretty, Statement},
    pcfg::{BlockPCFG, LoopPCFG, StatementPCFG, TopPCFG},
};

use super::{
    context::{Context, FuncList},
    gen_aexpr, gen_bexpr, gen_stmt, Distribs, ExprInfo, StatementTy, StepType,
    Type, EXPR_FUEL,
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
    let (switch, sw_info) = gen_aexpr(&tp.expr.a_expr, ctx, distribs, funcs, 2);
    let mut cases = vec![];
    while distribs.uniform.sample(&mut thread_rng()) < pcfg.case_seq.exp() {
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
    while distribs.uniform.sample(&mut thread_rng()) < pcfg.seq.exp()
        && fuel > 0
        && (ctx.can_follow() || ctx.is_dead())
    {
        res.push(gen_block(pcfg, tp, ctx, distribs, funcs, fuel));
    }
    res
}

struct LoopInit {
    init: AExpr,
    limit: AExpr,
    limit_info: ExprInfo,
    step: AExpr,
    step_info: ExprInfo,
}

fn correct_loop_step(
    step_ty: StepType,
    mut step: AExpr,
    mut step_info: ExprInfo,
) -> (AExpr, ExprInfo) {
    if step_info.interval.lower_bound() == 0 {
        step = AExpr::Add(Box::new(step), Box::new(AExpr::Num(1)));
        step_info = step_info + ExprInfo::from_const(1);
    }
    if step_ty == StepType::Inc && step_info.interval.contains(0) {
        step = AExpr::Add(
            Box::new(step),
            Box::new(AExpr::Num(
                step_info
                    .interval
                    .lower_bound()
                    .saturating_abs()
                    .saturating_add(1),
            )),
        );
    } else if step_ty == StepType::Dec && step_info.interval.contains(0) {
        step = AExpr::Sub(
            Box::new(step),
            Box::new(AExpr::Num(
                step_info
                    .interval
                    .upper_bound()
                    .saturating_abs()
                    .saturating_add(1),
            )),
        );
    }
    (step, step_info)
}

fn gen_loop_init(
    loop_pcfg: &LoopPCFG,
    ctx: &mut Context,
    distribs: &mut Distribs,
    funcs: &mut FuncList,
    step_ty: StepType,
) -> LoopInit {
    const LOOP_EXPR_FUEL: usize = 4;
    const LOOP_STEP_FUEL: usize = 2;
    let (init, init_info) =
        gen_aexpr(&loop_pcfg.init, ctx, distribs, funcs, LOOP_EXPR_FUEL);
    let (mut limit, mut limit_info) =
        gen_aexpr(&loop_pcfg.limit, ctx, distribs, funcs, LOOP_EXPR_FUEL);
    let (mut step, mut step_info) =
        gen_aexpr(&loop_pcfg.step, ctx, distribs, funcs, LOOP_STEP_FUEL);
    (step, step_info) = correct_loop_step(step_ty, step, step_info);
    while (limit_info
        .interval
        .upper_bound()
        .saturating_sub(init_info.interval.lower_bound())
        .saturating_div(step_info.interval.lower_bound())
        .abs()
        > LOOP_MAX_ITER)
        || (init_info
            .interval
            .upper_bound()
            .saturating_sub(limit_info.interval.lower_bound())
            .saturating_div(step_info.interval.lower_bound())
            .abs()
            > LOOP_MAX_ITER)
    {
        (limit, limit_info) =
            gen_aexpr(&loop_pcfg.limit, ctx, distribs, funcs, LOOP_EXPR_FUEL);
        (step, step_info) =
            gen_aexpr(&loop_pcfg.step, ctx, distribs, funcs, LOOP_STEP_FUEL);
        (step, step_info) = correct_loop_step(step_ty, step, step_info);
    }
    LoopInit {
        init,
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
    let mut body_frame = ctx.loop_child_frame(step_ty, 0.5);
    body_frame.pin_vars(&init.limit_info.vars);
    body_frame.pin_vars(&init.step_info.vars);
    let body = gen_blocks(
        &tp.loop_block,
        tp,
        &mut body_frame,
        distribs,
        funcs,
        fuel - 1,
    );
    Block::For {
        var,
        init: init.init,
        limit: init.limit,
        step: init.step,
        body,
        is_inc: step_ty == StepType::Inc,
    }
}

pub(super) fn gen_block<T: Pretty + StatementTy, P: StatementPCFG>(
    pcfg: &BlockPCFG<P>,
    tp: &TopPCFG,
    ctx: &mut Context,
    distribs: &mut Distribs,
    funcs: &mut FuncList,
    fuel: usize,
) -> Block<T> {
    let mut idx = distribs.block_idx.sample(&mut rand::thread_rng());
    while ctx.is_dead() && idx == Block::<Statement>::DEAD_IDX {
        // No nested dead blocks
        idx = distribs.block_idx.sample(&mut rand::thread_rng());
    }
    match idx {
        Block::<Statement>::DEAD_IDX => Block::Dead(gen_blocks(
            pcfg,
            tp,
            &mut ctx.dead_child_frame(),
            distribs,
            funcs,
            fuel - 1,
        )),
        Block::<Statement>::STMT_IDX => {
            let stmt =
                gen_stmt(&pcfg.stmt, &tp.expr, ctx, distribs, funcs, None);
            Block::Stmt(T::from(stmt))
        }
        Block::<Statement>::IF_IDX => {
            gen_if(pcfg, tp, ctx, distribs, funcs, fuel)
        }
        Block::<Statement>::SWITCH_IDX => {
            gen_switch(pcfg, tp, ctx, distribs, funcs, fuel)
        }
        Block::<Statement>::TRYCATCH_IDX => {
            gen_try_catch(pcfg, tp, ctx, distribs, funcs, fuel)
        }
        Block::<Statement>::FOR_IDX => {
            gen_for(pcfg, tp, ctx, distribs, funcs, fuel)
        }
        _ => gen_block(pcfg, tp, ctx, distribs, funcs, fuel),
    }
}
