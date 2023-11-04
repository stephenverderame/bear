use rand::{distributions::Uniform, prelude::Distribution, thread_rng};

use crate::{
    bare_c::{AExpr, Block, Statement},
    pcfg::{BlockPCFG, ExprPCFG, StmtPCFG},
};

use super::{
    context::{Context, FuncList},
    gen_aexpr, gen_bexpr, gen_stmt,
    interval::Interval,
    Distribs, Type, EXPR_FUEL,
};

/// Generates an if statement
pub(super) fn gen_if(
    pcfg: &BlockPCFG<StmtPCFG>,
    expr_pcfg: &ExprPCFG,
    ctx: &mut Context,
    distribs: &mut Distribs,
    funcs: &mut FuncList,
    fuel: usize,
) -> Block<Statement> {
    let (cond, _) =
        gen_bexpr(&pcfg.if_pcfg.guard, ctx, distribs, funcs, EXPR_FUEL);
    let mut true_frame = ctx.child_frame();
    let mut false_frame = ctx.child_frame();
    let then_block =
        gen_blocks(pcfg, expr_pcfg, &mut true_frame, distribs, funcs, fuel - 1);
    let else_block = gen_blocks(
        pcfg,
        expr_pcfg,
        &mut false_frame,
        distribs,
        funcs,
        fuel - 1,
    );
    let sf = Context::meet(true_frame, false_frame);
    ctx.update(sf);
    Block::If {
        guard: cond,
        then: then_block,
        otherwise: else_block,
    }
}

/// Generates a switch statement
fn gen_switch(
    pcfg: &BlockPCFG<StmtPCFG>,
    expr_pcfg: &ExprPCFG,
    ctx: &mut Context,
    distribs: &mut Distribs,
    funcs: &mut FuncList,
    fuel: usize,
) -> Block<Statement> {
    let (switch, sw_info) =
        gen_aexpr(&expr_pcfg.a_expr, ctx, distribs, funcs, 2);
    let mut cases = vec![];
    while distribs.uniform.sample(&mut thread_rng()) < pcfg.case_seq.exp() {
        let case_rng =
            sw_info.interval.lower_bound()..=sw_info.interval.upper_bound();
        let case_num = Uniform::from(case_rng).sample(&mut thread_rng());
        let case = AExpr::Num(case_num);
        let mut case_frame = ctx.child_frame();
        let case_block = gen_blocks(
            pcfg,
            expr_pcfg,
            &mut case_frame,
            distribs,
            funcs,
            fuel - 1,
        );
        cases.push((case, case_block, case_frame));
    }
    let mut default_frame = ctx.child_frame();
    let default = gen_blocks(
        pcfg,
        expr_pcfg,
        &mut default_frame,
        distribs,
        funcs,
        fuel - 1,
    );
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

fn gen_try_catch(
    pcfg: &BlockPCFG<StmtPCFG>,
    expr_pcfg: &ExprPCFG,
    ctx: &mut Context,
    distribs: &mut Distribs,
    funcs: &mut FuncList,
    fuel: usize,
) -> Block<Statement> {
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
            .new_avar(catch_name.as_ref().unwrap(), Interval::make_unknown());
    } else if catch_type == Type::Bool {
        catch_frame.new_bvar(catch_name.as_ref().unwrap());
    }
    let r = Block::TryCatch {
        try_block: gen_blocks(
            pcfg,
            expr_pcfg,
            &mut try_frame,
            distribs,
            funcs,
            fuel - 1,
        ),
        catch_block: gen_blocks(
            pcfg,
            expr_pcfg,
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
pub(super) fn gen_blocks(
    pcfg: &BlockPCFG<StmtPCFG>,
    expr_pcfg: &ExprPCFG,
    ctx: &mut Context,
    distribs: &mut Distribs,
    funcs: &mut FuncList,
    fuel: usize,
) -> Vec<Block<Statement>> {
    let mut res = vec![];
    while distribs.uniform.sample(&mut thread_rng()) < pcfg.seq.exp()
        && fuel > 0
        && (ctx.can_follow() || ctx.is_dead())
    {
        res.push(gen_block(pcfg, expr_pcfg, ctx, distribs, funcs, fuel));
    }
    res
}

pub(super) fn gen_block(
    pcfg: &BlockPCFG<StmtPCFG>,
    expr_pcfg: &ExprPCFG,
    ctx: &mut Context,
    distribs: &mut Distribs,
    funcs: &mut FuncList,
    fuel: usize,
) -> Block<Statement> {
    let mut idx = distribs.block_idx.sample(&mut rand::thread_rng());
    while ctx.is_dead() && idx == Block::<Statement>::DEAD_IDX {
        // No nested dead blocks
        idx = distribs.block_idx.sample(&mut rand::thread_rng());
    }
    match idx {
        Block::<Statement>::DEAD_IDX => Block::Dead(gen_blocks(
            pcfg,
            expr_pcfg,
            &mut ctx.dead_child_frame(),
            distribs,
            funcs,
            fuel - 1,
        )),
        Block::<Statement>::STMT_IDX => {
            let stmt = gen_stmt(&pcfg.stmt, expr_pcfg, ctx, distribs, funcs);
            Block::Stmt(stmt)
        }
        Block::<Statement>::IF_IDX => {
            gen_if(pcfg, expr_pcfg, ctx, distribs, funcs, fuel)
        }
        Block::<Statement>::SWITCH_IDX => {
            gen_switch(pcfg, expr_pcfg, ctx, distribs, funcs, fuel)
        }
        Block::<Statement>::TRYCATCH_IDX => {
            gen_try_catch(pcfg, expr_pcfg, ctx, distribs, funcs, fuel)
        }
        _ => gen_block(pcfg, expr_pcfg, ctx, distribs, funcs, fuel),
    }
}
