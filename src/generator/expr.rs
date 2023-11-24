use rand::{distributions::Distribution, Rng};
use strum::EnumCount;

use crate::{
    bare_c::{AExpr, BExpr},
    generator::interval::Interval,
    pcfg::{AExprPCFG, ExprPCFG},
};

use super::{
    context::{Context, FuncList},
    get_rand_avar, get_rand_bvar, get_rand_prev_aexpr, get_rand_prev_bexpr,
    rnd, Distribs, ExprInfo,
};

/// Generates a division, ensuring that the divisor is not 0
fn gen_div(
    pcfg: &AExprPCFG,
    ctx: &mut Context,
    distrib: &mut Distribs,
    funcs: &mut FuncList,
    fuel: usize,
) -> (AExpr, ExprInfo) {
    if fuel == 0 {
        return gen_aexpr(pcfg, ctx, distrib, funcs, fuel);
    }
    let (lhs, lhs_info) = gen_aexpr(pcfg, ctx, distrib, funcs, fuel - 1);
    let (mut rhs, mut rhs_info) =
        gen_aexpr(pcfg, ctx, distrib, funcs, fuel - 1);
    while rhs_info.interval.contains(0)
        && rhs_info
            .interval
            .lower_bound()
            .saturating_abs()
            .saturating_add(1)
            .saturating_add(rhs_info.interval.upper_bound())
            == i64::MAX
    {
        (rhs, rhs_info) = gen_aexpr(pcfg, ctx, distrib, funcs, fuel - 1);
    }
    if rhs_info.interval.contains(0) {
        let addend = rhs_info.interval.lower_bound().abs() + 1;
        rhs = AExpr::Add(Box::new(rhs), Box::new(AExpr::Num(addend)));
        rhs_info.interval = rhs_info.interval + Interval::from_const(addend);
    }
    assert!(!rhs_info.interval.contains(0));
    (
        AExpr::Div(Box::new(lhs), Box::new(rhs)),
        lhs_info / rhs_info,
    )
}

fn get_redundant_expr(
    pcfg: &AExprPCFG,
    ctx: &mut Context,
    distrib: &mut Distribs,
    funcs: &mut FuncList,
    fuel: usize,
) -> (AExpr, ExprInfo) {
    // need this for lifetimes
    #[allow(clippy::map_unwrap_or)]
    get_rand_prev_aexpr(ctx)
        .map(|(x, x_info)| {
            if distrib.uniform.sample(&mut *rnd::get_rng()) < pcfg.reuse_swap {
                match x {
                    AExpr::Add(lhs, rhs) => (AExpr::Add(rhs, lhs), x_info),
                    AExpr::Mul(lhs, rhs) => (AExpr::Mul(rhs, lhs), x_info),
                    _ => (x, x_info),
                }
            } else {
                (x, x_info)
            }
        })
        .map(|(expr, info)| (AExpr::Redundant(Box::new(expr)), info))
        .unwrap_or_else(|| gen_aexpr(pcfg, ctx, distrib, funcs, fuel - 1))
}

fn get_redundant_bexpr(
    pcfg: &ExprPCFG,
    ctx: &mut Context,
    distrib: &mut Distribs,
    funcs: &mut FuncList,
    fuel: usize,
) -> (BExpr, Vec<String>) {
    get_rand_prev_bexpr(ctx).map_or_else(
        || gen_bexpr(pcfg, ctx, distrib, funcs, fuel - 1),
        |(expr, info)| (BExpr::Redundant(Box::new(expr)), info),
    )
}

/// Generates an arithmetic binary operation (except division)
fn gen_abop(
    pcfg: &AExprPCFG,
    ctx: &mut Context,
    distrib: &mut Distribs,
    funcs: &mut FuncList,
    fuel: usize,
    idx: usize,
) -> (AExpr, ExprInfo) {
    let (lhs, lhs_info) = gen_aexpr(pcfg, ctx, distrib, funcs, fuel - 1);
    let (rhs, rhs_info) = gen_aexpr(pcfg, ctx, distrib, funcs, fuel - 1);
    match idx {
        AExpr::ADD_IDX => (
            AExpr::Add(Box::new(lhs), Box::new(rhs)),
            lhs_info + rhs_info,
        ),
        AExpr::SUB_IDX => (
            AExpr::Sub(Box::new(lhs), Box::new(rhs)),
            lhs_info - rhs_info,
        ),
        AExpr::MUL_IDX => (
            AExpr::Mul(Box::new(lhs), Box::new(rhs)),
            lhs_info * rhs_info,
        ),
        _ => unreachable!(),
    }
}

fn gen_derived_iv(
    pcfg: &AExprPCFG,
    ctx: &mut Context,
    distrib: &mut Distribs,
    funcs: &mut FuncList,
    fuel: usize,
) -> (AExpr, ExprInfo) {
    if let Some(basic) = ctx.get_rand_loop_var() {
        let basic_info = ctx.get_avar_interval(&basic).unwrap();
        let (fact, fact_info) =
            gen_aexpr(pcfg, &mut ctx.loop_inv(), distrib, funcs, fuel);
        // (factor * basic) + addend = lhs + addend
        let lhs = AExpr::Mul(Box::new(fact), Box::new(AExpr::Id(basic)));
        let lhs_info = fact_info * ExprInfo::from_interval(basic_info);
        let (addend, addend_info) =
            gen_aexpr(pcfg, &mut ctx.loop_inv(), distrib, funcs, fuel);
        (
            AExpr::Derived(Box::new(AExpr::Add(
                Box::new(lhs),
                Box::new(addend),
            ))),
            lhs_info + addend_info,
        )
    } else {
        gen_aexpr(pcfg, ctx, distrib, funcs, fuel)
    }
}

fn gen_loop_inv_aexpr(
    pcfg: &AExprPCFG,
    ctx: &mut Context,
    distrib: &mut Distribs,
    funcs: &mut FuncList,
    fuel: usize,
) -> (AExpr, ExprInfo) {
    if ctx.loop_depth() == 0 {
        gen_aexpr(pcfg, ctx, distrib, funcs, fuel - 1)
    } else {
        let (res, info) =
            gen_aexpr(pcfg, &mut ctx.loop_inv(), distrib, funcs, fuel - 1);
        (AExpr::LoopInvariant(Box::new(res)), info)
    }
}

/// Generates an arithmetic expression
///
/// # Arguments
/// * `pcfg` - The PCFG to use
/// * `ctx` - The context to use
/// * `distrib` - The distribution to use
/// * `fuel` - The amount of fuel to use (maximum tree depth)
/// # Returns
/// Returns the generated expression and information regarding it,
/// such as the range of values it can take and the variables it uses
pub(super) fn gen_aexpr(
    pcfg: &AExprPCFG,
    ctx: &mut Context,
    distrib: &mut Distribs,
    funcs: &mut FuncList,
    fuel: usize,
) -> (AExpr, ExprInfo) {
    let idx = distrib.aexpr_idx.sample(&mut *rnd::get_rng());
    if fuel == 0 {
        if idx < AExpr::COUNT / 2 && !ctx.get_avars().is_empty() {
            let var = get_rand_avar(ctx).unwrap();
            return (AExpr::Id(var.clone()), ExprInfo::from_var(&var, ctx));
        }
        let n = rnd::get_rng().gen_range(-100_i64..100_i64);
        return (AExpr::Num(n), ExprInfo::from_const(n));
    }
    let mut is_redundant = false;
    #[allow(clippy::map_unwrap_or)]
    let (expr, expr_info) = match idx {
        AExpr::ADD_IDX | AExpr::SUB_IDX | AExpr::MUL_IDX => {
            gen_abop(pcfg, ctx, distrib, funcs, fuel, idx)
        }
        AExpr::DIV_IDX => gen_div(pcfg, ctx, distrib, funcs, fuel),
        AExpr::ID_IDX => get_rand_avar(ctx)
            .map(|x| (AExpr::Id(x.clone()), ExprInfo::from_var(&x, ctx)))
            .unwrap_or_else(|| gen_aexpr(pcfg, ctx, distrib, funcs, fuel - 1)),
        AExpr::NUM_IDX => {
            let v = rnd::get_rng().gen_range(-100..100);
            (AExpr::Num(v), ExprInfo::from_const(v))
        } // TODO: make this configurable
        AExpr::FCALL_IDX => gen_aexpr(pcfg, ctx, distrib, funcs, fuel - 1), // TODO FCall
        AExpr::REDUNDANT_IDX => {
            is_redundant = true;
            get_redundant_expr(pcfg, ctx, distrib, funcs, fuel)
        }
        AExpr::LOOPINVARIANT_IDX => {
            gen_loop_inv_aexpr(pcfg, ctx, distrib, funcs, fuel)
        }
        AExpr::DERIVED_IDX => {
            gen_derived_iv(pcfg, ctx, distrib, funcs, fuel - 1)
        }
        _ => unreachable!(),
    };
    if !is_redundant
        && distrib.uniform.sample(&mut *rnd::get_rng()) < pcfg.reuse
    {
        ctx.new_aexpr(expr.clone(), expr_info.clone());
    }
    (expr, expr_info)
}

/// Generates a bexpr which operates on two booleans
fn gen_bool_bexpr(
    pcfg: &ExprPCFG,
    ctx: &mut Context,
    distrib: &mut Distribs,
    funcs: &mut FuncList,
    fuel: usize,
    idx: usize,
) -> (BExpr, Vec<String>) {
    if fuel == 0 {
        return gen_bexpr(pcfg, ctx, distrib, funcs, fuel);
    }
    let (lhs, lhs_info) = gen_bexpr(pcfg, ctx, distrib, funcs, fuel - 1);
    let (rhs, rhs_info) = gen_bexpr(pcfg, ctx, distrib, funcs, fuel - 1);
    match idx {
        BExpr::AND_IDX => (
            BExpr::And(Box::new(lhs), Box::new(rhs)),
            lhs_info.iter().chain(rhs_info.iter()).cloned().collect(),
        ),
        BExpr::OR_IDX => (
            BExpr::Or(Box::new(lhs), Box::new(rhs)),
            lhs_info.iter().chain(rhs_info.iter()).cloned().collect(),
        ),
        _ => unreachable!(),
    }
}

/// Generates a bexpr which operates on two ints
fn gen_cmp_bexpr(
    pcfg: &ExprPCFG,
    ctx: &mut Context,
    distrib: &mut Distribs,
    funcs: &mut FuncList,
    fuel: usize,
    idx: usize,
) -> (BExpr, Vec<String>) {
    if fuel == 0 {
        return gen_bexpr(pcfg, ctx, distrib, funcs, fuel);
    }
    let (lhs, lhs_info) =
        gen_aexpr(&pcfg.a_expr, ctx, distrib, funcs, fuel - 1);
    let (rhs, rhs_info) =
        gen_aexpr(&pcfg.a_expr, ctx, distrib, funcs, fuel - 1);
    match idx {
        BExpr::LT_IDX => (
            BExpr::Lt(Box::new(lhs), Box::new(rhs)),
            lhs_info.union(rhs_info),
        ),
        BExpr::GT_IDX => (
            BExpr::Gt(Box::new(lhs), Box::new(rhs)),
            lhs_info.union(rhs_info),
        ),
        BExpr::LE_IDX => (
            BExpr::Le(Box::new(lhs), Box::new(rhs)),
            lhs_info.union(rhs_info),
        ),
        BExpr::GE_IDX => (
            BExpr::Ge(Box::new(lhs), Box::new(rhs)),
            lhs_info.union(rhs_info),
        ),
        BExpr::EQA_IDX => (
            BExpr::Eqa(Box::new(lhs), Box::new(rhs)),
            lhs_info.union(rhs_info),
        ),
        _ => unreachable!(),
    }
}

fn gen_loop_inv_bexpr(
    pcfg: &ExprPCFG,
    ctx: &mut Context,
    distrib: &mut Distribs,
    funcs: &mut FuncList,
    fuel: usize,
) -> (BExpr, Vec<String>) {
    if ctx.loop_depth() == 0 {
        gen_bexpr(pcfg, ctx, distrib, funcs, fuel - 1)
    } else {
        let (res, info) =
            gen_bexpr(pcfg, &mut ctx.loop_inv(), distrib, funcs, fuel - 1);
        (BExpr::LoopInvariant(Box::new(res)), info)
    }
}

/// Generates an expression which results in a boolean
/// # Arguments
/// * `pcfg` - The PCFG to use
/// * `ctx` - The context to use
/// * `distrib` - The distribution to use
/// * `fuel` - The amount of fuel to use (maximum tree depth)
/// # Returns
/// * The generated expression and a set of variables used in the expression
pub(super) fn gen_bexpr(
    pcfg: &ExprPCFG,
    ctx: &mut Context,
    distrib: &mut Distribs,
    funcs: &mut FuncList,
    fuel: usize,
) -> (BExpr, Vec<String>) {
    let idx = distrib.bexpr_idx.sample(&mut *rnd::get_rng());
    let mut is_redundant = false;
    let idx = if fuel == 0 { BExpr::BOOL_IDX } else { idx };
    #[allow(clippy::map_unwrap_or)]
    let (expr, vars) = match idx {
        BExpr::AND_IDX | BExpr::OR_IDX => {
            gen_bool_bexpr(pcfg, ctx, distrib, funcs, fuel, idx)
        }
        BExpr::LT_IDX
        | BExpr::GT_IDX
        | BExpr::LE_IDX
        | BExpr::GE_IDX
        | BExpr::EQA_IDX => gen_cmp_bexpr(pcfg, ctx, distrib, funcs, fuel, idx),
        BExpr::NOT_IDX => {
            let (expr, expr_info) =
                gen_bexpr(pcfg, ctx, distrib, funcs, fuel - 1);
            (BExpr::Not(Box::new(expr)), expr_info)
        }
        BExpr::BOOL_IDX => {
            let b = distrib.uniform.sample(&mut *rnd::get_rng())
                < pcfg.b_expr.boolean;
            (BExpr::Bool(b), vec![])
        }
        BExpr::REDUNDANT_IDX => {
            is_redundant = true;
            get_redundant_bexpr(pcfg, ctx, distrib, funcs, fuel)
        }
        BExpr::ID_IDX => get_rand_bvar(ctx)
            .map(|x| (BExpr::Id(x.clone()), vec![x]))
            .unwrap_or_else(|| gen_bexpr(pcfg, ctx, distrib, funcs, fuel - 1)),
        BExpr::FCALL_IDX => gen_bexpr(pcfg, ctx, distrib, funcs, fuel - 1), // TODO FCall
        BExpr::LOOPINVARIANT_IDX => {
            gen_loop_inv_bexpr(pcfg, ctx, distrib, funcs, fuel)
        }

        x => unreachable!("Invalid index: {}", x),
    };
    if !is_redundant
        && distrib.uniform.sample(&mut *rnd::get_rng()) < pcfg.b_expr.reuse
    {
        ctx.new_bexpr(expr.clone(), vars.clone());
    }
    (expr, vars)
}
