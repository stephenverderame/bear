use std::collections::HashSet;

use rand::distributions::{Uniform, WeightedIndex};
use rand::prelude::*;
use static_assertions as sa;
use strum::EnumCount;
mod interval;
use interval::Interval;
mod context;

use crate::generator::context::FuncList;
/// Overflow is not an error in BRIL, so we just don't worry about it.
/// Furthermore, we use random probing for a few reasons:
/// 1. We don't want small programs
/// 2. With such a complex DSL, it's unlikely to generate the same program twice
/// 3. We want some redundancy in the programs to stress redundancy elimination
///
use crate::{
    bare_c::AExpr,
    pcfg::{AExprPCFG, TopPCFG},
};

use self::context::Context;

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
enum Type {
    Int,
    Bool,
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
enum StepType {
    None,
    Inc,
    Dec,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct ExprInfo {
    /// The vars that are used in this expression
    vars: Vec<String>,
    /// The range of values this expression can take
    interval: Interval,
}

impl ExprInfo {
    /// Constructs an `ExprInfo` from a constant
    pub const fn from_const(a: i64) -> Self {
        Self {
            vars: Vec::new(),
            interval: Interval::from_const(a),
        }
    }

    /// Constructs an `ExprInfo` from a variable
    pub fn from_var(a: &str, ctx: &Context) -> Self {
        ctx.lookup_var(a).unwrap_or_else(|| Self {
            vars: vec![a.to_string()],
            interval: Interval::make_unknown(),
        })
    }
}

impl std::ops::Add<Self> for ExprInfo {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self {
            vars: self.vars.into_iter().chain(rhs.vars).collect(),
            interval: self.interval + rhs.interval,
        }
    }
}

impl std::ops::Sub<Self> for ExprInfo {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Self {
            vars: self.vars.into_iter().chain(rhs.vars).collect(),
            interval: self.interval - rhs.interval,
        }
    }
}

impl std::ops::Mul<Self> for ExprInfo {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        Self {
            vars: self.vars.into_iter().chain(rhs.vars).collect(),
            interval: self.interval * rhs.interval,
        }
    }
}

impl std::ops::Div<Self> for ExprInfo {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        Self {
            vars: self.vars.into_iter().chain(rhs.vars).collect(),
            interval: self.interval / rhs.interval,
        }
    }
}

fn get_rand_avar(ctx: &Context) -> Option<String> {
    ctx.get_avars().choose(&mut rand::thread_rng()).cloned()
}

fn get_rand_bvar(ctx: &Context) -> Option<String> {
    ctx.get_bvars().choose(&mut rand::thread_rng()).cloned()
}

fn get_rand_prev_aexpr(ctx: &Context) -> Option<(AExpr, ExprInfo)> {
    ctx.get_aexprs().choose(&mut rand::thread_rng()).cloned()
}

/// Generates a division, ensuring that the divisor is not 0
fn gen_div(
    pcfg: &AExprPCFG,
    ctx: &mut Context,
    distrib: &mut AExprDistribs,
    funcs: &mut FuncList,
    fuel: usize,
) -> (AExpr, ExprInfo) {
    if fuel == 0 {
        return gen_aexpr(pcfg, ctx, distrib, funcs, fuel);
    }
    let (lhs, lhs_info) = gen_aexpr(pcfg, ctx, distrib, funcs, fuel - 1);
    let (mut rhs, mut rhs_info) = gen_aexpr(pcfg, ctx, distrib, funcs, fuel - 1);
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
        rhs = AExpr::Add(
            Box::new(rhs),
            Box::new(AExpr::Num(rhs_info.interval.lower_bound().abs() + 1)),
        );
        rhs_info.interval = Interval::new_clamped_lower(
            rhs_info.interval.lower_bound().abs() + 1,
            rhs_info.interval.upper_bound(),
        );
    }
    (
        AExpr::Div(Box::new(lhs), Box::new(rhs)),
        lhs_info / rhs_info,
    )
}

/// Distributions for generating arithmetic expressions
struct AExprDistribs {
    index: WeightedIndex<f64>,
    uniform: Uniform<f64>,
}

impl AExprDistribs {
    fn new(pcfg: &AExprPCFG) -> Self {
        let index = WeightedIndex::new(pcfg.choice.iter().map(|x| x.exp())).unwrap();
        let uniform = Uniform::new(0.0, 1.0);
        Self { index, uniform }
    }
}

fn get_redundant_expr(
    pcfg: &AExprPCFG,
    ctx: &mut Context,
    distrib: &mut AExprDistribs,
    funcs: &mut FuncList,
    fuel: usize,
) -> (AExpr, ExprInfo) {
    // need this for lifetimes
    #[allow(clippy::map_unwrap_or)]
    get_rand_prev_aexpr(ctx)
        .map(|(x, x_info)| {
            if distrib.uniform.sample(&mut thread_rng()) < pcfg.reuse_swap.exp() {
                match x {
                    AExpr::Add(lhs, rhs) => (AExpr::Add(rhs, lhs), x_info),
                    AExpr::Mul(lhs, rhs) => (AExpr::Mul(rhs, lhs), x_info),
                    _ => (x, x_info),
                }
            } else {
                (x, x_info)
            }
        })
        .unwrap_or_else(|| gen_aexpr(pcfg, ctx, distrib, funcs, fuel - 1))
}

/// Generates an arithmetic expression
///
/// # Arguments
/// * `pcfg` - The PCFG to use
/// * `ctx` - The context to use
/// * `distrib` - The distribution to use
/// * `fuel` - The amount of fuel to use (maximum tree depth)
fn gen_aexpr(
    pcfg: &AExprPCFG,
    ctx: &mut Context,
    distrib: &mut AExprDistribs,
    funcs: &mut FuncList,
    fuel: usize,
) -> (AExpr, ExprInfo) {
    let idx = distrib.index.sample(&mut rand::thread_rng());
    sa::const_assert_eq!(AExpr::COUNT, 10);
    if fuel == 0 {
        if idx < AExpr::COUNT / 2 && !ctx.get_avars().is_empty() {
            let var = get_rand_avar(ctx).unwrap();
            return (AExpr::Id(var.clone()), ExprInfo::from_var(&var, ctx));
        }
        let n = rand::thread_rng().gen_range(-100_i64..100_i64);
        return (AExpr::Num(n), ExprInfo::from_const(n));
    }
    let mut is_redundant = false;
    #[allow(clippy::map_unwrap_or)]
    let (expr, expr_info) = match idx {
        0 => {
            let (lhs, lhs_info) = gen_aexpr(pcfg, ctx, distrib, funcs, fuel - 1);
            let (rhs, rhs_info) = gen_aexpr(pcfg, ctx, distrib, funcs, fuel - 1);
            (
                AExpr::Add(Box::new(lhs), Box::new(rhs)),
                lhs_info + rhs_info,
            )
        }
        1 => {
            let (lhs, lhs_info) = gen_aexpr(pcfg, ctx, distrib, funcs, fuel - 1);
            let (rhs, rhs_info) = gen_aexpr(pcfg, ctx, distrib, funcs, fuel - 1);
            (
                AExpr::Sub(Box::new(lhs), Box::new(rhs)),
                lhs_info - rhs_info,
            )
        }
        2 => {
            let (lhs, lhs_info) = gen_aexpr(pcfg, ctx, distrib, funcs, fuel - 1);
            let (rhs, rhs_info) = gen_aexpr(pcfg, ctx, distrib, funcs, fuel - 1);
            (
                AExpr::Mul(Box::new(lhs), Box::new(rhs)),
                lhs_info * rhs_info,
            )
        }
        3 => gen_div(pcfg, ctx, distrib, funcs, fuel),
        4 => get_rand_avar(ctx)
            .map(|x| (AExpr::Id(x.clone()), ExprInfo::from_var(&x, ctx)))
            .unwrap_or_else(|| gen_aexpr(pcfg, ctx, distrib, funcs, fuel - 1)),
        5 => {
            let v = rand::thread_rng().gen_range(-100..100);
            (AExpr::Num(v), ExprInfo::from_const(v))
        } // TODO: make this configurable
        6 => gen_aexpr(pcfg, ctx, distrib, funcs, fuel - 1), // TODO FCall
        7 => {
            is_redundant = true;
            get_redundant_expr(pcfg, ctx, distrib, funcs, fuel)
        }

        // can't use unwrap_or bc we need to borrow ctx mutably
        // and thus can't have the lifetimes overlap
        8 => ctx
            .loop_inv()
            .map(|f| gen_aexpr(pcfg, &mut f.child_frame(), distrib, funcs, fuel - 1))
            .unwrap_or_else(|| gen_aexpr(pcfg, ctx, distrib, funcs, fuel - 1)),
        9 => gen_aexpr(pcfg, ctx, distrib, funcs, fuel - 1), // TODO Derived
        _ => unreachable!(),
    };
    if !is_redundant && distrib.uniform.sample(&mut rand::thread_rng()) < pcfg.reuse.exp() {
        ctx.new_aexpr(expr.clone(), expr_info.clone());
    }
    (expr, expr_info)
}

pub fn gen_program(pcfg: &TopPCFG) -> AExpr {
    let mut funcs = FuncList::new();
    let mut ctx = Context::make_root();
    ctx.new_var("a".to_string(), Interval::new(0, 100));
    ctx.new_var("b".to_string(), Interval::new(0, 100));
    ctx.new_var("c".to_string(), Interval::new(0, 100));
    let (prog, info) = gen_aexpr(
        &pcfg.expr.a_expr,
        &mut ctx,
        &mut AExprDistribs::new(&pcfg.expr.a_expr),
        &mut funcs,
        10,
    );
    println!("{}: ", info.interval);
    prog
}
