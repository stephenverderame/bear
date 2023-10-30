use rand::distributions::{Uniform, WeightedIndex};
use rand::prelude::*;
use strum::EnumCount;
mod interval;
use interval::Interval;
mod context;

use crate::bare_c::{BExpr, Expr};
use crate::generator::context::FuncList;
use crate::pcfg::ExprPCFG;
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

    /// Gets the combined list of used variables in both expressions
    pub fn union(self, rhs: Self) -> Vec<String> {
        self.vars.into_iter().chain(rhs.vars).collect()
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

fn get_rand_prev_bexpr(ctx: &Context) -> Option<(BExpr, Vec<String>)> {
    ctx.get_bexprs().choose(&mut rand::thread_rng()).cloned()
}

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
struct Distribs {
    aexpr_idx: WeightedIndex<f64>,
    bexpr_idx: WeightedIndex<f64>,
    uniform: Uniform<f64>,
}

impl Distribs {
    fn new(pcfg: &TopPCFG) -> Self {
        let index =
            WeightedIndex::new(pcfg.expr.a_expr.choice.iter().map(|x| x.exp()))
                .unwrap();
        let uniform = Uniform::new(0.0, 1.0);
        Self {
            aexpr_idx: index,
            bexpr_idx: WeightedIndex::new(
                pcfg.expr.b_expr.choice.iter().map(|x| x.exp()),
            )
            .unwrap(),
            uniform,
        }
    }
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
            if distrib.uniform.sample(&mut thread_rng()) < pcfg.reuse_swap.exp()
            {
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

fn get_redundant_bexpr(
    pcfg: &ExprPCFG,
    ctx: &mut Context,
    distrib: &mut Distribs,
    funcs: &mut FuncList,
    fuel: usize,
) -> (BExpr, Vec<String>) {
    get_rand_prev_bexpr(ctx)
        .unwrap_or_else(|| gen_bexpr(pcfg, ctx, distrib, funcs, fuel - 1))
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
    distrib: &mut Distribs,
    funcs: &mut FuncList,
    fuel: usize,
) -> (AExpr, ExprInfo) {
    let idx = distrib.aexpr_idx.sample(&mut rand::thread_rng());
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
        AExpr::ADD_IDX | AExpr::SUB_IDX | AExpr::MUL_IDX => {
            gen_abop(pcfg, ctx, distrib, funcs, fuel, idx)
        }
        AExpr::DIV_IDX => gen_div(pcfg, ctx, distrib, funcs, fuel),
        AExpr::ID_IDX => get_rand_avar(ctx)
            .map(|x| (AExpr::Id(x.clone()), ExprInfo::from_var(&x, ctx)))
            .unwrap_or_else(|| gen_aexpr(pcfg, ctx, distrib, funcs, fuel - 1)),
        AExpr::NUM_IDX => {
            let v = rand::thread_rng().gen_range(-100..100);
            (AExpr::Num(v), ExprInfo::from_const(v))
        } // TODO: make this configurable
        AExpr::FCALL_IDX => gen_aexpr(pcfg, ctx, distrib, funcs, fuel - 1), // TODO FCall
        AExpr::REDUNDANT_IDX => {
            is_redundant = true;
            get_redundant_expr(pcfg, ctx, distrib, funcs, fuel)
        }
        // can't use unwrap_or bc we need to borrow ctx mutably
        // and thus can't have the lifetimes overlap
        AExpr::LOOPINVARIANT_IDX => ctx
            .loop_inv()
            .map(|f| {
                gen_aexpr(pcfg, &mut f.child_frame(), distrib, funcs, fuel - 1)
            })
            .unwrap_or_else(|| gen_aexpr(pcfg, ctx, distrib, funcs, fuel - 1)),
        AExpr::DERIVED_IDX => gen_aexpr(pcfg, ctx, distrib, funcs, fuel - 1), // TODO Derived
        _ => unreachable!(),
    };
    if !is_redundant
        && distrib.uniform.sample(&mut rand::thread_rng()) < pcfg.reuse.exp()
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
        BExpr::EQB_IDX => (
            BExpr::Eqb(Box::new(lhs), Box::new(rhs)),
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

fn gen_bexpr(
    pcfg: &ExprPCFG,
    ctx: &mut Context,
    distrib: &mut Distribs,
    funcs: &mut FuncList,
    fuel: usize,
) -> (BExpr, Vec<String>) {
    let idx = distrib.bexpr_idx.sample(&mut rand::thread_rng());
    let mut is_redundant = false;
    #[allow(clippy::map_unwrap_or)]
    let (expr, vars) = match idx {
        BExpr::AND_IDX | BExpr::OR_IDX | BExpr::EQB_IDX => {
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
            let b = distrib.uniform.sample(&mut rand::thread_rng())
                < pcfg.b_expr.boolean.exp();
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
        BExpr::LOOPINVARIANT_IDX => ctx
            .loop_inv()
            .map(|f| {
                gen_bexpr(pcfg, &mut f.child_frame(), distrib, funcs, fuel - 1)
            })
            .unwrap_or_else(|| gen_bexpr(pcfg, ctx, distrib, funcs, fuel - 1)),
        x => unreachable!("Invalid index: {}", x),
    };
    if !is_redundant
        && distrib.uniform.sample(&mut rand::thread_rng())
            < pcfg.b_expr.reuse.exp()
    {
        ctx.new_bexpr(expr.clone(), vars.clone());
    }
    (expr, vars)
}

pub fn gen_program(pcfg: &TopPCFG) -> Expr {
    let mut funcs = FuncList::new();
    let mut ctx = Context::make_root();
    ctx.new_avar("a", Interval::new(0, 100));
    ctx.new_avar("b", Interval::new(0, 100));
    ctx.new_avar("c", Interval::new(0, 100));
    ctx.new_bvar("d");
    ctx.new_bvar("e");
    if thread_rng().sample(rand::distributions::Bernoulli::new(0.5).unwrap()) {
        let (prog, info) = gen_aexpr(
            &pcfg.expr.a_expr,
            &mut ctx,
            &mut Distribs::new(pcfg),
            &mut funcs,
            10,
        );
        println!("{}: ", info.interval);
        Expr::AExpr(prog)
    } else {
        let (prog, _) = gen_bexpr(
            &pcfg.expr,
            &mut ctx,
            &mut Distribs::new(pcfg),
            &mut funcs,
            11,
        );
        Expr::BExpr(prog)
    }
}
