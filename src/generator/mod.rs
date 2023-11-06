use rand::distributions::{Uniform, WeightedIndex};
use rand::prelude::*;
use strum::EnumCount;
mod interval;
use interval::Interval;
use strum_macros::EnumCount;
use support::Indexable;
mod context;
mod control_flow;

use crate::bare_c::{BExpr, Block, Expr, LoopStatement, Pretty, Statement};
use crate::generator::context::FuncList;
use crate::pcfg::{ExprPCFG, StatementPCFG};
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

const EXPR_FUEL: usize = 5;
const STMT_FUEL: usize = 4;

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, EnumCount, Indexable)]
enum Type {
    Int,
    Bool,
    Void,
}

pub const NUM_TYPES: usize = Type::COUNT;
pub type TypeChoice = [f64; NUM_TYPES];

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

    /// Performs the meet of two `ExprInfo`s
    /// # Panics
    /// Panics if the two `ExprInfo`s do not use the same variables
    pub fn meet(self, rhs: &Self) -> Self {
        assert_eq!(self.vars, rhs.vars);
        Self {
            vars: self.vars,
            interval: self.interval.union(rhs.interval),
        }
    }

    pub const fn make_unknown() -> Self {
        Self {
            vars: Vec::new(),
            interval: Interval::make_unknown(),
        }
    }

    pub const fn from_interval(interval: Interval) -> Self {
        Self {
            vars: Vec::new(),
            interval,
        }
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

fn get_rand_mutable_avar(ctx: &Context) -> Option<String> {
    ctx.get_mutable_avars()
        .choose(&mut rand::thread_rng())
        .cloned()
}

fn get_rand_bvar(ctx: &Context) -> Option<String> {
    ctx.get_bvars().choose(&mut rand::thread_rng()).cloned()
}

fn get_rand_mutable_bvar(ctx: &Context) -> Option<String> {
    ctx.get_mutable_bvars()
        .choose(&mut rand::thread_rng())
        .cloned()
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
    block_idx: WeightedIndex<f64>,
    uniform: Uniform<f64>,
    type_idx: WeightedIndex<f64>,
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
            block_idx: WeightedIndex::new(
                pcfg.block.choice.iter().map(|x| x.exp()),
            )
            .unwrap(),
            type_idx: WeightedIndex::new(
                pcfg.block.case_try_type.iter().map(|x| x.exp()),
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
        let (fact, fact_info) =
            gen_aexpr(pcfg, &mut ctx.loop_inv(), distrib, funcs, fuel);
        let lhs =
            AExpr::Mul(Box::new(fact), Box::new(AExpr::Id(basic.clone())));
        let basic_info = ctx.get_avar_interval(&basic).unwrap();
        let lhs_info = fact_info * ExprInfo::from_interval(basic_info);
        let (addend, addend_info) =
            gen_aexpr(pcfg, &mut ctx.loop_inv(), distrib, funcs, fuel);
        (
            AExpr::Derived(Box::new(AExpr::Add(
                Box::new(AExpr::Mul(Box::new(AExpr::Id(basic)), Box::new(lhs))),
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
        AExpr::LOOPINVARIANT_IDX => {
            gen_loop_inv_aexpr(pcfg, ctx, distrib, funcs, fuel)
        }
        AExpr::DERIVED_IDX => {
            gen_derived_iv(pcfg, ctx, distrib, funcs, fuel - 1)
        }
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
fn gen_bexpr(
    pcfg: &ExprPCFG,
    ctx: &mut Context,
    distrib: &mut Distribs,
    funcs: &mut FuncList,
    fuel: usize,
) -> (BExpr, Vec<String>) {
    let idx = distrib.bexpr_idx.sample(&mut rand::thread_rng());
    let mut is_redundant = false;
    let idx = if fuel == 0 { BExpr::BOOL_IDX } else { idx };
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
        BExpr::LOOPINVARIANT_IDX => {
            gen_loop_inv_bexpr(pcfg, ctx, distrib, funcs, fuel)
        }

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

/// Generates an assignment statement, updating the context accordingly
/// for dataflow updates
fn gen_assign<P: StatementPCFG>(
    pcfg: &P,
    expr_pcfg: &ExprPCFG,
    ctx: &mut Context,
    distribs: &mut Distribs,
    funcs: &mut FuncList,
) -> Statement {
    let typ = if distribs.uniform.sample(&mut thread_rng())
        >= pcfg.get_bool_type().exp()
    {
        Type::Int
    } else {
        Type::Bool
    };
    let var = if distribs.uniform.sample(&mut thread_rng())
        < pcfg.get_new_var().exp()
    {
        if typ == Type::Int {
            get_rand_mutable_avar(ctx).unwrap_or_else(|| ctx.new_var())
        } else {
            get_rand_mutable_bvar(ctx).unwrap_or_else(|| ctx.new_var())
        }
    } else {
        ctx.new_var()
    };
    ctx.kill_available_exprs(&var, typ);
    let expr = if typ == Type::Int {
        let (expr, expr_info) =
            gen_aexpr(&expr_pcfg.a_expr, ctx, distribs, funcs, EXPR_FUEL);
        ctx.new_avar(&var, &expr_info);
        Expr::AExpr(expr)
    } else {
        let (expr, info) =
            gen_bexpr(expr_pcfg, ctx, distribs, funcs, EXPR_FUEL);
        ctx.new_bvar(&var, info);
        Expr::BExpr(expr)
    };
    Statement::Assign {
        dest: var,
        src: expr,
    }
}

fn gen_throw<P: StatementPCFG>(
    pcfg: &P,
    expr_pcfg: &ExprPCFG,
    ctx: &mut Context,
    distribs: &mut Distribs,
    funcs: &mut FuncList,
) -> StatementEnum {
    if let Some((idx, typ)) = ctx.rand_catch(&distribs.uniform) {
        ctx.set_throw();
        match typ {
            Type::Int => {
                let (expr, _) = gen_aexpr(
                    &expr_pcfg.a_expr,
                    ctx,
                    distribs,
                    funcs,
                    EXPR_FUEL,
                );
                // TODO: interval info
                StatementEnum::Statement(Statement::Throw(
                    idx,
                    Some(Expr::AExpr(expr)),
                ))
            }
            Type::Bool => {
                let (expr, _) =
                    gen_bexpr(expr_pcfg, ctx, distribs, funcs, EXPR_FUEL);
                StatementEnum::Statement(Statement::Throw(
                    idx,
                    Some(Expr::BExpr(expr)),
                ))
            }
            Type::Void => StatementEnum::Statement(Statement::Throw(idx, None)),
        }
    } else {
        gen_stmt(pcfg, expr_pcfg, ctx, distribs, funcs, None)
    }
}
pub enum StatementEnum {
    Statement(Statement),
    LoopStmt(LoopStatement),
}
pub trait StatementTy {
    fn from(stmt: StatementEnum) -> Self
    where
        Self: Sized;
}

fn gen_loop_stmt(
    ctx: &mut Context,
    x: usize,
    expr_pcfg: &AExprPCFG,
    distrib: &mut Distribs,
    funcs: &mut FuncList,
) -> Option<StatementEnum> {
    // loop statements start indexing after the normal statements
    let cont_id = LoopStatement::CONTINUE_IDX - 1 + Statement::COUNT;
    let step_id = LoopStatement::STEP_IDX - 1 + Statement::COUNT;
    match x {
        x if x == LoopStatement::BREAK_IDX - 1 + Statement::COUNT => {
            ctx.set_loop_exit();
            Some(StatementEnum::LoopStmt(LoopStatement::Break(
                Uniform::new(0, ctx.loop_depth()).sample(&mut thread_rng()),
            )))
        }
        x if x == cont_id && ctx.get_pending_step() == StepType::None => {
            ctx.set_loop_exit();
            Some(StatementEnum::LoopStmt(LoopStatement::Continue(
                Uniform::new(0, ctx.loop_depth()).sample(&mut thread_rng()),
            )))
        }
        x if x == step_id
            || x == cont_id && ctx.get_pending_step() != StepType::None =>
        {
            let step_ty = ctx.get_pending_step();
            if step_ty == StepType::None {
                None
            } else {
                ctx.set_step();
                let (step_expr, step_info) = gen_aexpr(
                    expr_pcfg,
                    &mut ctx.loop_inv(),
                    distrib,
                    funcs,
                    3,
                );
                let (step, _) = control_flow::correct_loop_step(
                    step_ty, step_expr, step_info,
                );
                let loop_var = ctx.get_nearest_loop_var().unwrap();
                // don't update the loop variable range
                // this is correct, just not as precise as it could be
                ctx.kill_available_exprs(&loop_var, Type::Int);
                Some(StatementEnum::LoopStmt(LoopStatement::Step(
                    Statement::Assign {
                        dest: loop_var.clone(),
                        src: Expr::AExpr(AExpr::Add(
                            Box::new(AExpr::Id(loop_var)),
                            Box::new(step),
                        )),
                    },
                )))
            }
        }
        _ => unreachable!(),
    }
}

/// Generates a statement
/// # Arguments
/// * `pcfg` - The PCFG to use
/// * `expr_pcfg` - The PCFG to use for expressions
/// * `ctx` - The context to use
/// * `distrib` - The distribution to use
/// * `funcs` - The list of functions
/// * `idx` - The index of the statement to generate or None to sample
/// # Returns
/// * The generated statement
fn gen_stmt<P: StatementPCFG>(
    pcfg: &P,
    expr_pcfg: &ExprPCFG,
    ctx: &mut Context,
    distribs: &mut Distribs,
    funcs: &mut FuncList,
    idx: Option<usize>,
) -> StatementEnum {
    let idx = if ctx.is_dead() {
        Statement::ASSIGN_IDX
    } else {
        idx.unwrap_or_else(|| {
            WeightedIndex::new(pcfg.get_choice().iter().map(|x| x.exp()))
                .unwrap()
                .sample(&mut rand::thread_rng())
        })
    };
    match idx {
        Statement::ASSIGN_IDX => StatementEnum::Statement(gen_assign(
            pcfg, expr_pcfg, ctx, distribs, funcs,
        )),
        Statement::RET_IDX if ctx.ret_type() == Type::Int => {
            let (expr, info) =
                gen_aexpr(&expr_pcfg.a_expr, ctx, distribs, funcs, EXPR_FUEL);
            ctx.union_ret_rng(info.interval);
            ctx.set_return();
            StatementEnum::Statement(Statement::Ret(Some(Expr::AExpr(expr))))
        }
        Statement::RET_IDX if ctx.ret_type() == Type::Bool => {
            let (expr, _) =
                gen_bexpr(expr_pcfg, ctx, distribs, funcs, EXPR_FUEL);
            ctx.set_return();
            StatementEnum::Statement(Statement::Ret(Some(Expr::BExpr(expr))))
        }
        Statement::RET_IDX => {
            ctx.set_return();
            StatementEnum::Statement(Statement::Ret(None))
        }
        Statement::THROW_IDX => {
            gen_throw(pcfg, expr_pcfg, ctx, distribs, funcs)
        }
        Statement::PRINT_IDX => StatementEnum::Statement(Statement::Print(
            get_rand_avar(ctx)
                .map(|x| vec![Expr::AExpr(AExpr::Id(x))])
                .unwrap_or_default(),
        )),
        // TODO
        Statement::PCALL_IDX => {
            gen_stmt(pcfg, expr_pcfg, ctx, distribs, funcs, None)
        }
        x => gen_loop_stmt(ctx, x, &expr_pcfg.a_expr, distribs, funcs)
            .unwrap_or_else(|| {
                gen_stmt(pcfg, expr_pcfg, ctx, distribs, funcs, None)
            }),
    }
}

impl StatementTy for Statement {
    fn from(stmt: StatementEnum) -> Self {
        match stmt {
            StatementEnum::Statement(stmt) => stmt,
            StatementEnum::LoopStmt(..) => unreachable!(),
        }
    }
}

impl StatementTy for LoopStatement {
    fn from(stmt: StatementEnum) -> Self {
        match stmt {
            StatementEnum::Statement(stmt) => Self::Stmt(stmt),
            StatementEnum::LoopStmt(stmt) => stmt,
        }
    }
}

pub fn gen_program(pcfg: &TopPCFG) -> Vec<Block<Statement>> {
    let mut funcs = FuncList::new();
    let mut ctx = Context::make_root();
    ctx.new_avar("a", &ExprInfo::from_interval(Interval::new(0, 100)));
    ctx.new_avar("b", &ExprInfo::from_interval(Interval::new(0, 100)));
    ctx.new_avar("c", &ExprInfo::from_interval(Interval::new(0, 100)));
    ctx.new_bvar("d", vec![]);
    ctx.new_bvar("e", vec![]);
    control_flow::gen_blocks(
        &pcfg.block,
        pcfg,
        &mut ctx,
        &mut Distribs::new(pcfg),
        &mut funcs,
        STMT_FUEL,
    )
}

pub fn display(program: &[Block<Statement>]) -> String {
    let mut res = String::new();
    for block in program {
        res += &block.pretty(0);
        res += "\n";
    }
    res
}

#[cfg(test)]
mod test {
    use crate::pcfg::PCFG;

    use super::gen_program;

    #[test]
    fn running_test() {
        let pcfg = crate::pcfg::TopPCFG::uniform();
        for _ in 0..30 {
            gen_program(&pcfg);
        }
    }
}
