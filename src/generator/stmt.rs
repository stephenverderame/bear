use rand::{
    distributions::{Distribution, Uniform, WeightedIndex},
    Rng,
};
use strum::EnumCount;

use crate::{
    bare_c::{AExpr, BExpr, Expr, LoopStatement, Statement},
    pcfg::{AExprPCFG, ExprPCFG, StatementPCFG},
};

use super::{
    context::{Context, FuncList},
    control_flow,
    expr::{gen_aexpr, gen_bexpr},
    get_rand_avar, get_rand_bvar, get_rand_mutable_avar, get_rand_mutable_bvar,
    rnd, Complexity, Distribs, ExprInfo, StepType, Type, EXPR_FUEL,
};

/// Generates an assignment statement, updating the context accordingly
/// for dataflow updates
fn gen_assign<P: StatementPCFG>(
    pcfg: &P,
    expr_pcfg: &ExprPCFG,
    ctx: &mut Context,
    distribs: &mut Distribs,
    funcs: &mut FuncList,
) -> Statement {
    let typ = if distribs.uniform.sample(&mut rnd::get_rng())
        >= pcfg.get_bool_type()
    {
        Type::Int
    } else {
        Type::Bool
    };
    let var =
        if distribs.uniform.sample(&mut rnd::get_rng()) < pcfg.get_new_var() {
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
        if expr_info.vars.contains(&var) && ctx.loop_depth() != 0 {
            ctx.kill_available_exprs(&var, typ);
            // mutation in a loop
            // for now, just make it unknown
            ctx.new_avar(&var, &ExprInfo::make_unknown());
        } else {
            ctx.new_avar(&var, &expr_info);
        }
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
    complexity: &mut Complexity,
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
        gen_stmt(pcfg, expr_pcfg, ctx, distribs, funcs, None, complexity)
    }
}

/// A variant containing either a regular or loop statement
pub enum StatementEnum {
    Statement(Statement),
    LoopStmt(LoopStatement),
}

/// A trait that is implemented by all statement types
pub trait StatementTy {
    fn from(stmt: StatementEnum) -> Self
    where
        Self: Sized;

    fn to_enum(self) -> StatementEnum;

    fn is_print(&self) -> bool;
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
                Uniform::new(0, ctx.loop_depth()).sample(&mut rnd::get_rng()),
            )))
        }
        x if x == cont_id && ctx.get_pending_step() == StepType::None => {
            ctx.set_loop_exit();
            Some(StatementEnum::LoopStmt(LoopStatement::Continue(
                Uniform::new(0, ctx.loop_depth()).sample(&mut rnd::get_rng()),
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
pub(super) fn gen_stmt<P: StatementPCFG>(
    pcfg: &P,
    expr_pcfg: &ExprPCFG,
    ctx: &mut Context,
    distribs: &mut Distribs,
    funcs: &mut FuncList,
    idx: Option<usize>,
    complexity: &mut Complexity,
) -> StatementEnum {
    let idx = if ctx.is_dead() {
        Statement::ASSIGN_IDX
    } else {
        idx.unwrap_or_else(|| {
            WeightedIndex::new(pcfg.get_choice().iter())
                .unwrap()
                .sample(&mut rnd::get_rng())
        })
    };
    match idx {
        Statement::ASSIGN_IDX => StatementEnum::Statement(gen_assign(
            pcfg, expr_pcfg, ctx, distribs, funcs,
        )),
        Statement::RET_IDX => {
            gen_ret(pcfg, expr_pcfg, ctx, distribs, funcs, complexity)
        }
        Statement::THROW_IDX => {
            gen_throw(pcfg, expr_pcfg, ctx, distribs, funcs, complexity)
        }
        Statement::PRINT_IDX => gen_print(ctx).unwrap_or_else(|| {
            gen_stmt(pcfg, expr_pcfg, ctx, distribs, funcs, None, complexity)
        }),
        // TODO
        Statement::PCALL_IDX => {
            gen_stmt(pcfg, expr_pcfg, ctx, distribs, funcs, None, complexity)
        }
        x => gen_loop_stmt(ctx, x, &expr_pcfg.a_expr, distribs, funcs)
            .unwrap_or_else(|| {
                gen_stmt(
                    pcfg, expr_pcfg, ctx, distribs, funcs, None, complexity,
                )
            }),
    }
}

/// Generates a return if we can, otherwise generates a statement
fn gen_ret<P: StatementPCFG>(
    pcfg: &P,
    expr_pcfg: &ExprPCFG,
    ctx: &mut Context,
    distribs: &mut Distribs,
    funcs: &mut FuncList,
    complexity: &mut Complexity,
) -> StatementEnum {
    if complexity.stmts >= complexity.min_stmts {
        if ctx.ret_type() == Type::Bool {
            let (expr, _) =
                gen_bexpr(expr_pcfg, ctx, distribs, funcs, EXPR_FUEL);
            ctx.set_return();
            StatementEnum::Statement(Statement::Ret(Some(Expr::BExpr(expr))))
        } else if ctx.ret_type() == Type::Int {
            let (expr, info) =
                gen_aexpr(&expr_pcfg.a_expr, ctx, distribs, funcs, EXPR_FUEL);
            ctx.union_ret_rng(info.interval);
            ctx.set_return();
            StatementEnum::Statement(Statement::Ret(Some(Expr::AExpr(expr))))
        } else {
            ctx.set_return();
            StatementEnum::Statement(Statement::Ret(None))
        }
    } else {
        gen_stmt(pcfg, expr_pcfg, ctx, distribs, funcs, None, complexity)
    }
}

/// Generates a print statement to print an existing variable
pub(super) fn gen_print(ctx: &mut Context) -> Option<StatementEnum> {
    if (rnd::get_rng().gen_bool(0.7) || ctx.get_bvars().is_empty())
        && !ctx.get_avars().is_empty()
    {
        get_rand_avar(ctx).map(|x| {
            StatementEnum::Statement(Statement::Print(vec![Expr::AExpr(
                AExpr::Id(x),
            )]))
        })
    } else {
        get_rand_bvar(ctx).map(|x| {
            StatementEnum::Statement(Statement::Print(vec![Expr::BExpr(
                BExpr::Id(x),
            )]))
        })
    }
}

impl StatementTy for Statement {
    fn from(stmt: StatementEnum) -> Self {
        match stmt {
            StatementEnum::Statement(stmt) => stmt,
            StatementEnum::LoopStmt(..) => unreachable!(),
        }
    }

    fn to_enum(self) -> StatementEnum {
        StatementEnum::Statement(self)
    }

    fn is_print(&self) -> bool {
        matches!(self, Self::Print(..))
    }
}

impl StatementTy for LoopStatement {
    fn from(stmt: StatementEnum) -> Self {
        match stmt {
            StatementEnum::Statement(stmt) => Self::Stmt(stmt),
            StatementEnum::LoopStmt(stmt) => stmt,
        }
    }

    fn to_enum(self) -> StatementEnum {
        StatementEnum::LoopStmt(self)
    }

    fn is_print(&self) -> bool {
        matches!(self, Self::Stmt(Statement::Print(..)))
    }
}
