use bril_rs::{EffectOps, Instruction, Type, ValueOps};
use cfg::{CfgEdgeTo, CFG_END_ID};

use crate::{
    bare_c::{Expr, LoopStatement, Pretty, Statement},
    generator::{StatementEnum, StatementTy},
};

use super::flattening::{flatten_expr, FlattenResult};
use super::LowerResult;

/// Adds instructions of the given `FlattenResult` to the
/// current block of `LowerResult` and returns the updated
/// `LowerResult` and the top of the result stack of the `FlattenResult`.
#[must_use]
pub(super) fn add_instrs_to_result(
    mut flatten_result: FlattenResult,
    mut res: LowerResult,
) -> (LowerResult, Option<String>) {
    let instr_len = flatten_result.instrs.len() as u64;
    res.cur_block.instrs.extend(
        flatten_result
            .instrs
            .into_iter()
            .enumerate()
            .map(|(i, instr)| (res.cfg.next_instr_id + i as u64, instr)),
    );
    res.cur_temp_id = flatten_result.cur_temp_id;
    res.cfg.next_instr_id += instr_len;
    (res, flatten_result.result_stack.pop())
}

/// Gets the type of an expression.
const fn expr_type(expr: &Expr) -> Type {
    match expr {
        Expr::AExpr(_) => Type::Int,
        Expr::BExpr(_) => Type::Bool,
    }
}

fn lower_assign(dest: String, src: Expr, res: LowerResult) -> LowerResult {
    let op_type = expr_type(&src);
    let (mut res, src) = add_instrs_to_result(
        flatten_expr(src, FlattenResult::new(res.cur_temp_id)),
        res,
    );
    res.cur_block.instrs.push((
        res.cfg.next_instr_id,
        Instruction::Value {
            op: ValueOps::Id,
            args: vec![src.unwrap()],
            funcs: vec![],
            labels: vec![],
            pos: None,
            op_type,
            dest,
        },
    ));
    res.cfg.next_instr_id += 1;
    res
}

fn lower_ret(e: Option<Expr>, mut res: LowerResult) -> LowerResult {
    #[allow(clippy::option_if_let_else)]
    let src = if let Some(e) = e {
        let (r, src) = add_instrs_to_result(
            flatten_expr(e, FlattenResult::new(res.cur_temp_id)),
            res,
        );
        res = r;
        Some(src.unwrap())
    } else {
        None
    };
    res.cur_block.terminator = Some((
        res.cfg.next_instr_id,
        Instruction::Effect {
            op: EffectOps::Return,
            args: src.map_or_else(Vec::new, |src| vec![src]),
            funcs: vec![],
            labels: vec![],
            pos: None,
        },
    ));
    res.cfg.next_instr_id += 1;
    res.finish_tail_block(CfgEdgeTo::Next(CFG_END_ID))
}

fn lower_print(arg_exprs: Vec<Expr>, mut res: LowerResult) -> LowerResult {
    let mut args = vec![];
    for expr in arg_exprs {
        let (r, src) = add_instrs_to_result(
            flatten_expr(expr, FlattenResult::new(res.cur_temp_id)),
            res,
        );
        args.push(src.unwrap());
        res = r;
    }
    res.cur_block.instrs.push((
        res.cfg.next_instr_id,
        Instruction::Effect {
            op: EffectOps::Print,
            args,
            funcs: vec![],
            labels: vec![],
            pos: None,
        },
    ));
    res.cfg.next_instr_id += 1;
    res
}

/// Lowers a Bare C throw into a BRIL instruction sequence.
fn lower_throw(n: usize, e: Option<Expr>, mut res: LowerResult) -> LowerResult {
    if let Some(e) = e {
        let op_type = expr_type(&e);
        let (r, src) = add_instrs_to_result(
            flatten_expr(e, FlattenResult::new(res.cur_temp_id)),
            res,
        );
        res = r;
        let dest = res.catch_blocks[res.catch_blocks.len() - 1 - n]
            .0
            .clone()
            .unwrap();
        res.cur_block.instrs.push((
            res.cfg.next_instr_id,
            Instruction::Value {
                op: ValueOps::Id,
                dest,
                args: vec![src.unwrap()],
                funcs: vec![],
                labels: vec![],
                pos: None,
                op_type,
            },
        ));
        res.cfg.next_instr_id += 1;
    }
    let target = res.catch_blocks[res.catch_blocks.len() - 1 - n].1;
    res.cur_block.terminator = Some((
        res.cfg.next_instr_id,
        Instruction::Effect {
            op: EffectOps::Jump,
            args: vec![],
            funcs: vec![],
            labels: vec![format!("block.{target}")],
            pos: None,
        },
    ));
    res.cfg.next_instr_id += 1;
    res.finish_tail_block(CfgEdgeTo::Next(target))
}

#[must_use]
fn lower_stmt(stmt: Statement, res: LowerResult) -> LowerResult {
    match stmt {
        Statement::Assign { dest, src } => lower_assign(dest, src, res),
        Statement::PCall(..) => unimplemented!(),
        Statement::Ret(e) => lower_ret(e, res),
        Statement::Print(exprs) => lower_print(exprs, res),
        Statement::Throw(n, e) => lower_throw(n, e, res),
    }
}

#[must_use]
fn lower_loop_stmt(stmt: LoopStatement, mut res: LowerResult) -> LowerResult {
    match stmt {
        LoopStatement::Break(n) => {
            let target = res.break_blocks[res.break_blocks.len() - 1 - n];
            res.cur_block.terminator = Some((
                res.cfg.next_instr_id,
                Instruction::Effect {
                    op: EffectOps::Jump,
                    args: vec![],
                    funcs: vec![],
                    labels: vec![format!("block.{target}")],
                    pos: None,
                },
            ));
            res.cfg.next_instr_id += 1;
            res.finish_tail_block(CfgEdgeTo::Next(target))
        }
        LoopStatement::Continue(n) => {
            let target = res.continue_blocks[res.continue_blocks.len() - 1 - n];
            res.cur_block.terminator = Some((
                res.cfg.next_instr_id,
                Instruction::Effect {
                    op: EffectOps::Jump,
                    args: vec![],
                    funcs: vec![],
                    labels: vec![format!("block.{target}")],
                    pos: None,
                },
            ));
            res.cfg.next_instr_id += 1;
            res.finish_tail_block(CfgEdgeTo::Next(target))
        }
        LoopStatement::Step(stmt) | LoopStatement::Stmt(stmt) => {
            lower_stmt(stmt, res)
        }
    }
}

/// Lowers a statement
#[must_use]
pub(super) fn lower_any_stmt<S: StatementTy + Pretty>(
    s: S,
    r: LowerResult,
) -> LowerResult {
    match s.to_enum() {
        StatementEnum::Statement(s) => lower_stmt(s, r),
        StatementEnum::LoopStmt(s) => lower_loop_stmt(s, r),
    }
}
