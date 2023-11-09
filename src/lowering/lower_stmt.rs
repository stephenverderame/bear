use bril_rs::{EffectOps, Instruction, Type, ValueOps};
use cfg::{BasicBlock, Cfg};

use crate::bare_c::{Expr, LoopStatement, Statement};

use super::flattening::{flatten_expr, FlattenResult};

pub(super) struct LowerResult {
    pub(super) cfg: Cfg,
    pub(super) block_id: usize,
    pub(super) cur_block: BasicBlock,
    pub(super) catch_blocks: Vec<(Option<String>, usize)>,
    pub(super) continue_blocks: Vec<usize>,
    pub(super) break_blocks: Vec<usize>,
}

/// Adds instructions of the given `FlattenResult` to the
/// current block of `LowerResult` and returns the updated
/// `LowerResult` and the top of the result stack of the `FlattenResult`.
fn add_instrs_to_result(
    mut res: LowerResult,
    mut flatten_result: FlattenResult,
) -> (LowerResult, Option<String>) {
    let instr_len = flatten_result.instrs.len() as u64;
    res.cur_block.instrs.extend(
        flatten_result
            .instrs
            .into_iter()
            .enumerate()
            .map(|(i, instr)| (res.cfg.next_instr_id + i as u64, instr)),
    );
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
    let (mut res, src) =
        add_instrs_to_result(res, flatten_expr(src, FlattenResult::default()));
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
            res,
            flatten_expr(e, FlattenResult::default()),
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
    res
}

fn lower_print(arg_exprs: Vec<Expr>, mut res: LowerResult) -> LowerResult {
    let mut args = vec![];
    for expr in arg_exprs {
        let (r, src) = add_instrs_to_result(
            res,
            flatten_expr(expr, FlattenResult::default()),
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
            res,
            flatten_expr(e, FlattenResult::default()),
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
    res
}

fn lower_stmt(stmt: Statement, mut res: LowerResult) -> LowerResult {
    match stmt {
        Statement::Assign { dest, src } => lower_assign(dest, src, res),
        Statement::PCall(..) => unimplemented!(),
        Statement::Ret(e) => lower_ret(e, res),
        Statement::Print(exprs) => lower_print(exprs, res),
        Statement::Throw(n, e) => lower_throw(n, e, res),
    }
}

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
            res
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
            res
        }
        LoopStatement::Step(stmt) | LoopStatement::Stmt(stmt) => {
            lower_stmt(stmt, res)
        }
    }
}
