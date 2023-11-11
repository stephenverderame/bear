use bril_rs::{EffectOps, Instruction};
use cfg::{CfgEdgeTo, CfgNode};

use crate::{
    bare_c::{BExpr, Block, Pretty},
    generator::StatementTy,
    lowering::flattening::{flatten_bexpr, FlattenResult},
};

use super::{
    lower_stmt::{add_instrs_to_result, lower_any_stmt},
    LowerResult, PendingBlockEntry,
};

/// Construct a branch instruction.
fn branch_instr(
    cond: String,
    true_node: usize,
    false_node: usize,
) -> Instruction {
    Instruction::Effect {
        args: vec![cond],
        funcs: vec![],
        labels: vec![
            format!("block.{true_node}"),
            format!("block.{false_node}"),
        ],
        op: EffectOps::Branch,
        pos: None,
    }
}

#[must_use]
fn lower_if<S: StatementTy + Pretty>(
    guard: BExpr,
    then: Vec<Block<S>>,
    otherwise: Vec<Block<S>>,
    mut r: LowerResult,
) -> LowerResult {
    let fr = flatten_bexpr(guard, FlattenResult::new(r.cur_temp_id));
    let if_header_id = r.cur_block_id;
    let res;
    (r, res) = add_instrs_to_result(fr, r);
    r = r.finish_block(false);
    // push delim
    r.pending_block_stack.push(PendingBlockEntry::Delim);
    let then_id = r.cur_block_id;
    r = lower_blocks(then, r).finish_block(true);
    let otherwise_id = r.cur_block_id;
    r = lower_blocks(otherwise, r).finish_block(true);
    // pop since last delim and have them connect to the new (empty) block
    r = r.start_block();
    if let Some(CfgNode::Block(b)) = r.cfg.blocks.get_mut(&if_header_id) {
        assert!(b.terminator.is_none());
        b.terminator = Some((
            r.cfg.next_instr_id,
            branch_instr(res.unwrap(), then_id, otherwise_id),
        ));
        r.cfg.next_instr_id += 1;
    }
    r.cfg.adj_lst.insert(
        if_header_id,
        CfgEdgeTo::Branch {
            true_node: then_id,
            false_node: otherwise_id,
        },
    );
    r
}

#[must_use]
pub(super) fn lower_block<S: StatementTy + Pretty>(
    b: Block<S>,
    r: LowerResult,
) -> LowerResult {
    match b {
        Block::Stmt(s) => lower_any_stmt(s, r),
        Block::Dead(b) => lower_blocks(b, r),
        Block::If {
            guard,
            then,
            otherwise,
        } => lower_if(guard, then, otherwise, r),
        _ => todo!(),
    }
}

/// Lowers a list of blocks.
#[must_use]
pub(super) fn lower_blocks<S: StatementTy + Pretty>(
    bs: Vec<Block<S>>,
    r: LowerResult,
) -> LowerResult {
    bs.into_iter().fold(r, |r, b| lower_block(b, r))
}
