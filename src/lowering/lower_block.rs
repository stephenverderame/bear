use bril_rs::{EffectOps, Instruction, Type, ValueOps};
use cfg::{CfgEdgeTo, CfgNode};

use crate::{
    bare_c::{AExpr, BExpr, Block, DuffsInfo, LoopStatement, Pretty},
    generator::StatementTy,
    lowering::flattening::{flatten_bexpr, FlattenResult},
};

use super::{
    flattening::flatten_aexpr,
    lower_stmt::{add_instrs_to_result, lower_any_stmt},
    LowerResult, PendingBlockEntry, CFG_HOLE_ID,
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

/// Lower's an if. The returned `LowerResult`'s current block is the block
/// immediately after the if.
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
    r = r.connect_next_block();
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

/// Adds an instruction to the current block, incrementing the instruction id
/// counter
#[must_use]
fn add_instr_to_cur_block(i: Instruction, mut r: LowerResult) -> LowerResult {
    let next_instr_id = r.cfg.next_instr_id;
    r.get_cur_block().instrs.push((next_instr_id, i));
    r.cfg.next_instr_id += 1;
    r
}

/// Lowers the preheader block of a loop. This function finished the preheader
/// block and returns the id of the preheader block. It **DOES NOT**
/// set the blocks outgoing edge nor does it add the block to the pending block
/// stack.
fn lower_preheader(
    var: &str,
    init: AExpr,
    mut r: LowerResult,
) -> (usize, LowerResult) {
    let init_res;
    (r, init_res) = add_instrs_to_result(
        flatten_aexpr(init, FlattenResult::new(r.cur_temp_id)),
        r,
    );
    r = add_instr_to_cur_block(
        Instruction::Value {
            args: vec![init_res.unwrap()],
            dest: var.to_string(),
            funcs: vec![],
            labels: vec![],
            op: ValueOps::Id,
            op_type: Type::Int,
            pos: None,
        },
        r,
    );
    let preheader_id = r.cur_block_id;
    (preheader_id, r.finish_block(false))
}

/// Lowers the header block of a loop. This function finished the header block
/// and returns the id of the header block.
///
/// It **DOES NOT** set the blocks outgoing edge nor does it add the block to
/// the pending block stack.
fn lower_header(
    var: &str,
    limit: AExpr,
    is_inc: bool,
    mut r: LowerResult,
) -> (usize, LowerResult) {
    let limit_res;
    (r, limit_res) = add_instrs_to_result(
        flatten_aexpr(limit, FlattenResult::new(r.cur_temp_id)),
        r,
    );
    let cmp_res = r.fresh_temp();
    r = add_instr_to_cur_block(
        Instruction::Value {
            args: vec![var.to_string(), limit_res.unwrap()],
            dest: cmp_res.clone(),
            funcs: vec![],
            labels: vec![],
            op: if is_inc { ValueOps::Lt } else { ValueOps::Gt },
            op_type: Type::Bool,
            pos: None,
        },
        r,
    );
    r.cur_block.terminator = Some((
        r.cfg.next_instr_id,
        // we can put holes here because all branch targets will be resolved
        // by the edges in the CFG during cfg2src
        branch_instr(cmp_res, CFG_HOLE_ID, CFG_HOLE_ID),
    ));
    r.cfg.next_instr_id += 1;
    let header_id = r.cur_block_id;
    (header_id, r.finish_block(false))
}

/// Lowers the step block of a loop. This function finished the step block
/// and does not set the blocks outgoing edge.
///
/// It **DOES NOT** set the blocks outgoing edge nor does it add the block to
/// the pending block stack.
fn lower_step(
    var: &str,
    step: AExpr,
    mut r: LowerResult,
) -> (usize, LowerResult) {
    let step_res;
    (r, step_res) = add_instrs_to_result(
        flatten_aexpr(step, FlattenResult::new(r.cur_temp_id)),
        r,
    );
    // let op = if is_inc { ValueOps::Add } else { ValueOps::Sub };
    r = add_instr_to_cur_block(
        Instruction::Value {
            args: vec![var.to_string(), step_res.unwrap()],
            dest: var.to_string(),
            funcs: vec![],
            labels: vec![],
            // decreasing loop will have a negative step
            op: ValueOps::Add,
            op_type: Type::Int,
            pos: None,
        },
        r,
    );
    let step_id = r.cur_block_id;
    (step_id, r.finish_block(false))
}

/// Lowers a for loop. The returned `LowerResult`'s current block is the block
/// immediately following the for loop.
fn lower_for(
    var: &str,
    init: AExpr,
    limit: AExpr,
    step: AExpr,
    body: Vec<Block<LoopStatement>>,
    is_inc: bool,
    mut r: LowerResult,
) -> LowerResult {
    // header -> preheader
    let preheader_id;
    (preheader_id, r) = lower_preheader(var, init, r);
    let header_id;
    (header_id, r) = lower_header(var, limit, is_inc, r);
    r.cfg
        .adj_lst
        .insert(preheader_id, CfgEdgeTo::Next(header_id));

    // step -> header
    let step_block;
    (step_block, r) = lower_step(var, step, r);
    r.cfg.adj_lst.insert(step_block, CfgEdgeTo::Next(header_id));
    // break and continue, continue goes to the step block.
    // break goes to an empty block that will just transition
    // to the block after the loop
    let break_block = r.cur_block_id;
    r = r.finish_block(false);
    r.continue_blocks.push(step_block);
    r.break_blocks.push(break_block);

    // handle body
    r.pending_block_stack.push(PendingBlockEntry::Delim);
    let body_block = r.cur_block_id;
    r = lower_blocks(body, r).finish_block(true);
    // pop and connext edges to step block
    r = r.connect_to_block(step_block);
    r.break_blocks.pop();
    r.continue_blocks.pop();

    // connect unresolved edges to the next block (after the loop)
    let next_block_id = r.cur_block_id;
    r.cfg.adj_lst.insert(
        header_id,
        CfgEdgeTo::Branch {
            true_node: body_block,
            false_node: next_block_id,
        },
    );
    r.cfg
        .adj_lst
        .insert(break_block, CfgEdgeTo::Next(next_block_id));
    r
}

/// Generates blocks for branches which test whether the guard is equal to
/// each of the case guards.
///
/// Each branch has a hole set for the `true` side of the branch CFG edge which
/// must be filled in by the caller. The next block created should be the default
/// body block since the last branch will fallthrough to it.
fn switch_case_test_block(
    case_guards: Vec<AExpr>,
    guard_name: &str,
    mut r: LowerResult,
) -> (LowerResult, Vec<usize>) {
    let mut case_test_start_blocks = vec![];
    for guard_val in case_guards {
        let val_name;
        (r, val_name) = add_instrs_to_result(
            flatten_aexpr(guard_val, FlattenResult::new(r.cur_temp_id)),
            r,
        );
        let cmp_res = r.fresh_temp();
        r = add_instr_to_cur_block(
            Instruction::Value {
                args: vec![guard_name.to_string(), val_name.unwrap()],
                dest: cmp_res.clone(),
                funcs: vec![],
                labels: vec![],
                op: ValueOps::Eq,
                op_type: Type::Bool,
                pos: None,
            },
            r,
        );
        r.cur_block.terminator = Some((
            r.cfg.next_instr_id,
            branch_instr(cmp_res, CFG_HOLE_ID, r.cur_block_id + 1),
        ));
        r.cfg.next_instr_id += 1;
        r.cfg.adj_lst.insert(
            r.cur_block_id,
            CfgEdgeTo::Branch {
                true_node: CFG_HOLE_ID,
                false_node: r.cur_block_id + 1,
            },
        );
        case_test_start_blocks.push(r.cur_block_id);
        r = r.finish_block(false);
    }
    (r, case_test_start_blocks)
}

/// Generates the blocks for the body of each case in a switch statement.
/// If `fallthrough` is true, then the last block will fallthrough to the next
/// case and the final block will fallthrough to `default_block`.
/// Otherwise, terminal blocks of each case will be pushed to the pending
/// block stack.
fn switch_body_blocks<S: StatementTy + Pretty>(
    case_blocks: Vec<Vec<Block<S>>>,
    fallthrough: bool,
    default_block: usize,
    mut r: LowerResult,
) -> (LowerResult, Vec<usize>) {
    let mut body_start_blocks = vec![];
    let num_blocks = case_blocks.len();
    for (i, case_block) in case_blocks.into_iter().enumerate() {
        body_start_blocks.push(r.cur_block_id);
        r = lower_blocks(case_block, r).finish_block(true);
        if fallthrough {
            if let PendingBlockEntry::Block(body_end_block) =
                r.pending_block_stack.pop().unwrap()
            {
                let fallthrough_block = if i == num_blocks - 1 {
                    // last fallthrough goes to the default block
                    default_block
                } else {
                    r.cur_block_id
                };
                r.cfg
                    .adj_lst
                    .insert(body_end_block, CfgEdgeTo::Next(fallthrough_block));
            } else {
                panic!("expected block")
            }
        }
    }
    (r, body_start_blocks)
}

/// Lowers a switch statement and returns the lower result, id of the first block
/// in the series of case body blocks, and the last guard block id.
///
/// After this function is called, the resulting `LowerResult`'s current block
/// is the block immediately following the switch
fn lower_switch<S: StatementTy + Pretty>(
    guard: AExpr,
    cases: Vec<(AExpr, Vec<Block<S>>)>,
    default: Vec<Block<S>>,
    fallthrough: bool,
    mut r: LowerResult,
) -> (LowerResult, usize) {
    let (case_guards, case_blocks): (Vec<_>, Vec<_>) =
        cases.into_iter().unzip();
    let guard_name;
    (r, guard_name) = add_instrs_to_result(
        flatten_aexpr(guard, FlattenResult::new(r.cur_temp_id)),
        r,
    );
    let guard_name = guard_name.unwrap();
    let case_test_start_blocks;
    (r, case_test_start_blocks) =
        switch_case_test_block(case_guards, &guard_name, r);
    // default case so that this is the fallthrough of the last branch
    r.pending_block_stack.push(PendingBlockEntry::Delim);
    let default_block = r.cur_block_id;
    r = lower_blocks(default, r).finish_block(true);
    let body_start_blocks;
    (r, body_start_blocks) =
        switch_body_blocks(case_blocks, fallthrough, default_block, r);
    r = r.connect_next_block();
    let cases_start_block =
        body_start_blocks.first().copied().unwrap_or(default_block);
    for (guard_id, body_id) in
        case_test_start_blocks.into_iter().zip(body_start_blocks)
    {
        if let Some(CfgEdgeTo::Branch {
            true_node,
            false_node,
        }) = r.cfg.adj_lst.get_mut(&guard_id)
        {
            assert_eq!(*true_node, CFG_HOLE_ID);
            *true_node = body_id;
            assert_ne!(*false_node, CFG_HOLE_ID);
        } else {
            panic!("expected branch")
        }
    }
    (r, cases_start_block)
}

/// Lowers Duff's Device into BRIL. The returned `LowerResult`'s current
/// block is the block immediately after Duff's Device.
fn lower_duffs(info: DuffsInfo, mut r: LowerResult) -> LowerResult {
    // preheader/start
    let preheader_id;
    (preheader_id, r) = lower_preheader(&info.var, info.init, r);
    let step_block;
    (step_block, r) = lower_step(&info.var, info.step, r);
    let break_block = r.cur_block_id;
    r = r.finish_block(false);

    r.break_blocks.push(break_block);
    r.continue_blocks.push(step_block);
    let body_start_id;
    let test_start_id = r.cur_block_id;
    (r, body_start_id) =
        lower_switch(info.guard, info.bodies, info.default, true, r);
    r.break_blocks.pop();
    r.continue_blocks.pop();
    let end_switch_id = r.cur_block_id;
    r.cfg
        .adj_lst
        .insert(end_switch_id, CfgEdgeTo::Next(step_block));
    r = r.finish_block(false);

    r.cfg
        .adj_lst
        .insert(preheader_id, CfgEdgeTo::Next(test_start_id));

    // step -> header
    let header_id;
    (header_id, r) = lower_header(&info.var, info.limit, info.is_inc, r);
    r.cfg.adj_lst.insert(step_block, CfgEdgeTo::Next(header_id));
    r.cfg.adj_lst.insert(
        header_id,
        CfgEdgeTo::Branch {
            true_node: body_start_id,
            false_node: break_block,
        },
    );

    let final_block_id = r.cur_block_id;
    r.cfg
        .adj_lst
        .insert(break_block, CfgEdgeTo::Next(final_block_id));
    r
}

/// Lowers a try-catch block. The returned `LowerResult`'s current block is the
/// block immediately after the try-catch.
fn lower_try_catch<S: StatementTy + Pretty>(
    try_block: Vec<Block<S>>,
    catch_block: Vec<Block<S>>,
    catch_name: Option<String>,
    mut r: LowerResult,
) -> LowerResult {
    let try_start_id = r.cur_block_id;
    r = r.finish_block(false);

    let catch_start_id = r.cur_block_id;
    r.pending_block_stack.push(PendingBlockEntry::Delim);
    r = lower_blocks(catch_block, r).finish_block(true);

    // try block
    r.catch_blocks.push((catch_name, catch_start_id));
    let try_block_start_id = r.cur_block_id;
    r = lower_blocks(try_block, r).finish_block(true);
    r.catch_blocks.pop();

    r = r.connect_next_block();
    r.cfg
        .adj_lst
        .insert(try_start_id, CfgEdgeTo::Next(try_block_start_id));
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
        Block::For {
            var,
            init,
            limit,
            step,
            body,
            is_inc,
        } => lower_for(&var, init, limit, step, body, is_inc, r),
        Block::Switch {
            guard,
            cases,
            default,
        } => lower_switch(guard, cases, default, false, r).0,
        Block::Duffs(info) => lower_duffs(info, r),
        Block::TryCatch {
            try_block,
            catch_block,
            catch_name,
        } => lower_try_catch(try_block, catch_block, catch_name, r),
        Block::While { .. } => todo!(),
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
