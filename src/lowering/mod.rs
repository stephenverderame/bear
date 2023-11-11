mod flattening;
mod lower_block;
mod lower_stmt;

use cfg::{BasicBlock, Cfg, CfgEdgeTo, CfgNode, CFG_END_ID, CFG_FIRST_FREE_ID};

use crate::{
    bare_c::{Block, Pretty},
    generator::StatementTy,
};

use self::lower_block::lower_blocks;

enum PendingBlockEntry {
    Block(usize),
    Delim,
}

struct LowerResult {
    pub cfg: Cfg,
    /// Block id of the current block we're working on
    pub cur_block_id: usize,
    /// Current block we're working on
    pub cur_block: BasicBlock,
    /// Stack of tuple of catch argument names and block ids of catch blocks.
    /// Matches the heirarchy of try-catch nests.
    pub catch_blocks: Vec<(Option<String>, usize)>,
    /// Stack of block ids of continue blocks. Matches the catch_blocks stack
    /// and the heirarchy of loop nests.
    pub continue_blocks: Vec<usize>,
    /// Stack of block ids of break blocks. Matches the continue_blocks stack
    /// and the heirarchy of loop nests.
    pub break_blocks: Vec<usize>,
    /// Current temporary variable id.
    pub cur_temp_id: usize,
    /// Stack of block ids that are waiting to be added to the cfg adjacency list.
    /// Each block's children pending blocks will be separated by a `Delim` entry.
    pub pending_block_stack: Vec<PendingBlockEntry>,
}

impl LowerResult {
    /// Adds the current block to the cfg and resets the current block.
    /// Does not set add an outgoing edge from the current block.
    /// Adds the block to the pending block stack if `push_to_stack` is true.
    #[must_use]
    fn finish_block(mut self, push_to_stack: bool) -> Self {
        self.cfg
            .blocks
            .insert(self.cur_block_id, CfgNode::Block(self.cur_block));
        if push_to_stack {
            self.pending_block_stack
                .push(PendingBlockEntry::Block(self.cur_block_id));
        }
        self.cur_block_id += 1;
        self.cur_block = BasicBlock::default();
        self
    }

    /// Adds the current block to the cfg and resets the current block.
    /// Sets an outgoing edge from the current block to the given block.
    /// Does not add the block to the pending block stack.
    #[must_use]
    fn finish_tail_block(mut self, e: CfgEdgeTo) -> Self {
        self.cfg.adj_lst.insert(self.cur_block_id, e);
        self.cfg
            .blocks
            .insert(self.cur_block_id, CfgNode::Block(self.cur_block));
        self.cur_block_id += 1;
        self.cur_block = BasicBlock::default();
        self
    }

    /// Starts a new block by popping all blocks from the pending block stack
    /// and adding an outgoing edge from each of them to the current block.
    #[must_use]
    fn start_block(mut self) -> Self {
        while let Some(PendingBlockEntry::Block(block_id)) =
            self.pending_block_stack.pop()
        {
            assert!(self.cfg.adj_lst.get(&block_id).is_none());
            self.cfg
                .adj_lst
                .insert(block_id, CfgEdgeTo::Next(self.cur_block_id));
        }
        self
    }
}

impl LowerResult {
    #[must_use]
    pub fn new() -> Self {
        Self {
            cfg: Cfg::default(),
            cur_block_id: CFG_FIRST_FREE_ID,
            cur_block: BasicBlock::default(),
            catch_blocks: vec![],
            continue_blocks: vec![],
            break_blocks: vec![],
            cur_temp_id: 0,
            pending_block_stack: vec![],
        }
    }
}

/// Lowers a program into a CFG.
pub fn lower<S: StatementTy + Pretty>(blocks: Vec<Block<S>>) -> Cfg {
    let mut r = LowerResult::new();
    r = lower_blocks(blocks, r).finish_block(true);
    while let Some(node) = r.pending_block_stack.pop() {
        if let PendingBlockEntry::Block(block_id) = node {
            assert!(r.cfg.adj_lst.get(&block_id).is_none());
            r.cfg.adj_lst.insert(block_id, CfgEdgeTo::Next(CFG_END_ID));
        }
    }
    r.cfg
}
