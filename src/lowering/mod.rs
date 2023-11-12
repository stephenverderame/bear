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

/// We use a hole in CFG adj list to indicate that it needs to be filled
/// when we pop the pending block stack.
///
/// This is used if we know one, but not both of the branch targets.
const CFG_HOLE_ID: usize = usize::MAX;

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
    ///
    /// A delim should be pushed to the stack before a new scope is entered.
    /// The blocks should be popped up to and including the delim
    /// by the same syntax construct which pushed the delim.
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

    /// Pops all blocks from the pending block stack
    /// and adds an outgoing edge from each of them to the specified block.
    /// # Arguments
    /// * `starting_block_id` - The block id of the block being started
    #[must_use]
    fn connect_to_block(mut self, starting_block_id: usize) -> Self {
        while let Some(PendingBlockEntry::Block(block_id)) =
            self.pending_block_stack.pop()
        {
            match self.cfg.adj_lst.get_mut(&block_id) {
                Some(CfgEdgeTo::Branch {
                    true_node,
                    false_node,
                }) if *true_node == CFG_HOLE_ID
                    && *false_node != CFG_HOLE_ID =>
                {
                    *true_node = starting_block_id;
                }
                Some(CfgEdgeTo::Branch {
                    true_node,
                    false_node,
                }) if *false_node == CFG_HOLE_ID
                    && *true_node != CFG_HOLE_ID =>
                {
                    *false_node = starting_block_id;
                }
                None => {
                    self.cfg
                        .adj_lst
                        .insert(block_id, CfgEdgeTo::Next(starting_block_id));
                }
                Some(x) => {
                    panic!("block {block_id} already has an edge: {x:?}")
                }
            }
        }
        self
    }

    /// Starts a new block by popping all blocks from the pending block stack
    /// and adding an outgoing edge from each of them to the current block.
    #[must_use]
    fn connect_next_block(self) -> Self {
        let cur_id = self.cur_block_id;
        self.connect_to_block(cur_id)
    }

    /// Returns a fresh temporary variable name.
    fn fresh_temp(&mut self) -> String {
        let temp_name = format!("_t{}", self.cur_temp_id);
        self.cur_temp_id += 1;
        temp_name
    }

    /// Returns a mutable reference to the current block.
    /// # Panics
    /// Panics if the current block is not a basic block.
    fn get_cur_block(&mut self) -> &mut BasicBlock {
        &mut self.cur_block
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
    r = r.connect_to_block(CFG_END_ID);
    r.cfg
}
