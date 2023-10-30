use super::interval::Interval;
use super::{ExprInfo, StepType, Type};
use crate::bare_c::{AExpr, BExpr, StmtBlock};
use std::collections::HashMap;
use std::collections::HashSet;

/// A `StackFrame` keeps track of the variables and expressions available at a
/// given point in the program generation. The `StackFrame` also keeps track of
/// the current loop and try-catch nesting level so that the correct number of
/// breaks and continues can be generated, along with any pending expressions
/// that need to be generated.
pub(super) struct StackFrame {
    nested_trys: usize,
    nested_loops: usize,
    avars: HashMap<String, Interval>,
    bvars: HashSet<String>,
    pending_step: StepType,
    pending_ret: bool,
    available_aexprs: HashMap<AExpr, ExprInfo>,
    available_bexprs: HashMap<BExpr, Vec<String>>,
    ret_type: Option<Type>,
    is_loop: bool,
}

impl StackFrame {
    fn new_child(other: &Self) -> Self {
        Self {
            nested_trys: other.nested_trys,
            nested_loops: other.nested_loops,
            avars: HashMap::new(),
            bvars: HashSet::new(),
            pending_step: other.pending_step,
            pending_ret: other.pending_ret,
            available_aexprs: HashMap::new(),
            available_bexprs: HashMap::new(),
            ret_type: other.ret_type,
            is_loop: other.is_loop,
        }
    }
}

impl Default for StackFrame {
    fn default() -> Self {
        Self {
            nested_trys: Default::default(),
            nested_loops: Default::default(),
            avars: HashMap::default(),
            bvars: HashSet::default(),
            pending_step: StepType::None,
            pending_ret: Default::default(),
            available_aexprs: HashMap::default(),
            available_bexprs: HashMap::default(),
            ret_type: Option::default(),
            is_loop: false,
        }
    }
}
pub(super) type FunctionDecl = (String, Vec<Type>, Type);
pub(super) type FuncList = HashMap<FunctionDecl, StmtBlock>;

/// A `Context` is a stack of `StackFrame`s which keep track of the variables
/// and expressions available at a given point in the program generation. The
/// `Context` also keeps track of the current loop and try-catch nesting level
/// so that the correct number of breaks and continues can be generated.
pub(super) struct Context<'a> {
    cur: StackFrame,
    parent: Option<&'a Context<'a>>,
}

impl<'a> Context<'a> {
    pub fn get_avars(&self) -> Vec<String> {
        self.parent
            .as_ref()
            .map(|x| x.get_avars())
            .unwrap_or_default()
            .into_iter()
            .chain(self.cur.avars.keys().cloned())
            .collect()
    }

    pub fn get_bvars(&self) -> Vec<String> {
        self.parent
            .as_ref()
            .map(|x| x.get_bvars())
            .unwrap_or_default()
            .into_iter()
            .chain(self.cur.bvars.iter().cloned())
            .collect()
    }

    pub fn get_aexprs(&self) -> Vec<(AExpr, ExprInfo)> {
        self.parent
            .as_ref()
            .map(|x| x.get_aexprs())
            .unwrap_or_default()
            .into_iter()
            .chain(
                self.cur
                    .available_aexprs
                    .iter()
                    .map(|x| (x.0.clone(), x.1.clone())),
            )
            .collect()
    }

    pub fn get_bexprs(&self) -> Vec<(BExpr, Vec<String>)> {
        self.parent
            .as_ref()
            .map(|x| x.get_bexprs())
            .unwrap_or_default()
            .into_iter()
            .chain(
                self.cur
                    .available_bexprs
                    .iter()
                    .map(|(expr, vars)| (expr.clone(), vars.clone())),
            )
            .collect()
    }

    /// Gets the context right before a loop in the stack frame
    /// This may be the context before the nearest loop, or a parent loop
    #[deprecated(note = "Fix this")]
    pub fn loop_inv(&self) -> Option<&Context<'a>> {
        // TODO: fix this
        if self.cur.is_loop {
            Some(self)
        } else {
            self.parent.as_ref().and_then(|p| p.loop_inv())
        }
    }

    /// Constructs a new context with the current context as its parent
    pub fn child_frame(&'a self) -> Self {
        Self {
            cur: StackFrame::new_child(&self.cur),
            parent: Some(self),
        }
    }

    pub(super) fn lookup_var(&self, var: &str) -> Option<ExprInfo> {
        self.cur
            .avars
            .get(var)
            .map(|x| ExprInfo {
                vars: vec![var.to_string()],
                interval: *x,
            })
            .or_else(|| self.parent.and_then(|p| p.lookup_var(var)))
    }

    pub(super) fn make_root() -> Self {
        Self {
            cur: StackFrame::default(),
            parent: None,
        }
    }

    pub(super) fn new_aexpr(&mut self, expr: AExpr, info: ExprInfo) {
        self.cur.available_aexprs.insert(expr, info);
    }

    pub(super) fn new_bexpr(&mut self, expr: BExpr, vars: Vec<String>) {
        self.cur.available_bexprs.insert(expr, vars);
    }

    pub(super) fn new_avar(&mut self, name: &str, interval: Interval) {
        self.cur.avars.insert(name.to_string(), interval);
    }

    pub(super) fn new_bvar(&mut self, name: &str) {
        self.cur.bvars.insert(name.to_string());
    }
}
