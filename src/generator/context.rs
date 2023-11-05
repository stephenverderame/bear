use rand::distributions::{Alphanumeric, Uniform};
use rand::prelude::Distribution;
use rand::Rng;

use super::interval::Interval;
use super::{ExprInfo, StepType, Type};
use crate::bare_c::{AExpr, BExpr, StmtBlock};
use std::collections::HashMap;
use std::collections::HashSet;

/// Dataflow analysis facts for a dataflow analysis that is computed as
/// we are generating the program
#[derive(Default, Debug, Clone)]
pub(super) struct AnalysisFacts {
    avars: HashMap<String, Interval>,
    bvars: HashSet<String>,
    available_aexprs: HashMap<AExpr, ExprInfo>,
    available_bexprs: HashMap<BExpr, Vec<String>>,
    // variables that cannot be mutated by a non-loop invariant expression
    pinned: HashSet<String>,
}

impl AnalysisFacts {
    /// Performs the meet operation on two sets of analysis facts
    #[allow(clippy::similar_names)]
    pub fn meet(a: Self, b: &Self) -> Self {
        let mut avars = HashMap::new();
        for (k, v) in a.avars {
            if let Some(v2) = b.avars.get(&k) {
                avars.insert(k, v.union(*v2));
            }
        }
        let bvars = a
            .bvars
            .intersection(&b.bvars)
            .cloned()
            .collect::<HashSet<_>>();
        let mut available_aexprs = HashMap::new();
        for (k, v) in a.available_aexprs {
            if let Some(v2) = b.available_aexprs.get(&k) {
                available_aexprs.insert(k, v.meet(v2));
            }
        }
        let mut available_bexprs = HashMap::new();
        for (k, v) in a.available_bexprs {
            if let Some(v2) = b.available_bexprs.get(&k) {
                assert_eq!(&v, v2);
                available_bexprs.insert(k, v);
            }
        }
        let mut loop_invariant = HashSet::new();
        for k in a.pinned {
            if b.pinned.contains(&k) {
                loop_invariant.insert(k);
            }
        }
        Self {
            avars,
            bvars,
            available_aexprs,
            available_bexprs,
            pinned: loop_invariant,
        }
    }
}

/// A `StackFrame` keeps track of the variables and expressions available at a
/// given point in the program generation. The `StackFrame` also keeps track of
/// the current loop and try-catch nesting level so that the correct number of
/// breaks and continues can be generated, along with any pending expressions
/// that need to be generated.
pub(super) struct StackFrame {
    nests: StackLevel,
    /// Whether the closest loop counter must increment or decrement
    /// None if there is no pending step (if the counter has already been stepped)
    pending_step: StepType,
    /// Whether we need to return from the current function
    pending_ret: bool,
    ret_type: Type,
    facts: AnalysisFacts,
    ret_intval: Option<Interval>,
    /// Whether we can continue on the current path (no throws or returns)
    can_follow: bool,
    /// Whether the current path is dead
    is_dead: bool,
}

/// The `StackLevel` keeps track of the current nesting level of the stack
/// frame. This is used to determine the correct number of breaks and continues
/// to generate.
#[derive(Clone, Debug, Default)]
pub(super) struct StackLevel {
    nested_loops: usize,
    nested_trys: Vec<Type>,
}

impl StackFrame {
    fn new_child(&self) -> Self {
        Self {
            nests: self.nests.clone(),
            facts: self.facts.clone(),
            pending_step: self.pending_step,
            pending_ret: self.pending_ret,
            ret_type: self.ret_type,
            ret_intval: self.ret_intval,
            can_follow: self.can_follow,
            is_dead: self.is_dead,
        }
    }

    fn dead_child(&self) -> Self {
        Self {
            nests: self.nests.clone(),
            facts: AnalysisFacts::default(),
            pending_step: StepType::None,
            pending_ret: false,
            ret_type: Type::Void,
            ret_intval: None,
            can_follow: true,
            is_dead: true,
        }
    }

    fn try_child(&self, t: Type) -> Self {
        let mut c = self.new_child();
        c.nests.nested_trys.push(t);
        c
    }

    fn loop_child(&self, step: StepType, pin_prob: f64) -> Self {
        let mut c = self.new_child();
        for nm in self
            .facts
            .avars
            .keys()
            .chain(self.facts.bvars.iter())
            .filter(|&x| !self.facts.pinned.contains(x))
        {
            if rand::thread_rng().gen_range(0_f64..1_f64) < pin_prob.exp() {
                c.facts.pinned.insert(nm.clone());
            } else {
                break;
            }
        }
        c.nests.nested_loops += 1;
        c.pending_step = step;
        c
    }

    fn loop_inv_ctx(&self) -> Self {
        let mut c = self.new_child();
        c.facts.avars.retain(|k, _| self.facts.pinned.contains(k));
        c.facts.bvars.retain(|k| self.facts.pinned.contains(k));
        c.facts.available_aexprs.retain(|_, v| {
            v.vars.iter().all(|x| self.facts.pinned.contains(x))
        });
        c.facts
            .available_bexprs
            .retain(|_, v| v.iter().all(|x| self.facts.pinned.contains(x)));
        c
    }
}

impl StackFrame {
    /// Meets two stack frames of siblings
    /// Sibilings are two blocks on the same level of the call stack
    /// (i.e. they have the same parent such as the `then` and `else` blocks of an if statement)
    #[allow(clippy::needless_pass_by_value)]
    pub(super) fn meet(self, other: Self) -> Self {
        // we pass by value so we consume the stack frame which prevents
        // us from accidentally meeting and then mutating the same stack frame
        Self {
            nests: self.nests,
            facts: AnalysisFacts::meet(self.facts, &other.facts),
            pending_step: match (self.pending_step, other.pending_step) {
                (StepType::Inc, StepType::Dec)
                | (StepType::Dec, StepType::Inc) => {
                    panic!("Siblings have different pending steps")
                }
                (StepType::Inc | StepType::None, StepType::Inc)
                | (StepType::Inc, StepType::None) => StepType::Inc,
                (StepType::Dec | StepType::None, StepType::Dec)
                | (StepType::Dec, StepType::None) => StepType::Dec,
                (StepType::None, StepType::None) => StepType::None,
            },
            pending_ret: self.pending_ret || other.pending_ret,
            ret_type: self.ret_type,
            ret_intval: self.ret_intval,
            can_follow: self.can_follow || other.can_follow,
            is_dead: self.is_dead && other.is_dead,
        }
    }
}

impl Default for StackFrame {
    fn default() -> Self {
        Self {
            nests: StackLevel::default(),
            facts: AnalysisFacts::default(),
            pending_step: StepType::None,
            pending_ret: Default::default(),
            ret_type: Type::Void,
            ret_intval: Option::default(),
            can_follow: true,
            is_dead: false,
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
        self.cur.facts.avars.keys().cloned().collect()
    }

    pub fn get_mutable_avars(&self) -> Vec<String> {
        self.cur
            .facts
            .avars
            .iter()
            .filter_map(|(n, _)| {
                if self.cur.facts.pinned.contains(n) {
                    None
                } else {
                    Some(n.clone())
                }
            })
            .collect()
    }

    pub fn get_mutable_bvars(&self) -> Vec<String> {
        self.cur
            .facts
            .bvars
            .iter()
            .filter(|&n| !self.cur.facts.pinned.contains(n))
            .cloned()
            .collect()
    }

    pub fn get_bvars(&self) -> Vec<String> {
        self.cur.facts.bvars.iter().cloned().collect()
    }

    pub fn get_aexprs(&self) -> Vec<(AExpr, ExprInfo)> {
        self.cur
            .facts
            .available_aexprs
            .iter()
            .map(|(x, y)| (x.clone(), y.clone()))
            .collect()
    }

    pub fn get_bexprs(&self) -> Vec<(BExpr, Vec<String>)> {
        self.cur
            .facts
            .available_bexprs
            .iter()
            .map(|(x, y)| (x.clone(), y.clone()))
            .collect()
    }

    /// Gets the context right before a loop in the stack frame
    /// This may be the context before the nearest loop, or a parent loop
    pub fn loop_inv(&'a self) -> Context<'a> {
        if self.cur.nests.nested_loops == 0 {
            self.child_frame()
        } else {
            Self {
                cur: self.cur.loop_inv_ctx(),
                parent: Some(self),
            }
        }
    }

    /// Constructs a new context with the current context as its parent
    pub fn child_frame(&'a self) -> Self {
        Self {
            cur: StackFrame::new_child(&self.cur),
            parent: Some(self),
        }
    }

    /// Constructs a new context with the current context as its parent
    /// where no analysis facts are passed to the child
    pub fn dead_child_frame(&'a self) -> Self {
        Self {
            cur: StackFrame::dead_child(&self.cur),
            parent: Some(self),
        }
    }

    pub fn loop_child_frame(&'a self, step: StepType, pin_prob: f64) -> Self {
        Self {
            cur: self.cur.loop_child(step, pin_prob),
            parent: Some(self),
        }
    }

    pub(super) fn lookup_var(&self, var: &str) -> Option<ExprInfo> {
        self.cur
            .facts
            .avars
            .get(var)
            .map(|x| ExprInfo {
                vars: vec![var.to_string()],
                interval: *x,
            })
            .or_else(|| self.parent.and_then(|p| p.lookup_var(var)))
    }

    /// Make a new root context
    pub(super) fn make_root() -> Self {
        Self {
            cur: StackFrame::default(),
            parent: None,
        }
    }

    pub(super) fn new_aexpr(&mut self, expr: AExpr, info: ExprInfo) {
        self.cur.facts.available_aexprs.insert(expr, info);
    }

    pub(super) fn new_bexpr(&mut self, expr: BExpr, vars: Vec<String>) {
        self.cur.facts.available_bexprs.insert(expr, vars);
    }

    pub(super) fn new_avar(&mut self, name: &str, info: &ExprInfo) {
        self.cur.facts.avars.insert(name.to_string(), info.interval);
    }

    pub(super) fn new_bvar(&mut self, name: &str, _: Vec<String>) {
        self.cur.facts.bvars.insert(name.to_string());
    }

    /// Kills all available expressions that contain the given variable
    pub(super) fn kill_available_exprs(&mut self, name: &str, typ: Type) {
        if typ == Type::Int {
            self.cur
                .facts
                .available_aexprs
                .retain(|_, v| !v.vars.contains(&name.to_string()));
        } else {
            self.cur
                .facts
                .available_bexprs
                .retain(|_, v| !v.contains(&name.to_string()));
        }
    }

    /// Returns a unique variable name
    pub(super) fn new_var(&self) -> String {
        let mut rng = rand::thread_rng();
        let mut name: String = std::iter::repeat(())
            .map(|()| rng.sample(Alphanumeric))
            .map(char::from)
            .take(7)
            .collect();
        let mut i = 0;
        while self.cur.facts.avars.contains_key(&name)
            || self.cur.facts.bvars.contains(&name)
        {
            name += &i.to_string();
            i += 1;
        }
        name
    }

    pub(super) const fn ret_type(&self) -> Type {
        self.cur.ret_type
    }

    pub(super) fn union_ret_rng(&mut self, interval: Interval) {
        if let Some(ret_intval) = &mut self.cur.ret_intval {
            *ret_intval = ret_intval.union(interval);
        } else {
            self.cur.ret_intval = Some(interval);
        }
    }

    /// Returns a random catch point and the type of value expected to be caught
    /// Returns None if there are no catch points
    #[allow(
        clippy::cast_possible_truncation,
        clippy::cast_sign_loss,
        clippy::cast_precision_loss
    )]
    pub(super) fn rand_catch(
        &self,
        distrib: &Uniform<f64>,
    ) -> Option<(usize, Type)> {
        if self.cur.nests.nested_trys.is_empty() {
            return None;
        }
        let idx = (distrib.sample(&mut rand::thread_rng())
            * self.cur.nests.nested_trys.len() as f64)
            as usize;
        Some((idx, self.cur.nests.nested_trys[idx]))
    }

    /// Returns the current loop depth
    pub(super) const fn loop_depth(&self) -> usize {
        self.cur.nests.nested_loops
    }

    /// Sets the specified variables as pinned (loop invariant)
    pub(super) fn pin_vars(&mut self, vars: &[String]) {
        for v in vars {
            self.cur.facts.pinned.insert(v.clone());
        }
    }

    /// Meets the current context with another, sibling context, popping the bottom
    /// stack frame and returning it
    /// # Arguments
    /// * `nests` - The nesting level of the parent context we are joining into
    /// * `other` - The sibling context we are joining with
    pub(super) fn meet(child1: Self, child2: Self) -> StackFrame {
        child1.cur.meet(child2.cur)
    }

    /// Updates the current context with a new current stack frame
    /// # Arguments
    /// * `other` - The new bottom of the stack frame
    pub(super) fn update(&mut self, mut other: StackFrame) {
        other.nests = self.cur.nests.clone();
        self.cur = other;
    }

    pub(super) const fn can_follow(&self) -> bool {
        self.cur.can_follow
    }

    /// Indicates the current context returns from the current function
    pub(super) fn set_return(&mut self) {
        self.cur.pending_ret = false;
        self.cur.can_follow = false;
    }

    /// Indicates the current context throws an exception
    pub(super) fn set_throw(&mut self) {
        self.cur.can_follow = false;
    }

    pub(super) fn set_loop_exit(&mut self) {
        self.cur.can_follow = false;
    }

    pub(super) const fn is_dead(&self) -> bool {
        self.cur.is_dead
    }

    #[allow(clippy::missing_const_for_fn)]
    pub(super) fn stack_frame(self) -> StackFrame {
        self.cur
    }

    pub(super) fn meet_sf(a: StackFrame, b: Self) -> StackFrame {
        a.meet(b.cur)
    }

    pub(super) fn try_child(&'a self, t: Type) -> Self {
        Self {
            cur: self.cur.try_child(t),
            parent: Some(self),
        }
    }
}
