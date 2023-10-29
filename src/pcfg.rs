use super::bare_c::{AExpr, BExpr, Statement};
use strum::EnumCount;

/// Probability space represented as a vector of floats of log probabilities
/// The probabilities in a probability space must sum to 1
/// and each probability must be non-negative
///
/// A probability space with one variable is assumed to be Bernoulli
pub struct PSpace(Vec<f64>);

/// Pops a `PSpace` from a vector and asserts that it is a singleton
/// (i.e. it has only one element)
/// Returns the value of the singleton
fn pop_singleton(v: &mut Vec<PSpace>) -> f64 {
    let res = v.pop().unwrap();
    assert_eq!(res.0.len(), 1);
    res.0[0]
}

#[allow(clippy::upper_case_acronyms)]
pub trait PCFG: Default {
    /// Serializes a PCFG into a vector of floats (left to right)
    fn serialize(&self, out: Vec<PSpace>) -> Vec<PSpace>;
    /// Deserializes a PCFG from a vector of floats (right to left)
    fn deserialize(input: Vec<PSpace>) -> (Self, Vec<PSpace>)
    where
        Self: Sized;

    /// Initializes a PCFG from the uniform distribution
    fn uniform() -> Self
    where
        Self: Sized;

    /// How many floats are needed to serialize this PCFG
    const COUNT: usize;
}

#[derive(Default, Clone, Debug)]
pub struct AExprPCFG {
    choice: [f64; AExpr::COUNT],
    seq: f64,
}

impl PCFG for AExprPCFG {
    fn serialize(&self, mut out: Vec<PSpace>) -> Vec<PSpace> {
        out.push(PSpace(self.choice.to_vec()));
        out.push(PSpace(vec![self.seq]));
        out
    }

    #[allow(clippy::int_plus_one)]
    fn deserialize(mut input: Vec<PSpace>) -> (Self, Vec<PSpace>) {
        let res = Self {
            seq: pop_singleton(&mut input),
            choice: input
                .pop()
                .unwrap()
                .0
                .into_iter()
                .rev()
                .collect::<Vec<_>>()
                .try_into()
                .unwrap(),
        };
        (res, input)
    }

    fn uniform() -> Self {
        let mut res = Self {
            seq: 0.5_f64.ln(),
            ..Default::default()
        };
        #[allow(clippy::cast_precision_loss)]
        for i in 0..AExpr::COUNT {
            res.choice[i] = (1.0 / AExpr::COUNT as f64).ln();
        }
        res
    }

    const COUNT: usize = AExpr::COUNT + 1;
}

#[derive(Debug, Clone, Default)]
pub struct BExprPCFG {
    choice: [f64; BExpr::COUNT],
    seq: f64,
    boolean: f64,
}

impl PCFG for BExprPCFG {
    const COUNT: usize = BExpr::COUNT + 2;

    fn serialize(&self, mut out: Vec<PSpace>) -> Vec<PSpace> {
        out.push(PSpace(self.choice.to_vec()));
        out.push(PSpace(vec![self.seq]));
        out.push(PSpace(vec![self.boolean]));
        out
    }

    fn deserialize(mut input: Vec<PSpace>) -> (Self, Vec<PSpace>)
    where
        Self: Sized,
    {
        let res = Self {
            boolean: pop_singleton(&mut input),
            seq: pop_singleton(&mut input),
            choice: input
                .pop()
                .unwrap()
                .0
                .into_iter()
                .rev()
                .collect::<Vec<_>>()
                .try_into()
                .unwrap(),
        };
        (res, input)
    }

    fn uniform() -> Self {
        let mut res = Self {
            seq: 0.5_f64.ln(),
            boolean: 0.5_f64.ln(),
            ..Default::default()
        };
        #[allow(clippy::cast_precision_loss)]
        for i in 0..BExpr::COUNT {
            res.choice[i] = (1.0 / BExpr::COUNT as f64).ln();
        }
        res
    }
}

/// PCFG for an expression
#[derive(Debug, Clone, Default)]
pub struct ExprPCFG {
    a_expr: AExprPCFG,
    b_expr: BExprPCFG,
    expr_type: f64,
    seq: f64,
}

impl PCFG for ExprPCFG {
    const COUNT: usize = AExprPCFG::COUNT + BExprPCFG::COUNT + 2;
    fn serialize(&self, mut out: Vec<PSpace>) -> Vec<PSpace> {
        out = self.a_expr.serialize(out);
        out = self.b_expr.serialize(out);
        out.push(PSpace(vec![self.expr_type]));
        out.push(PSpace(vec![self.seq]));
        out
    }

    fn deserialize(mut input: Vec<PSpace>) -> (Self, Vec<PSpace>)
    where
        Self: Sized,
    {
        let mut res = Self {
            seq: pop_singleton(&mut input),
            expr_type: pop_singleton(&mut input),
            ..Default::default()
        };
        let (b_expr, input) = BExprPCFG::deserialize(input);
        let (a_expr, input) = AExprPCFG::deserialize(input);
        res.b_expr = b_expr;
        res.a_expr = a_expr;
        (res, input)
    }

    fn uniform() -> Self {
        Self {
            a_expr: AExprPCFG::uniform(),
            b_expr: BExprPCFG::uniform(),
            expr_type: 0.5_f64.ln(),
            seq: 0.5_f64.ln(),
        }
    }
}

/// PCFG for an if statement
#[derive(Debug, Clone, Default)]
pub struct IfPCFG<P: PCFG> {
    guard: BExprPCFG,
    then: P,
    otherwise: P,
}

impl<P: PCFG> PCFG for IfPCFG<P> {
    const COUNT: usize = BExprPCFG::COUNT + P::COUNT * 2;

    fn serialize(&self, mut out: Vec<PSpace>) -> Vec<PSpace> {
        out = self.guard.serialize(out);
        out = self.then.serialize(out);
        out = self.otherwise.serialize(out);
        out
    }

    fn deserialize(input: Vec<PSpace>) -> (Self, Vec<PSpace>)
    where
        Self: Sized,
    {
        // deserialize right to left
        assert!(input.len() >= Self::COUNT);
        let mut res = Self::default();
        let (otherwise, input) = P::deserialize(input);
        let (then, input) = P::deserialize(input);
        let (guard, input) = BExprPCFG::deserialize(input);
        res.otherwise = otherwise;
        res.then = then;
        res.guard = guard;
        (res, input)
    }

    fn uniform() -> Self {
        Self {
            guard: BExprPCFG::uniform(),
            then: P::uniform(),
            otherwise: P::uniform(),
        }
    }
}

/// PCFG for a for loop
#[derive(Debug, Clone, Default)]
pub struct LoopPCFG<P: PCFG> {
    init: ExprPCFG,
    limit: ExprPCFG,
    step: ExprPCFG,
    body: P,
}

impl<P: PCFG> PCFG for LoopPCFG<P> {
    const COUNT: usize = ExprPCFG::COUNT * 3 + P::COUNT;

    fn serialize(&self, mut out: Vec<PSpace>) -> Vec<PSpace> {
        out = self.init.serialize(out);
        out = self.limit.serialize(out);
        out = self.step.serialize(out);
        out = self.body.serialize(out);
        out
    }

    fn deserialize(input: Vec<PSpace>) -> (Self, Vec<PSpace>)
    where
        Self: Sized,
    {
        let mut res = Self::default();
        let (body, input) = P::deserialize(input);
        let (step, input) = ExprPCFG::deserialize(input);
        let (limit, input) = ExprPCFG::deserialize(input);
        let (init, input) = ExprPCFG::deserialize(input);
        res.body = body;
        res.step = step;
        res.limit = limit;
        res.init = init;
        (res, input)
    }

    fn uniform() -> Self {
        Self {
            init: ExprPCFG::uniform(),
            limit: ExprPCFG::uniform(),
            step: ExprPCFG::uniform(),
            body: P::uniform(),
        }
    }
}

/// PCFG for a nested statement
#[derive(Debug, Clone, Default)]
pub struct NestedStmtPCFG {
    base: [f64; Statement::COUNT],
    assign: ExprPCFG,
    if_stmt: IfPCFG<[f64; Statement::COUNT]>,
    for_stmt: LoopPCFG<[f64; Statement::COUNT]>,
    ret: ExprPCFG,
    pcall: ExprPCFG,
    print: ExprPCFG,
    /// probability of there being a following statement
    seq: f64,
}

impl PCFG for NestedStmtPCFG {
    fn serialize(&self, mut out: Vec<PSpace>) -> Vec<PSpace> {
        out.push(PSpace(self.base.to_vec()));
        out = self.assign.serialize(out);
        out = self.if_stmt.serialize(out);
        out = self.for_stmt.serialize(out);
        out = self.ret.serialize(out);
        out = self.pcall.serialize(out);
        out = self.print.serialize(out);
        out.push(PSpace(vec![self.seq]));
        out
    }

    fn deserialize(mut input: Vec<PSpace>) -> (Self, Vec<PSpace>)
    where
        Self: Sized,
    {
        let o_len = input.len();
        assert!(input.len() >= Self::COUNT);
        let mut res = Self {
            seq: pop_singleton(&mut input),
            ..Default::default()
        };
        let (print, input) = ExprPCFG::deserialize(input);
        let (pcall, input) = ExprPCFG::deserialize(input);
        let (retn, input) = ExprPCFG::deserialize(input);
        let (for_stmt, input) = LoopPCFG::<[f64; Statement::COUNT]>::deserialize(input);
        let (if_stmt, input) = IfPCFG::<[f64; Statement::COUNT]>::deserialize(input);
        let (assign, input) = ExprPCFG::deserialize(input);
        let (base, input) = <[f64; Statement::COUNT]>::deserialize(input);
        res.print = print;
        res.pcall = pcall;
        res.ret = retn;
        res.for_stmt = for_stmt;
        res.if_stmt = if_stmt;
        res.assign = assign;
        res.base = base;
        assert_eq!(input.len(), o_len - Self::COUNT);
        (res, input)
    }

    fn uniform() -> Self {
        Self {
            base: <[f64; Statement::COUNT]>::uniform(),
            assign: ExprPCFG::uniform(),
            if_stmt: IfPCFG::<[f64; Statement::COUNT]>::uniform(),
            for_stmt: LoopPCFG::<[f64; Statement::COUNT]>::uniform(),
            ret: ExprPCFG::uniform(),
            pcall: ExprPCFG::uniform(),
            print: ExprPCFG::uniform(),
            seq: 0.5_f64.ln(),
        }
    }

    const COUNT: usize = Statement::COUNT
        + ExprPCFG::COUNT * 5
        + IfPCFG::<[f64; Statement::COUNT]>::COUNT
        + LoopPCFG::<[f64; Statement::COUNT]>::COUNT;
}

/// A PCFG for a top level statement
#[derive(Debug, Clone, Default)]
pub struct StmtPCFG {
    base: [f64; Statement::COUNT],
    assign: ExprPCFG,
    if_stmt: IfPCFG<NestedStmtPCFG>,
    for_stmt: LoopPCFG<NestedStmtPCFG>,
    ret: ExprPCFG,
    pcall: ExprPCFG,
    print: ExprPCFG,
    /// probability of a following statement
    seq: f64,
}

impl PCFG for StmtPCFG {
    fn serialize(&self, mut out: Vec<PSpace>) -> Vec<PSpace> {
        out.push(PSpace(self.base.to_vec()));
        out = self.assign.serialize(out);
        out = self.if_stmt.serialize(out);
        out = self.for_stmt.serialize(out);
        out = self.ret.serialize(out);
        out = self.pcall.serialize(out);
        out = self.print.serialize(out);
        out.push(PSpace(vec![self.seq]));
        out
    }

    fn deserialize(mut input: Vec<PSpace>) -> (Self, Vec<PSpace>)
    where
        Self: Sized,
    {
        assert!(input.len() >= Self::COUNT);
        let o_len = input.len();
        let mut res = Self {
            seq: pop_singleton(&mut input),
            ..Default::default()
        };
        let (print, input) = ExprPCFG::deserialize(input);
        let (pcall, input) = ExprPCFG::deserialize(input);
        let (retn, input) = ExprPCFG::deserialize(input);
        let (for_stmt, input) = LoopPCFG::<NestedStmtPCFG>::deserialize(input);
        let (if_stmt, input) = IfPCFG::<NestedStmtPCFG>::deserialize(input);
        let (assign, input) = ExprPCFG::deserialize(input);
        let (base, input) = <[f64; Statement::COUNT]>::deserialize(input);
        assert_eq!(input.len(), o_len - Self::COUNT);
        res.print = print;
        res.pcall = pcall;
        res.ret = retn;
        res.for_stmt = for_stmt;
        res.if_stmt = if_stmt;
        res.assign = assign;
        res.base = base;
        (res, input)
    }

    fn uniform() -> Self {
        Self {
            base: <[f64; Statement::COUNT]>::uniform(),
            assign: ExprPCFG::uniform(),
            if_stmt: IfPCFG::<NestedStmtPCFG>::uniform(),
            for_stmt: LoopPCFG::<NestedStmtPCFG>::uniform(),
            ret: ExprPCFG::uniform(),
            pcall: ExprPCFG::uniform(),
            print: ExprPCFG::uniform(),
            seq: 0.5_f64.ln(),
        }
    }

    const COUNT: usize = Statement::COUNT
        + ExprPCFG::COUNT * 5
        + IfPCFG::<NestedStmtPCFG>::COUNT
        + LoopPCFG::<NestedStmtPCFG>::COUNT;
}

/// Probability of a function call
#[derive(Default, Debug, Clone)]
pub struct FnPCFG {
    /// probability of int, bool, void
    ret_type: [f64; 3],
    /// probability of int, bool, void
    /// The end of input argument list is represented with void
    arg_type: [f64; 3],
}

impl PCFG for FnPCFG {
    fn serialize(&self, mut out: Vec<PSpace>) -> Vec<PSpace> {
        out.push(PSpace(self.ret_type.to_vec()));
        out.push(PSpace(self.arg_type.to_vec()));
        out
    }

    fn deserialize(mut input: Vec<PSpace>) -> (Self, Vec<PSpace>)
    where
        Self: Sized,
    {
        assert!(input.len() >= Self::COUNT);
        let res = Self {
            arg_type: input.pop().unwrap().0.try_into().unwrap(),
            ret_type: input.pop().unwrap().0.try_into().unwrap(),
        };
        (res, input)
    }

    fn uniform() -> Self {
        let mut res = Self::default();
        for i in 0..3 {
            res.arg_type[i] = (1.0 / 3.0_f64).ln();
            res.ret_type[i] = (1.0 / 3.0_f64).ln();
        }
        res
    }

    const COUNT: usize = 6;
}

impl PCFG for [f64; Statement::COUNT] {
    fn serialize(&self, mut out: Vec<PSpace>) -> Vec<PSpace> {
        out.push(PSpace(self.to_vec()));
        out
    }

    fn deserialize(mut input: Vec<PSpace>) -> (Self, Vec<PSpace>)
    where
        Self: Sized,
    {
        let res = input.pop().unwrap().0.try_into().unwrap();
        (res, input)
    }

    fn uniform() -> Self {
        #[allow(clippy::cast_precision_loss)]
        std::iter::repeat((1.0 / Statement::COUNT as f64).ln())
            .take(Statement::COUNT)
            .collect::<Vec<_>>()
            .try_into()
            .unwrap()
    }

    const COUNT: usize = Statement::COUNT;
}
