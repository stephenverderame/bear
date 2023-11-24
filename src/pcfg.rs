use crate::generator::{rnd, TypeChoice, NUM_TYPES};

use super::bare_c::*;
use rand::Rng;
use strum::EnumCount;

/// Probability space represented as a vector of floats of probabilities
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
}

#[derive(Default, Clone, Debug, PartialEq)]
pub struct AExprPCFG {
    pub choice: [f64; AExpr::COUNT],
    /// Probability of saving an expression for reuse
    pub reuse: f64,
    pub reuse_swap: f64,
}

impl PCFG for AExprPCFG {
    fn serialize(&self, mut out: Vec<PSpace>) -> Vec<PSpace> {
        out.push(PSpace(self.choice.to_vec()));
        out.push(PSpace(vec![self.reuse]));
        out.push(PSpace(vec![self.reuse_swap]));
        out
    }

    #[allow(clippy::int_plus_one)]
    fn deserialize(mut input: Vec<PSpace>) -> (Self, Vec<PSpace>) {
        let res = Self {
            reuse_swap: pop_singleton(&mut input),
            reuse: pop_singleton(&mut input),
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
            reuse: 0.5_f64,
            reuse_swap: 0.5_f64,
            ..Default::default()
        };
        #[allow(clippy::cast_precision_loss)]
        for i in 0..AExpr::COUNT {
            res.choice[i] = 1.0 / AExpr::COUNT as f64;
        }
        res
    }
}

#[derive(Debug, Clone, Default, PartialEq)]
pub struct BExprPCFG {
    pub choice: [f64; BExpr::COUNT],
    pub boolean: f64,
    pub reuse: f64,
}

impl PCFG for BExprPCFG {
    fn serialize(&self, mut out: Vec<PSpace>) -> Vec<PSpace> {
        out.push(PSpace(self.choice.to_vec()));
        out.push(PSpace(vec![self.boolean]));
        out.push(PSpace(vec![self.reuse]));
        out
    }

    fn deserialize(mut input: Vec<PSpace>) -> (Self, Vec<PSpace>)
    where
        Self: Sized,
    {
        let res = Self {
            reuse: pop_singleton(&mut input),
            boolean: pop_singleton(&mut input),
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
            boolean: 0.5_f64,
            reuse: 0.5_f64,
            ..Default::default()
        };
        #[allow(clippy::cast_precision_loss)]
        for i in 0..BExpr::COUNT {
            res.choice[i] = 1.0 / BExpr::COUNT as f64;
        }
        res
    }
}

/// PCFG for an expression
#[derive(Debug, Clone, Default, PartialEq)]
pub struct ExprPCFG {
    pub a_expr: AExprPCFG,
    pub b_expr: BExprPCFG,
    pub expr_type: f64,
    pub seq: f64,
}

impl PCFG for ExprPCFG {
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
            seq: pop_singleton(&mut input).min(0.6),
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
            expr_type: 0.5_f64,
            seq: 0.5_f64,
        }
    }
}

#[derive(Debug, Clone, Default, PartialEq)]
pub struct StmtPCFG {
    pub choice: [f64; Statement::COUNT],
    pub new_var: f64,
    pub var_type: f64,
}

/// A PCFG for a statement
pub trait StatementPCFG: PCFG {
    const COUNT: usize;

    /// Returns the choice probabilities for this statement
    fn get_choice(&self) -> &[f64];
    /// Returns the probability of creating a new variable
    fn get_new_var(&self) -> f64;
    /// Returns the probability of creating a boolean
    fn get_bool_type(&self) -> f64;
}

impl PCFG for StmtPCFG {
    fn serialize(&self, out: Vec<PSpace>) -> Vec<PSpace> {
        let mut out = <[f64; Statement::COUNT]>::serialize(&self.choice, out);
        out.push(PSpace(vec![self.new_var]));
        out.push(PSpace(vec![self.var_type]));
        out
    }

    fn deserialize(mut input: Vec<PSpace>) -> (Self, Vec<PSpace>)
    where
        Self: Sized,
    {
        let var_type = pop_singleton(&mut input);
        let new_var = pop_singleton(&mut input);
        let (choice, input) = <[f64; Statement::COUNT]>::deserialize(input);
        (
            Self {
                choice,
                new_var,
                var_type,
            },
            input,
        )
    }

    fn uniform() -> Self
    where
        Self: Sized,
    {
        Self {
            choice: <[f64; Statement::COUNT]>::uniform(),
            new_var: 0.5_f64,
            var_type: 0.5_f64,
        }
    }
}

impl StatementPCFG for StmtPCFG {
    const COUNT: usize = Statement::COUNT;

    fn get_choice(&self) -> &[f64] {
        &self.choice
    }

    fn get_new_var(&self) -> f64 {
        self.new_var
    }

    fn get_bool_type(&self) -> f64 {
        self.var_type
    }
}
#[derive(Debug, Clone, Default, PartialEq)]
pub struct LoopStmtPCFG {
    pub choice: [f64; LoopStatement::COUNT],
    pub new_var: f64,
    pub var_type: f64,
}

impl PCFG for LoopStmtPCFG {
    fn serialize(&self, mut out: Vec<PSpace>) -> Vec<PSpace> {
        out.push(PSpace(vec![self.var_type]));
        out.push(PSpace(vec![self.new_var]));
        <[f64; LoopStatement::COUNT]>::serialize(&self.choice, out)
    }

    fn deserialize(input: Vec<PSpace>) -> (Self, Vec<PSpace>)
    where
        Self: Sized,
    {
        let (choice, mut input) =
            <[f64; LoopStatement::COUNT]>::deserialize(input);
        let new_var = pop_singleton(&mut input);
        let var_type = pop_singleton(&mut input);
        (
            Self {
                choice,
                new_var,
                var_type,
            },
            input,
        )
    }

    fn uniform() -> Self
    where
        Self: Sized,
    {
        Self {
            choice: <[f64; LoopStatement::COUNT]>::uniform(),
            new_var: 0.5_f64,
            var_type: 0.5_f64,
        }
    }
}

impl StatementPCFG for LoopStmtPCFG {
    const COUNT: usize = LoopStatement::COUNT;

    fn get_choice(&self) -> &[f64] {
        &self.choice
    }

    fn get_new_var(&self) -> f64 {
        self.new_var
    }

    fn get_bool_type(&self) -> f64 {
        self.var_type
    }
}

/// PCFG for an if statement
#[derive(Debug, Clone, Default, PartialEq)]
pub struct IfPCFG<P: PCFG> {
    pub guard: ExprPCFG,
    pub child: P,
}

impl<P: PCFG> PCFG for IfPCFG<P> {
    fn serialize(&self, mut out: Vec<PSpace>) -> Vec<PSpace> {
        out = self.guard.serialize(out);
        self.child.serialize(out)
    }

    fn deserialize(input: Vec<PSpace>) -> (Self, Vec<PSpace>)
    where
        Self: Sized,
    {
        let mut res = Self::default();
        let (child, input) = P::deserialize(input);
        let (guard, input) = ExprPCFG::deserialize(input);
        res.child = child;
        res.guard = guard;
        (res, input)
    }

    fn uniform() -> Self {
        Self {
            guard: ExprPCFG::uniform(),
            child: P::uniform(),
        }
    }
}

/// PCFG for a for loop
#[derive(Debug, Clone, Default, PartialEq)]
pub struct LoopPCFG {
    pub init: AExprPCFG,
    pub limit: AExprPCFG,
    pub step: AExprPCFG,
    pub inc_or_dec: f64,
}

impl PCFG for LoopPCFG {
    fn serialize(&self, mut out: Vec<PSpace>) -> Vec<PSpace> {
        out = self.init.serialize(out);
        out = self.limit.serialize(out);
        out = self.step.serialize(out);
        out.push(PSpace(vec![self.inc_or_dec]));
        out
    }

    fn deserialize(mut input: Vec<PSpace>) -> (Self, Vec<PSpace>)
    where
        Self: Sized,
    {
        let mut res = Self::default();
        let inc_or_dec = pop_singleton(&mut input);
        let (step, input) = AExprPCFG::deserialize(input);
        let (limit, input) = AExprPCFG::deserialize(input);
        let (init, input) = AExprPCFG::deserialize(input);
        res.limit = limit;
        res.init = init;
        res.step = step;
        res.inc_or_dec = inc_or_dec;
        (res, input)
    }

    fn uniform() -> Self {
        Self {
            init: AExprPCFG::uniform(),
            limit: AExprPCFG::uniform(),
            step: AExprPCFG::uniform(),
            inc_or_dec: 0.5_f64,
        }
    }
}

#[derive(Debug, Clone, Default, PartialEq)]
pub struct BlockPCFG<T: PCFG> {
    pub choice: [f64; StmtBlock::COUNT],
    pub if_pcfg: IfPCFG<T>,
    pub loop_pcfg: LoopPCFG,
    pub stmt: T,
    pub seq: f64,
    pub case_seq: f64,
    pub case_try_type: TypeChoice,
    pub step_ty: f64,
}

impl<T: PCFG> PCFG for BlockPCFG<T> {
    fn serialize(&self, out: Vec<PSpace>) -> Vec<PSpace> {
        let mut out = <[f64; StmtBlock::COUNT]>::serialize(&self.choice, out);
        out = self.if_pcfg.serialize(out);
        out = self.loop_pcfg.serialize(out);
        out = self.stmt.serialize(out);
        out.push(PSpace(vec![self.seq]));
        out.push(PSpace(vec![self.case_seq]));
        out = self.case_try_type.serialize(out);
        out.push(PSpace(vec![self.step_ty]));
        out
    }

    fn deserialize(mut input: Vec<PSpace>) -> (Self, Vec<PSpace>)
    where
        Self: Sized,
    {
        let mut res = Self::default();
        let step_ty = pop_singleton(&mut input);
        let (case_try_type, mut input) = TypeChoice::deserialize(input);
        let case_seq = pop_singleton(&mut input).min(0.6);
        let seq = pop_singleton(&mut input).min(0.6);
        let (stmt_pcfg, input) = T::deserialize(input);
        let (loop_pcfg, input) = LoopPCFG::deserialize(input);
        let (if_pcfg, input) = IfPCFG::deserialize(input);
        let (choice, input) = <[f64; StmtBlock::COUNT]>::deserialize(input);
        res.seq = seq;
        res.case_seq = case_seq;
        res.stmt = stmt_pcfg;
        res.loop_pcfg = loop_pcfg;
        res.if_pcfg = if_pcfg;
        res.choice = choice;
        res.case_try_type = case_try_type;
        res.step_ty = step_ty;
        (res, input)
    }

    fn uniform() -> Self
    where
        Self: Sized,
    {
        Self {
            choice: <[f64; StmtBlock::COUNT]>::uniform(),
            if_pcfg: IfPCFG::uniform(),
            stmt: T::uniform(),
            loop_pcfg: LoopPCFG::uniform(),
            seq: 0.5_f64,
            case_seq: 0.5_f64,
            case_try_type: TypeChoice::uniform(),
            step_ty: 0.5_f64,
        }
    }
}

#[derive(Debug, Clone, Default, PartialEq)]
pub struct TopPCFG {
    pub block: BlockPCFG<StmtPCFG>,
    pub loop_block: BlockPCFG<LoopStmtPCFG>,
    pub expr: ExprPCFG,
    pub fn_pcfg: FnPCFG,
}

impl PCFG for TopPCFG {
    fn serialize(&self, out: Vec<PSpace>) -> Vec<PSpace> {
        let out = self.block.serialize(out);
        let out = self.loop_block.serialize(out);
        let out = self.expr.serialize(out);
        self.fn_pcfg.serialize(out)
    }

    fn deserialize(input: Vec<PSpace>) -> (Self, Vec<PSpace>)
    where
        Self: Sized,
    {
        let (fn_pcfg, input) = FnPCFG::deserialize(input);
        let (expr, input) = ExprPCFG::deserialize(input);
        let (loop_block, input) = BlockPCFG::<LoopStmtPCFG>::deserialize(input);
        let (block, input) = BlockPCFG::<StmtPCFG>::deserialize(input);
        (
            Self {
                block,
                loop_block,
                expr,
                fn_pcfg,
            },
            input,
        )
    }

    fn uniform() -> Self
    where
        Self: Sized,
    {
        Self {
            fn_pcfg: FnPCFG::uniform(),
            block: BlockPCFG::<StmtPCFG>::uniform(),
            loop_block: BlockPCFG::<LoopStmtPCFG>::uniform(),
            expr: ExprPCFG::uniform(),
        }
    }
}

/// Probability of a function call
#[derive(Default, Debug, Clone, PartialEq)]
pub struct FnPCFG {
    /// probability of int, bool, void
    pub ret_type: [f64; 3],
    /// probability of int, bool, void
    /// The end of input argument list is represented with void
    pub arg_type: [f64; 3],
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
        let res = Self {
            arg_type: input.pop().unwrap().0.try_into().unwrap(),
            ret_type: input.pop().unwrap().0.try_into().unwrap(),
        };
        (res, input)
    }

    fn uniform() -> Self {
        let mut res = Self::default();
        for i in 0..3 {
            res.arg_type[i] = 1.0 / 3.0_f64;
            res.ret_type[i] = 1.0 / 3.0_f64;
        }
        res
    }
}

support::array_pcfg!([f64; Statement::COUNT]);
support::array_pcfg!([f64; LoopStatement::COUNT]);
// support::array_pcfg!([f64; StmtBlock::COUNT]);
support::array_pcfg!([f64; NUM_TYPES]);

pub fn flatten_pcfg(pcfg: &Vec<PSpace>) -> Vec<f64> {
    let mut res = Vec::new();
    for p in pcfg {
        res.extend_from_slice(&p.0);
    }
    res
}

pub fn flat_to_pcfg(
    template: &Vec<PSpace>,
    mut flattened: &[f64],
) -> Vec<PSpace> {
    let mut res = Vec::new();
    for p in template {
        let len = p.0.len();
        let head;
        (head, flattened) = flattened.split_at(len);
        let sum = head.iter().sum::<f64>();
        let sum = if sum.abs() < f64::EPSILON {
            f64::EPSILON
        } else {
            sum
        };
        let head = head.iter().map(|x| x / sum).collect::<Vec<_>>();
        res.push(PSpace(head));
    }
    res
}

pub fn random_pcfg() -> TopPCFG {
    let t = TopPCFG::uniform();
    let template = t.serialize(Vec::new());
    let mut f = flatten_pcfg(&template);
    let mut rng = rnd::get_rng();
    for e in &mut f {
        *e = rng.gen_range(0.0..1.0);
    }
    TopPCFG::deserialize(flat_to_pcfg(&template, &f)).0
}

#[cfg(test)]
mod serialization_test {
    #[test]
    fn test() {
        use super::*;
        let pcfg = TopPCFG::uniform();
        let mut out = Vec::new();
        out = pcfg.serialize(out);
        let (pcfg2, _) = TopPCFG::deserialize(out);
        assert_eq!(pcfg, pcfg2);
    }
}
