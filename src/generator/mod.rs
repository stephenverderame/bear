use rand::distributions::{Uniform, WeightedIndex};
use rand::prelude::*;
use strum::EnumCount;
mod interval;
use interval::Interval;
use strum_macros::EnumCount;
use support::Indexable;
mod context;
mod control_flow;
mod expr;
mod stmt;

use crate::bare_c::{BExpr, Block, Statement};
use crate::generator::context::FuncList;
/// Overflow is not an error in BRIL, so we just don't worry about it.
/// Furthermore, we use random probing for a few reasons:
/// 1. We don't want small programs
/// 2. With such a complex DSL, it's unlikely to generate the same program twice
/// 3. We want some redundancy in the programs to stress redundancy elimination
///
use crate::{bare_c::AExpr, pcfg::TopPCFG};

use self::context::Context;
pub use self::stmt::{StatementEnum, StatementTy};

const EXPR_FUEL: usize = 4;
const STMT_FUEL: usize = 3;
/// Maximum total amount of loop iteration a loop nest can have
const LOOP_MAX_ITER: u64 = 8_000;
const BLOCK_LIMIT: usize = 20;

/// Bare C types
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, EnumCount, Indexable)]
enum Type {
    Int,
    Bool,
    Void,
}

pub const NUM_TYPES: usize = Type::COUNT;
pub type TypeChoice = [f64; NUM_TYPES];

pub mod rnd {
    use rand::rngs::ThreadRng;
    use rand::SeedableRng;
    use rand_chacha::ChaCha12Rng;
    use std::sync::atomic::AtomicU64;
    use std::sync::atomic::Ordering;
    use std::sync::Mutex;
    use std::time::{SystemTime, UNIX_EPOCH};

    type NumGen = ChaCha12Rng;

    /// System time now as a u64
    /// # Panics
    /// Panics if the system time cannot be converted to a u64
    fn time_now_u64() -> u64 {
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_millis()
            .try_into()
            .unwrap()
    }

    lazy_static! {
        static ref SEED: AtomicU64 = AtomicU64::new(time_now_u64());
        static ref GEN: Mutex<NumGen> =
            Mutex::new(NumGen::seed_from_u64(SEED.load(Ordering::SeqCst)));
    }

    /// Gets the PRNG for the program generator
    pub fn get_rng() -> ThreadRng {
        // GEN.lock().unwrap()
        rand::thread_rng()
    }

    #[allow(unused)]
    pub fn get_seed() -> u64 {
        SEED.load(Ordering::SeqCst)
    }

    /// Sets the seed of the program random generator
    #[allow(unused)]
    pub fn set_seed(seed: u64) {
        SEED.store(seed, Ordering::SeqCst);
        *GEN.lock().unwrap() = NumGen::seed_from_u64(seed);
    }

    /// Reseeds the program random generator to the current system time
    #[allow(unused)]
    pub fn reseed() {
        set_seed(time_now_u64());
    }
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
enum StepType {
    None,
    Inc,
    Dec,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct ExprInfo {
    /// The vars that are used in this expression
    vars: Vec<String>,
    /// The range of values this expression can take
    interval: Interval,
}

impl ExprInfo {
    /// Constructs an `ExprInfo` from a constant
    pub const fn from_const(a: i64) -> Self {
        Self {
            vars: Vec::new(),
            interval: Interval::from_const(a),
        }
    }

    /// Constructs an `ExprInfo` from a variable
    pub fn from_var(a: &str, ctx: &Context) -> Self {
        ctx.lookup_var(a).unwrap_or_else(|| Self {
            vars: vec![a.to_string()],
            interval: Interval::make_unknown(),
        })
    }

    /// Gets the combined list of used variables in both expressions
    pub fn union(self, rhs: Self) -> Vec<String> {
        self.vars.into_iter().chain(rhs.vars).collect()
    }

    /// Performs the meet of two `ExprInfo`s
    /// # Panics
    /// Panics if the two `ExprInfo`s do not use the same variables
    pub fn meet(self, rhs: &Self) -> Self {
        assert_eq!(self.vars, rhs.vars);
        Self {
            vars: self.vars,
            interval: self.interval.union(rhs.interval),
        }
    }

    pub const fn make_unknown() -> Self {
        Self {
            vars: Vec::new(),
            interval: Interval::make_unknown(),
        }
    }

    pub const fn from_interval(interval: Interval) -> Self {
        Self {
            vars: Vec::new(),
            interval,
        }
    }
}

impl From<Interval> for ExprInfo {
    fn from(interval: Interval) -> Self {
        Self {
            vars: Vec::new(),
            interval,
        }
    }
}

impl std::ops::Add<Self> for ExprInfo {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self {
            vars: self.vars.into_iter().chain(rhs.vars).collect(),
            interval: self.interval + rhs.interval,
        }
    }
}

impl std::ops::Sub<Self> for ExprInfo {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Self {
            vars: self.vars.into_iter().chain(rhs.vars).collect(),
            interval: self.interval - rhs.interval,
        }
    }
}

impl std::ops::Mul<Self> for ExprInfo {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        Self {
            vars: self.vars.into_iter().chain(rhs.vars).collect(),
            interval: self.interval * rhs.interval,
        }
    }
}

impl std::ops::Div<Self> for ExprInfo {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        Self {
            vars: self.vars.into_iter().chain(rhs.vars).collect(),
            interval: self.interval / rhs.interval,
        }
    }
}

fn get_rand_avar(ctx: &Context) -> Option<String> {
    ctx.get_avars().choose(&mut rnd::get_rng()).cloned()
}

fn get_rand_mutable_avar(ctx: &Context) -> Option<String> {
    ctx.get_mutable_avars().choose(&mut rnd::get_rng()).cloned()
}

fn get_rand_bvar(ctx: &Context) -> Option<String> {
    ctx.get_bvars().choose(&mut rnd::get_rng()).cloned()
}

fn get_rand_mutable_bvar(ctx: &Context) -> Option<String> {
    ctx.get_mutable_bvars().choose(&mut rnd::get_rng()).cloned()
}

fn get_rand_prev_aexpr(ctx: &Context) -> Option<(AExpr, ExprInfo)> {
    ctx.get_aexprs().choose(&mut rnd::get_rng()).cloned()
}

fn get_rand_prev_bexpr(ctx: &Context) -> Option<(BExpr, Vec<String>)> {
    ctx.get_bexprs().choose(&mut rnd::get_rng()).cloned()
}

/// Distributions for generating arithmetic expressions
struct Distribs {
    aexpr_idx: WeightedIndex<f64>,
    bexpr_idx: WeightedIndex<f64>,
    block_idx: WeightedIndex<f64>,
    uniform: Uniform<f64>,
    type_idx: WeightedIndex<f64>,
}

impl Distribs {
    fn new(pcfg: &TopPCFG) -> Self {
        let index = WeightedIndex::new(pcfg.expr.a_expr.choice.iter()).unwrap();
        let uniform = Uniform::new(0.0, 1.0);
        Self {
            aexpr_idx: index,
            bexpr_idx: WeightedIndex::new(pcfg.expr.b_expr.choice.iter())
                .unwrap(),
            block_idx: WeightedIndex::new(pcfg.block.choice.iter()).unwrap(),
            type_idx: WeightedIndex::new(pcfg.block.case_try_type.iter())
                .unwrap(),
            uniform,
        }
    }
}

/// A function argument and its range of values
pub enum FArg {
    Int { name: String, interval: Interval },
    Bool { name: String },
}

impl FArg {
    pub fn int(name: &str, min: i64, max: i64) -> Self {
        Self::Int {
            name: name.to_string(),
            interval: Interval::new(min, max),
        }
    }

    pub fn bool(name: &str) -> Self {
        Self::Bool {
            name: name.to_string(),
        }
    }
}

pub fn gen_function(pcfg: &TopPCFG, args: &[FArg]) -> Vec<Block<Statement>> {
    let mut funcs = FuncList::new();
    let mut ctx = Context::make_root();
    for a in args {
        match a {
            FArg::Int { name, interval } => {
                ctx.new_avar(name, &ExprInfo::from_interval(*interval));
            }
            FArg::Bool { name } => ctx.new_bvar(name, vec![]),
        }
    }
    let mut block_limit = BLOCK_LIMIT;
    control_flow::gen_blocks(
        &pcfg.block,
        pcfg,
        &mut ctx,
        &mut Distribs::new(pcfg),
        &mut funcs,
        STMT_FUEL,
        &mut block_limit,
    )
}

#[cfg(test)]
mod test {
    use crate::pcfg::PCFG;

    use super::{gen_function, interval::Interval, FArg};

    #[test]
    fn running_test() {
        let pcfg = crate::pcfg::TopPCFG::uniform();
        let args = vec![
            FArg::Int {
                name: "a".to_string(),
                interval: Interval::new(0, 100),
            },
            FArg::Int {
                name: "b".to_string(),
                interval: Interval::new(-100, 100),
            },
            FArg::Int {
                name: "c".to_string(),
                interval: Interval::new(0, 100),
            },
            FArg::Bool {
                name: "d".to_string(),
            },
            FArg::Bool {
                name: "e".to_string(),
            },
        ];
        for _ in 0..30 {
            gen_function(&pcfg, &args);
        }
    }
}
