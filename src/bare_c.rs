use crate::strum::EnumCount;
use strum_macros::EnumCount as CountMacro;
use support::Indexable;

/// An expression which evaluates to a number or a boolean
///
/// We use nested expressions instead of an instruction based IR
/// to better encode longer dependencies
#[derive(CountMacro, Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expr {
    AExpr(AExpr),
    BExpr(BExpr),
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::AExpr(a) => write!(f, "{a}"),
            Self::BExpr(b) => write!(f, "{b}"),
        }
    }
}

/// An expression which evaluates to a boolean
#[derive(CountMacro, Debug, Clone, PartialEq, Eq, Indexable, Hash)]
pub enum BExpr {
    And(Box<BExpr>, Box<BExpr>),
    Not(Box<BExpr>),
    Or(Box<BExpr>, Box<BExpr>),
    Lt(Box<AExpr>, Box<AExpr>),
    Gt(Box<AExpr>, Box<AExpr>),
    Le(Box<AExpr>, Box<AExpr>),
    Ge(Box<AExpr>, Box<AExpr>),
    Eqb(Box<BExpr>, Box<BExpr>),
    Eqa(Box<AExpr>, Box<AExpr>),
    Id(String),
    Bool(bool),
    FCall(String, Vec<Expr>),
    Redundant(Box<BExpr>),
    LoopInvariant(Box<BExpr>),
}

impl std::fmt::Display for BExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::And(left, right) => write!(f, "(&& {left} {right})"),
            Self::Not(expr) => write!(f, "(not {expr})"),
            Self::Or(left, right) => write!(f, "(|| {left} {right})"),
            Self::Lt(left, right) => write!(f, "(< {left} {right})"),
            Self::Gt(left, right) => write!(f, "(> {left} {right})"),
            Self::Le(left, right) => write!(f, "(<= {left} {right})"),
            Self::Ge(left, right) => write!(f, "(>= {left} {right})"),
            Self::Eqb(left, right) => write!(f, "(== {left} {right})"),
            Self::Eqa(left, right) => write!(f, "(== {left} {right})"),
            Self::Id(id) => write!(f, "{id}"),
            Self::Bool(b) => write!(f, "{b}"),
            Self::FCall(name, args) => {
                write!(
                    f,
                    "({name} {})",
                    args.iter()
                        .map(|e| format!("{e}"))
                        .collect::<Vec<_>>()
                        .join(" ")
                )
            }
            Self::Redundant(expr) | Self::LoopInvariant(expr) => write!(f, "{expr}"),
        }
    }
}

/// An expression which evaluates to a number
#[derive(CountMacro, Debug, Clone, PartialEq, Hash, Eq, Indexable)]
pub enum AExpr {
    Add(Box<AExpr>, Box<AExpr>),
    Sub(Box<AExpr>, Box<AExpr>),
    Mul(Box<AExpr>, Box<AExpr>),
    Div(Box<AExpr>, Box<AExpr>),
    Id(String),
    Num(i64),
    FCall(String, Vec<Expr>),
    Redundant(Box<AExpr>),
    LoopInvariant(Box<AExpr>),
    Derived {
        basic: String,
        factor: Box<AExpr>,
        offset: Box<AExpr>,
    },
}

impl std::fmt::Display for AExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Add(left, right) => write!(f, "(+ {left} {right})"),
            Self::Sub(left, right) => write!(f, "(- {left} {right})"),
            Self::Mul(left, right) => write!(f, "(* {left} {right})"),
            Self::Div(left, right) => write!(f, "(/ {left} {right})"),
            Self::Id(id) => write!(f, "{id}"),
            Self::Num(n) => write!(f, "{n}"),
            Self::FCall(name, args) => {
                write!(
                    f,
                    "({name} {})",
                    args.iter()
                        .map(|e| format!("{e}"))
                        .collect::<Vec<_>>()
                        .join(" ")
                )
            }
            Self::Redundant(expr) | Self::LoopInvariant(expr) => write!(f, "{expr}"),
            Self::Derived {
                basic,
                factor,
                offset,
            } => write!(f, "(+ (* {basic} {factor}) {offset})"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, CountMacro, Eq, Indexable)]
pub enum Statement {
    Assign {
        dest: String,
        src: Expr,
    },
    Ret(Option<Expr>),
    /// A throw statement with the number of nested try-catch blocks to throw out of
    /// and the value to throw
    Throw(usize, Option<Expr>),
    PCall(String, Vec<Expr>),
    Print(Vec<Expr>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LoopStatement {
    Stmt(Statement),
    /// A break statement with the number of loops to break out of
    Break(usize),
    /// A continue statement with the number of nested loops to continue to
    Continue(usize),
}

impl LoopStatement {
    pub fn index(&self) -> usize {
        match self {
            Self::Stmt(s) => s.index(),
            Self::Break(_) => Statement::COUNT,
            Self::Continue(_) => Statement::COUNT + 1,
        }
    }

    pub const COUNT: usize = Statement::COUNT + 2;
}

pub type LoopBlock = Block<LoopStatement>;
pub type StmtBlock = Block<Statement>;

#[derive(Debug, Clone, PartialEq, CountMacro, Eq, Indexable)]
pub enum Block<T> {
    If {
        guard: BExpr,
        then: Vec<Self>,
        otherwise: Vec<Self>,
    },
    While {
        var: String,
        limit: AExpr,
        body: Vec<LoopBlock>,
    },
    For {
        var: String,
        init: AExpr,
        limit: AExpr,
        step: AExpr,
        body: Vec<LoopBlock>,
    },
    MatchedFor {
        var1: String,
        var2: String,
        init: AExpr,
        limit: AExpr,
        step: AExpr,
        body1: Vec<LoopBlock>,
        body2: Vec<LoopBlock>,
    },
    Switch {
        guard: AExpr,
        cases: Vec<(AExpr, Vec<Self>)>,
        default: Vec<Self>,
    },
    TryCatch {
        try_block: Vec<Self>,
        catch_block: Vec<Self>,
    },
    /// A block whose results are never used outside the block
    Dead(Vec<Self>),
    Stmt(T),
}
