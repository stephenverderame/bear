use strum_macros::EnumCount as CountMacro;

#[derive(CountMacro, Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    AExpr(AExpr),
    BExpr(BExpr),
}

/// An expression which evaluates to a boolean
#[derive(CountMacro, Debug, Clone, PartialEq, Eq)]
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
    FCall(String, Seq<Expr>),
}

/// An expression which evaluates to a number
#[derive(CountMacro, Debug, Clone, PartialEq, Eq)]
pub enum AExpr {
    Add(Box<AExpr>, Box<AExpr>),
    Sub(Box<AExpr>, Box<AExpr>),
    Mul(Box<AExpr>, Box<AExpr>),
    Div(Box<AExpr>, Box<AExpr>),
    Id(String),
    Num(i64),
    FCall(String, Seq<Expr>),
}

#[derive(CountMacro, Debug, Clone, PartialEq, Eq)]
pub enum Seq<T> {
    Nil,
    Cons(Box<T>, Box<Seq<T>>),
}

#[derive(Debug, Clone, PartialEq, CountMacro, Eq)]
pub enum Statement {
    Assign {
        dest: String,
        src: Expr,
    },
    If {
        guard: BExpr,
        then: Seq<Statement>,
        otherwise: Seq<Statement>,
    },
    For {
        var: String,
        init: AExpr,
        limit: AExpr,
        step: AExpr,
        body: Seq<Statement>,
    },
    Ret(Seq<Expr>),
    PCall(String, Seq<Expr>),
    Break,
    Continue,
    Print(Seq<Expr>),
}
