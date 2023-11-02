use crate::strum::EnumCount;
use pretty::RcDoc;
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

/// Something that can be pretty printed
pub trait Pretty {
    fn to_doc(&self) -> RcDoc<()>;
}

impl Pretty for Expr {
    fn to_doc(&self) -> RcDoc<()> {
        match self {
            Self::AExpr(a) => a.to_doc(),
            Self::BExpr(b) => b.to_doc(),
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
        let mut v = Vec::new();
        self.to_doc()
            .render(80, &mut v)
            .map_err(|_| std::fmt::Error)?;
        write!(f, "{}", String::from_utf8_lossy(&v))
    }
}

impl Pretty for BExpr {
    #[allow(clippy::too_many_lines)]
    fn to_doc(&self) -> RcDoc<()> {
        match self {
            Self::Id(x) => RcDoc::as_string(x),
            Self::Bool(b) => RcDoc::as_string(b),
            Self::And(left, right) => RcDoc::text("(")
                .append("&&")
                .append(RcDoc::softline())
                .append(left.to_doc())
                .append(RcDoc::softline())
                .append(right.to_doc())
                .append(")"),
            Self::Not(expr) => RcDoc::text("(")
                .append("!")
                .append(RcDoc::softline())
                .append(expr.to_doc())
                .append(")"),
            Self::Or(left, right) => RcDoc::text("(")
                .append("||")
                .append(RcDoc::softline())
                .append(left.to_doc())
                .append(RcDoc::softline())
                .append(right.to_doc())
                .append(")"),
            Self::Lt(left, right) => RcDoc::text("(")
                .append("<")
                .append(RcDoc::softline())
                .append(left.to_doc())
                .append(RcDoc::softline())
                .append(right.to_doc())
                .append(")"),
            Self::Gt(left, right) => RcDoc::text("(")
                .append(">")
                .append(RcDoc::softline())
                .append(left.to_doc())
                .append(RcDoc::softline())
                .append(right.to_doc())
                .append(")"),
            Self::Le(left, right) => RcDoc::text("(")
                .append("<=")
                .append(RcDoc::softline())
                .append(left.to_doc())
                .append(RcDoc::softline())
                .append(right.to_doc())
                .append(")"),
            Self::Ge(left, right) => RcDoc::text("(")
                .append(">=")
                .append(RcDoc::softline())
                .append(left.to_doc())
                .append(RcDoc::softline())
                .append(right.to_doc())
                .append(")"),
            Self::Eqb(left, right) => RcDoc::text("(")
                .append("=?")
                .append(RcDoc::softline())
                .append(left.to_doc())
                .append(RcDoc::softline())
                .append(right.to_doc())
                .append(")"),
            Self::Eqa(left, right) => RcDoc::text("(")
                .append("=")
                .append(RcDoc::softline())
                .append(left.to_doc())
                .append(RcDoc::softline())
                .append(right.to_doc())
                .append(")"),
            Self::FCall(name, args) => RcDoc::text("(")
                .append(name)
                .append(RcDoc::softline())
                .append(RcDoc::intersperse(
                    args.iter().map(Pretty::to_doc),
                    RcDoc::space(),
                ))
                .append(")"),
            Self::Redundant(expr) | Self::LoopInvariant(expr) => expr.to_doc(),
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

impl Pretty for AExpr {
    #[allow(clippy::too_many_lines)]
    fn to_doc(&self) -> RcDoc<()> {
        match self {
            Self::Id(x) => RcDoc::as_string(x),
            Self::Num(i) => RcDoc::as_string(i),
            Self::Add(left, right) => RcDoc::text("(")
                .append("+")
                .append(RcDoc::softline())
                .append(left.to_doc())
                .append(RcDoc::softline())
                .append(right.to_doc())
                .append(")"),
            Self::Sub(left, right) => RcDoc::text("(")
                .append("-")
                .append(RcDoc::softline())
                .append(left.to_doc())
                .append(RcDoc::softline())
                .append(right.to_doc())
                .append(")"),
            Self::Mul(left, right) => RcDoc::text("(")
                .append("*")
                .append(RcDoc::softline())
                .append(left.to_doc())
                .append(RcDoc::softline())
                .append(right.to_doc())
                .append(")"),
            Self::Div(left, right) => RcDoc::text("(")
                .append("/")
                .append(RcDoc::softline())
                .append(left.to_doc())
                .append(RcDoc::softline())
                .append(right.to_doc())
                .append(")"),
            Self::FCall(name, args) => RcDoc::text("(")
                .append(name)
                .append(RcDoc::softline())
                .append(RcDoc::intersperse(
                    args.iter().map(|e| e.to_doc()),
                    RcDoc::space(),
                ))
                .append(")"),
            Self::Redundant(expr) | Self::LoopInvariant(expr) => expr.to_doc(),
            Self::Derived {
                basic,
                factor,
                offset,
            } => RcDoc::text("(")
                .append("+")
                .append(RcDoc::softline())
                .append(
                    RcDoc::text("(")
                        .append("*")
                        .append(RcDoc::softline())
                        .append(basic)
                        .append(RcDoc::softline())
                        .append(factor.to_doc())
                        .append(")"),
                )
                .append(RcDoc::softline())
                .append(offset.to_doc())
                .append(")"),
        }
    }
}

impl std::fmt::Display for AExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut v = Vec::new();
        self.to_doc()
            .render(80, &mut v)
            .map_err(|_| std::fmt::Error)?;
        write!(f, "{}", String::from_utf8_lossy(&v))
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

impl Pretty for Statement {
    #[allow(clippy::too_many_lines)]
    fn to_doc(&self) -> RcDoc<()> {
        match self {
            Self::Assign { dest, src } => RcDoc::text(dest)
                .append(RcDoc::softline())
                .append(":=")
                .append(RcDoc::softline())
                .append(src.to_doc())
                .append(";")
                .append(RcDoc::hardline()),
            Self::Ret(None) => {
                RcDoc::text("return").append(";").append(RcDoc::hardline())
            }
            Self::Ret(Some(expr)) => RcDoc::text("return")
                .append(RcDoc::softline())
                .append(expr.to_doc())
                .append(";")
                .append(RcDoc::hardline()),
            Self::Throw(n, None) => RcDoc::text("throw")
                .append("(")
                .append(n.to_string())
                .append(")")
                .append(";")
                .append(RcDoc::hardline()),
            Self::Throw(n, Some(expr)) => RcDoc::text("throw")
                .append("(")
                .append(n.to_string())
                .append(")")
                .append(RcDoc::softline())
                .append(expr.to_doc())
                .append(";")
                .append(RcDoc::hardline()),
            Self::Print(exprs) => RcDoc::text("print")
                .append(RcDoc::softline())
                .append(RcDoc::intersperse(
                    exprs.iter().map(Pretty::to_doc),
                    RcDoc::space(),
                ))
                .append(";")
                .append(RcDoc::hardline()),
            Self::PCall(name, args) => RcDoc::text(name)
                .append("(")
                .append(RcDoc::intersperse(
                    args.iter().map(Pretty::to_doc),
                    RcDoc::text(",").append(RcDoc::softline()),
                ))
                .append(")")
                .append(";")
                .append(RcDoc::hardline()),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LoopStatement {
    Stmt(Statement),
    /// A break statement with the number of loops to break out of
    Break(usize),
    /// A continue statement with the number of nested loops to continue to
    Continue(usize),
}

impl Pretty for LoopStatement {
    fn to_doc(&self) -> RcDoc<()> {
        match self {
            Self::Stmt(s) => s.to_doc(),
            Self::Break(n) => RcDoc::text("break")
                .append("(")
                .append(n.to_string())
                .append(")")
                .append(";")
                .append(RcDoc::hardline()),
            Self::Continue(n) => RcDoc::text("continue")
                .append("(")
                .append(n.to_string())
                .append(")")
                .append(";")
                .append(RcDoc::hardline()),
        }
    }
}

impl LoopStatement {
    pub const COUNT: usize = Statement::COUNT + 2;
}

pub type LoopBlock = Block<LoopStatement>;
pub type StmtBlock = Block<Statement>;

#[derive(Debug, Clone, PartialEq, CountMacro, Eq, Indexable)]
pub enum Block<T: Pretty> {
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

impl<T: Pretty> Block<T> {
    #[allow(clippy::too_many_lines)]
    pub fn to_doc(&self) -> RcDoc<()> {
        match self {
            Self::If {
                guard,
                then,
                otherwise,
            } => RcDoc::text("if")
                .append(RcDoc::softline())
                .append(guard.to_doc())
                .append(RcDoc::softline())
                .append("{")
                .nest(4)
                .append(RcDoc::hardline())
                .append(RcDoc::intersperse(
                    then.iter().map(Self::to_doc),
                    RcDoc::space(),
                ))
                .append(RcDoc::softline())
                .append("}")
                .nest(-4)
                .append(RcDoc::softline())
                .append("else")
                .append(RcDoc::softline())
                .append("{")
                .nest(4)
                .append(RcDoc::hardline())
                .append(RcDoc::intersperse(
                    otherwise.iter().map(Self::to_doc),
                    RcDoc::space(),
                ))
                .nest(-4)
                .append(RcDoc::softline())
                .append("}")
                .append(RcDoc::hardline()),
            Self::While { var, limit, body } => RcDoc::text("while")
                .append(RcDoc::softline())
                .append(var)
                .append(RcDoc::softline())
                .append(limit.to_doc())
                .append(RcDoc::softline())
                .append("{")
                .nest(4)
                .append(RcDoc::hardline())
                .append(RcDoc::intersperse(
                    body.iter().map(Block::<LoopStatement>::to_doc),
                    RcDoc::space(),
                ))
                .nest(-4)
                .append(RcDoc::softline())
                .append("}")
                .append(RcDoc::hardline()),
            Self::For {
                var,
                init,
                limit,
                step,
                body,
            } => RcDoc::text("for")
                .append(RcDoc::softline())
                .append(var)
                .append(" in ")
                .append(init.to_doc())
                .append(" .. ")
                .append(limit.to_doc())
                .append(RcDoc::softline())
                .append(step.to_doc())
                .append(RcDoc::softline())
                .append("{")
                .nest(4)
                .append(RcDoc::hardline())
                .append(RcDoc::intersperse(
                    body.iter().map(Block::<LoopStatement>::to_doc),
                    RcDoc::space(),
                ))
                .nest(-4)
                .append(RcDoc::softline())
                .append("}")
                .append(RcDoc::hardline()),
            Self::MatchedFor {
                var1,
                var2,
                init,
                limit,
                step,
                body1,
                body2,
            } => RcDoc::text("for")
                .append(RcDoc::softline())
                .append(var1)
                .append(" in ")
                .append(init.to_doc())
                .append(" .. ")
                .append(limit.to_doc())
                .append(RcDoc::softline())
                .append(step.to_doc())
                .append(RcDoc::softline())
                .append("{")
                .nest(4)
                .append(RcDoc::hardline())
                .append(RcDoc::intersperse(
                    body1.iter().map(Block::<LoopStatement>::to_doc),
                    RcDoc::space(),
                ))
                .nest(-4)
                .append(RcDoc::softline())
                .append("}")
                .append(RcDoc::hardline())
                .append("for")
                .append(RcDoc::softline())
                .append(var2)
                .append(" in ")
                .append(init.to_doc())
                .append(" .. ")
                .append(limit.to_doc())
                .append(RcDoc::softline())
                .append(step.to_doc())
                .append(RcDoc::softline())
                .append("{")
                .nest(4)
                .append(RcDoc::hardline())
                .append(RcDoc::intersperse(
                    body2.iter().map(Block::<LoopStatement>::to_doc),
                    RcDoc::space(),
                ))
                .nest(-4)
                .append(RcDoc::softline())
                .append("}")
                .append(RcDoc::hardline()),
            Self::Switch {
                guard,
                cases,
                default,
            } => RcDoc::text("switch")
                .append(RcDoc::softline())
                .append(guard.to_doc())
                .append(RcDoc::softline())
                .append("{")
                .nest(4)
                .append(RcDoc::hardline())
                .append(RcDoc::intersperse(
                    cases
                        .iter()
                        .map(|(guard, body)| {
                            RcDoc::text("case")
                                .append(RcDoc::softline())
                                .append(guard.to_doc())
                                .append(RcDoc::softline())
                                .append("{")
                                .nest(4)
                                .append(RcDoc::hardline())
                                .append(RcDoc::intersperse(
                                    body.iter().map(Self::to_doc),
                                    RcDoc::space(),
                                ))
                                .nest(-4)
                                .append(RcDoc::softline())
                                .append("}")
                                .append(RcDoc::hardline())
                        })
                        .chain(std::iter::once(
                            RcDoc::text("default")
                                .append(RcDoc::softline())
                                .append("{")
                                .nest(4)
                                .append(RcDoc::softline())
                                .append(RcDoc::intersperse(
                                    default.iter().map(Self::to_doc),
                                    RcDoc::space(),
                                ))
                                .nest(-4)
                                .append(RcDoc::softline())
                                .append("}")
                                .append(RcDoc::hardline()),
                        )),
                    RcDoc::space(),
                ))
                .nest(-4)
                .append(RcDoc::softline())
                .append("}")
                .append(RcDoc::hardline()),
            Self::TryCatch {
                try_block,
                catch_block,
            } => RcDoc::text("try")
                .append(RcDoc::softline())
                .append("{")
                .nest(4)
                .append(RcDoc::hardline())
                .append(RcDoc::intersperse(
                    try_block.iter().map(Self::to_doc),
                    RcDoc::space(),
                ))
                .nest(-4)
                .append(RcDoc::hardline())
                .append("}")
                .append(RcDoc::softline())
                .append("catch")
                .append(RcDoc::softline())
                .append("{")
                .nest(4)
                .append(RcDoc::hardline())
                .append(RcDoc::intersperse(
                    catch_block.iter().map(Self::to_doc),
                    RcDoc::space(),
                ))
                .nest(-4)
                .append(RcDoc::softline())
                .append("}")
                .append(RcDoc::hardline()),
            Self::Dead(body) => RcDoc::text("{")
                .nest(4)
                .append(RcDoc::softline())
                .append(RcDoc::intersperse(
                    body.iter().map(Self::to_doc),
                    RcDoc::space(),
                ))
                .nest(-4)
                .append(RcDoc::softline())
                .append("}")
                .append(RcDoc::hardline()),
            Self::Stmt(s) => s.to_doc(),
        }
    }
}

impl<T: Pretty> std::fmt::Display for Block<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut v = Vec::new();
        self.to_doc()
            .render(80, &mut v)
            .map_err(|_| std::fmt::Error)?;
        write!(f, "{}", String::from_utf8_lossy(&v))
    }
}
