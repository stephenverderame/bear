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

impl Pretty for Expr {
    fn pretty(&self, indent: isize) -> String {
        match self {
            Self::AExpr(expr) => expr.pretty(indent),
            Self::BExpr(expr) => expr.pretty(indent),
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
    Eqa(Box<AExpr>, Box<AExpr>),
    Id(String),
    Bool(bool),
    FCall(String, Vec<Expr>),
    Redundant(Box<BExpr>),
    LoopInvariant(Box<BExpr>),
}

impl Pretty for BExpr {
    fn pretty(&self, indent: isize) -> String {
        match self {
            Self::And(lhs, rhs) => {
                format!("(&& {} {})", lhs.pretty(indent), rhs.pretty(indent))
            }
            Self::Not(expr) => {
                format!("(! {})", expr.pretty(indent))
            }
            Self::Or(lhs, rhs) => {
                format!("(|| {} {})", lhs.pretty(indent), rhs.pretty(indent))
            }
            Self::Lt(lhs, rhs) => {
                format!("(< {} {})", lhs.pretty(indent), rhs.pretty(indent))
            }
            Self::Gt(lhs, rhs) => {
                format!("(> {} {})", lhs.pretty(indent), rhs.pretty(indent))
            }
            Self::Le(lhs, rhs) => {
                format!("(<= {} {})", lhs.pretty(indent), rhs.pretty(indent))
            }
            Self::Ge(lhs, rhs) => {
                format!("(>= {} {})", lhs.pretty(indent), rhs.pretty(indent))
            }
            Self::Eqa(lhs, rhs) => {
                format!("(== {} {})", lhs.pretty(indent), rhs.pretty(indent))
            }
            Self::Id(id) => id.clone(),
            Self::Bool(b) => b.to_string(),
            Self::FCall(name, args) => {
                let mut res = format!("({name}");
                for arg in args {
                    res += &format!(" {}", arg.pretty(indent));
                }
                res += ")";
                res
            }
            Self::Redundant(expr) => {
                format!("(r {{ {} }})", expr.pretty(indent))
            }
            Self::LoopInvariant(expr) => {
                format!("(liv {{ {} }})", expr.pretty(indent))
            }
        }
    }
}

pub trait Pretty {
    fn pretty(&self, indent: isize) -> String;

    fn indent(&self, indent: isize) -> String {
        let mut res = String::new();
        for _ in 0..indent {
            res += "    ";
        }
        res
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
    Derived(Box<AExpr>),
}

impl Pretty for AExpr {
    fn pretty(&self, indent: isize) -> String {
        match self {
            Self::Add(lhs, rhs) => {
                format!("(+ {} {})", lhs.pretty(indent), rhs.pretty(indent))
            }
            Self::Sub(lhs, rhs) => {
                format!("(- {} {})", lhs.pretty(indent), rhs.pretty(indent))
            }
            Self::Mul(lhs, rhs) => {
                format!("(* {} {})", lhs.pretty(indent), rhs.pretty(indent))
            }
            Self::Div(lhs, rhs) => {
                format!("(/ {} {})", lhs.pretty(indent), rhs.pretty(indent))
            }
            Self::Id(id) => id.clone(),
            Self::Num(n) => n.to_string(),
            Self::FCall(name, args) => {
                let mut res = format!("({name}");
                for arg in args {
                    res += &format!(" {}", arg.pretty(indent));
                }
                res += ")";
                res
            }
            Self::Redundant(expr) => {
                format!("(r {{ {} }})", expr.pretty(indent))
            }
            Self::LoopInvariant(expr) => {
                format!("(liv {{ {} }})", expr.pretty(indent))
            }
            Self::Derived(expr) => {
                format!("(iv {{ {} }})", expr.pretty(indent))
            }
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

impl Pretty for Statement {
    fn pretty(&self, indent: isize) -> String {
        match self {
            Self::Assign { dest, src } => {
                format!(
                    "{}{dest} := {};",
                    self.indent(indent),
                    src.pretty(indent)
                )
            }
            Self::Ret(None) => {
                format!("{}return;", self.indent(indent),)
            }
            Self::Ret(Some(expr)) => {
                format!(
                    "{}return {};",
                    self.indent(indent),
                    expr.pretty(indent)
                )
            }
            Self::Throw(n, None) => {
                format!("{}throw({});", self.indent(indent), n)
            }
            Self::Throw(n, Some(expr)) => {
                format!(
                    "{}throw({}) {};",
                    self.indent(indent),
                    n,
                    expr.pretty(indent)
                )
            }
            Self::PCall(name, args) => {
                let mut res = format!("{}({} ", self.indent(indent), name);
                for arg in args {
                    res += &format!(" {}", arg.pretty(indent));
                }
                res += ");";
                res
            }
            Self::Print(args) => {
                let mut res = format!("{}print!", self.indent(indent));
                for arg in args {
                    res += &format!(" {}", arg.pretty(indent));
                }
                res += ";";
                res
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Indexable)]
pub enum LoopStatement {
    Stmt(Statement),
    /// A break statement with the number of loops to break out of
    Break(usize),
    /// A continue statement with the number of nested loops to continue to
    Continue(usize),
    /// Stepping of the loop variable
    Step(Statement),
}

impl Pretty for LoopStatement {
    fn pretty(&self, indent: isize) -> String {
        match self {
            Self::Stmt(stmt) | Self::Step(stmt) => stmt.pretty(indent),
            Self::Break(n) => {
                format!("{}break({});", self.indent(indent), n)
            }
            Self::Continue(n) => {
                format!("{}continue({});", self.indent(indent), n)
            }
        }
    }
}

impl LoopStatement {
    pub const COUNT: usize = Statement::COUNT + 3;
}

pub type LoopBlock = Block<LoopStatement>;
pub type StmtBlock = Block<Statement>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DuffsInfo {
    pub(super) var: String,
    pub(super) init: AExpr,
    pub(super) limit: AExpr,
    pub(super) step: AExpr,
    pub(super) guard: AExpr,
    pub(super) bodies: Vec<(AExpr, Vec<LoopBlock>)>,
    pub(super) default: Vec<LoopBlock>,
    pub(super) is_inc: bool,
}

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
        do_while: bool,
        is_inc: bool,
    },
    For {
        var: String,
        init: AExpr,
        limit: AExpr,
        step: AExpr,
        body: Vec<LoopBlock>,
        is_inc: bool,
    },
    Duffs(DuffsInfo),
    Switch {
        guard: AExpr,
        cases: Vec<(AExpr, Vec<Self>)>,
        default: Vec<Self>,
    },
    TryCatch {
        try_block: Vec<Self>,
        catch_name: Option<String>,
        catch_block: Vec<Self>,
    },
    /// A block whose results are never used outside the block
    Dead(Vec<Self>),
    Stmt(T),
}

impl<T: Pretty> Block<T> {
    fn print_while(
        &self,
        var: &str,
        limit: &AExpr,
        body: &[LoopBlock],
        do_while: bool,
        is_inc: bool,
        indent: isize,
    ) -> String {
        let mut res = String::new();
        let op = if is_inc { "<" } else { ">" };
        if do_while {
            res += &format!("{}do {{\n", self.indent(indent));
            for stmt in body {
                res += &format!("{}\n", stmt.pretty(indent + 1));
            }
            res += &format!(
                "{}}} while {var} {op} {};\n",
                self.indent(indent),
                limit.pretty(indent)
            );
        } else {
            res += &format!(
                "{}while {var} {op} {} {{\n",
                self.indent(indent),
                limit.pretty(indent)
            );
            for stmt in body {
                res += &format!("{}\n", stmt.pretty(indent + 1));
            }
            res += &format!("{}}}\n", self.indent(indent));
        }
        res
    }

    fn print_duffs(&self, df: &DuffsInfo, indent: isize) -> String {
        let mut res = String::new();
        let op = if df.is_inc { "<" } else { ">" };
        res += &format!(
            "{}switch {} {{\n",
            self.indent(indent),
            df.guard.pretty(indent)
        );
        res += &format!("{}do {{\n", self.indent(indent + 1));
        for (case, body) in &df.bodies {
            res += &format!(
                "{}case {}:\n",
                self.indent(indent + 1),
                case.pretty(indent + 1)
            );
            for stmt in body {
                res += &format!("{}\n", stmt.pretty(indent + 2));
            }
        }
        res += &format!("{}default:\n", self.indent(indent + 1));
        for stmt in &df.default {
            res += &format!("{}\n", stmt.pretty(indent + 2));
        }
        res += &format!(
            "{}}} for {} {op} {} by {};\n",
            self.indent(indent + 1),
            df.var,
            df.limit.pretty(indent + 1),
            df.step.pretty(indent + 1),
        );
        res += &format!("{}}}\n", self.indent(indent));
        res
    }
}

impl<T: Pretty> Pretty for Block<T> {
    #[allow(clippy::too_many_lines)]
    fn pretty(&self, indent: isize) -> String {
        match self {
            Self::Dead(stmts) => {
                let mut res = String::new();
                res += &self.indent(indent);
                res += "dead {\n";
                for stmt in stmts {
                    res += &format!("{}\n", stmt.pretty(indent + 1));
                }
                res += &self.indent(indent);
                res += "}";
                res
            }
            Self::If {
                guard,
                then,
                otherwise,
            } => {
                let mut res = String::new();
                res += &self.indent(indent);
                res += &format!("if {} {{\n", guard.pretty(indent));
                for stmt in then {
                    res += &format!("{}\n", stmt.pretty(indent + 1));
                }
                res += &self.indent(indent);
                res += "} else {\n";
                for stmt in otherwise {
                    res += &format!("{}\n", stmt.pretty(indent + 1));
                }
                res += &self.indent(indent);
                res += "}";
                res
            }
            Self::While {
                var,
                limit,
                body,
                do_while,
                is_inc,
            } => self.print_while(var, limit, body, *do_while, *is_inc, indent),
            Self::For {
                var,
                init,
                limit,
                step,
                body,
                is_inc,
            } => {
                let mut res = String::new();
                let op = if *is_inc { "<" } else { ">" };
                res += &self.indent(indent);
                res += &format!(
                    "for {} in {} {op} {} by {} {{\n",
                    var,
                    init.pretty(indent),
                    limit.pretty(indent),
                    step.pretty(indent)
                );
                for stmt in body {
                    res += &format!("{}\n", stmt.pretty(indent + 1));
                }
                res += &self.indent(indent);
                res += "}\n";
                res
            }
            Self::Stmt(stmt) => stmt.pretty(indent),
            Self::Duffs(duffs_info) => self.print_duffs(duffs_info, indent),
            Self::TryCatch {
                try_block,
                catch_name,
                catch_block,
            } => {
                let mut res = String::new();
                res += &self.indent(indent);
                res += "try {\n";
                for stmt in try_block {
                    res += &format!("{}\n", stmt.pretty(indent + 1));
                }
                res += &self.indent(indent);
                let catch_var = catch_name
                    .as_ref()
                    .map_or_else(String::new, |name| name.clone() + " ");
                res += &format!("}} catch {catch_var}{{\n",);
                for stmt in catch_block {
                    res += &format!("{}\n", stmt.pretty(indent + 1));
                }
                res += &self.indent(indent);
                res += "}";
                res
            }
            Self::Switch {
                guard,
                cases,
                default,
            } => {
                let mut res = String::new();
                res += &self.indent(indent);
                res += &format!("match {} {{\n", guard.pretty(indent));
                for (guard, body) in cases {
                    res += &format!(
                        "{}{} => {{\n",
                        self.indent(indent + 1),
                        guard.pretty(0)
                    );
                    for stmt in body {
                        res += &format!("{}\n", stmt.pretty(indent + 2));
                    }
                    res += &self.indent(indent + 1);
                    res += "}\n";
                }
                res += &self.indent(indent + 1);
                res += "_ => {\n";
                for stmt in default {
                    res += &format!("{}\n", stmt.pretty(indent + 2));
                }
                res += &self.indent(indent + 1);
                res += "}\n";
                res += &self.indent(indent);
                res += "}";
                res
            }
        }
    }
}
