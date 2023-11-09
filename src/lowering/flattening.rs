use bril_rs::{ConstOps, Instruction, Type, ValueOps};

use crate::bare_c::{AExpr, BExpr, Expr};

/// The result of flattening Bare C expressions into BRIL instructions.
#[derive(Default)]
pub(super) struct FlattenResult {
    /// Stack of resulting variable names.
    pub(super) result_stack: Vec<String>,
    pub(super) cur_temp_id: usize,
    pub(super) instrs: Vec<Instruction>,
}

fn flatten_abinop(
    mut flatten_result: FlattenResult,
    lhs: AExpr,
    rhs: AExpr,
    op: ValueOps,
    op_type: bril_rs::Type,
) -> FlattenResult {
    flatten_result = flatten_aexpr(lhs, flatten_result);
    flatten_result = flatten_aexpr(rhs, flatten_result);
    flatten_op_helper(flatten_result, op, op_type)
}

/// Flattens a binary boolean operation.
fn flatten_bbinop(
    mut flatten_result: FlattenResult,
    lhs: BExpr,
    rhs: BExpr,
    op: ValueOps,
) -> FlattenResult {
    flatten_result = flatten_bexpr(lhs, flatten_result);
    flatten_result = flatten_bexpr(rhs, flatten_result);
    flatten_op_helper(flatten_result, op, Type::Bool)
}

/// Helper function for flattening binary operations.
/// Pops the top two values from the result stack (expecting the RHS value to be on top),
/// performs the operation,
/// and pushes the result back onto the stack.
fn flatten_op_helper(
    mut flatten_result: FlattenResult,
    op: ValueOps,
    op_type: bril_rs::Type,
) -> FlattenResult {
    let rhs = flatten_result.result_stack.pop().unwrap();
    let lhs = flatten_result.result_stack.pop().unwrap();
    let temp_name = format!("_t{}", flatten_result.cur_temp_id);
    let temp = Instruction::Value {
        dest: temp_name.clone(),
        op,
        op_type,
        args: vec![lhs, rhs],
        funcs: vec![],
        labels: vec![],
        pos: None,
    };
    flatten_result.cur_temp_id += 1;
    flatten_result.instrs.push(temp);
    flatten_result.result_stack.push(temp_name);
    flatten_result
}

/// Flattens a Bare C expression into BRIL instructions.
/// Returns the resulting instruction sequence and the name of the variable holding the result
/// as the top of the result stack.
pub(super) fn flatten_aexpr(
    expr: AExpr,
    mut flatten_result: FlattenResult,
) -> FlattenResult {
    match expr {
        AExpr::Num(c) => {
            let temp_name = format!("_t{}", flatten_result.cur_temp_id);
            let temp = Instruction::Constant {
                dest: temp_name.clone(),
                op: ConstOps::Const,
                const_type: bril_rs::Type::Int,
                value: bril_rs::Literal::Int(c),
                pos: None,
            };
            flatten_result.cur_temp_id += 1;
            flatten_result.instrs.push(temp);
            flatten_result.result_stack.push(temp_name);
            flatten_result
        }
        AExpr::Id(id) => {
            flatten_result.result_stack.push(id);
            flatten_result
        }
        AExpr::Redundant(e) | AExpr::LoopInvariant(e) => {
            flatten_aexpr(*e, flatten_result)
        }
        AExpr::Add(lhs, rhs) => {
            flatten_abinop(flatten_result, *lhs, *rhs, ValueOps::Add, Type::Int)
        }
        AExpr::Sub(lhs, rhs) => {
            flatten_abinop(flatten_result, *lhs, *rhs, ValueOps::Sub, Type::Int)
        }
        AExpr::Mul(lhs, rhs) => {
            flatten_abinop(flatten_result, *lhs, *rhs, ValueOps::Mul, Type::Int)
        }
        AExpr::Div(lhs, rhs) => {
            flatten_abinop(flatten_result, *lhs, *rhs, ValueOps::Div, Type::Int)
        }
        AExpr::Derived(e) => flatten_aexpr(*e, flatten_result),
        AExpr::FCall(..) => unimplemented!(),
    }
}

/// Flattens a Bare C boolean expression into BRIL instructions.
/// Returns the resulting instruction sequence and the name of the variable holding the result
/// as the top of the result stack.
#[allow(clippy::too_many_lines)]
pub(super) fn flatten_bexpr(
    expr: BExpr,
    mut flatten_result: FlattenResult,
) -> FlattenResult {
    match expr {
        BExpr::Bool(b) => {
            let temp_name = format!("_t{}", flatten_result.cur_temp_id);
            let temp = Instruction::Constant {
                dest: temp_name.clone(),
                op: ConstOps::Const,
                const_type: bril_rs::Type::Bool,
                value: bril_rs::Literal::Bool(b),
                pos: None,
            };
            flatten_result.cur_temp_id += 1;
            flatten_result.instrs.push(temp);
            flatten_result.result_stack.push(temp_name);
            flatten_result
        }
        BExpr::Eqa(lhs, rhs) => {
            flatten_abinop(flatten_result, *lhs, *rhs, ValueOps::Eq, Type::Bool)
        }
        BExpr::Eqb(lhs, rhs) => {
            flatten_bbinop(flatten_result, *lhs, *rhs, ValueOps::Eq)
        }
        BExpr::Le(lhs, rhs) => {
            flatten_abinop(flatten_result, *lhs, *rhs, ValueOps::Le, Type::Bool)
        }
        BExpr::Ge(lhs, rhs) => {
            flatten_abinop(flatten_result, *lhs, *rhs, ValueOps::Ge, Type::Bool)
        }
        BExpr::Lt(lhs, rhs) => {
            flatten_abinop(flatten_result, *lhs, *rhs, ValueOps::Lt, Type::Bool)
        }
        BExpr::Gt(lhs, rhs) => {
            flatten_abinop(flatten_result, *lhs, *rhs, ValueOps::Gt, Type::Bool)
        }
        BExpr::And(lhs, rhs) => {
            flatten_bbinop(flatten_result, *lhs, *rhs, ValueOps::And)
        }
        BExpr::Or(lhs, rhs) => {
            flatten_bbinop(flatten_result, *lhs, *rhs, ValueOps::Or)
        }
        BExpr::LoopInvariant(e) | BExpr::Redundant(e) => {
            flatten_bexpr(*e, flatten_result)
        }
        BExpr::Not(e) => {
            flatten_result = flatten_bexpr(*e, flatten_result);
            let temp_name = format!("_t{}", flatten_result.cur_temp_id);
            let temp = Instruction::Value {
                dest: temp_name.clone(),
                op: ValueOps::Not,
                op_type: Type::Bool,
                args: vec![flatten_result.result_stack.pop().unwrap()],
                funcs: vec![],
                labels: vec![],
                pos: None,
            };
            flatten_result.cur_temp_id += 1;
            flatten_result.instrs.push(temp);
            flatten_result.result_stack.push(temp_name);
            flatten_result
        }
        BExpr::Id(id) => {
            flatten_result.result_stack.push(id);
            flatten_result
        }
        BExpr::FCall(..) => unimplemented!(),
    }
}

/// Flattens a Bare C expression into BRIL instructions.
/// Returns the resulting instruction sequence and the name of the variable holding the result
/// as the top of the result stack.
pub(super) fn flatten_expr(
    expr: Expr,
    flatten_result: FlattenResult,
) -> FlattenResult {
    match expr {
        Expr::AExpr(e) => flatten_aexpr(e, flatten_result),
        Expr::BExpr(e) => flatten_bexpr(e, flatten_result),
    }
}
