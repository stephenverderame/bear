use std::{
    collections::HashMap,
    ops::{Div, Mul},
};

use bril_rs::{EffectOps, Instruction, ValueOps};

use crate::{
    lowering::{
        TRACE_BREAK, TRACE_CATCH, TRACE_CONTINUE, TRACE_LOOP_NEST,
        TRACE_MATCH_CASE, TRACE_SWITCH_DEFAULT,
    },
    runner::{RunnerError, TestResult},
};

const fn map_val_op_info(
    op: ValueOps,
    mut behavior_vec: BehaviorVec,
) -> BehaviorVec {
    match op {
        bril_rs::ValueOps::Mul => behavior_vec.mul_count += 1,
        bril_rs::ValueOps::Div => behavior_vec.div_count += 1,
        bril_rs::ValueOps::Add => behavior_vec.add_count += 1,
        bril_rs::ValueOps::Sub => behavior_vec.sub_count += 1,
        bril_rs::ValueOps::And => behavior_vec.and_count += 1,
        bril_rs::ValueOps::Or => behavior_vec.or_count += 1,
        bril_rs::ValueOps::Eq => behavior_vec.eq_count += 1,
        bril_rs::ValueOps::Lt => behavior_vec.lt_count += 1,
        bril_rs::ValueOps::Gt => behavior_vec.gt_count += 1,
        bril_rs::ValueOps::Le => behavior_vec.le_count += 1,
        bril_rs::ValueOps::Ge => behavior_vec.ge_count += 1,
        _ => (),
    }
    behavior_vec
}

fn map_trace_info(
    args: &[String],
    _labels: &[String],
    mut bv: BehaviorVec,
) -> BehaviorVec {
    if !args.is_empty() {
        match args[0].as_str() {
            TRACE_LOOP_NEST => {
                bv.max_loop_nest =
                    bv.max_loop_nest.max(args[1].parse().unwrap());
            }
            TRACE_CATCH => bv.catches += 1,
            TRACE_CONTINUE => bv.continue_count += 1,
            TRACE_BREAK => bv.break_count += 1,
            TRACE_MATCH_CASE => bv.switch_cases += 1,
            TRACE_SWITCH_DEFAULT => {
                if let Some(last) = args.get(1) {
                    if last == "true" {
                        bv.duffs_count += 1;
                    } else {
                        bv.switch_default += 1;
                    }
                }
            }
            _ => (),
        }
    }
    bv
}

/// A summary of the behavior of a program execution.
#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct BehaviorVec {
    failure_type: FailureType,
    icount: i64,
    mul_count: i64,
    div_count: i64,
    add_count: i64,
    sub_count: i64,
    and_count: i64,
    or_count: i64,
    eq_count: i64,
    lt_count: i64,
    gt_count: i64,
    le_count: i64,
    ge_count: i64,
    jmp_count: i64,
    br_count: i64,
    max_loop_nest: i64,
    catches: i64,
    duffs_count: i64,
    switch_cases: i64,
    switch_default: i64,
    break_count: i64,
    continue_count: i64,
    max_repeat_jmps: i64,
}

impl BehaviorVec {
    /// Constructs a new `BehaviorVec` from a trace file and a failure type.
    pub fn new(trace_file: &str, fail: FailureType) -> Self {
        let mut behavior_vec = Self::default();
        let mut jmps = HashMap::new();
        for line in std::fs::read_to_string(trace_file).unwrap().lines() {
            if let Ok(instr) = serde_json::from_str::<Instruction>(line) {
                behavior_vec.icount += 1;
                match instr {
                    Instruction::Value { op, .. } => {
                        behavior_vec = map_val_op_info(op, behavior_vec);
                    }
                    Instruction::Effect {
                        labels,
                        op: EffectOps::Jump,
                        ..
                    } => {
                        behavior_vec.jmp_count += 1;
                        if let Some(label) = labels.first() {
                            *jmps.entry(label.clone()).or_insert(0) += 1;
                        }
                    }
                    Instruction::Effect {
                        op: EffectOps::Branch,
                        ..
                    } => {
                        behavior_vec.br_count += 1;
                    }
                    Instruction::Effect {
                        args,
                        labels,
                        op: EffectOps::Nop,
                        ..
                    } => {
                        behavior_vec =
                            map_trace_info(&args, &labels, behavior_vec);
                    }
                    _ => (),
                }
            }
        }
        behavior_vec.max_repeat_jmps =
            jmps.values().copied().max().unwrap_or(0);
        behavior_vec.failure_type = fail;
        behavior_vec
    }

    /// The distance, in behavior space, between two `BehaviorVec`s.
    #[allow(clippy::too_many_lines, clippy::cast_precision_loss)]
    pub fn dist(a: &Self, b: &Self) -> f64 {
        const INSTR_TYPE_DIVISOR: f64 = 100.0;
        let mut dist = 0.0;
        dist += (a.icount - b.icount).pow(2) as f64;
        dist += ((a.failure_type as i64) - (b.failure_type as i64))
            .mul(10_000)
            .pow(2) as f64;
        dist += ((a.mul_count - b.mul_count) as f64)
            .div(INSTR_TYPE_DIVISOR)
            .powi(2);
        dist += ((a.div_count - b.div_count) as f64)
            .div(INSTR_TYPE_DIVISOR)
            .powi(2);
        dist += ((a.add_count - b.add_count) as f64)
            .div(INSTR_TYPE_DIVISOR)
            .powi(2);
        dist += ((a.sub_count - b.sub_count) as f64)
            .div(INSTR_TYPE_DIVISOR)
            .powi(2);
        dist += ((a.and_count - b.and_count) as f64)
            .div(INSTR_TYPE_DIVISOR)
            .powi(2);
        dist += ((a.or_count - b.or_count) as f64)
            .div(INSTR_TYPE_DIVISOR)
            .powi(2);
        dist += ((a.eq_count - b.eq_count) as f64)
            .div(INSTR_TYPE_DIVISOR)
            .powi(2);
        dist += ((a.lt_count - b.lt_count) as f64)
            .div(INSTR_TYPE_DIVISOR)
            .powi(2);
        dist += ((a.gt_count - b.gt_count) as f64)
            .div(INSTR_TYPE_DIVISOR)
            .powi(2);
        dist += ((a.le_count - b.le_count) as f64)
            .div(INSTR_TYPE_DIVISOR)
            .powi(2);
        dist += ((a.ge_count - b.ge_count) as f64)
            .div(INSTR_TYPE_DIVISOR)
            .powi(2);
        dist += ((a.jmp_count - b.jmp_count) as f64)
            .div(INSTR_TYPE_DIVISOR)
            .powi(2);
        dist += ((a.br_count - b.br_count) as f64)
            .div(INSTR_TYPE_DIVISOR)
            .powi(2);
        dist += (a.max_loop_nest - b.max_loop_nest).mul(100).pow(2) as f64;
        dist += (a.catches - b.catches).pow(2) as f64;
        dist += (a.duffs_count - b.duffs_count).pow(2) as f64;
        dist += (a.switch_cases - b.switch_cases).pow(2) as f64;
        dist += (a.switch_default - b.switch_default).pow(2) as f64;
        dist += (a.break_count - b.break_count).pow(2) as f64;
        dist += (a.continue_count - b.continue_count).pow(2) as f64;
        dist += (a.max_repeat_jmps - b.max_repeat_jmps).pow(2) as f64;
        dist.sqrt()
    }
}

/// The type of error that occurred during a test.
#[derive(Default, Debug, Clone, PartialEq, Eq, Copy)]
pub enum FailureType {
    #[default]
    Success,
    /// The output did not match the expected output, but was the same length
    Mismatch,
    /// The output was shorter than expected
    Shorter,
    /// The output was longer than expected
    Longer,
    /// The program timed out
    RunnerTimeout,
    /// The program failed with an error
    RunnerOther,
    /// The program failed with an error in a specific stage
    StageOther,
    /// The program failed with a division by zero error
    DivByZero,
    UnknownLabel,
    UndefinedVar,
    InvalidArgs,
    InvalidLabel,
    OpcodeErr,
    MiscErr,
}

impl From<&TestResult> for FailureType {
    fn from(value: &TestResult) -> Self {
        match value {
            TestResult::Success => Self::Success,
            TestResult::Fail {
                expected, actual, ..
            } => match actual {
                Err(RunnerError::Timeout(_)) => Self::RunnerTimeout,
                Err(RunnerError::StageError(_, stage, stderr)) => {
                    if stage.contains("brili") {
                        if stderr.contains("division by zero") {
                            Self::DivByZero
                        } else if stderr.contains("label")
                            && stderr.contains("not found")
                        {
                            Self::UnknownLabel
                        } else if stderr.contains("undefined variable") {
                            Self::UndefinedVar
                        } else if stderr.contains("argument") {
                            Self::InvalidArgs
                        } else if stderr.contains("label") {
                            Self::InvalidLabel
                        } else if stderr.contains("opcode") {
                            Self::OpcodeErr
                        } else {
                            Self::MiscErr
                        }
                    } else {
                        Self::StageOther
                    }
                }
                Err(_) => Self::RunnerOther,
                Ok(res) if res.stdout.len() < expected.stdout.len() => {
                    Self::Shorter
                }
                Ok(res) if res.stdout.len() > expected.stdout.len() => {
                    Self::Longer
                }
                Ok(_) => Self::Mismatch,
            },
        }
    }
}

impl From<TestResult> for FailureType {
    fn from(value: TestResult) -> Self {
        Self::from(&value)
    }
}
