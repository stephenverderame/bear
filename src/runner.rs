use rand::Rng;
use std::{
    io::Read,
    process::{Child, Command, Stdio},
};
use std::{io::Write, time::Duration};
use wait_timeout::ChildExt;

use crate::generator::{rnd, FArg};

/// A command line compiler stage
#[derive(Debug, Clone)]
pub struct CompilerStage {
    pub cmd: String,
    pub args: Vec<String>,
}

/// An error that can occur while running a program
#[derive(Debug)]
pub enum RunnerError {
    /// Compiler stage timed out
    Timeout(String),
    /// Error waiting for result from a compiler stage
    WaitingError(String, String),
    /// Error converting final output to string
    Utf8Conversion(String),
    /// Nonzero error code from a compiler stage
    StageError(i32, String),
}

/// Reads the stdout from the child process into `buf`.
/// Returns a string representation of the output.
fn read_stdout(
    child: &mut Child,
    buf: &mut Vec<u8>,
    error_msg: &str,
) -> Result<String, RunnerError> {
    let stdout = child.stdout.as_mut().unwrap();
    buf.clear();
    stdout.read_to_end(buf).unwrap();
    String::from_utf8(buf.clone())
        .map_err(|e| RunnerError::Utf8Conversion(format!("{error_msg}: {e}")))
}

/// Runs a program through the `brili` interpreter, returning the output.
/// If `pipeline` is `Some`, the program will be piped through the given
/// compiler stages before being run.
/// # Arguments
/// * `prog_json` - The program to run, as a JSON string
/// * `pipeline` - The compiler stages to pipe the program through
/// * `brili_args` - The arguments to pass to the `brili` interpreter
/// * `timeout` - The maximum amount of time to wait for any stage
///     of this program to finish
/// # Errors
/// Returns an error if the program times out, if there is an error waiting
pub fn run_prog(
    prog_json: &str,
    pipeline: Option<&[CompilerStage]>,
    brili_args: Vec<String>,
    timeout: Duration,
    enable_retries: bool,
) -> Result<String, RunnerError> {
    let mut buf = prog_json.as_bytes().to_owned();
    let mut processes = vec![];
    let mut stage_names = vec![];
    let mut pipeline =
        pipeline.map(<[CompilerStage]>::to_vec).unwrap_or_default();
    pipeline.push(CompilerStage {
        cmd: "brili".to_string(),
        args: brili_args,
    });
    for stage in pipeline {
        let mut process = Command::new(&stage.cmd);
        process.args(&stage.args);
        process.stdin(Stdio::piped());
        process.stdout(Stdio::piped());
        processes.push(process);
        stage_names.push(stage.cmd.clone());
    }
    for (stage, name) in processes.into_iter().zip(stage_names.into_iter()) {
        run_stage_with_retries(&mut buf, stage, name, timeout, enable_retries)?;
    }
    String::from_utf8(buf)
        .map_err(|e| RunnerError::Utf8Conversion(format!("{e}")))
}

/// Runs the command `stage`, reading stdin from `buf` and writing stdout to `buf`.
/// If a timeout occurs and `enable_retries` is true, the stage will be retried once.
fn run_stage_with_retries(
    buf: &mut Vec<u8>,
    mut stage: Command,
    name: String,
    timeout: Duration,
    enable_retries: bool,
) -> Result<(), RunnerError> {
    // we allow retries bc there seems to be a thing where a timeout occurs
    // spuriously

    // I realized this is bc the process hangs if its output pipe gets filled
    // (default pipe buffer is 16 pages or 65,536 bytes)
    let mut retry = false;
    loop {
        let mut child = stage
            .spawn()
            .unwrap_or_else(|_| panic!("Failed to spawn {name}"));
        let stdin = child.stdin.as_mut().unwrap();
        stdin.write_all(buf).unwrap();
        match child.wait_timeout(timeout) {
            Ok(Some(status)) if status.code() == Some(0) => (),
            Ok(Some(status)) => {
                return Err(RunnerError::StageError(
                    status.code().unwrap_or(-1),
                    name,
                ))
            }
            Ok(None) => {
                child.kill().unwrap();
                if !retry && enable_retries {
                    retry = true;
                    continue;
                }
                return Err(RunnerError::Timeout(name));
            }
            Err(e) => {
                return Err(RunnerError::WaitingError(format!("{e}"), name))
            }
        }
        let stdout = child.stdout.as_mut().unwrap();
        buf.clear();
        stdout.read_to_end(buf).unwrap();
        break;
    }
    Ok(())
}

/// Generates a set of arguments for a program based on the
/// allowed argument types and ranges.
/// # Returns
/// A vector of strings representing the arguments to pass to the program
/// in the same order as `args`.
pub fn gen_main_args(args: &[FArg]) -> Vec<String> {
    let mut rng = rnd::get_rng();
    args.iter()
        .map(|arg| match arg {
            FArg::Int { interval, .. } => {
                rng.gen_range(interval.to_range()).to_string()
            }
            FArg::Bool { .. } => rng.gen_bool(0.5).to_string(),
        })
        .collect()
}
