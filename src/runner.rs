use rand::Rng;
use std::{
    io::Read,
    process::{Command, Stdio},
};
use std::{io::Write, time::Duration};
use wait_timeout::ChildExt;

use crate::generator::FArg;

/// A command line compiler stage
#[derive(Debug, Clone)]
pub struct CompilerStage {
    pub cmd: String,
    pub args: Vec<String>,
}

/// An error that can occur while running a program
#[derive(Debug)]
pub enum RunnerError {
    Timeout(String),
    WaitingError(String, String),
    Utf8Conversion(String),
    StageError(i32, String),
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
    for (mut stage, name) in processes.into_iter().zip(stage_names.into_iter())
    {
        let mut child = stage.spawn().expect("Failed to spawn process");
        let stdin = child.stdin.as_mut().unwrap();
        stdin.write_all(&buf).unwrap();
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
                return Err(RunnerError::Timeout(name));
            }
            Err(e) => {
                return Err(RunnerError::WaitingError(format!("{e}"), name))
            }
        }
        let stdout = child.stdout.as_mut().unwrap();
        buf.clear();
        stdout.read_to_end(&mut buf).unwrap();
    }
    String::from_utf8(buf)
        .map_err(|e| RunnerError::Utf8Conversion(format!("{e}")))
}

/// Generates a set of arguments for a program based on the
/// allowed argument types and ranges.
/// # Returns
/// A vector of strings representing the arguments to pass to the program
/// in the same order as `args`.
pub fn gen_main_args(args: &[FArg]) -> Vec<String> {
    let mut rng = rand::thread_rng();
    args.iter()
        .map(|arg| match arg {
            FArg::Int { interval, .. } => {
                rng.gen_range(interval.to_range()).to_string()
            }
            FArg::Bool { .. } => rng.gen_bool(0.5).to_string(),
        })
        .collect()
}
