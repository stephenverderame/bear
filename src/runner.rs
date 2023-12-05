use rand::Rng;
use std::{
    fs::File,
    io::Read,
    path::Path,
    process::{Command, Stdio},
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
#[allow(clippy::module_name_repetitions)]
pub enum RunnerError {
    /// Compiler stage timed out
    Timeout(String),
    /// Error waiting for result from a compiler stage
    WaitingError(String, String),
    /// Error converting final output to string
    Utf8Conversion(String),
    /// Nonzero error code from a compiler stage
    StageError(i32, String, String),
    /// Error reading/writing or creating a file
    FileError(String),
}

const INTERPRETER: &str = "brili-t";

/// The result of running a program
#[derive(Debug, PartialEq, Eq)]
pub struct RunResult {
    pub stdout: String,
    pub stderr: String,
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
    out_name: &str,
) -> Result<RunResult, RunnerError> {
    let mut buf = prog_json.as_bytes().to_owned();
    let mut err = vec![];
    let mut processes = vec![];
    let mut stage_names = vec![];
    let pipeline = pipeline.map(<[CompilerStage]>::to_vec).unwrap_or_default();
    let temp_file = format!("{out_name}.temp");
    std::fs::create_dir_all(Path::new(&temp_file).parent().unwrap()).unwrap();
    for stage in pipeline {
        let mut process = Command::new(&stage.cmd);
        process.args(&stage.args);
        process.stdin(Stdio::piped());
        process.stderr(Stdio::piped());
        processes.push(process);
        stage_names.push(stage.cmd.clone());
    }
    let mut interpreter = Command::new(INTERPRETER);
    interpreter.args(brili_args);
    interpreter.stdin(Stdio::piped());
    let out_file = format!("{out_name}.out");
    let final_out = File::create(&out_file).unwrap();
    interpreter.stdout(Stdio::from(final_out));
    interpreter.stderr(Stdio::piped());
    stage_names.push(String::from(INTERPRETER));
    processes.push(interpreter);
    for (stage, name) in processes.into_iter().zip(stage_names.into_iter()) {
        let temp_file: Option<&str> = if name == INTERPRETER {
            None
        } else {
            Some(&temp_file)
        };
        run_stage(&mut buf, &mut err, stage, name, timeout, temp_file)?;
        err.push(b'\n');
    }
    match (std::fs::read_to_string(&out_file), String::from_utf8(err)) {
        (Ok(stdout), Ok(stderr)) => Ok(RunResult { stdout, stderr }),
        (Err(e), _) => Err(RunnerError::Utf8Conversion(format!(
            "Error reading output file {out_file}: {e}"
        ))),
        (_, Err(e)) => Err(RunnerError::Utf8Conversion(format!(
            "Error reading stderr: {e}"
        ))),
    }
}

/// Runs the command `stage`, reading stdin from `buf` and writing stdout to `buf`.
/// If a timeout occurs and `enable_retries` is true, the stage will be retried once.
fn run_stage(
    buf: &mut Vec<u8>,
    err: &mut Vec<u8>,
    mut stage: Command,
    name: String,
    timeout: Duration,
    temp_file: Option<&str>,
) -> Result<(), RunnerError> {
    if let Some(temp_file) = temp_file {
        stage.stdout(Stdio::from(File::create(temp_file).map_err(|x| {
            RunnerError::FileError(format!("Error creating temp file: {x}"))
        })?));
    }
    let mut child = stage
        .spawn()
        .unwrap_or_else(|_| panic!("Failed to spawn {name}"));
    let stdin = child.stdin.as_mut().unwrap();
    stdin.write_all(buf).unwrap();
    match child.wait_timeout(timeout) {
        Ok(Some(status)) if status.code() == Some(0) => (),
        Ok(Some(status)) => {
            if let Some(mut stderr) = child.stderr {
                stderr.read_to_end(err).unwrap();
            }
            return Err(RunnerError::StageError(
                status.code().unwrap_or(-1),
                name,
                String::from_utf8(err.clone()).unwrap(),
            ));
        }
        Ok(None) => {
            child.kill().unwrap();
            return Err(RunnerError::Timeout(name));
        }
        Err(e) => return Err(RunnerError::WaitingError(format!("{e}"), name)),
    }
    buf.clear();
    if let Some(temp_file) = temp_file {
        *buf = std::fs::read_to_string(temp_file)
            .map_err(|e| {
                RunnerError::FileError(format!("Error reading temp file: {e}"))
            })?
            .as_bytes()
            .to_owned();
    } else if let Some(mut stdout) = child.stdout {
        stdout.read_to_end(buf).unwrap();
    }
    if let Some(mut stderr) = child.stderr {
        stderr.read_to_end(err).unwrap();
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

/// Runs the program directly through the interpreter and through the pipeline.
/// If the result after running through the pipeline is different from the
/// result of running directly through the interpreter, returns `false`.
/// Otherwise, returns `true`.
///
/// Stores stdout temporarily in `out/real.out` and `out/test.out`.
///
/// # Returns
/// `true` if the program passes the differential test, `false` otherwise.
pub fn differential_test(
    prog_json: &str,
    pipeline: &[CompilerStage],
    brili_args: Vec<String>,
    timeout: Duration,
    trace_out: Option<&str>,
    out_base: &str,
) -> TestResult {
    let mut first_args = trace_out
        .map_or_else(Vec::new, |out| vec![String::from("-t"), out.to_string()]);
    first_args.extend(brili_args.clone());
    run_prog(prog_json, None, first_args, timeout, &format!("{out_base}_real")).map_or_else(|_| {
        eprintln!(
            "\nError running program through interpreter. This is likely a bug in the fuzzer"
        );
        TestResult::Success
    }, |res| match run_prog(
            prog_json,
            Some(pipeline),
            brili_args,
            timeout,
            &format!("{out_base}_test"),
        ) {
            Ok(res2) if res2.stdout == res.stdout => TestResult::Success,
            actual => TestResult::Fail {
                expected: res,
                actual,
            },
        })
}

/// The result of a differential test
#[must_use]
pub enum TestResult {
    Success,
    Fail {
        expected: RunResult,
        actual: Result<RunResult, RunnerError>,
    },
}
