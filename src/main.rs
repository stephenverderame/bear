#![warn(clippy::pedantic, clippy::nursery)]
#![deny(clippy::all)]
#![allow(clippy::wildcard_imports)]
mod bare_c;
mod pcfg;
extern crate strum;
extern crate support;
use std::time::Duration;

use bril_rs::Program;
use clap::Parser;
use generator::FArg;
use pcfg::*;
use runner::CompilerStage;

use crate::runner::{differential_test, gen_main_args};

mod generator;
mod lowering;
mod runner;
#[macro_use]
extern crate lazy_static;
#[cfg(test)]
mod test;

#[derive(Parser)]
struct Args {
    stages: Vec<String>,
}

#[allow(clippy::too_many_lines)]
fn main() {
    let cli = Args::parse();
    let pcfg = pcfg::TopPCFG::uniform();
    let args = vec![
        FArg::int("a", 0, 100),
        FArg::int("b", -50, 50),
        FArg::int("c", 0, 100),
        FArg::bool("d"),
        FArg::bool("e"),
    ];
    let stages = cli_to_stages(cli.stages);
    for _ in 0..1 {
        let prog = generator::gen_function(&pcfg, &args);
        println!("{}", bare_c::display(&prog));
        println!("DONE");
        let prog = lowering::lower(prog);
        let prog = serde_json::to_string(&to_prog(prog.to_src(true))).unwrap();
        let prog_args = gen_main_args(&args);
        match differential_test(
            &prog,
            &stages,
            prog_args,
            Duration::from_secs(5),
        ) {
            runner::TestResult::Success => (),
            runner::TestResult::Fail { .. } => {
                panic!("Differential test failed");
            }
        }
    }
}

fn to_prog(body: Vec<bril_rs::Code>) -> Program {
    Program {
        functions: vec![bril_rs::Function {
            name: "main".to_string(),
            args: vec![
                bril_rs::Argument {
                    name: String::from("a"),
                    arg_type: bril_rs::Type::Int,
                },
                bril_rs::Argument {
                    name: String::from("b"),
                    arg_type: bril_rs::Type::Int,
                },
                bril_rs::Argument {
                    name: String::from("c"),
                    arg_type: bril_rs::Type::Int,
                },
                bril_rs::Argument {
                    name: String::from("d"),
                    arg_type: bril_rs::Type::Bool,
                },
                bril_rs::Argument {
                    name: String::from("e"),
                    arg_type: bril_rs::Type::Bool,
                },
            ],
            instrs: body,
            return_type: None,
            pos: None,
        }],
    }
}

/// Converts a vector of strings, where each string is a command to run,
/// into a vector of `CompilerStages`.
fn cli_to_stages(v: Vec<String>) -> Vec<CompilerStage> {
    let mut res = vec![];
    for s in v {
        let mut split = s.split(' ');
        let cmd = split.next().unwrap().to_string();
        let args = split.map(ToString::to_string).collect();
        res.push(CompilerStage { cmd, args });
    }
    res
}
