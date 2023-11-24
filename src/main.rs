#![warn(clippy::pedantic, clippy::nursery)]
#![deny(clippy::all)]
#![allow(clippy::wildcard_imports)]
mod bare_c;
mod pcfg;
extern crate strum;
extern crate support;

use bril_rs::Program;
use clap::Parser;
use runner::CompilerStage;
use search::find_bugs;
mod search;

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
    let stages = cli_to_stages(cli.stages);
    find_bugs(&stages);
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
