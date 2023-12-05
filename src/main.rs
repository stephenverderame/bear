#![warn(clippy::pedantic, clippy::nursery)]
#![deny(clippy::all)]
#![allow(clippy::wildcard_imports, clippy::module_name_repetitions)]
mod bare_c;
mod pcfg;
extern crate strum;
extern crate support;

use bare_c::display;
use bril_rs::Program;
use clap::Parser;
use generator::FArg;
use runner::CompilerStage;
use search::find_bugs;

use crate::pcfg::random_pcfg;
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

    /// Specify this option to just print a random BareC program
    #[clap(long, short)]
    sample: bool,
}

fn main() {
    let cli = Args::parse();
    let stages = cli_to_stages(cli.stages);
    if cli.sample {
        print_random_prog();
    } else {
        find_bugs(&stages);
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

fn print_random_prog() {
    let pcfg = random_pcfg();
    let args = vec![
        FArg::int("a", 0, 100),
        FArg::int("b", -50, 50),
        FArg::int("c", 0, 100),
        FArg::bool("d"),
        FArg::bool("e"),
    ];
    let brc = generator::gen_function(&pcfg, &args);
    println!("{}", display(&brc));
    let prog = lowering::lower(brc);
    let bril = to_prog(prog.to_src(true));
    let prog = serde_json::to_string(&bril).unwrap();
    println!();
    println!();
    println!("{prog}");
}
