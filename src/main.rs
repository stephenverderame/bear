#![warn(clippy::pedantic, clippy::nursery)]
#![deny(clippy::all)]
#![allow(clippy::wildcard_imports)]
mod bare_c;
mod pcfg;
extern crate strum;
extern crate support;
use bril_rs::Program;
use pcfg::*;
mod generator;
mod lowering;
mod runner;

fn main() {
    let pcfg = pcfg::TopPCFG::uniform();
    let args = vec![
        generator::FArg::int("a", 0, 100),
        generator::FArg::int("b", -50, 50),
        generator::FArg::int("c", 0, 100),
        generator::FArg::bool("d"),
        generator::FArg::bool("e"),
    ];
    for _ in 0..1 {
        let prog = generator::gen_function(&pcfg, &args);
        println!("{}", bare_c::display(&prog));
        println!("DONE");
        let prog = lowering::lower(prog);
        println!(
            "{}",
            serde_json::to_string(&to_prog(prog.to_src(true))).unwrap()
        );
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

#[cfg(test)]
mod test {
    use std::time::Duration;

    use crate::{bare_c::display, generator::FArg, pcfg::PCFG};

    #[test]
    fn end_to_end() {
        let test_args = vec![
            FArg::int("a", -10, 20),
            FArg::int("b", -20, 20),
            FArg::int("c", 0, 100),
            FArg::bool("d"),
            FArg::bool("e"),
        ];
        for _ in 0..30 {
            let pcfg = crate::pcfg::TopPCFG::uniform();
            let brc_prog = crate::generator::gen_function(&pcfg, &test_args);
            let prog = crate::lowering::lower(brc_prog.clone());
            let prog = crate::to_prog(prog.to_src(true));
            let prog_json = serde_json::to_string(&prog).unwrap();
            let args = crate::runner::gen_main_args(&test_args);
            let output = crate::runner::run_prog(
                &prog_json,
                None,
                args.clone(),
                Duration::from_secs(20),
            );
            if output.is_err() {
                std::fs::write("failed_test.bril", prog_json).unwrap();
                eprintln!("Failed test with {output:?} and args {args:#?}");
                std::fs::write("failed_test.brc", display(&brc_prog)).unwrap();
            }
            assert!(output.is_ok());
        }
    }
}
