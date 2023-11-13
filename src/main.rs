#![warn(clippy::pedantic, clippy::nursery)]
#![deny(clippy::all)]
#![allow(clippy::wildcard_imports)]
mod bare_c;
mod pcfg;
extern crate strum;
extern crate support;
use bril_rs::Program;
use pcfg::*;

use crate::generator::{rnd, FArg};
mod generator;
mod lowering;
mod runner;
#[macro_use]
extern crate lazy_static;

#[allow(clippy::too_many_lines)]
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
        let prog = vec![bare_c::Block::<bare_c::Statement>::Switch {
            guard: bare_c::AExpr::Add(
                Box::new(bare_c::AExpr::Id("a".to_string())),
                Box::new(bare_c::AExpr::Id("b".to_string())),
            ),
            cases: vec![
                (
                    bare_c::AExpr::Num(10),
                    vec![bare_c::Block::Stmt(bare_c::Statement::Assign {
                        dest: "a".to_string(),
                        src: bare_c::Expr::AExpr(bare_c::AExpr::Num(10)),
                    })],
                ),
                (
                    bare_c::AExpr::Num(20),
                    vec![bare_c::Block::If {
                        guard: bare_c::BExpr::Eqa(
                            Box::new(bare_c::AExpr::Id("a".to_string())),
                            Box::new(bare_c::AExpr::Num(10)),
                        ),
                        then: vec![bare_c::Block::Stmt(
                            bare_c::Statement::Assign {
                                dest: "a".to_string(),
                                src: bare_c::Expr::AExpr(bare_c::AExpr::Num(
                                    20,
                                )),
                            },
                        )],
                        otherwise: vec![bare_c::Block::Stmt(
                            bare_c::Statement::Assign {
                                dest: "a".to_string(),
                                src: bare_c::Expr::AExpr(bare_c::AExpr::Num(
                                    30,
                                )),
                            },
                        )],
                    }],
                ),
            ],
            default: vec![bare_c::Block::Stmt(bare_c::Statement::Assign {
                dest: "a".to_string(),
                src: bare_c::Expr::AExpr(bare_c::AExpr::Num(20)),
            })],
        }];
        let test_args = vec![
            FArg::int("a", -10, 20),
            FArg::int("b", -20, 20),
            FArg::int("c", 0, 100),
            FArg::bool("d"),
            FArg::bool("e"),
        ];
        let prog = generator::gen_function(&pcfg, &test_args);
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

    use crate::{
        bare_c::display,
        generator::{rnd, FArg},
        pcfg::PCFG,
    };

    #[test]
    fn end_to_end() {
        let test_args = vec![
            FArg::int("a", -10, 20),
            FArg::int("b", -20, 20),
            FArg::int("c", 0, 100),
            FArg::bool("d"),
            FArg::bool("e"),
        ];
        for _ in 0..100 {
            rnd::reseed();
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
                Duration::from_secs(10),
                true,
            );
            if output.is_err() {
                std::fs::write("failed_test.bril", prog_json).unwrap();
                let out = format!("Failed test with {output:?} and args {args:#?}\nFailed seed: {}\n", rnd::get_seed());
                eprintln!("{out}");
                std::fs::write("failed_log.txt", out).unwrap();
                std::fs::write("failed_test.brc", display(&brc_prog)).unwrap();
            }
            assert!(output.is_ok());
        }
    }
}
