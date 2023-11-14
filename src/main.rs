#![warn(clippy::pedantic, clippy::nursery)]
#![deny(clippy::all)]
#![allow(clippy::wildcard_imports)]
mod bare_c;
mod pcfg;
extern crate strum;
extern crate support;
use bril_rs::Program;
use pcfg::*;

use crate::generator::FArg;
mod generator;
mod lowering;
mod runner;
#[macro_use]
extern crate lazy_static;
#[cfg(test)]
mod test;

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
        //let prog = generator::gen_function(&pcfg, &args);
        let prog = vec![
            bare_c::Block::Stmt(bare_c::Statement::Print(vec![
                bare_c::Expr::AExpr(bare_c::AExpr::Id(String::from("c"))),
            ])),
            bare_c::Block::TryCatch {
                try_block: vec![
                    bare_c::Block::Stmt(bare_c::Statement::Throw(
                        0,
                        Some(bare_c::Expr::AExpr(bare_c::AExpr::Id(
                            String::from("b"),
                        ))),
                    )),
                    bare_c::Block::Stmt(bare_c::Statement::Print(vec![
                        bare_c::Expr::AExpr(bare_c::AExpr::Id(String::from(
                            "a",
                        ))),
                    ])),
                ],
                catch_block: vec![bare_c::Block::Stmt(
                    bare_c::Statement::Print(vec![bare_c::Expr::AExpr(
                        bare_c::AExpr::Id(String::from("foo")),
                    )]),
                )],
                catch_name: Some(String::from("foo")),
            },
            bare_c::Block::Stmt(bare_c::Statement::Print(vec![
                bare_c::Expr::BExpr(bare_c::BExpr::Id(String::from("d"))),
            ])),
        ];
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
