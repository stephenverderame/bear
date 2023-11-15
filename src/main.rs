#![warn(clippy::pedantic, clippy::nursery)]
#![deny(clippy::all)]
#![allow(clippy::wildcard_imports)]
mod bare_c;
mod pcfg;
extern crate strum;
extern crate support;
use bril_rs::Program;
use generator::FArg;
use pcfg::*;

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
        FArg::int("a", 0, 100),
        FArg::int("b", -50, 50),
        FArg::int("c", 0, 100),
        FArg::bool("d"),
        FArg::bool("e"),
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
