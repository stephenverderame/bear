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

    /// Specify this option to just print a random BareC program, ignoring
    /// the rest of the arguments
    #[clap(long, short)]
    sample: bool,

    /// Specify the number of threads (independent populations) to use
    #[clap(long, short, default_value = "6")]
    threads: usize,

    /// Specify how many generations to spawn before saving the the parent set's
    /// novelties and behavior vectors for each thread/population (0 to disable).
    /// The parent set is the set of individuals of the previous generation that
    /// passed on their genes to the current generation.
    #[clap(long, alias = "ln", default_value = "0")]
    log_novelties: u64,

    /// Specify after how many tests to generate from a population before
    /// saving the test (0 to disable)
    #[clap(long, alias = "lt", default_value = "0")]
    log_tests: u64,

    /// Specify the maximum number of mutations to apply to a single program
    /// during reproduction
    #[clap(long, alias = "mut", default_value = "10")]
    max_mutations: usize,

    /// Specify the maximum number of crossovers to apply to a single program
    /// during reproduction
    #[clap(long, alias = "cross", default_value = "5")]
    max_crossovers: usize,

    /// Specify the population size which is the number of PCFGs in a population.
    /// The number of tests generated from a population is equal to the population
    /// size times 3.
    #[clap(long, short, default_value = "10")]
    pop_size: usize,

    /// Specify the percentage of programs to select from a population to generate
    /// the next generation. Defaults to one-third.
    /// The select set size will be `round(pop_size * select_percentage)`
    #[clap(long, alias = "select", default_value = "0.33")]
    select_percentage: f64,
}

impl Args {
    #[allow(
        clippy::cast_possible_truncation,
        clippy::cast_sign_loss,
        clippy::cast_precision_loss
    )]
    fn search_params(&self) -> search::SearchParams {
        assert!(
            self.select_percentage > 0.0,
            "select_percentage must be > 0"
        );
        assert!(
            self.select_percentage <= 1.0,
            "select_percentage must be <= 1"
        );
        assert!(self.pop_size > 0, "pop_size must be > 0");
        assert!(self.threads > 0, "threads must be > 0");
        assert!(self.max_mutations > 1, "max_mutations must be > 1");
        assert!(self.max_crossovers > 1, "max_crossovers must be > 1");
        search::SearchParams {
            threads: self.threads,
            log_novelties: self.log_novelties,
            log_tests: self.log_tests,
            max_num_mutations: self.max_mutations,
            max_num_crosses: self.max_crossovers,
            pop_size: self.pop_size,
            select_size: (self.pop_size as f64 * self.select_percentage).round()
                as usize,
        }
    }
}

fn main() {
    let cli = Args::parse();
    let search_params = cli.search_params();
    let stages = cli_to_stages(cli.stages);
    if cli.sample {
        print_random_prog();
    } else {
        find_bugs(&stages, &search_params);
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
