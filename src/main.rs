#![warn(clippy::pedantic, clippy::nursery)]
#![deny(clippy::all)]
#![allow(clippy::wildcard_imports)]
mod bare_c;
mod pcfg;
extern crate strum;
extern crate support;
use pcfg::*;
mod generator;
mod lowering;

fn main() {
    let pcfg = pcfg::TopPCFG::uniform();
    for _ in 0..1 {
        println!("{}", generator::display(&generator::gen_program(&pcfg)));
        println!("DONE");
    }
}
