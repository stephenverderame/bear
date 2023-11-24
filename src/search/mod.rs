use std::{
    process::exit,
    sync::mpsc::{channel, Receiver},
    time::Duration,
};

use knn::PointCloud;

use crate::{
    bare_c::{display, Block, Statement},
    generator::{self, FArg},
    lowering,
    pcfg::{random_pcfg, TopPCFG},
    runner::{differential_test, gen_main_args, CompilerStage, TestResult},
    to_prog,
};

use self::trace::{BehaviorVec, FailureType};

mod trace;

const POPULATION_SIZE: usize = 20;

pub fn find_bugs(test_pipeline: &[CompilerStage]) {
    let args = vec![
        FArg::int("a", 0, 100),
        FArg::int("b", -50, 50),
        FArg::int("c", 0, 100),
        FArg::bool("d"),
        FArg::bool("e"),
    ];
    let mut pop = std::iter::repeat_with(random_pcfg)
        .take(POPULATION_SIZE)
        .collect::<Vec<_>>();
    let mut archive = knn::PointCloud::new(BehaviorVec::dist);
    let (tx, mut rx) = channel();
    ctrlc::set_handler(move || {
        tx.send(()).unwrap();
    })
    .unwrap();
    let mut saved = vec![];
    let mut bug_count = 0;
    let mut test_count = 0;
    while rx.recv_timeout(Duration::from_millis(1)).is_err() {
        (bug_count, rx, test_count) = novelty_search_generation(
            &mut archive,
            &mut pop,
            &args,
            test_pipeline,
            &mut saved,
            bug_count,
            rx,
            test_count,
        );
    }
}

#[allow(clippy::too_many_arguments)]
fn novelty_search_generation(
    archive: &mut PointCloud<BehaviorVec>,
    pop: &mut Vec<TopPCFG>,
    args: &[FArg],
    test_pipeline: &[CompilerStage],
    saved: &mut [Box<BehaviorVec>],
    mut bug_count: u64,
    rx: Receiver<()>,
    mut test_count: u64,
) -> (u64, Receiver<()>, u64) {
    let mut cur_gen = archive.clone();
    let mut temp = vec![];
    std::fs::create_dir_all("bugs").unwrap();
    // for each generation
    for pcfg in pop {
        let brc = generator::gen_function(pcfg, args);
        let prog = lowering::lower(brc.clone());
        let bril = to_prog(prog.to_src(true));
        let prog = serde_json::to_string(&bril).unwrap();
        let prog_args = gen_main_args(args);
        let result = differential_test(
            &prog,
            test_pipeline,
            prog_args,
            Duration::from_secs(20),
            Some("out/rt.trace"),
        );
        temp.push(Box::new(BehaviorVec::new(
            "out/rt.trace",
            FailureType::from(&result),
        )));
        bug_count = out_test_files(bug_count, &result, &brc, &prog);
        test_count += 1;
        print!(
            "\rPasses: {} | Bugs: {bug_count}                 ",
            test_count - bug_count
        );
        if rx.recv_timeout(Duration::from_millis(1)).is_ok() {
            exit(0);
        }
    }
    for t in &temp {
        cur_gen.add_point(t);
    }
    (bug_count, rx, test_count)
}

/// Writes the test files to the bugs directory, if the test fails.
/// Also increments the bug count.
fn out_test_files(
    mut bug_count: u64,
    result: &TestResult,
    brc: &[Block<Statement>],
    prog: &str,
) -> u64 {
    if let TestResult::Fail { actual, .. } = result {
        std::fs::write(format!("bugs/bug_{bug_count}.brc"), display(brc))
            .unwrap();
        std::fs::write(format!("bugs/bug_{bug_count}.bril"), prog).unwrap();
        match actual {
            Ok(actual) => {
                std::fs::write(
                    format!("bugs/bug_{bug_count}.out"),
                    &actual.stdout,
                )
                .unwrap();
                std::fs::write(
                    format!("bugs/bug_{bug_count}.err"),
                    &actual.stderr,
                )
                .unwrap();
            }
            Err(e) => {
                std::fs::write(
                    format!("bugs/bug_{bug_count}.err"),
                    format!("{e:?}"),
                )
                .unwrap();
            }
        }

        bug_count += 1;
    }
    bug_count
}
