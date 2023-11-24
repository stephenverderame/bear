use std::{
    collections::HashSet,
    io::{stdout, Write},
    ops::RangeInclusive,
    process::exit,
    sync::mpsc::{channel, Receiver},
    time::Duration,
};

use knn::PointCloud;
use rand::{
    distributions::{Distribution, Uniform},
    seq::SliceRandom,
};

use crate::{
    bare_c::{display, Block, Statement},
    generator::{self, rnd, FArg},
    lowering,
    pcfg::{flat_to_pcfg, flatten_pcfg, random_pcfg, TopPCFG, PCFG},
    runner::{differential_test, gen_main_args, CompilerStage, TestResult},
    to_prog,
};

use self::trace::{BehaviorVec, FailureType};

mod trace;

const POPULATION_SIZE: usize = 10;
const K: usize = 8;
const SELECT_SIZE: usize = POPULATION_SIZE / 2;
const TRIALS_PER_PCFG: usize = 3;
const CROSSOVER_RATE: f64 = 0.8;
const MUTATION_RATE: f64 = 0.15;
const MAX_NUM_MUTATIONS: usize = 10;
const MUT_DELTA: RangeInclusive<f64> = -0.1..=0.1;
const MUT_DELTA_CHANCE: f64 = 0.8;
const NOVELTY_THRESH: f64 = 3_000.0;

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
    let mut archive_max_dist = 0_f64;
    std::fs::create_dir_all("bugs").unwrap();
    // for each generation
    loop {
        (pop, bug_count, rx, test_count) = novelty_search_generation(
            &mut archive,
            &pop,
            &args,
            test_pipeline,
            &mut saved,
            bug_count,
            rx,
            test_count,
            &mut archive_max_dist,
        );
    }
}

#[allow(clippy::too_many_arguments, clippy::vec_box)]
fn novelty_search_generation(
    archive: &mut PointCloud<BehaviorVec>,
    pop: &[TopPCFG],
    args: &[FArg],
    test_pipeline: &[CompilerStage],
    saved: &mut Vec<Box<BehaviorVec>>,
    mut bug_count: u64,
    rx: Receiver<()>,
    mut test_count: u64,
    archive_max_dist: &mut f64,
) -> (Vec<TopPCFG>, u64, Receiver<()>, u64) {
    let mut temp = vec![];
    for pcfg in pop.iter() {
        for i in 0..TRIALS_PER_PCFG {
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
            temp.push((
                i,
                Box::new(BehaviorVec::new(
                    "out/rt.trace",
                    FailureType::from(&result),
                )),
            ));
            bug_count = out_test_files(bug_count, &result, &brc, &prog);
            test_count += 1;
            print!(
                "\rGen: {} | Passes: {} | Bugs: {bug_count} | Novel: {}                 ",
                test_count / (TRIALS_PER_PCFG as u64 * POPULATION_SIZE as u64),
                test_count - bug_count,
                saved.len(),
            );
            stdout().flush().unwrap();
            if rx.recv_timeout(Duration::from_millis(1)).is_ok() {
                exit(0);
            }
        }
    }
    let r: Vec<_> = select(temp, archive, archive_max_dist, saved)
        .iter()
        .map(|(pop_id, _)| pop[*pop_id].clone())
        .collect();
    (reproduce(&r), bug_count, rx, test_count)
}

#[derive(PartialEq, Clone)]
struct HeapElem {
    dist: f64,
    index: usize,
}

impl PartialOrd for HeapElem {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.dist.partial_cmp(&other.dist)
    }
}

impl Ord for HeapElem {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.partial_cmp(other).unwrap()
    }
}

impl Eq for HeapElem {}

/// Extracts the element that has the median sparseness from each chunk of `TRIALS_PER_PCFG` elements
/// in `gen` and returns them in a new vector.
#[allow(clippy::vec_box, clippy::cast_precision_loss)]
fn extract_medians(
    gen: Vec<(usize, Box<BehaviorVec>)>,
    archive: &PointCloud<BehaviorVec>,
) -> Vec<(usize, Box<BehaviorVec>)> {
    let mut res = vec![];
    let mut idx = 0;
    let mut indices = HashSet::new();
    for b in gen.chunks(TRIALS_PER_PCFG) {
        let mut dists = vec![];
        for (i, e) in b.iter().enumerate() {
            let dist = archive
                .get_nearest_k(&e.1, K)
                .iter()
                .map(|(b, _)| b)
                .sum::<f64>()
                / K as f64;
            dists.push(HeapElem {
                dist,
                index: idx + i,
            });
        }
        let median_idx = (TRIALS_PER_PCFG - 1) / 2;
        dists.select_nth_unstable(median_idx);
        indices.insert(dists[median_idx].index);
        idx += b.len();
    }
    for (i, e) in gen.into_iter().enumerate() {
        if indices.contains(&i) {
            res.push(e);
        }
    }
    res
}

/// Selects `SELECT_SIZE` elements from `gen` probabilistically, weighted by their
/// novelty
#[allow(clippy::vec_box, clippy::cast_precision_loss)]
fn select(
    mut gen: Vec<(usize, Box<BehaviorVec>)>,
    archive: &mut PointCloud<BehaviorVec>,
    archive_max_dist: &mut f64,
    saved: &mut Vec<Box<BehaviorVec>>,
) -> Vec<(usize, Box<BehaviorVec>)> {
    let old_len = gen.len();
    assert!(gen.len() % TRIALS_PER_PCFG == 0);
    gen = extract_medians(gen, archive);
    assert!(old_len / TRIALS_PER_PCFG == gen.len());
    let mut cur_gen = archive.clone();
    for individual in &gen {
        cur_gen.add_point(&individual.1);
    }
    // ith element of distance is the average distance of the ith element of gen
    let mut distances = vec![];
    for (i, b) in gen.iter().enumerate() {
        let dist = cur_gen
            .get_nearest_k(&b.1, K)
            .iter()
            .map(|(b, _)| b)
            .sum::<f64>()
            / K as f64;
        distances.push(HeapElem { dist, index: i });

        if dist > *archive_max_dist * 0.8 || dist > NOVELTY_THRESH {
            saved.push(b.1.clone());
            let ptr = saved.last().unwrap().as_ref() as *const BehaviorVec;
            // safe bc each value is boxed, so ptr won't move and we don't mutate
            // elements of saved
            archive.add_point(unsafe { &*ptr });

            if dist > *archive_max_dist {
                *archive_max_dist = dist;
            }
        }
    }
    let max = distances.iter().max().unwrap().dist.max(f64::EPSILON);

    let res: Vec<_> = gen
        .into_iter()
        .zip(distances.into_iter().map(|e| e.dist / max))
        .collect();

    res.choose_multiple_weighted(&mut rnd::get_rng(), SELECT_SIZE, |e| e.1)
        .unwrap()
        .map(|(b, _)| b.clone())
        .collect()
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

/// Creates a new population of `POPULATION_SIZE` `TopPCFG`s from the survivors.
fn reproduce(survivors: &[TopPCFG]) -> Vec<TopPCFG> {
    let mut new_pop = vec![];
    let u = Uniform::new(0.0, 1.0);
    let rng = &mut rnd::get_rng();
    while new_pop.len() < POPULATION_SIZE {
        let samp = u.sample(&mut rnd::get_rng());
        if samp < CROSSOVER_RATE {
            let parents: Vec<_> = survivors.choose_multiple(rng, 2).collect();
            new_pop.push(crossover(parents[0], parents[1]));
        } else if samp < CROSSOVER_RATE + MUTATION_RATE {
            new_pop.push(mutate(survivors.choose(rng).unwrap()));
        } else {
            new_pop.push(survivors.choose(rng).unwrap().clone());
        }
    }
    new_pop
}

/// Performs crossover on two `TopPCFG`s to get a child pcfg.
fn crossover(a: &TopPCFG, b: &TopPCFG) -> TopPCFG {
    let f = a.serialize(vec![]);
    let g = b.serialize(vec![]);
    let mut f_flat = flatten_pcfg(&f);
    let g_flat = flatten_pcfg(&g);
    let begin = Uniform::new(0, g_flat.len() - 1).sample(&mut rnd::get_rng());
    let end = Uniform::new(1, g_flat.len() - begin).sample(&mut rnd::get_rng())
        + begin;
    f_flat[begin..end].copy_from_slice(&g_flat[begin..end]);
    TopPCFG::deserialize(flat_to_pcfg(&f, &f_flat)).0
}

/// Performs mutation on a `TopPCFG` to get a child pcfg.
fn mutate(a: &TopPCFG) -> TopPCFG {
    let f = a.serialize(vec![]);
    let mut f_flat = flatten_pcfg(&f);
    let b = Uniform::new(0.0, 1.0);
    for _ in 0..Uniform::new(0, MAX_NUM_MUTATIONS).sample(&mut rnd::get_rng()) {
        let idx = Uniform::new(0, f_flat.len() - 1).sample(&mut rnd::get_rng());
        if b.sample(&mut rnd::get_rng()) < MUT_DELTA_CHANCE {
            f_flat[idx] +=
                Uniform::new_inclusive(*MUT_DELTA.start(), *MUT_DELTA.end())
                    .sample(&mut rnd::get_rng());
        } else {
            f_flat[idx] = 0.0;
        }
    }
    TopPCFG::deserialize(flat_to_pcfg(&f, &f_flat)).0
}
