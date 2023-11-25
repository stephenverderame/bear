use atomic_float::AtomicF64;
use std::{
    collections::HashSet,
    ops::RangeInclusive,
    panic,
    sync::{
        atomic::{self, AtomicBool, AtomicU64, Ordering},
        mpsc::channel,
        Arc, Barrier, RwLock,
    },
    thread,
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

/// Size of the population. This is the nominal population size, but the actual
/// number of tests run per generation is `POPULATION_SIZE * TRIALS_PER_PCFG`.
const POPULATION_SIZE: usize = 10;
/// # of nearest neighbors to consider when calculating novelty
const K: usize = 8;
/// # of individuals to select from each population
const SELECT_SIZE: usize = POPULATION_SIZE / 2;
/// # of trials to run per pcfg
const TRIALS_PER_PCFG: usize = 3;
/// Chance to crossover two pcfgs
const CROSSOVER_RATE: f64 = 0.6;
/// Chance to mutate a pcfg
const MUTATION_RATE: f64 = 0.25;
/// Maximum number of mutations to perform on a pcfg
const MAX_NUM_MUTATIONS: usize = 10;
/// Max slices to swap during a single crossover
const MAX_NUM_CROSSES: usize = 5;
/// Range of mutation delta
const MUT_DELTA: RangeInclusive<f64> = -0.1..=0.1;
/// Chance to mutate a probability by `MUT_DELTA`
const MUT_DELTA_CHANCE: f64 = 0.4;
/// Chance to swap two elements in a single pcfg
const MUT_SWAP_CHANCE: f64 = 0.4;
/// Novelty threshold at which to save a behavior vector
const NOVELTY_THRESH: f64 = 3_000.0;
/// # of independent populations
const POPULATIONS: usize = 3;
/// # of local generations before sharing DNA across populations
const GENS_TO_CROSS_POP: u64 = 2;
/// # of tests to generate from a population before saving the test
const DEBUG_OUT_TEST_NUM: u64 = 50;

/// The archive all the most novel individuals.
#[allow(clippy::vec_box)]
struct Archive<'a> {
    knn: PointCloud<'a, BehaviorVec>,
    saved: Vec<Box<BehaviorVec>>,
}

impl<'a> Archive<'a> {
    fn new() -> Self {
        Self {
            knn: PointCloud::new(BehaviorVec::dist),
            saved: vec![],
        }
    }

    fn add_point(&mut self, point: BehaviorVec) {
        self.saved.push(Box::new(point));
        let ptr = self.saved.last().unwrap().as_ref() as *const BehaviorVec;
        // safe bc each value is boxed, so ptr won't move and we don't mutate
        // elements of saved
        self.knn.add_point(unsafe { &*ptr });
    }

    fn get_nearest_k(
        &self,
        point: &BehaviorVec,
        k: usize,
    ) -> Vec<(f64, &BehaviorVec)> {
        self.knn.get_nearest_k(point, k)
    }

    fn clone_point_cloud(&self) -> PointCloud<'a, BehaviorVec> {
        self.knn.clone()
    }

    fn len(&self) -> usize {
        self.saved.len()
    }
}

/// Shared stats across all populations
struct SharedStats {
    pub bug_count: AtomicU64,
    pub test_count: AtomicU64,
    pub archive_max_dist: AtomicF64,
    pub stop_flag: AtomicBool,
    pub gens: AtomicU64,
    pub shares: AtomicU64,
}

impl SharedStats {
    const fn new() -> Self {
        Self {
            bug_count: AtomicU64::new(0),
            test_count: AtomicU64::new(0),
            archive_max_dist: AtomicF64::new(0.0),
            stop_flag: AtomicBool::new(false),
            gens: AtomicU64::new(0),
            shares: AtomicU64::new(0),
        }
    }
}

#[derive(Default)]
struct LocalStats {
    pub test_count: u64,
    pub save_count: u64,
}

/// Runs the novelty search algorithm on the given pipeline of compiler stages.
pub fn find_bugs(test_pipeline: &[CompilerStage]) {
    let archive = Arc::new(RwLock::new(Archive::new()));
    let (tx, rx) = channel();
    ctrlc::set_handler(move || {
        tx.send(()).unwrap();
    })
    .unwrap();
    let old = panic::take_hook();
    panic::set_hook(Box::new(move |info| {
        // if any thread panics, stop the program
        old(info);
        std::process::exit(1);
    }));
    let stats = Arc::new(SharedStats::new());
    let stop_flag = Arc::new(AtomicBool::new(false));
    let cross_pop_genomes: Arc<RwLock<[Option<TopPCFG>; POPULATIONS]>> =
        Arc::new(RwLock::new(std::array::from_fn(|_| None)));
    let cross_pop_barrier = Arc::new(Barrier::new(POPULATIONS));
    std::fs::create_dir_all("bugs").unwrap();
    let mut threads = vec![];
    for i in 0..POPULATIONS {
        let a = archive.clone();
        let t = test_pipeline.to_vec();
        let st = stats.clone();
        let bar = cross_pop_barrier.clone();
        let cpg = cross_pop_genomes.clone();
        threads.push(thread::spawn(move || {
            population_search(&a, i, &st, &t, &bar, &cpg);
        }));
    }
    loop {
        print!(
            "\rGens: {} | Shares: {} | Bugs: {} | Tests: {} | Novel: {}         ",
            stats.gens.load(atomic::Ordering::SeqCst),
            stats.shares.load(atomic::Ordering::SeqCst),
            stats.bug_count.load(atomic::Ordering::SeqCst),
            stats.test_count.load(atomic::Ordering::SeqCst),
            archive.read().unwrap().len(),
        );
        if rx.recv_timeout(Duration::from_millis(100)).is_ok() {
            println!("\nStopping...");
            stop_flag.store(true, atomic::Ordering::SeqCst);
            break;
        }
    }
    for t in threads {
        t.join().unwrap();
    }
}

/// Performs novelty search for a single population.
fn population_search(
    archive: &Arc<RwLock<Archive>>,
    pop_idx: usize,
    stats: &Arc<SharedStats>,
    test_pipeline: &[CompilerStage],
    cross_pop_barrier: &Arc<Barrier>,
    cross_pop_genomes: &Arc<RwLock<[Option<TopPCFG>; POPULATIONS]>>,
) {
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
    let mut local_gens = 0_u64;
    let mut local_shares = 0_u64;
    let mut local_stats = LocalStats::default();
    while !stats.stop_flag.load(atomic::Ordering::SeqCst) {
        pop = novelty_search_generation(
            archive,
            &pop,
            &args,
            test_pipeline,
            stats,
            pop_idx,
            &mut local_stats,
        );
        local_gens += 1;
        if local_gens % GENS_TO_CROSS_POP == 0 {
            local_shares += 1;
            share_dna_across_pops(
                &mut pop,
                pop_idx,
                cross_pop_barrier,
                cross_pop_genomes,
            );
            stats.shares.fetch_max(local_shares, Ordering::SeqCst);
        }
        stats.gens.fetch_max(local_gens, Ordering::SeqCst);
    }
}

/// Shares the DNA of a random individual in the population with another
/// population. Waits for all populations to finish sharing before returning.
/// Also mutates the individual that was shared to be the child of the shared
/// individual and another individual in the other population.
/// # Arguments
/// * `pop` - The population to share DNA from
/// * `pop_idx` - Thread index of the population
/// * `cross_pop_barrier` - Barrier to wait on before sharing DNA
/// * `cross_pop_genomes` - The shared genomes from multiple populations
fn share_dna_across_pops(
    pop: &mut [TopPCFG],
    pop_idx: usize,
    cross_pop_barrier: &Arc<Barrier>,
    cross_pop_genomes: &Arc<RwLock<[Option<TopPCFG>; POPULATIONS]>>,
) {
    let mutating_individual =
        Uniform::new(0, POPULATION_SIZE).sample(&mut rnd::get_rng());
    cross_pop_genomes.write().unwrap()[pop_idx] =
        Some(pop[mutating_individual].clone());
    cross_pop_barrier.wait();
    let u = Uniform::new(0, POPULATIONS);
    let mut s = u.sample(&mut rnd::get_rng());
    while s == pop_idx {
        s = u.sample(&mut rnd::get_rng());
    }
    pop[mutating_individual] = single_reproduce(
        &[
            pop[mutating_individual].clone(),
            cross_pop_genomes.read().unwrap()[s].clone().unwrap(),
        ],
        &Uniform::new(0.0, 1.0),
    );
}

/// Performs a single generation of novelty search on the population.
/// # Arguments
/// * `archive` - The archive of behavior vectors keeping track of the most novel
/// individuals across all populations
/// * `pop` - The population to perform novelty search on
/// * `args` - The argument types and ranges to generate args to pass to the program
/// * `test_pipeline` - The pipeline of stages to run the program through
/// * `stats` - The shared stats across all populations
/// * `pop_idx` - The index of the current population
fn novelty_search_generation(
    archive: &Arc<RwLock<Archive>>,
    pop: &[TopPCFG],
    args: &[FArg],
    test_pipeline: &[CompilerStage],
    stats: &Arc<SharedStats>,
    pop_idx: usize,
    local_stats: &mut LocalStats,
) -> Vec<TopPCFG> {
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
                prog_args.clone(),
                Duration::from_secs(20),
                Some(&format!("out/rt_{pop_idx}.trace")),
                &format!("out/out_{pop_idx}"),
            );
            temp.push((
                i,
                BehaviorVec::new(
                    &format!("out/rt_{pop_idx}.trace"),
                    FailureType::from(&result),
                ),
            ));
            out_test_files(stats, &result, &brc, &prog, &prog_args, pop_idx);
            if local_stats.test_count % DEBUG_OUT_TEST_NUM == 0 {
                std::fs::write(
                    format!(
                        "out/test_{pop_idx}_{}.brc",
                        local_stats.save_count
                    ),
                    display(&brc),
                )
                .unwrap();
                local_stats.save_count += 1;
            }
            local_stats.test_count += 1;
            stats.test_count.fetch_add(1, atomic::Ordering::SeqCst);
            if stats.stop_flag.load(atomic::Ordering::SeqCst) {
                return pop.to_vec();
            }
        }
    }
    let r: Vec<_> = select(temp, archive, stats)
        .iter()
        .map(|(pop_id, _)| pop[*pop_id].clone())
        .collect();
    reproduce(&r)
}

/// An element which is ordered by its average distance to its k-nearest neighbors
/// in behavior space. Requires the distance is not `NaN`.
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
#[allow(clippy::cast_precision_loss)]
fn extract_medians(
    gen: Vec<(usize, BehaviorVec)>,
    archive: &Arc<RwLock<Archive>>,
) -> Vec<(usize, BehaviorVec)> {
    let mut res = vec![];
    let mut idx = 0;
    let mut indices = HashSet::new();
    for b in gen.chunks(TRIALS_PER_PCFG) {
        let mut dists = vec![];
        for (i, e) in b.iter().enumerate() {
            let dist = archive
                .read()
                .unwrap()
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
#[allow(clippy::cast_precision_loss)]
fn select(
    mut gen: Vec<(usize, BehaviorVec)>,
    archive: &Arc<RwLock<Archive>>,
    stats: &Arc<SharedStats>,
) -> Vec<(usize, BehaviorVec)> {
    let old_len = gen.len();
    assert!(gen.len() % TRIALS_PER_PCFG == 0);
    gen = extract_medians(gen, archive);
    assert!(old_len / TRIALS_PER_PCFG == gen.len());
    // ith element of distance is the average distance of the ith element of gen
    let mut distances = vec![];
    let mut to_save = vec![];
    {
        let mut cur_gen = archive.read().unwrap().clone_point_cloud();
        for individual in &gen {
            cur_gen.add_point(&individual.1);
        }
        for (i, b) in gen.iter().enumerate() {
            let dist = cur_gen
                .get_nearest_k(&b.1, K)
                .iter()
                .map(|(b, _)| b)
                .sum::<f64>()
                / K as f64;
            distances.push(HeapElem { dist, index: i });

            let max_dist =
                stats.archive_max_dist.fetch_max(dist, Ordering::SeqCst);
            if dist > max_dist * 0.8 || dist > NOVELTY_THRESH {
                to_save.push(b.1.clone());
            }
        }
    }
    for e in to_save {
        archive.write().unwrap().add_point(e);
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
    stats: &Arc<SharedStats>,
    result: &TestResult,
    brc: &[Block<Statement>],
    prog: &str,
    prog_args: &[String],
    pop_idx: usize,
) {
    if let TestResult::Fail { actual, .. } = result {
        let bc = stats.bug_count.fetch_add(1, atomic::Ordering::SeqCst);
        std::fs::write(format!("bugs/bug_{pop_idx}_{bc}.brc"), display(brc))
            .unwrap();
        std::fs::write(format!("bugs/bug_{pop_idx}_{bc}.bril"), prog).unwrap();
        std::fs::write(
            format!("bugs/bug_{pop_idx}_{bc}.args"),
            prog_args.join(", "),
        )
        .unwrap();
        match actual {
            Ok(actual) => {
                std::fs::write(
                    format!("bugs/bug_{pop_idx}_{bc}.out"),
                    &actual.stdout,
                )
                .unwrap();
                std::fs::write(
                    format!("bugs/bug_{pop_idx}_{bc}.err"),
                    &actual.stderr,
                )
                .unwrap();
            }
            Err(e) => {
                std::fs::write(
                    format!("bugs/bug_{pop_idx}_{bc}.err"),
                    format!("{e:?}"),
                )
                .unwrap();
            }
        }
    }
}

/// Creates a new population of `POPULATION_SIZE` `TopPCFG`s from the survivors.
fn reproduce(survivors: &[TopPCFG]) -> Vec<TopPCFG> {
    let mut new_pop = vec![];
    let u = Uniform::new(0.0, 1.0);
    while new_pop.len() < POPULATION_SIZE {
        new_pop.push(single_reproduce(survivors, &u));
    }
    new_pop
}

/// Creates a new `TopPCFG` from the survivors. Requires `survivors` to have
/// at least two elements.
fn single_reproduce(survivors: &[TopPCFG], u: &Uniform<f64>) -> TopPCFG {
    let samp = u.sample(&mut rnd::get_rng());
    if samp < CROSSOVER_RATE {
        let parents: Vec<_> =
            survivors.choose_multiple(&mut rnd::get_rng(), 2).collect();
        crossover(parents[0], parents[1])
    } else if samp < CROSSOVER_RATE + MUTATION_RATE {
        mutate(survivors.choose(&mut rnd::get_rng()).unwrap())
    } else {
        survivors.choose(&mut rnd::get_rng()).unwrap().clone()
    }
}

/// Performs crossover on two `TopPCFG`s to get a child pcfg.
fn crossover(a: &TopPCFG, b: &TopPCFG) -> TopPCFG {
    let f = a.serialize(vec![]);
    let g = b.serialize(vec![]);
    let mut f_flat = flatten_pcfg(&f);
    let mut g_flat = flatten_pcfg(&g);
    for _ in 0..Uniform::new(1, MAX_NUM_CROSSES).sample(&mut rnd::get_rng()) {
        let begin =
            Uniform::new(0, g_flat.len() - 1).sample(&mut rnd::get_rng());
        let end = Uniform::new(1, g_flat.len() - begin)
            .sample(&mut rnd::get_rng())
            + begin;
        let prev = f_flat[begin..end].to_vec();
        f_flat[begin..end].copy_from_slice(&g_flat[begin..end]);
        g_flat[begin..end].copy_from_slice(&prev);
    }
    TopPCFG::deserialize(flat_to_pcfg(&f, &f_flat)).0
}

/// Performs mutation on a `TopPCFG` to get a child pcfg.
fn mutate(a: &TopPCFG) -> TopPCFG {
    let f = a.serialize(vec![]);
    let mut f_flat = flatten_pcfg(&f);
    let b = Uniform::new(0.0, 1.0);
    for _ in 0..Uniform::new(1, MAX_NUM_MUTATIONS).sample(&mut rnd::get_rng()) {
        let u = Uniform::new(0, f_flat.len() - 1);
        let idx = u.sample(&mut rnd::get_rng());
        let sample = b.sample(&mut rnd::get_rng());
        if sample < MUT_DELTA_CHANCE {
            f_flat[idx] +=
                Uniform::new_inclusive(*MUT_DELTA.start(), *MUT_DELTA.end())
                    .sample(&mut rnd::get_rng())
                    .max(0.0);
        } else if sample < MUT_DELTA_CHANCE + MUT_SWAP_CHANCE {
            let mut idx2 = u.sample(&mut rnd::get_rng());
            while idx == idx2 {
                idx2 = u.sample(&mut rnd::get_rng());
            }
            f_flat.swap(idx, idx2);
        } else {
            f_flat[idx] = 0.0;
        }
    }
    TopPCFG::deserialize(flat_to_pcfg(&f, &f_flat)).0
}
