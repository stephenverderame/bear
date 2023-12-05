use atomic_float::AtomicF64;
use std::{
    collections::HashSet,
    ops::RangeInclusive,
    panic,
    sync::{
        atomic::{self, AtomicBool, AtomicU64, Ordering},
        mpsc::{channel, Receiver},
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

/// Number of nearest neighbors to consider when calculating novelty
const K: usize = 8;
/// Number of trials to run per pcfg
const TRIALS_PER_PCFG: usize = 3;
/// Chance to crossover two pcfgs
const CROSSOVER_RATE: f64 = 0.6;
/// Chance to mutate a pcfg
const MUTATION_RATE: f64 = 0.25;
/// Range of mutation delta
const MUT_DELTA: RangeInclusive<f64> = -0.1..=0.1;
/// Chance to mutate a probability by `MUT_DELTA`
const MUT_DELTA_CHANCE: f64 = 0.6;
/// Chance to swap two elements in a single pcfg
// const MUT_SWAP_CHANCE: f64 = 0.4;
/// Novelty threshold at which to save a behavior vector
const NOVELTY_THRESH: f64 = 1.0;
/// Number of local generations before sharing DNA across populations
const GENS_TO_CROSS_POP: u64 = 8;

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

/// Local stats for a single population
#[derive(Default)]
struct LocalStats {
    pub test_count: u64,
    pub save_count: u64,
    pub gen_count: u64,
}

/// Search params for all populations
#[derive(Clone, Debug)]
pub struct SearchParams {
    pub threads: usize,
    pub log_novelties: u64,
    pub log_tests: u64,
    pub max_num_mutations: usize,
    pub max_num_crosses: usize,
    pub pop_size: usize,
    pub select_size: usize,
}

/// Search params for a single population
struct PopParams {
    global: SearchParams,
    pop_idx: usize,
}

impl PopParams {
    const fn new(global: SearchParams, pop_idx: usize) -> Self {
        Self { global, pop_idx }
    }
}

fn setup_handlers() -> Receiver<()> {
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
    rx
}

/// Runs the novelty search algorithm on the given pipeline of compiler stages.
pub fn find_bugs(test_pipeline: &[CompilerStage], params: &SearchParams) {
    let archive = Arc::new(RwLock::new(Archive::new()));
    let stats = Arc::new(SharedStats::new());
    let stop_flag = Arc::new(AtomicBool::new(false));
    let cross_pop_genomes: Arc<RwLock<Vec<Option<TopPCFG>>>> = Arc::new(
        RwLock::new(std::iter::repeat(None).take(params.threads).collect()),
    );
    let rx = setup_handlers();
    let cross_pop_barrier = Arc::new(Barrier::new(params.threads));
    std::fs::create_dir_all("bugs").unwrap();
    let mut threads = vec![];
    for i in 0..params.threads {
        let a = archive.clone();
        let t = test_pipeline.to_vec();
        let st = stats.clone();
        let bar = cross_pop_barrier.clone();
        let cpg = cross_pop_genomes.clone();
        let params = PopParams::new(params.clone(), i);
        threads.push(thread::spawn(move || {
            population_search(&a, &params, &st, &t, &bar, &cpg);
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
    params: &PopParams,
    stats: &Arc<SharedStats>,
    test_pipeline: &[CompilerStage],
    cross_pop_barrier: &Arc<Barrier>,
    cross_pop_genomes: &Arc<RwLock<Vec<Option<TopPCFG>>>>,
) {
    let args = vec![
        FArg::int("a", 0, 100),
        FArg::int("b", -50, 50),
        FArg::int("c", 0, 100),
        FArg::bool("d"),
        FArg::bool("e"),
    ];
    let mut pop = std::iter::repeat_with(random_pcfg)
        .take(params.global.pop_size)
        .collect::<Vec<_>>();
    let mut local_gens = 0_u64;
    let mut local_shares = 0_u64;
    let mut local_stats = LocalStats::default();
    let mut waited = false;
    while !stats.stop_flag.load(atomic::Ordering::SeqCst) {
        pop = novelty_search_generation(
            archive,
            &pop,
            &args,
            test_pipeline,
            stats,
            params,
            &mut local_stats,
        );
        local_gens += 1;
        if local_gens % GENS_TO_CROSS_POP == 0 {
            local_shares += 1;
            share_dna_across_pops(
                &mut pop,
                params,
                cross_pop_barrier,
                cross_pop_genomes,
                &mut waited,
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
/// * `waited` - Whether or not this population has already waited on the barrier.
/// This is used so that we only wait for the other populations for the first
/// sharing. Subsequent DNA shares will not wait for other populations to update
/// their shared genomes. Purely for performance.
fn share_dna_across_pops(
    pop: &mut [TopPCFG],
    params: &PopParams,
    cross_pop_barrier: &Arc<Barrier>,
    cross_pop_genomes: &Arc<RwLock<Vec<Option<TopPCFG>>>>,
    waited: &mut bool,
) {
    let mutating_individual =
        Uniform::new(0, params.global.pop_size).sample(&mut rnd::get_rng());
    cross_pop_genomes.write().unwrap()[params.pop_idx] =
        Some(pop[mutating_individual].clone());
    if !*waited {
        cross_pop_barrier.wait();
        *waited = true;
    }
    let u = Uniform::new(0, params.global.threads);
    let mut s = u.sample(&mut rnd::get_rng());
    while s == params.pop_idx {
        s = u.sample(&mut rnd::get_rng());
    }
    pop[mutating_individual] = single_reproduce(
        &[
            pop[mutating_individual].clone(),
            cross_pop_genomes.read().unwrap()[s].clone().unwrap(),
        ],
        &Uniform::new(0.0, 1.0),
        params,
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
    params: &PopParams,
    local_stats: &mut LocalStats,
) -> Vec<TopPCFG> {
    let pop_idx = params.pop_idx;
    let mut temp = vec![];
    for (i, pcfg) in pop.iter().enumerate() {
        for _ in 0..TRIALS_PER_PCFG {
            let prog_args = gen_main_args(args);
            let (brc, prog, result) =
                gen_good_test(pcfg, args, test_pipeline, &prog_args, pop_idx);
            temp.push((
                i,
                BehaviorVec::new(
                    &format!("out/rt_{pop_idx}.trace"),
                    FailureType::from(&result),
                ),
            ));
            debug_search(
                stats,
                params,
                local_stats,
                &brc,
                &prog,
                &prog_args,
                &result,
            );
            local_stats.test_count += 1;
            stats.test_count.fetch_add(1, atomic::Ordering::SeqCst);
            if stats.stop_flag.load(atomic::Ordering::SeqCst) {
                return pop.to_vec();
            }
        }
    }
    local_stats.gen_count += 1;
    let r: Vec<_> = select(temp, archive, stats, local_stats, params)
        .iter()
        .map(|(pop_id, _)| pop[*pop_id].clone())
        .collect();
    reproduce(&r, params)
}

/// Generates a good test. Repeatedly trying until the test at least
/// can be run through the interpreter without error.
/// # Returns
/// A tuple containing the test in brc, the BRIL program as json,
/// and the test result.
fn gen_good_test(
    pcfg: &TopPCFG,
    args: &[FArg],
    test_pipeline: &[CompilerStage],
    prog_args: &[String],
    pop_idx: usize,
) -> (Vec<Block<Statement>>, String, TestResult) {
    let mut result;
    let mut brc;
    let mut prog;
    let mut prog_str;
    let mut bril;
    let mut count = 1;
    loop {
        if count >= 3 {
            eprintln!("\nFailed to generate a test which passes baseline after {} tries!\n \
                      This is likely a bug in the fuzzer", count - 1);
        }
        brc = generator::gen_function(pcfg, args);
        prog = lowering::lower(brc.clone());
        bril = to_prog(prog.to_src(true));
        prog_str = serde_json::to_string(&bril).unwrap();
        result = differential_test(
            &prog_str,
            test_pipeline,
            prog_args.to_vec(),
            Duration::from_secs(20),
            Some(&format!("out/rt_{pop_idx}.trace")),
            &format!("out/out_{pop_idx}"),
        );
        count += 1;
        if result.is_some() {
            break;
        }
    }
    (brc, prog_str, result.unwrap())
}

/// Writes the test files to the out directory, if the test fails.
/// Also logs the test files every `params.log_tests` tests.
fn debug_search(
    stats: &Arc<SharedStats>,
    params: &PopParams,
    local_stats: &mut LocalStats,
    brc: &[Block<Statement>],
    prog: &str,
    prog_args: &[String],
    result: &TestResult,
) {
    out_test_files(stats, result, brc, prog, prog_args, params.pop_idx);
    if params.global.log_tests > 0
        && local_stats.test_count % params.global.log_tests == 0
    {
        std::fs::write(
            format!(
                "out/test_{}_{}.brc",
                params.pop_idx, local_stats.save_count
            ),
            display(brc),
        )
        .unwrap();
        local_stats.save_count += 1;
    }
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
    let mut indices = HashSet::new();
    for b in gen.chunks(TRIALS_PER_PCFG) {
        let mut dists = vec![];
        for (pcfg_idx, e) in b {
            let dist = archive
                .read()
                .unwrap()
                .get_nearest_k(e, K)
                .iter()
                .map(|(b, _)| b)
                .sum::<f64>()
                / K as f64;
            dists.push(HeapElem {
                dist,
                index: *pcfg_idx,
            });
        }
        let median_idx = (TRIALS_PER_PCFG - 1) / 2;
        dists.select_nth_unstable(median_idx);
        indices.insert(dists[median_idx].index);
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
/// # Arguments
/// * `gen` - The population to select from. The first element of each tuple is
/// the index of the behavior vec's pcfg in the population.
/// # Returns
/// A vector of the selected behavior vectors. The first element of each tuple
/// is the index of the behavior vec's pcfg in the population.
#[allow(clippy::cast_precision_loss)]
fn select(
    mut gen: Vec<(usize, BehaviorVec)>,
    archive: &Arc<RwLock<Archive>>,
    stats: &Arc<SharedStats>,
    local_stats: &LocalStats,
    params: &PopParams,
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
        for (_, individual) in &gen {
            cur_gen.add_point(individual);
        }
        for (pcfg_idx, b) in &gen {
            let dist = cur_gen
                .get_nearest_k(b, K)
                .iter()
                .map(|(b, _)| b)
                .sum::<f64>()
                / K as f64;
            distances.push(HeapElem {
                dist,
                index: *pcfg_idx,
            });

            let max_dist =
                stats.archive_max_dist.fetch_max(dist, Ordering::SeqCst);
            if dist > max_dist * 0.8 || dist > NOVELTY_THRESH {
                to_save.push(b.clone());
            }
        }
    }
    for e in to_save {
        archive.write().unwrap().add_point(e);
    }
    gen_select_set(&distances, gen, params, local_stats)
}

/// Selects `params.select_size` elements from `gen` probabilistically, weighted by their
/// novelty (`distances`). Also saves the debug files of the novelty.
fn gen_select_set(
    distances: &[HeapElem],
    gen: Vec<(usize, BehaviorVec)>,
    params: &PopParams,
    local_stats: &LocalStats,
) -> Vec<(usize, BehaviorVec)> {
    assert_eq!(distances.len(), gen.len());
    let sum: f64 = distances.iter().map(|e| e.dist).sum();
    let sum = sum.max(f64::EPSILON);

    let res: Vec<_> = gen
        .into_iter()
        .zip(distances.iter().map(|e| e.dist / sum))
        .collect();

    let r: Vec<_> = res
        .choose_multiple_weighted(
            &mut rnd::get_rng(),
            params.global.select_size,
            |e| e.1,
        )
        .unwrap()
        .collect();
    debug_out_novelties(&r, sum, local_stats, params);
    r.into_iter()
        .map(|((pcfg_idx, bv), _)| (*pcfg_idx, bv.clone()))
        .collect()
}

/// Every `DEBUG_OUT_GEN_NUM` generations, writes the selected parents to a file,
/// saving their novelties and behavior vectors.
/// # Arguments
/// * `selected` - The selected parents
/// * `sum` - The sum of the novelties of all individuals in the current generation
fn debug_out_novelties(
    selected: &[&((usize, BehaviorVec), f64)],
    sum: f64,
    local_stats: &LocalStats,
    params: &PopParams,
) {
    if params.global.log_novelties > 0
        && local_stats.gen_count % params.global.log_novelties == 0
    {
        let mut out_str = String::new();
        for ((_pcfg_idx, bv), normalized_dist) in selected {
            let dist = normalized_dist * sum;
            let vec = bv.vectorize();
            out_str += &format!("dist: {dist}\nprob: {normalized_dist}\n{bv:#?}\nbv: {vec:#?}\n\n");
        }
        std::fs::write(
            format!(
                "out/novelties_{}_{}.txt",
                params.pop_idx, local_stats.gen_count
            ),
            out_str,
        )
        .unwrap();
    }
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
fn reproduce(survivors: &[TopPCFG], params: &PopParams) -> Vec<TopPCFG> {
    let mut new_pop = vec![];
    let u = Uniform::new(0.0, 1.0);
    while new_pop.len() < params.global.pop_size {
        new_pop.push(single_reproduce(survivors, &u, params));
    }
    new_pop
}

/// Creates a new `TopPCFG` from the survivors. Requires `survivors` to have
/// at least two elements.
fn single_reproduce(
    survivors: &[TopPCFG],
    u: &Uniform<f64>,
    params: &PopParams,
) -> TopPCFG {
    let samp = u.sample(&mut rnd::get_rng());
    if samp < CROSSOVER_RATE {
        let parents: Vec<_> =
            survivors.choose_multiple(&mut rnd::get_rng(), 2).collect();
        crossover(parents[0], parents[1], params)
    } else if samp < CROSSOVER_RATE + MUTATION_RATE {
        mutate(survivors.choose(&mut rnd::get_rng()).unwrap(), params)
    } else {
        survivors.choose(&mut rnd::get_rng()).unwrap().clone()
    }
}

/// Performs crossover on two `TopPCFG`s to get a child pcfg.
fn crossover(a: &TopPCFG, b: &TopPCFG, params: &PopParams) -> TopPCFG {
    let f = a.serialize(vec![]);
    let g = b.serialize(vec![]);
    let mut f_flat = flatten_pcfg(&f);
    let mut g_flat = flatten_pcfg(&g);
    for _ in 0..Uniform::new(1, params.global.max_num_crosses)
        .sample(&mut rnd::get_rng())
    {
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
fn mutate(a: &TopPCFG, params: &PopParams) -> TopPCFG {
    let f = a.serialize(vec![]);
    let mut f_flat = flatten_pcfg(&f);
    let b = Uniform::new(0.0, 1.0);
    for _ in 0..Uniform::new(1, params.global.max_num_mutations)
        .sample(&mut rnd::get_rng())
    {
        let u = Uniform::new(0, f_flat.len() - 1);
        let idx = u.sample(&mut rnd::get_rng());
        let sample = b.sample(&mut rnd::get_rng());
        if sample < MUT_DELTA_CHANCE {
            f_flat[idx] +=
                Uniform::new_inclusive(*MUT_DELTA.start(), *MUT_DELTA.end())
                    .sample(&mut rnd::get_rng())
                    .max(0.0);
        } else {
            /*if sample < MUT_DELTA_CHANCE + MUT_SWAP_CHANCE */
            let mut idx2 = u.sample(&mut rnd::get_rng());
            while idx == idx2 {
                idx2 = u.sample(&mut rnd::get_rng());
            }
            f_flat.swap(idx, idx2);
        }
        // else {
        //     f_flat[idx] = 0.0;
        // }
    }
    TopPCFG::deserialize(flat_to_pcfg(&f, &f_flat)).0
}
