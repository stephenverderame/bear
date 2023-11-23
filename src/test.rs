use std::{time::Duration, vec};
// end to end tests

use crate::{
    bare_c::{self, display},
    generator::{rnd, FArg},
    lowering,
    pcfg::PCFG,
    runner,
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
            Duration::from_secs(15),
            "out/test_end_to_end",
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

#[allow(clippy::cognitive_complexity, clippy::too_many_lines)]
#[test]
fn lower_duffs() {
    let prog = vec![
        bare_c::Block::<bare_c::Statement>::Duffs(bare_c::DuffsInfo {
            var: String::from("foo"),
            init: bare_c::AExpr::Add(
                Box::new(bare_c::AExpr::Id(String::from("a"))),
                Box::new(bare_c::AExpr::Id(String::from("b"))),
            ),
            limit: bare_c::AExpr::Num(30),
            step: bare_c::AExpr::Num(10),
            guard: bare_c::AExpr::Sub(
                Box::new(bare_c::AExpr::Id(String::from("c"))),
                Box::new(bare_c::AExpr::Num(10)),
            ),
            bodies: vec![
                (
                    bare_c::AExpr::Num(1),
                    vec![bare_c::Block::If {
                        guard: bare_c::BExpr::Id(String::from("d")),
                        then: vec![bare_c::Block::Stmt(
                            bare_c::LoopStatement::Stmt(
                                bare_c::Statement::Print(vec![
                                    bare_c::Expr::AExpr(bare_c::AExpr::Id(
                                        String::from("foo"),
                                    )),
                                ]),
                            ),
                        )],
                        otherwise: vec![bare_c::Block::Stmt(
                            bare_c::LoopStatement::Stmt(
                                bare_c::Statement::Print(vec![
                                    bare_c::Expr::AExpr(bare_c::AExpr::Id(
                                        String::from("c"),
                                    )),
                                ]),
                            ),
                        )],
                    }],
                ),
                (
                    bare_c::AExpr::Num(2),
                    vec![bare_c::Block::Stmt(bare_c::LoopStatement::Stmt(
                        bare_c::Statement::Print(vec![bare_c::Expr::AExpr(
                            bare_c::AExpr::Id(String::from("b")),
                        )]),
                    ))],
                ),
            ],
            default: vec![bare_c::Block::If {
                guard: bare_c::BExpr::Id(String::from("e")),
                then: vec![bare_c::Block::Stmt(bare_c::LoopStatement::Stmt(
                    bare_c::Statement::Print(vec![bare_c::Expr::AExpr(
                        bare_c::AExpr::Id(String::from("c")),
                    )]),
                ))],
                otherwise: vec![bare_c::Block::Stmt(
                    bare_c::LoopStatement::Stmt(bare_c::Statement::Print(
                        vec![bare_c::Expr::AExpr(bare_c::AExpr::Id(
                            String::from("a"),
                        ))],
                    )),
                )],
            }],
            is_inc: true,
        }),
        bare_c::Block::Stmt(bare_c::Statement::Print(vec![
            bare_c::Expr::AExpr(bare_c::AExpr::Id(String::from("foo"))),
        ])),
    ];
    let prog = lowering::lower(prog);
    let json =
        serde_json::to_string(&super::to_prog(prog.to_src(true))).unwrap();
    let res = runner::run_prog(
        &json,
        None,
        vec![
            String::from("6"),
            String::from("-5"),
            String::from("10"),
            String::from("true"),
            String::from("false"),
        ],
        Duration::from_secs(3),
        "out/test_duffs",
    )
    .unwrap();
    assert_eq!(res.stdout, String::from("6\n11\n-5\n6\n21\n-5\n6\n31\n"));
    let res = runner::run_prog(
        &json,
        None,
        vec![
            String::from("6"),
            String::from("3"),
            String::from("11"),
            String::from("false"),
            String::from("true"),
        ],
        Duration::from_secs(3),
        "out/test_duffs2",
    )
    .unwrap();
    assert_eq!(
        res.stdout,
        String::from("11\n3\n11\n11\n3\n11\n11\n3\n11\n39\n")
    );
    let res = runner::run_prog(
        &json,
        None,
        vec![
            String::from("0"),
            String::from("2"),
            String::from("12"),
            String::from("false"),
            String::from("false"),
        ],
        Duration::from_secs(3),
        "out/test_duffs",
    )
    .unwrap();
    assert_eq!(res.stdout, String::from("2\n0\n12\n2\n0\n12\n2\n0\n32\n"));
}

#[test]
fn lower_single_switch() {
    let prog = vec![
        bare_c::Block::Stmt(bare_c::Statement::Print(vec![
            bare_c::Expr::AExpr(bare_c::AExpr::Id(String::from("c"))),
        ])),
        bare_c::Block::Switch {
            guard: bare_c::AExpr::Add(
                Box::new(bare_c::AExpr::Num(10)),
                Box::new(bare_c::AExpr::Id(String::from("a"))),
            ),
            cases: vec![],
            default: vec![bare_c::Block::Stmt(bare_c::Statement::Print(vec![
                bare_c::Expr::AExpr(bare_c::AExpr::Id(String::from("a"))),
            ]))],
        },
        bare_c::Block::Stmt(bare_c::Statement::Print(vec![
            bare_c::Expr::AExpr(bare_c::AExpr::Id(String::from("b"))),
        ])),
    ];
    let prog = lowering::lower(prog);
    let json =
        serde_json::to_string(&super::to_prog(prog.to_src(true))).unwrap();
    let res = runner::run_prog(
        &json,
        None,
        vec![
            String::from("101"),
            String::from("-5"),
            String::from("10"),
            String::from("true"),
            String::from("false"),
        ],
        Duration::from_secs(3),
        "out/test_single_switch",
    );
    assert_eq!(res.unwrap().stdout, String::from("10\n101\n-5\n"));
}

#[test]
fn lower_single_duff() {
    let prog = vec![
        bare_c::Block::Stmt(bare_c::Statement::Print(vec![
            bare_c::Expr::AExpr(bare_c::AExpr::Id(String::from("c"))),
        ])),
        bare_c::Block::Duffs(bare_c::DuffsInfo {
            var: String::from("foo"),
            init: bare_c::AExpr::Num(10),
            limit: bare_c::AExpr::Num(0),
            step: bare_c::AExpr::Num(-1),
            guard: bare_c::AExpr::Id(String::from("a")),
            bodies: vec![],
            default: vec![bare_c::Block::Stmt(bare_c::LoopStatement::Stmt(
                bare_c::Statement::Print(vec![bare_c::Expr::AExpr(
                    bare_c::AExpr::Id(String::from("foo")),
                )]),
            ))],
            is_inc: false,
        }),
        bare_c::Block::Stmt(bare_c::Statement::Print(vec![
            bare_c::Expr::AExpr(bare_c::AExpr::Id(String::from("b"))),
        ])),
    ];
    let prog = lowering::lower(prog);
    let json =
        serde_json::to_string(&super::to_prog(prog.to_src(true))).unwrap();
    let res = runner::run_prog(
        &json,
        None,
        vec![
            String::from("101"),
            String::from("-5"),
            String::from("-20"),
            String::from("true"),
            String::from("false"),
        ],
        Duration::from_secs(3),
        "out/test_single_duff",
    );
    assert_eq!(
        res.unwrap().stdout,
        String::from("-20\n10\n9\n8\n7\n6\n5\n4\n3\n2\n1\n-5\n")
    );
}

#[test]
fn test_loop() {
    let loops = 32_000;
    let prog = vec![bare_c::Block::<bare_c::Statement>::For {
        var: String::from("foo"),
        init: bare_c::AExpr::Num(0),
        limit: bare_c::AExpr::Num(loops),
        step: bare_c::AExpr::Num(1),
        body: vec![bare_c::Block::Stmt(bare_c::LoopStatement::Stmt(
            bare_c::Statement::Print(vec![bare_c::Expr::AExpr(
                bare_c::AExpr::Id(String::from("a")),
            )]),
        ))],
        is_inc: true,
    }];
    let prog = lowering::lower(prog);
    let json =
        serde_json::to_string(&super::to_prog(prog.to_src(true))).unwrap();
    let res = runner::run_prog(
        &json,
        None,
        vec![
            String::from("1"),
            String::from("-5"),
            String::from("10"),
            String::from("true"),
            String::from("false"),
        ],
        Duration::from_secs(20),
        "out/test_loop",
    )
    .unwrap();
    // * 2 for newlines
    let loops_u: usize = (loops * 2).try_into().unwrap();
    assert_eq!(res.stdout.len(), loops_u);
    assert!(res.stdout[0..1] == *"1");
}

#[test]
fn test_try_catch() {
    let prog = vec![
        bare_c::Block::Stmt(bare_c::Statement::Print(vec![
            bare_c::Expr::AExpr(bare_c::AExpr::Id(String::from("c"))),
        ])),
        bare_c::Block::TryCatch {
            try_block: vec![
                bare_c::Block::Stmt(bare_c::Statement::Throw(
                    0,
                    Some(bare_c::Expr::AExpr(bare_c::AExpr::Id(String::from(
                        "b",
                    )))),
                )),
                bare_c::Block::Stmt(bare_c::Statement::Print(vec![
                    bare_c::Expr::AExpr(bare_c::AExpr::Id(String::from("a"))),
                ])),
            ],
            catch_block: vec![bare_c::Block::Stmt(bare_c::Statement::Print(
                vec![bare_c::Expr::AExpr(bare_c::AExpr::Id(String::from(
                    "foo",
                )))],
            ))],
            catch_name: Some(String::from("foo")),
        },
        bare_c::Block::Stmt(bare_c::Statement::Print(vec![
            bare_c::Expr::BExpr(bare_c::BExpr::Id(String::from("d"))),
        ])),
    ];
    let prog = lowering::lower(prog);
    let json =
        serde_json::to_string(&super::to_prog(prog.to_src(true))).unwrap();
    let res = runner::run_prog(
        &json,
        None,
        vec![
            String::from("1"),
            String::from("2"),
            String::from("3"),
            String::from("true"),
            String::from("false"),
        ],
        Duration::from_secs(3),
        "out/test_try_cath",
    );
    assert_eq!(res.unwrap().stdout, String::from("3\n2\ntrue\n"));
}
