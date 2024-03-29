use lang1::expr::{self, *};
use lang1::parser::*;
use lang1::env::*;

#[test]
fn reduce() {
    let tests = vec![
        ("10", Expr::Int(10)),
        ("fn (x : int) -> int { x } (10)", Expr::Int(10)),
        (
            "fn (x : int) -> (bool, int) { (true, x) } (10)",
            Expr::Tuple(vec![
                Expr::Bool(true),
                Expr::Int(10),
            ]),
        ),
        (
            "match 10 { 1 -> { true }, 10 -> { false } }",
            Expr::Bool(false),
        ),
        (
            "match 10 { 1 -> { 2 }, x -> { x } }",
            Expr::Int(10),
        ),
    ];

    for (tc, exp) in tests {
        let expr = parse(tc).unwrap();
        let globals = Default::default();
        let mut env = Env::new(&globals);
        let got = expr::reduce(&expr, &mut env).unwrap();

        assert_eq!(exp, got, "\ntc: {:?}", tc)
    }
}
