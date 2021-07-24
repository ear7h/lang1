
use crate::expr::{Expr, Type, Pattern};

pub fn parse(s : &str) -> Option<Expr> {
    parser::main(s).ok()
}

peg::parser!{grammar parser() for str {

    pub rule pattern() -> Pattern
        = pat_bool()
        / pat_int()
        / pat_text()
        / pat_tuple()
        / s:ident() {
            Pattern::Var(s)
        }

    rule pat_tuple() -> Pattern
        = "(" _ pat:pattern() ** (_ "," _) _ ")" {
            Pattern::Tuple(pat)
        }

    rule pat_text() -> Pattern
        = "\"" s:$([^ '"']*) "\"" {
            Pattern::Text(String::from(s))
        }

    rule pat_int() -> Pattern
        = n:num_lit() {
            Pattern::Int(n as i64)
        } / "-" n:num_lit() {
            Pattern::Int(-(n as i64))
        }

    rule  pat_bool() -> Pattern
        = "true" {
            Pattern::Bool(true)
        } / "false" {
            Pattern::Bool(false)
        }

    pub rule ty() -> Type
        = "bool" { Type::Bool }
        / "int" { Type::Int }
        / "text" { Type::Text }
        / "(" _ ty:ty() ** (_ "," _) _ ")" { Type::Tuple(ty) }

    pub rule main() -> Expr
        = _ e:expr() _ { e }

    pub rule expr() -> Expr = precedence!{
        f:@ _ "(" _ args:expr1() ** (_ "," _) ","? _ ")" {
            Expr::App{
                args,
                f : box f,
            }
        }
        e:expr1() { e }
        "(" _ e:expr1() _ ")" { e }
    }

    pub rule expr1() -> Expr
        = expr_tuple()
        / expr_text()
        / expr_bool()
        / expr_int()
        / expr_let()
        / expr_func()
        / expr_match()
        / expr_tuple()
        / s:ident() {
            Expr::Var(s)
        }



    rule expr_match_arm() -> (Pattern, Expr)
        = pat:pattern() _ "->" _ "{" _ e:expr() _ "}" {
            (pat, e)
        }

    rule expr_match() -> Expr
        = "match" __ val:expr() _
        "{" _ arms:expr_match_arm() ** (_ "," _) _ ","? _ "}" {
            Expr::Match{ val : box val, arms }
        }

    rule expr_func_param() -> (String, Type)
        = name:ident() _ ":" _ ty:ty() { (name, ty) }

    rule expr_func() -> Expr
        = "fn" _ "("
        _ params:expr_func_param() ** (_ "," _) _
        ")" _ "->" _ ret:ty() _ "{" _ body:expr() _ "}" {
            Expr::Abs{ params, ret, body : box body }
        }

    rule expr_let() -> Expr
        = "let" __ name:ident() _ "=" _ bound:expr() __ "in" __ val:expr() {
            Expr::Let{name, bound: box bound, val: box val}
        } / expr_multi_let()

    rule expr_multi_let_binding() -> (String, Expr)
        = "let" __ name:ident() _ "=" _ bound:expr() { (name, bound) }

    rule expr_multi_let() -> Expr
        = b:(b:expr_multi_let_binding() (_ ";" _)+ { b })+ val:expr() {
            b
                .into_iter()
                .rev()
                .fold(val, |val, (name, bound)| {
                    Expr::Let{name, bound: box bound, val: box val}
                })
        }

    rule expr_tuple() -> Expr
        = "(" _ e:expr() **<2,> (_ "," _) _ ")" { Expr::Tuple(e) }
        / "(" _ e:(e:expr() _ "," _ { e })* _ ")" { Expr::Tuple(e) }

    rule expr_text() -> Expr
        = "\"" s:$([^ '"']*) "\"" {
            Expr::Text(String::from(s))
        }

    rule expr_bool() -> Expr
        = "true" {
            Expr::Bool(true)
        } / "false" {
            Expr::Bool(false)
        }

    rule expr_int() -> Expr
        = n:num_lit() {
            Expr::Int(n as i64)
        } / "-" n:num_lit() {
            Expr::Int(-(n as i64))
        }

    rule _() -> ()
        = [' ' | '\n' | '\t']* {}

    rule __() -> ()
        = [' ' | '\n' | '\t']+ {}


    rule ident() -> String
        = s:$(
            ['a'..='z' | 'A'..='Z' | '_' ]
            ['a'..='z' | 'A'..='Z' | '0'..='9' | '_' ]*
            "'"*
        ) { String::from(s) }


    pub rule num_lit() -> u64
        = num16()
        / num8()
        / num10()


    rule num10() -> u64
        = head:$(['0'..='9']) rest:$(['0'..='9' | '_']*) {
            let mut ret = 0;

            for b in head.bytes().chain(rest.bytes()) {
                let b = match b as char {
                    '0'..='9' => b - ('0' as u8),
                    '_' => continue,
                    _ => unreachable!(),
                };

                ret = (ret * 10) + (b as u64)
            }

            ret
        }

    rule num16() -> u64
        = "0x" n:$(['0'..='9' | 'a'..='f' | 'A'..='F' | '_']+) {
            let mut ret = 0;
            for b in n.bytes() {
                let b = match b as char {
                    '0'..='9' => b - ('0' as u8),
                    'a'..='f' => 10 + b - ('a' as u8),
                    'F'..='F' => 10 + b - ('F' as u8),
                    '_' => continue,
                    _ => unreachable!(),
                };

                ret = (ret << 4) | (b as u64)
            }

            ret
        }

    rule num8() -> u64
        = "0o" n:$(['0'..='7' | '_']+) {
            let mut ret = 0;
            for b in n.bytes() {
                let b = match b as char {
                    '0'..='7' => b - ('0' as u8),
                    '_' => continue,
                    _ => unreachable!(),
                };

                ret = (ret << 3) | (b as u64)
            }

            ret
        }

}}


#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn num_lit() {
        let tests = vec![
            ("0", Some(0)),
            ("10", Some(10)),
            ("10_0", Some(100)),
            ("_", None),
            ("0x10", Some(16)),
            ("0o10", Some(8)),
        ];

        for (tc, exp) in tests {
            let got = parser::num_lit(tc);

            assert_eq!(exp, got.ok(), "\ntc: {:?}", tc)
        }
    }

    #[test]
    fn pattern() {
        let tests = vec![
            ("10", Pattern::Int(10)),
            ("true", Pattern::Bool(true)),
            (
                "(1, \"hello\")",
                Pattern::Tuple(vec![
                    Pattern::Int(1),
                    Pattern::Text("hello".to_string()),
                ]),
            ),
            (
                "(1, hello)",
                Pattern::Tuple(vec![
                    Pattern::Int(1),
                    Pattern::Var("hello".to_string()),
                ]),
            ),

        ];

        for (tc, exp) in tests {
            let got = parser::pattern(tc);

            assert_eq!(Some(exp), got.ok(), "\ntc: {:?}", tc)
        }
    }


    #[test]
    fn expr() {
        let tests = vec![
            ("12", Expr::Int(12)),
            ("(12)", Expr::Int(12)),
            ("( 12 )", Expr::Int(12)),
            ("(-12)", Expr::Int(-12)),
            (
                "()",
                Expr::Tuple(vec![]),
            ),
            (
                "( )",
                Expr::Tuple(vec![]),
            ),
            (
                "(1,)",
                Expr::Tuple(vec![
                    Expr::Int(1),
                ]),
            ),
            (
                "( 1 , )",
                Expr::Tuple(vec![
                    Expr::Int(1),
                ]),
            ),
            (
                "(1,2)",
                Expr::Tuple(vec![
                    Expr::Int(1),
                    Expr::Int(2),
                ]),
            ),
            (
                "(\n1\t, 2, )",
                Expr::Tuple(vec![
                    Expr::Int(1),
                    Expr::Int(2),
                ]),
            ),
            (
                "(1,2,3)",
                Expr::Tuple(vec![
                    Expr::Int(1),
                    Expr::Int(2),
                    Expr::Int(3),
                ]),
            ),
        ];

        for (tc, exp) in tests {
            let got = parser::expr(tc);

            assert_eq!(Some(exp), got.ok(), "\ntc: {:?}", tc)
        }
    }

    #[test]
    fn expr_match() {
        let tests = vec![
            (
                "match 10 { 11 -> { 12 } }",
                Expr::Match{
                    val : box Expr::Int(10),
                    arms : vec![
                        (Pattern::Int(11), Expr::Int(12))
                    ],
                }
            ),
            (
                "match 10 { 11 -> { 12 }, }",
                Expr::Match{
                    val : box Expr::Int(10),
                    arms : vec![
                        (Pattern::Int(11), Expr::Int(12))
                    ],
                }
            ),
            (
                "match 10 {
                    11 -> { 12 },
                    13 -> {
                        true
                    },
                    _ -> { x }
                }",
                Expr::Match{
                    val : box Expr::Int(10),
                    arms : vec![
                        (Pattern::Int(11), Expr::Int(12)),
                        (Pattern::Int(13), Expr::Bool(true)),
                        (Pattern::Var("_".to_string()), Expr::Var("x".to_string())),
                    ],
                }
            ),
        ];

        for (tc, exp) in tests {
            let got = parser::expr(tc);

            assert_eq!(Some(exp), got.ok(), "\ntc: {:?}", tc)
        }
    }

    #[test]
    fn expr_abs() {
        let tests = vec![
            (
                "fn (a : int) -> int { 10 }",
                Expr::Abs{
                    params: vec![
                        ("a".to_string(), Type::Int),
                    ],
                    ret : Type::Int,
                    body : box Expr::Int(10),
                }
            ),
        ];

        for (tc, exp) in tests {
            let got = parser::expr(tc);

            assert_eq!(Some(exp), got.ok(), "\ntc: {:?}", tc)
        }
    }

    #[test]
    fn expr_app() {
        let tests = vec![
            (
                "f(hello, 10)",
                Expr::App{
                    f: box Expr::Var("f".to_string()),
                    args : vec![
                        Expr::Var("hello".to_string()),
                        Expr::Int(10),
                    ],
                },
            ),
            (
                "f(\nhello\n, 10,\n)",
                Expr::App{
                    f: box Expr::Var("f".to_string()),
                    args : vec![
                        Expr::Var("hello".to_string()),
                        Expr::Int(10),
                    ],
                },
            ),
            (
                "fn (x : int) -> (bool, int) { (true, x) } (10)",
                Expr::App{
                    f: box Expr::Abs{
                        params : vec![
                            ("x".to_string(), Type::Int),
                        ],
                        ret : Type::Tuple(vec![
                            Type::Bool,
                            Type::Int,
                        ]),
                        body: box Expr::Tuple(vec![
                            Expr::Bool(true),
                            Expr::Var("x".to_string())
                        ]),
                    },
                    args : vec![
                        Expr::Int(10),
                    ],
                },
            ),
        ];

        for (tc, exp) in tests {
            let got = parser::expr(tc);

            assert_eq!(Some(exp), got.ok(), "\ntc: {:?}", tc)
        }
    }

    #[test]
    fn expr_let() {
        let tests = vec![
            (
                "let x = 10 in hello",
                Expr::Let{
                    name: String::from("x"),
                    bound : box Expr::Int(10),
                    val : box Expr::Var("hello".to_string()),
                },
            ),
            (
                "let x = 10 in let y = 2 in hello",
                Expr::Let{
                    name: String::from("x"),
                    bound : box Expr::Int(10),
                    val : box Expr::Let{
                        name: String::from("y"),
                        bound : box Expr::Int(2),
                        val : box Expr::Var("hello".to_string()),
                    },
                },
            ),
            (
                "let x = 10;
                let y = 2;
                hello",
                Expr::Let{
                    name: String::from("x"),
                    bound : box Expr::Int(10),
                    val : box Expr::Let{
                        name: String::from("y"),
                        bound : box Expr::Int(2),
                        val : box Expr::Var("hello".to_string()),
                    },
                },
            ),
        ];

        for (tc, exp) in tests {
            let got = parser::expr(tc);

            assert_eq!(Some(exp), got.ok(), "\ntc: {:?}", tc)
        }
    }

    #[test]
    fn expr_text() {
        let tests = vec![
            (r#""hello""#, "hello"),
        ];

        for (tc, exp) in tests {
            let got = parser::expr(tc);

            assert_eq!(Some(Expr::Text(String::from(exp))), got.ok(), "\ntc: {:?}", tc)
        }
    }


    #[test]
    fn expr_bool() {
        let tests = vec![
            ("true", true),
            ("false", false),
        ];

        for (tc, exp) in tests {
            let got = parser::expr(tc);

            assert_eq!(Some(Expr::Bool(exp)), got.ok(), "\ntc: {:?}", tc)
        }
    }

    #[test]
    fn expr_int() {
        let tests = vec![
            ("19", 19),
            ("-19", -19),
        ];

        for (tc, exp) in tests {
            let got = parser::expr(tc);

            assert_eq!(Some(Expr::Int(exp)), got.ok(), "\ntc: {:?}", tc)
        }
    }
}
