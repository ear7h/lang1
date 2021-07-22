#![allow(dead_code)]

#[derive(Debug)]
enum Pattern {
    Id(String),
    Tuple(Vec<Pattern>),
    Expr(Expr),
    Adt{
        ty : String,
        var : String,
        pat : Box<Pattern>,
    },
}

#[derive(Debug)]
enum Type {
    Abs(Box<Type>, Box<Type>),
    Tuple(Vec<Type>),
    Adt{
        name : String,
        vars : Vec<(String, Type)>,
    },
    Bool,
    Int,
    Text
}

#[derive(Debug)]
enum Expr {
    // control flow
    Abs(String, Box<Expr>),
    App{
        f : Box<Expr>,
        arg : Box<Expr>,
    },
    Match{
        val : Box<Expr>,
        arms : Vec<(Pattern, Expr)>,
    },

    // bindings
    Let{
        name : String,
        bound : Box<Expr>,
        val : Box<Expr>,
    },
    Var(String),

    // literals
    Adt{
        ty : String,
        var : String,
        val : Box<Expr>,
    },
    Tuple(Vec<Expr>),
    Typed{
        ty : Type,
        val : Box<Expr>,
    },
    Bool(bool),
    Int(usize),
    Text(String),
}

trait Env {
    fn get(&self, var : &str) -> Option<Expr>;
    fn push(&self, scope : &[(String, Expr)]) -> Self;
}

// TODO: eager evaluation
fn normalize<E : Env>(val : Expr, env : &E) -> Result<Expr, String> {
    use Expr::*;

    match val {
        Abs(..) => Ok(val),
        App{box f, box arg} => {
            match normalize(f, env)? {
                Abs(param, box body) => {
                    let env = env.push(&[(param, arg)]);
                    normalize(body, &env)
                }
                f => Err(format!("cannot apply {:?} to {:?}", arg, f))
            }
        },
        Match{..} => todo!(),
        Let{name, box bound, box val} => {
            normalize(val, &env.push(&[(name, bound)]))
        },
        Var(name) => env.get(&name).ok_or(format!("undefined variable {}", name)),
        x => Ok(x),
    }
}

