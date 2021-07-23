#![allow(dead_code)]

use crate::env::Env;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Pattern {
    Id(String),
    Tuple(Vec<Pattern>),
    Expr(Expr),
    Adt{
        ty : String,
        var : String,
        pat : Box<Pattern>,
    },
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Type {
    Abs{
        params : Vec<Type>,
        ret : Box<Type>,
    },
    Tuple(Vec<Type>),
    Adt{
        name : String,
        vars : Vec<(String, Type)>,
    },
    Bool,
    Int,
    Text
}


#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expr {
    // control flow
    Abs{
        params : Vec<(String, Type)>,
        ret : Type,
        body : Box<Expr>,
    },
    App{
        f : Box<Expr>,
        args : Vec<Expr>,
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
    Int(i64),
    Text(String),
}


pub fn reduce<'a, 'b, 'c>(val : &'a Expr, env : &mut Env<'a, 'b>) -> Result<Expr, String>
where
    'a : 'b,
    'c : 'b
{
    use Expr::*;

    match val {
        Abs{..} | Tuple(..) | Adt{..} |
        Bool(_) | Int(_)    | Text(_) => Ok(val.clone()),
        Typed{val, ..} => reduce(val, env),
        App{box f, args} => {
            match reduce(f, env)? {
                Abs{params, mut body, ..} if params.len() == args.len() => {
                    let args : Result<Vec<_>, _>= params
                        .into_iter()
                        .map(|(x, _)| x)
                        .zip(
                            args
                                .into_iter()
                                .map(|arg| reduce(arg, env)) // eager eval
                        )
                        .map(|(name, v)| v.map(|v| (name, v)))
                        .collect();

                    let args = args?;

                    let mut env = env.new_frame();

                    for (name, expr) in args.iter() {
                        env.define(name, expr);
                    }

                    reduce(&mut body, &mut env)
                },
                f => Err(format!("cannot apply {:?} to {:?}", args, f))
            }
        },
        Match{..} => todo!(),
        Let{name, box bound, box val} => {

            let bound = reduce(bound, env)?;

            let mut env = env.new_lifetime();

            env.define(name, &bound);

            let ret = reduce(val, &mut env);

            ret
        },
        Var(name) => env.get(&name).map(Clone::clone).ok_or(format!("undefined variable {}", name)),
    }
}
