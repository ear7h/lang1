#![allow(dead_code)]

use crate::env::Env;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Pattern {
    Var(String),
    Tuple(Vec<Pattern>),
    Adt{
        ty : String,
        var : String,
        pat : Box<Pattern>,
    },
    Bool(bool),
    Int(i64),
    Text(String),
}

impl Pattern {
    fn try_match<'a, 'b>(
        &'a self,
        expr: &'a Expr,
        env : &mut Env<'a, 'b>,
    ) -> Result<bool, String>
    where
        'a : 'b
    {
        match (expr, self) {
            (Expr::Bool(l), Pattern::Bool(r)) => {
                Ok(l == r)
            },
            (Expr::Int(l), Pattern::Int(r)) => {
                Ok(l == r)
            },
            (Expr::Text(l), Pattern::Text(r)) => {
                Ok(l == r)
            },
            (Expr::Tuple(l), Pattern::Tuple(r)) if l.len() == r.len() => {
                let matches = l
                    .iter()
                    .zip(r.iter())
                    .map(|(expr, pat)| {
                        pat.try_match(expr, env)
                    });
                for m in matches {
                    if !m? {
                        return Ok(false)
                    }
                }

                Ok(true)
            },
            (_, Pattern::Var(s)) => {
                env.define(s, expr);
                Ok(true)

            },
            _ => Err(format!("incompatible pattern: {:?} {:?}", self, expr))
        }
    }
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


pub fn reduce<'a, 'b>(val : &'a Expr, env : &mut Env<'a, 'b>) -> Result<Expr, String>
where
    'a : 'b,
{
    use Expr::*;

    match val {
        Abs{..} | Adt{..} |
        Bool(_) | Int(_)  |
        Text(_) => Ok(val.clone()),
        Tuple(fields) => {
            fields
                .iter()
                .map(|x| reduce(x, env))
                .collect::<Result<Vec<_>, _>>()
                .map(Expr::Tuple)
        },
        Typed{val, ..} => reduce(val, env),
        App{box f, args} => {
            match reduce(f, env)? {
                Abs{params, body, ..} if params.len() == args.len() => {
                    let args : Result<Vec<_>, _>= params
                        .into_iter()
                        .map(|(x, _)| x)
                        .zip(

                            args
                                .iter()
                                .map(|arg| reduce(arg, env)) // eager eval
                        )
                        .map(|(name, v)| v.map(|v| (name, v)))
                        .collect();

                    let args = args?;

                    let mut env = env.new_frame();

                    for (name, expr) in args.iter() {
                        println!("arg: {:?} {:?}", name, expr);
                        env.define(name, expr);
                    }

                    reduce(&body, &mut env)
                },
                f => Err(format!("cannot apply {:?} to {:?}", args, f))
            }
        },
        whole@Match{val, arms} => {
            let val = reduce(&val, env)?;
            let mut env = env.new_lifetime();

            for arm in arms {
                let pat = &arm.0;
                let body = &arm.1;

                if pat.try_match(&val, &mut env)? {
                    return reduce(&body, &mut env)
                }
            }

            Err(format!("non exhaustive match: {:?}", whole))
        }
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
