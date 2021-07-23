#![allow(dead_code, unused_imports)]
use std::collections::HashMap;

use quickscope::ScopeMap;

use crate::expr::Expr;


pub struct Env<'a, 'b> {
    globals : &'a HashMap<String, Expr>,
    locals : ScopeMap<&'b str, &'b Expr>,
}

impl<'a, 'b> Env<'a, 'b>
where
    'a : 'b
{
    pub fn new(globals : &'a HashMap<String, Expr>) -> Self {
        Env{
            globals,
            locals : Default::default(),
        }
    }

    pub fn get(&self, name : &str) -> Option<&'b Expr> {
        self.locals.get(name).map(|x| *x).or_else(|| self.globals.get(name))
    }

    pub fn new_frame<'c>(&self) -> Env<'a, 'c> {
        Env{
            globals : self.globals,
            locals : Default::default(),
        }
    }

    // TODO: this is ineficient, can probably be removed with interning
    pub fn new_lifetime<'c>(&self) -> Env<'a, 'c>
    where
        'b : 'c
    {
        let mut new = self.new_frame();

        for (k, v) in self.locals.iter() {
            new.define(k, v)
        }

        new
    }


    pub fn push_layer(&mut self) {
        self.locals.push_layer()
    }

    pub fn pop_layer(&mut self) {
        self.locals.pop_layer();
    }

    pub fn define(&mut self, k : &'b str, v : &'b Expr) {
        self.locals.define(k, v)
    }
}
