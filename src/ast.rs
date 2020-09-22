/*
use std::rc::Rc;

use crate::{
    asm,
    types::Type,
};

pub struct Function {
    package: Rc<str>,
    name: String,
    exported: bool,
    param_type: Vec<Type>,
    ret_type: Vec<Type>,
    body: Vec<Stmt>,
}

impl Function {
    fn lower(self) -> asm::Function {
        Default::default()
    }
}

pub enum Stmt{
    Return(Expr),
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    Switch(Expr, Vec<(Expr, Stmt)>),
    Assign(Ident, Expr),
}

pub enum Expr {
    Lit
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
}

pub struct Ident {
    pub name: String,
}

*/
