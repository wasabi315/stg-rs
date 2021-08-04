#[macro_use]
pub mod dsl;

use std::collections::HashMap;

pub type Program = Binds;

pub type Binds = HashMap<Var, LambdaForm>;

#[derive(Debug, Clone)]
pub struct LambdaForm {
    pub free: Vec<Var>,
    pub updatable: bool,
    pub args: Vec<Var>,
    pub expr: Expr,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Let {
        rec: bool,
        binds: Binds,
        expr: Box<Expr>,
    },
    Case {
        expr: Box<Expr>,
        alts: Vec<Alt>,
    },
    VarApp {
        var: Var,
        args: Vec<Atom>,
    },
    ConApp {
        con: Con,
        args: Vec<Atom>,
    },
    PrimApp {
        prim: Prim,
        args: Vec<Atom>,
    },
    Lit(Literal),
}

#[derive(Debug, Clone)]
pub enum Alt {
    AlgAlt {
        con: Con,
        vars: Vec<Var>,
        expr: Expr,
    },
    PrimAlt {
        lit: Literal,
        expr: Expr,
    },
    VarAlt {
        var: Var,
        expr: Expr,
    },
    DefAlt {
        expr: Expr,
    },
}

pub type Var = String;
pub type Con = String;
pub type Prim = String;
pub type Literal = i64;

#[derive(Debug, Clone)]
pub enum Atom {
    Var(Var),
    Lit(Literal),
}
