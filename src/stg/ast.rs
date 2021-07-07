#[macro_use]
pub mod dsl;

use std::collections::HashMap;

pub type Program = Binds;

#[derive(Debug, Clone)]
pub struct Binds(pub HashMap<Var, LambdaForm>);

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
        alts: Alts,
    },
    VarApp {
        var: Var,
        args: Vec<Atom>,
    },
    ConstrApp {
        constr: Constr,
        args: Vec<Atom>,
    },
    PrimApp {
        prim: Prim,
        args: Vec<Atom>,
    },
    Lit(Literal),
}

#[derive(Debug, Clone)]
pub struct Alts(pub NonDefAlts, pub DefAlt);

#[derive(Debug, Clone)]
pub enum NonDefAlts {
    Empty,
    AlgAlts(Vec<AlgAlt>),
    PrimAlts(Vec<PrimAlt>),
}

#[derive(Debug, Clone)]
pub struct AlgAlt {
    pub constr: Constr,
    pub vars: Vec<Var>,
    pub expr: Expr,
}

#[derive(Debug, Clone)]
pub struct PrimAlt {
    pub lit: Literal,
    pub expr: Expr,
}

#[derive(Debug, Clone)]
pub enum DefAlt {
    VarAlt { var: Var, expr: Box<Expr> },
    DefAlt { expr: Box<Expr> },
}

pub type Var = String;
pub type Constr = String;
pub type Prim = String;
pub type Literal = i64;

#[derive(Debug, Clone)]
pub enum Atom {
    Var(Var),
    Lit(Literal),
}
