use std::collections::HashMap;

pub type Program = Binds;

#[derive(Debug)]
pub struct Binds(pub HashMap<Var, LambdaForm>);

#[derive(Debug)]
pub struct LambdaForm {
    pub free: Vec<Var>,
    pub updatable: bool,
    pub args: Vec<Var>,
    pub expr: Expr,
}

#[derive(Debug)]
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

#[derive(Debug)]
pub enum Alt {
    Alg {
        constr: Constr,
        vars: Vec<Var>,
        expr: Box<Expr>,
    },
    Prim {
        lit: Literal,
        expr: Box<Expr>,
    },
    Var {
        var: Var,
        expr: Box<Expr>,
    },
    Def {
        expr: Box<Expr>,
    },
}

pub type Var = String;
pub type Constr = String;
pub type Prim = String;
pub type Literal = i64;

#[derive(Debug)]
pub enum Atom {
    Var(Var),
    Lit(Literal),
}

#[macro_export]
macro_rules! stg {
    ($($program:tt)*) => {
        binds!($($program)*)
    };
}

#[macro_export]
macro_rules! binds {
    ($($i:ident = {$($free:tt)*} $pi:ident {$($args:tt)*} -> {$($expr:tt)+})*) => {
        Binds(std::array::IntoIter::new([
            $(bind!($i = {$($free)*} $pi {$($args)*} -> {$($expr)*}),)*
        ]).collect())
    };
}

#[macro_export]
macro_rules! bind {
    ($i:ident = {$($free:ident),*} $pi:ident {$($args:ident),*} -> {$($expr:tt)+}) => {
        (
            stringify!($i).to_owned(),
            LambdaForm {
                free: vec![$(stringify!($free).to_owned(),)*],
                updatable: pi!($pi),
                args: vec![$(stringify!($args).to_owned(),)*],
                expr: expr!($($expr)*),
            }
        )
    };
}

macro_rules! pi {
    (n) => {
        true
    };
    (u) => {
        false
    };
}

#[macro_export]
macro_rules! expr {
    (let rec $($rest:tt)+) => {
        let_expr!(true, [], $($rest)*)
    };
    (let $($rest:tt)+) => {
        let_expr!(false, [], $($rest)*)
    };
    (case {$($expr:tt)+} of $($alts:tt)*) => {
        Expr::Case {
            expr: Box::new(expr!($($expr)*)),
            alts: alts!($($alts)*)
        }
    };
    (var $var:ident {$($args:tt),* $(,)?}) => {
        Expr::VarApp {
            var: stringify!($var).to_owned(),
            args: vec![$(atom!($args),)*],
        }
    };
    (constr $constr:ident {$($args:tt),* $(,)?}) => {
        Expr::ConstrApp {
            constr: stringify!($constr).to_owned(),
            args: vec![$(atom!($args),)*],
        }
    };
    (prim $prim:ident {$($args:tt),* $(,)?}) => {
        Expr::PrimApp {
            prim: stringify!($prim).to_owned(),
            args: vec![$(atom!($args),)*],
        }
    };
    ($lit:literal) => {
        Expr::Lit($lit)
    };
}

macro_rules! let_expr {
    ($rec:literal, [$($binds:tt)*], in $($rest:tt)+) => {
        Expr::Let {
            rec: $rec,
            binds: Binds(std::array::IntoIter::new([$($binds)*]).collect()),
            expr: Box::new(expr!($($rest)*)),
        }
    };
    ($rec:literal, [$($binds:tt)*], $i:ident = {$($free:tt)*} $pi:ident {$($args:tt)*} -> {$($expr:tt)+} $($rest:tt)+) => {
        let_expr!($rec, [
            $($binds)*
            bind!($i = {$($free)*} $pi {$($args)*} -> {$($expr)*}),
        ], $($rest)*)
    };
}

#[macro_export]
macro_rules! alts {
    ([$($alts:tt)*], ) => {
        vec![$($alts)*]
    };
    ([$($alts:tt)*], $constr:ident {$($vars:ident),* $(,)?} -> {$($expr:tt)*} $($rest:tt)*) => {
        alts!([
            $($alts)*
            Alt::Alg {
                constr: stringify!($constr).to_owned(),
                vars: vec![$(stringify!($vars).to_owned())*],
                expr: Box::new(expr!($($expr)*)),
            },
        ], $($rest)*)
    };
    ([$($alts:tt)*], $lit:literal -> {$($expr:tt)*} $($rest:tt)*) => {
        alts!([
            $($alts)*
            Alt::Prim {
                lit: $lit,
                expr: Box::new(expr!($($expr)*)),
            },
        ], $($rest)*)
    };
    ([$($alts:tt)*], default -> {$($expr:tt)*} $($rest:tt)*) => {
        alts!([
            $($alts)*
            Alt::Def {
                expr: Box::new(expr!($($expr)*)),
            },
        ], $($rest)*)
    };
    ([$($alts:tt)*], $var:ident -> {$($expr:tt)*} $($rest:tt)*) => {
        alts!([
            $($alts)*
            Alt::Var {
                var: stringify!($var).to_owned(),
                expr: Box::new(expr!($($expr)*)),
            },
        ], $($rest)*)
    };
    ($($rest:tt)*) => {
        alts!([], $($rest)*)
    };
}

#[macro_export]
macro_rules! atom {
    ($var:ident) => {
        Atom::Var(stringify!($var).to_owned())
    };
    ($lit:literal) => {
        Atom::Lit($lit)
    };
}
