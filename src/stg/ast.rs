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

#[derive(Debug)]
pub struct Alts(pub NonDefAlts, pub DefAlt);

#[derive(Debug)]
pub enum NonDefAlts {
    AlgAlts(Vec<(Constr, Vec<Var>, Expr)>),
    PrimAlts(Vec<(Literal, Expr)>),
}

#[derive(Debug)]
pub enum DefAlt {
    VarAlt(Var, Box<Expr>),
    DefAlt(Box<Expr>),
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
        expr!(@let_accum [], true, $($rest)*);
    };
    (let $($rest:tt)+) => {
        expr!(@let_accum [], false, $($rest)*);
    };
    (@let_accum
        [$($binds:tt)*],
        $rec:literal,
        in $($rest:tt)+
    ) => {
        Expr::Let {
            rec: $rec,
            binds: Binds(std::array::IntoIter::new([$($binds)*]).collect()),
            expr: Box::new(expr!($($rest)*)),
        }
    };
    (@let_accum
        [$($binds:tt)*],
        $rec:literal,
        $i:ident = {$($free:tt)*} $pi:ident {$($args:tt)*} -> {$($expr:tt)+}
        $($rest:tt)+
    ) => {
        expr!(@let_accum [
            $($binds)*
            bind!($i = {$($free)*} $pi {$($args)*} -> {$($expr)*}),
        ], $rec, $($rest)*)
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

#[macro_export]
macro_rules! alts {
    (@accum_aalt [$($aalts:tt)*], default -> {$($expr:tt)*}) => {
        Alts(
            NonDefAlts::AlgAlts(vec![$($aalts)*]),
            DefAlt::DefAlt(Box::new(expr!($($expr)*)))
        )
    };
    (@accum_aalt [$($aalts:tt)*], $var:ident -> {$($expr:tt)*}) => {
        Alts(
            NonDefAlts::AlgAlts(vec![$($aalts)*]),
            DefAlt::VarAlt(stringify!($var).to_owned(), Box::new(expr!($($expr)*)))
        )
    };
    (@accum_aalt [$($aalts:tt)*], $constr:ident {$($vars:ident),*} -> {$($expr:tt)*} $($rest:tt)+) => {
        alts!(@accum_aalt [
            $($aalts)*
            (
                stringify!($constr).to_owned(),
                vec![$(stringify!($vars).to_owned())*],
                expr!($($expr)*),
            ),
        ], $($rest)*)
    };
    (@accum_palt [$($palts:tt)*], default -> {$($expr:tt)*}) => {
        Alts(
            NonDefAlts::PrimAlts(vec![$($palts)*]),
            DefAlt::DefAlt(Box::new(expr!($($expr)*)))
        )
    };
    (@accum_palt [$($palts:tt)*], $var:ident -> {$($expr:tt)*}) => {
        Alts(
            NonDefAlts::PrimAlts(vec![$($palts)*]),
            DefAlt::VarAlt(stringify!($var).to_owned(), Box::new(expr!($($expr)*)))
        )
    };
    (@accum_palt [$($palts:tt)*], $lit:literal -> {$($expr:tt)*} $($rest:tt)+) => {
        alts!(@accum_aalt [
            $($palts)*
            (
                $lit,
                expr!($($expr)*),
            ),
        ], $($rest)*)
    };
    ($lit:literal -> {$($expr:tt)*} $($rest:tt)+) => {
        alts!(@accum_palt [
            (
                $lit,
                expr!($($expr)*),
            ),
        ], $($rest)*)
    };
    ($($rest:tt)*) => {
        alts!(@accum_aalt [], $($rest)*)
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
