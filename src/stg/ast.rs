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
    Empty,
    AlgAlts(Vec<AlgAlt>),
    PrimAlts(Vec<PrimAlt>),
}

#[derive(Debug)]
pub struct AlgAlt {
    pub constr: Constr,
    pub vars: Vec<Var>,
    pub expr: Expr,
}

#[derive(Debug)]
pub struct PrimAlt {
    pub lit: Literal,
    pub expr: Expr,
}

#[derive(Debug)]
pub enum DefAlt {
    VarAlt { var: Var, expr: Box<Expr> },
    DefAlt { expr: Box<Expr> },
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
            $((
                stringify!($i).to_owned(),
                LambdaForm {
                    free: vec![$(stringify!($free).to_owned(),)*],
                    updatable: pi!($pi),
                    args: vec![$(stringify!($args).to_owned(),)*],
                    expr: expr!($($expr)*),
                }
            ),)*
        ]).collect())
    };
}

macro_rules! rec {
    () => {
        false
    };
    (rec) => {
        true
    };
}

macro_rules! pi {
    (n) => {
        false
    };
    (u) => {
        true
    };
}

#[macro_export]
macro_rules! expr {
    (let $($rec:ident)? {$($binds:tt)*} in $($expr:tt)+) => {
        Expr::Let {
            rec: rec!($($rec)*),
            binds: binds!($($binds)*),
            expr: Box::new(expr!($($expr)*))
        }
    };

    (case {$($expr:tt)+} of $($alts:tt)+) => {
        Expr::Case {
            expr: Box::new(expr!($($expr)*)),
            alts: alts!($($alts)*)
        }
    };

    (: $constr:ident {$($args:tt),* $(,)?}) => {
        Expr::ConstrApp {
            constr: stringify!($constr).to_owned(),
            args: vec![$(atom!($args),)*],
        }
    };

    ($prim:ident # {$($args:tt),* $(,)?}) => {
        Expr::PrimApp {
            prim: concat!(stringify!($prim), "#").to_owned(),
            args: vec![$(atom!($args),)*],
        }
    };

    ($var:ident {$($args:tt),* $(,)?}) => {
        Expr::VarApp {
            var: stringify!($var).to_owned(),
            args: vec![$(atom!($args),)*],
        }
    };

    ($lit:literal) => {
        Expr::Lit($lit)
    };
}

#[macro_export]
macro_rules! alts {
    (_ -> {$($expr:tt)+}) => {
        Alts(
            NonDefAlts::Empty,
            DefAlt::DefAlt(Box::new(expr!($($expr)*)))
        )
    };
    ($var:ident -> {$($expr:tt)+}) => {
        Alts(
            NonDefAlts::Empty,
            DefAlt::VarAlt {
                var: stringify!($var).to_owned(),
                expr: Box::new(expr!($($expr)*)),
            }
        )
    };
    ($($lit:literal -> {$($expr1:tt)+})* _ -> {$($expr2:tt)+}) => {
        Alts(
            NonDefAlts::PrimAlts(
                std::array::IntoIter::new([
                    $(PrimAlt {
                        lit: $lit,
                        expr: expr!($($expr1)*),
                    },)*
                ]).collect()
            ),
            DefAlt::DefAlt{ expr: Box::new(expr!($($expr2)*)) },
        )
    };
    ($($lit:literal -> {$($expr1:tt)+})* $var:ident -> {$($expr2:tt)+}) => {
        Alts(
            NonDefAlts::PrimAlts(
                std::array::IntoIter::new([
                    $(PrimAlt {
                        lit: $lit,
                        expr: expr!($($expr1)*),
                    },)*
                ]).collect()
            ),
            DefAlt::VarAlt {
                var: stringify!($var).to_owned(),
                expr: Box::new(expr!($($expr2)*)),
            }
        )
    };
    ($(: $constr:ident {$($var:ident),*} -> {$($expr1:tt)+})* _ -> {$($expr2:tt)+}) => {
        Alts(
            NonDefAlts::AlgAlts(
                std::array::IntoIter::new([
                    $(AlgAlt {
                        constr: stringify!($constr).to_owned(),
                        vars: vec![$(stringify!($var).to_owned(),)*],
                        expr: expr!($($expr1)*),
                    },)*
                ]).collect()
            ),
            DefAlt::DefAlt{ expr: Box::new(expr!($($expr2)*)) },
        )
    };
    ($(: $constr:ident {$($var1:ident),*} -> {$($expr1:tt)+})* $var2:ident -> {$($expr2:tt)+}) => {
        Alts(
            NonDefAlts::AlgAlts(
                std::array::IntoIter::new([
                    $(AlgAlt {
                        constr: stringify!($constr).to_owned(),
                        vars: vec![$(stringify!($var1).to_owned(),)*],
                        expr: expr!($($expr1)*),
                    },)*
                ]).collect()
            ),
            DefAlt::VarAlt {
                var: stringify!($var2).to_owned(),
                expr: Box::new(expr!($($expr2)*)),
            }
        )
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
