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

macro_rules! binds_and {
    (add_bind, $cont:ident, $m:expr, $i:ident = {$($free:ident),* $(,)?} $rec:ident {$($args:ident),* $(,)?} -> {$($expr:tt)*} $($rest:tt)*) => {{
        $m.insert(
            stringify!($i).to_owned(),
            LambdaForm {
                free: vec![$(stringify!($free).to_owned(),)*],
                updatable: pi!($rec),
                args: vec![$(stringify!($args).to_owned(),)*],
                expr: expr!($($expr)*),
            }
        );
        binds_and!(add_bind, $cont, $m, $($rest)*)
    }};
    (add_bind, $cont:ident, $m:expr, $($rest:tt)*) => {
        binds_and!(apply_cont, $cont, $($rest)*)
    };
    (apply_cont, $cont:ident, $($rest:tt)*) => {
        $cont!($($rest)*)
    };
    ($cont:ident, $($rest:tt)*) => {{
        let mut m = std::collections::HashMap::new();
        let x = binds_and!(add_bind, $cont, m, $($rest)*);
        (Binds(m), x)
    }};
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
macro_rules! binds {
    ($($binds:tt)*) => {{
        binds_and!(exhaust, $($binds)*).0
    }};
}

macro_rules! exhaust {
    () => {
        ()
    };
}

#[macro_export]
macro_rules! expr {
    (let rec $($rest:tt)+) => {{
        let (binds, expr) = binds_and!(let_body_expr, $($rest)*);
        Expr::Let { rec: true, binds, expr }
    }};
    (let $($rest:tt)+) => {{
        let (binds, expr) = binds_and!(let_body_expr, $($rest)*);
        Expr::Let { rec: false, binds, expr }
    }};
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

macro_rules! let_body_expr {
    (in $($expr:tt)+) => {
        Box::new(expr!($($expr)*))
    };
}

#[macro_export]
macro_rules! alts {
    (add_alts, $alts:expr, ) => {};
    (add_alts, $alts:expr, $constr:ident {$($vars:ident),* $(,)?} -> {$($expr:tt)+} $($rest:tt)*) => {
        $alts.push(Alt::Alg {
            constr: stringify!($constr).to_owned(),
            vars: vec![$(stringify!($vars).to_owned())*],
            expr: Box::new(expr!($($expr)*)),
        });
        alts!(add_alts, $alts, $($rest)*);
    };
    (add_alts, $alts:expr, $lit:literal -> {$($expr:tt)+} $($rest:tt)*) => {
        $alts.push(Alt::Prim {
            lit: $lit,
            expr: Box::new(expr!($($expr)*)),
        });
        alts!(add_alts, $alts, $($rest)*);
    };
    (add_alts, $alts:expr, $var:ident -> {$($expr:tt)+} $($rest:tt)*) => {
        $alts.push(Alt::Var {
            var: stringify!($var).to_owned(),
            expr: Box::new(expr!($($expr)*)),
        });
        alts!(add_alts, $alts, $($rest)*);
    };
    (add_alts, $alts:expr, default -> {$($expr:tt)+} $($rest:tt)*) => {
        $alts.push(Alt::Def {
            expr: Box::new(expr!($($expr)*)),
        });
        alts!(add_alts, $alts, $($rest)*);
    };
    ($($alts:tt)*) => {{
        let mut alts = Vec::new();
        alts!(add_alts, alts, $($alts)*);
        alts
    }};
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
