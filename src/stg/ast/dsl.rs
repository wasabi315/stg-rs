#[macro_export]
macro_rules! stg {
    ($($program:tt)*) => {
        binds!($($program)*)
    };
}

macro_rules! binds {
    ($($i:ident = {$($free:ident),*} $updatable:ident {$($args:ident),*} -> {$($expr:tt)+})*) => {
        Binds(std::array::IntoIter::new([
            $((
                stringify!($i).to_owned(),
                LambdaForm {
                    free: vec![$(stringify!($free).to_owned(),)*],
                    updatable: updatable!($updatable),
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

macro_rules! updatable {
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
    (: $constr:ident {$($args:tt),*}) => {
        Expr::ConstrApp {
            constr: concat!(":", stringify!($constr)).to_owned(),
            args: vec![$(atom!($args),)*],
        }
    };
    ($prim:ident # {$($args:tt),*}) => {
        Expr::PrimApp {
            prim: concat!(stringify!($prim), "#").to_owned(),
            args: vec![$(atom!($args),)*],
        }
    };
    ($var:ident {$($args:tt),*}) => {
        Expr::VarApp {
            var: stringify!($var).to_owned(),
            args: vec![$(atom!($args),)*],
        }
    };
    ($lit:literal) => {
        Expr::Lit($lit)
    };
}

macro_rules! alts {
    (_ -> {$($expr:tt)+}) => {
        Alts(
            NonDefAlts::Empty,
            DefAlt::DefAlt { expr: Box::new(expr!($($expr)*)) },
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
                        constr: concat!(":", stringify!($constr)).to_owned(),
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
                        constr: concat!(":", stringify!($constr)).to_owned(),
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

macro_rules! atom {
    ($var:ident) => {
        Atom::Var(stringify!($var).to_owned())
    };
    ($lit:literal) => {
        Atom::Lit($lit)
    };
}
