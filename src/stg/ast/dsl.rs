#[macro_export]
macro_rules! stg {
    ($($program:tt)*) => {
        binds!($($program)*)
    };
}

macro_rules! binds {
    ($($i:ident = {$($free:ident),*} $updatable:ident {$($args:ident),*} -> {$($expr:tt)+})*) => {
        ::std::collections::HashMap::from([
            $((
                stringify!($i).to_owned(),
                LambdaForm {
                    free: vec![$(stringify!($free).to_owned(),)*],
                    updatable: updatable!($updatable),
                    args: vec![$(stringify!($args).to_owned(),)*],
                    expr: expr!($($expr)*),
                }
            ),)*
        ])
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
    ($var_or_con:ident {$($args:tt),*}) => {
        {
            let var_or_con = stringify!($var_or_con).to_owned();
            if var_or_con.chars().nth(0).unwrap().is_uppercase() {
                Expr::ConApp {
                    con: var_or_con,
                    args: vec![$(atom!($args),)*],
                }
            } else {
                Expr::VarApp {
                    var: var_or_con,
                    args: vec![$(atom!($args),)*],
                }
            }
        }
    };
    ($prim_or_con:ident # {$($args:tt),*}) => {
        {
            let prim_or_con = stringify!($prim_or_con);
            if prim_or_con.chars().nth(0).unwrap().is_uppercase() {
                Expr::ConApp {
                    con: concat!(stringify!($prim_or_con), "#").to_owned(),
                    args: vec![$(atom!($args),)*],
                }
            } else {
                Expr::PrimApp {
                    prim: concat!(stringify!($prim_or_con), "#").to_owned(),
                    args: vec![$(atom!($args),)*],
                }
            }
        }
    };
    ($lit:literal) => {
        Expr::Lit($lit)
    };
}

macro_rules! alts {
    (@acc [$($alts:tt)*], ) => {
        vec![$($alts)*]
    };
    (@acc [$($alts:tt)*], $con:ident {$($vars:ident),*} -> {$($expr:tt)+} $($rest:tt)*) => {
        alts!(
            @acc [$($alts)* Alt::AlgAlt { con: stringify!($con).to_owned(), vars: vec![$(stringify!($vars).to_owned(),)*], expr: expr!($($expr)*) },],
            $($rest)*
        )
    };
    (@acc [$($alts:tt)*], $lit:literal -> {$($expr:tt)+} $($rest:tt)*) => {
        alts!(
            @acc [$($alts)* Alt::PrimAlt { lit: $lit, expr: expr!($($expr)*) },],
            $($rest)*
        )
    };
    (@acc [$($alts:tt)*], $var:ident -> {$($expr:tt)+} $($rest:tt)*) => {
        alts!(
            @acc [$($alts)* Alt::VarAlt { var: stringify!($var).to_owned(), expr: expr!($($expr)*) },],
            $($rest)*
        )
    };
    (@acc [$($alts:tt)*], _ -> {$($expr:tt)+} $($rest:tt)*) => {
        alts!(
            @acc [$($alts)* Alt::DefAlt { expr: expr!($($expr)*) },],
            $($rest)*
        )
    };
    (@acc $($rest:tt)*) => {
        compile_error!(stringify!($($rest)*))
    };
    ($($rest:tt)*) => {
        alts!(@acc [], $($rest)*)
    }
}

macro_rules! atom {
    ($var:ident) => {
        Atom::Var(stringify!($var).to_owned())
    };
    ($lit:literal) => {
        Atom::Lit($lit)
    };
}
