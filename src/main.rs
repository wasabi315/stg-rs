use std::io;

#[macro_use]
mod stg;
use stg::ast::*;
use stg::pretty::*;

fn main() {
    let program = stg! {
        fix = {} n {f} -> {
            let rec {
                x = {f, x} u {} -> { var f {x} }
            }
            in var x {}
        }

        fact = {} u {} -> {
            let {
                f = {} n {g, n} -> {
                    case {var n {}} of
                        0 -> { 1 }
                        default -> {
                            case {prim sub_ {n, 1}} of
                                m -> {
                                    case {var g {m}} of
                                        gm -> { prim mul_ {n, gm} }
                                }
                        }
                }
            }
            in var fix {f}
        }

        main = {} u {} -> { var fact {30} }
    };

    println!("{:?}", &program);

    pretty(&program, &mut io::stdout()).unwrap();
}
