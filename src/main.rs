use std::io::{stdout, Write};

#[macro_use]
mod stg;
use stg::ast::*;
use stg::machine::*;
use stg::pretty::*;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let program = stg! {
        main = {} u {} -> { var fact {10} }

        fact = {} n {n} -> {
            case {var n {}} of
                0 -> { 1 }
                default -> {
                    case {prim sub_ {n, 1}} of
                        m -> {
                            case {var fact {m}} of
                                fm -> { prim mul_ {n, fm} }
                        }
                }
        }
    };
    /*
    let program = stg! {
        main = {} u {} -> { var fact {10} }

        fix = {} n {f} -> {
            let rec {
                x = {f, x} u {} -> { var f {x} }
                y = {} u {} -> { 0 }
            } in
                x {}
        }

        fact = {} n {n} -> {
            case {var n {}} of
                0 -> { 1 }
                default -> {
                    case {prim sub_ {n, 1}} of
                        m -> {
                            case {var fact {m}} of
                                fm -> { prim mul_ {n, fm} }
                        }
                }
        }
    };
     */

    let mut stdout = stdout();
    pretty(&program, &mut stdout)?;
    stdout.flush().unwrap();

    let mut state = create_init_state(&program)?;
    run(&mut state)?;
    Ok(())
}
