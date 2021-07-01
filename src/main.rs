use std::io::{stdout, Write};

#[macro_use]
mod stg;
use stg::ast::*;
use stg::machine::*;
use stg::pretty::*;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let program = stg! {
        main = {} u {} -> { fact {10} }

        fix = {} n {f} -> {
            let rec {
                x = {f, x} u {} -> { f {x} }
            } in
                x {}
        }

        length = {} n {xs} -> {
            case {xs {}} of
                :Cons {y,ys} -> {
                    case {length {ys}} of
                        n -> { add# {n, 1} }
                }
                _ -> { 0 }
        }

        fact = {} n {n} -> {
            case {n {}} of
                0 -> { 1 }
                _ -> {
                    case {sub# {n, 1}} of
                        m -> {
                            case {fact {m}} of
                                fm -> { mul# {n, fm} }
                        }
                }
        }
    };

    let mut stdout = stdout();
    pretty(&program, &mut stdout)?;
    stdout.flush().unwrap();

    let mut state = create_init_state(&program)?;
    run(&mut state)?;
    Ok(())
}
