use std::io::{stdout, Write};

#[macro_use]
mod stg;
use stg::ast::*;
use stg::machine::*;
use stg::pretty::*;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let program = stg! {
        main = {} u {} -> { fact {10} }

        fact = {} n {x} -> {
            case {x {}} of
                0 -> { 1 }
                _ -> {
                    case {sub# {x, 1}} of
                        y -> {
                            case {fact {y}} of
                                fy -> {
                                    case {mul# {x, fy}} of
                                        fx -> { traceInt# {fx} }
                                }
                        }
                }
        }
    };

    let mut stdout = stdout();

    pretty(&program, &mut stdout)?;
    stdout.flush().unwrap();

    let mut machine = Machine { out: &mut stdout };
    machine.run(&program)?;

    Ok(())
}
