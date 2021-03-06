use std::io::{stdout, Write};

#[macro_use]
mod stg;
use stg::ast::*;
use stg::machine::*;
use stg::pretty::*;

mod extension;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let program = stg! {
        main = {} u {} -> {
            case { fact { 10 } } of
                n -> { Int# {n} }
        }

        fact = {} n {x} -> {
            case { x {} } of
                0 -> { 1 }
                _ -> {
                    case { sub# {x, 1} } of
                        y -> {
                            case { fact {y} } of
                                fy -> {
                                    case { mul# {x, fy} } of
                                        fx -> { traceInt# {fx} }
                                }
                        }
                }
        }
    };

    let mut stdout = stdout();

    pretty(&program, &mut stdout)?;
    stdout.flush().unwrap();

    run(&program)?;

    Ok(())
}
