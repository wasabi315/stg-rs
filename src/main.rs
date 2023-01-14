use std::io::stdout;

use anyhow::Result;

#[macro_use]
mod stg;
use stg::ast::*;
use stg::machine::*;
use stg::pretty::*;

mod extension;

fn main() -> Result<()> {
    let program = stg! {
        main = {} u {} -> {
            case { fact { 10 } } of
                n -> { Int # {n} }
        }

        fact = {} n {x} -> {
            case { x {} } of
                0 -> { 1 }
                _ -> {
                    case { sub # {x, 1} } of
                        y -> {
                            case { fact {y} } of
                                fy -> {
                                    case { mul # {x, fy} } of
                                        fx -> { traceInt # {fx} }
                                }
                        }
                }
        }
    };

    pretty(&program, &mut stdout().lock())?;

    run(&program)?;

    Ok(())
}
