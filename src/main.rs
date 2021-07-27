// Copyright 2021 Tatsuyuki Ishi <ishitatsuyuki@gmail.com>
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// https://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or https://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use rustyline::error::ReadlineError;
use rustyline::Editor;

mod ast;
mod infer;

fn main() {
    let mut rl = Editor::<()>::new();
    loop {
        let readline = rl.readline("> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                let _ = std::panic::catch_unwind(|| {
                    println!("{}", infer::infer(&ast::parse(&line)));
                });
            },
            Err(ReadlineError::Interrupted) => {
                continue;
            },
            Err(ReadlineError::Eof) => {
                return;
            },
            Err(err) => {
                println!("Readline error: {:?}", err);
                break;
            }
        }
    }
}
