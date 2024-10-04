use crate::parsing::parser::parse;
use clap::*;

mod parsing;
mod interpreter;
mod c_backend;
mod wasm_backend;

#[derive(clap::Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    #[arg(short, long, default_value = "interpreter")]
    backend: String,

    #[arg(short, long)]
    run: bool,

    #[arg(short, long)]
    repl: bool,

    #[arg(short, long)]
    input_file: Option<String>,

    #[arg(short, long)]
    output_file: Option<String>,
}

fn main() {
    let args = Args::parse();

    if args.repl {

    }
    else if args.run {

    }
    else {
        match args.backend.as_str() {
            "interpreter" | "lia" | "i" => {
                todo!();
            }
            "wasm" => {
                todo!();
            }
            "c" => {

            }
            _ => {
                todo!()
            }
        }
    }

    let input = "
(use sys print)
(val x 0b01)
(val b 'b')
(val c 0qZZ)
(val y 2)
(+ x y)
    (* @ 2.)
    (+ @ 0x03)
    (- 0 @)
(print @) ; prints -9.0
(print 'b')

".to_string();
    match parse(input) {
        Ok(ast) => println!("{:#?}", ast),
        Err(e) => eprintln!("Error: {}", e),
    }
}
