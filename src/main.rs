use std::{env, fs, path::Path, process::exit};

mod error;
mod interpret;
mod lex;
mod parse;
mod types;

use error::RHError;
use lex::lexer::Lexer;

use interpret::walker::Walker;
use parse::parser::Parser;
use std::io::{self, Write};

fn main() {
    let args: Vec<String> = env::args().collect();

    if let Some(filename) = args.get(1) {
        file(filename)
    } else {
        repl()
    }
}

fn file(filename: impl AsRef<Path>) {
    let contents = fs::read_to_string(filename).expect("Failed reading file.");

    let lexer = Lexer::new(&contents);
    let mut parser = Parser::new(lexer);

    let node = handle_error(parser.parse(), &contents);
    let mut walker = Walker::root();

    handle_error(walker.walk(&node), &contents);
}

fn repl() {
    const PROMPT: &str = "🐇: ";

    let mut walker = Walker::root();

    let mut input_buffer;

    loop {
        print!("{}", PROMPT);
        io::stdout().flush().expect("Failed flushing.");

        input_buffer = String::new();
        io::stdin()
            .read_line(&mut input_buffer)
            .expect("Failed reading input.");

        let lexer = Lexer::new(input_buffer.trim());
        let mut parser = Parser::new(lexer);

        let node = handle_error(parser.expression(), &input_buffer);
        let result = handle_error(walker.walk_expression(&node), &input_buffer);

        println!("🥕: {:?}\n", result);
    }
}

fn handle_error<T>(result: Result<T, RHError>, source: &str) -> T {
    match result {
        Ok(x) => x,
        Err(error) => {
            print!("{}", error.hydrate_source(&source));
            exit(1);
        }
    }
}
