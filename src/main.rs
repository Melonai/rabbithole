use std::{env, fs, path::Path};

mod interpret;
mod lex;
mod parse;

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

    let node = parser.parse().expect("Failed parsing.");
    let mut walker = Walker::new();

    walker.walk(&node);
}

fn repl() {
    const PROMPT: &'static str = "🐇: ";

    let mut walker = Walker::new();

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

        let node = parser.expression().expect("Failed parsing.");
        let result = walker.walk_expression(&node).expect("Failed interpreting.");

        println!("🥕: {:?}\n", result);
    }
}
