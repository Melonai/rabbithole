mod interpret;
mod lex;
mod parse;

use std::io::{self, Write};

use lex::lexer::Lexer;

use interpret::walker::Walker;
use parse::parser::Parser;

const PROMPT: &'static str = "🐇: ";

fn main() {
    let walker = Walker::new();

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

        let node = parser.parse().expect("Failed parsing.");
        let result = walker.walk(&node).expect("Failed interpreting.");

        println!("🥕: {}\n", result);
    }
}
