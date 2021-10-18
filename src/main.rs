mod lex;
mod parse;

use lex::lexer::Lexer;

use crate::parse::parser::Parser;

fn main() {
    let source = "1 * 2 + 3 + (-1)";

    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer);

    println!("{}", parser.parse().expect("Failed parsing."));
}
