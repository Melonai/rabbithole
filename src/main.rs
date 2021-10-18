mod interpret;
mod lex;
mod parse;

use lex::lexer::Lexer;

use interpret::walker::Walker;
use parse::parser::Parser;

fn main() {
    let source = "1 * 2 + 3 + (-1)";

    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer);

    let node = parser.parse().expect("Failed parsing.");

    let walker = Walker::new();
    let result = walker.walk(&node).expect("Failed interpreting.");
    println!("{}", result);
}
