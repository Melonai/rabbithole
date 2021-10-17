mod lex;
mod parse;

use lex::lexer::Lexer;

use crate::parse::parse;

fn main() {
    let source = "1 + 2";
    let lexer = Lexer::new(source);

    println!("{:?}", parse(lexer.peekable()));
}
