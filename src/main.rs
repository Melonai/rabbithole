mod lex;

use lex::lexer::Lexer;

fn main() {
    let source = "1 + 2";
    let lexer = Lexer::new(source);

    for token in lexer {
        println!("{:?}", token);
    }
}
