mod lexer;
mod parse_utils;
use lexer::Lexer;

const CODE: &str = " ((())) )  3    +    355    -  2516818510    ( ";

fn main() {
    println!("`{}` lexed : `{:?}`", CODE, Lexer::new(CODE).lex());
}
