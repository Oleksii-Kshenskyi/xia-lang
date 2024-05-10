mod lexer;
mod parse_utils;
use lexer::Lexer;

const CODE: &str = "3 + 3 - 2";

fn main() {
    println!("`{}` lexed : `{:?}`", CODE, Lexer::new(CODE).lex());
}
