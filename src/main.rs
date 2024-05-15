mod lexer;
mod parse_utils;
mod parser;
use lexer::Lexer;

use crate::parser::Parser;

// TODO: now write a parser and an evaluator that actually parse exprs recursively.
// TODO: after the above is done, add operator precedence so the arithmetic expressions are
//       evaluated mathematically correctly.
const CODE: &str = " ((())) )  3    +    355    -  2516818510    ( ";

fn main() {
    let lexed = Lexer::new(CODE).lex();
    println!("`{}` lexed : `{:?}`;\n", CODE, &lexed);
    let ast = Parser::new(lexed).parse();
    println!("...Parsed: `{:#?}`.", ast);
}
