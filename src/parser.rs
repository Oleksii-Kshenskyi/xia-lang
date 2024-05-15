use crate::lexer::Lexer;
use crate::parse_utils::*;

#[derive(Debug)]
pub struct Parser {
    tokens: Vec<Token>,
    index: usize,
}

impl Parser {
    pub fn new(lexed: Vec<Token>) -> Self {
        Self {
            tokens: lexed,
            index: 0,
        }
    }

    pub fn parse(&mut self) -> AstNode {
        AstNode::IntLiteral(3)
    }
}
