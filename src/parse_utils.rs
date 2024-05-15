#[derive(Debug, Clone, Copy)]
pub enum ArithmeticOp {
    Add,
    Subtract,
    Multiply,
    Divide,
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOp {
    Plus,
    Minus,
}

impl ArithmeticOp {
    pub fn from_str(op: &str) -> Option<Self> {
        match op {
            "+" => Some(ArithmeticOp::Add),
            "-" => Some(ArithmeticOp::Subtract),
            "*" => Some(ArithmeticOp::Multiply),
            "/" => Some(ArithmeticOp::Divide),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub enum Token {
    OpenParen,
    CloseParen,
    Int(i64),
    ArithmeticOp(ArithmeticOp),
    Borked(String),
}

impl Token {
    pub fn from_int(int_str: &str) -> Token {
        Token::Int(int_str.parse::<i64>().unwrap())
    }
}

#[derive(Debug, Clone)]
pub enum AstNode {
    IntLiteral(i64),
    BinaryOp(Box<AstNode>, ArithmeticOp, Box<AstNode>),
    UnaryOp(UnaryOp, Box<AstNode>),
    ParenthesizedExpression(Box<AstNode>),
}

pub fn is_paren(c: &char) -> bool {
    *c == '(' || *c == ')'
}

pub fn paren_token(c: &char) -> Token {
    match *c {
        '(' => Token::OpenParen,
        ')' => Token::CloseParen,
        _ => Token::Borked(c.to_string()),
    }
}

pub fn is_arithmetic_op(c: &str) -> bool {
    c.len() == 1 && "+-*/".contains(c)
}
