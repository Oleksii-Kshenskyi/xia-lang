use crate::parse_utils::*;

#[derive(Debug)]
pub enum ArithmeticOp {
    Add,
    Subtract,
    Multiply,
    Divide,
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
    Int(u64),
    ArithmeticOp(ArithmeticOp),
    Borked(String),
}

impl Token {
    pub fn from_int(int_str: &str) -> Token {
        Token::Int(int_str.parse::<u64>().unwrap())
    }
}

#[derive(Debug)]
pub struct Lexer {
    code: String,
    pos: usize,
}

impl Lexer {
    pub fn new(code: &str) -> Self {
        Self {
            code: code.to_owned(),
            pos: 0,
        }
    }

    pub fn lex(&mut self) -> Vec<Token> {
        let mut tokens: Vec<Token> = vec![];

        while self.pos < self.code.len() {
            tokens.push(self.next_token());
        }

        tokens
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespaces();

        let mut token_str = "".to_owned();
        while let Some(c) = self.code.chars().nth(self.pos) {
            if is_token_ender(self.code.chars().nth(self.pos - 1), &c) {
                break;
            }

            token_str.push(c);
            self.pos += 1;
        }

        let token = if token_str.chars().all(|c| c.is_digit(10)) {
            Token::from_int(&token_str)
        } else if is_arithmetic_op(&token_str) {
            Token::ArithmeticOp(ArithmeticOp::from_str(&token_str).unwrap())
        } else {
            Token::Borked(token_str)
        };

        self.skip_whitespaces();
        token
    }

    fn skip_whitespaces(&mut self) {
        while let Some(c) = self.code.chars().nth(self.pos) {
            if c.is_whitespace() {
                self.pos += 1;
            } else {
                return;
            }
        }
    }
}
