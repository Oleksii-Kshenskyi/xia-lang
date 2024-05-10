use crate::parse_utils::*;

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

        let token = if let Some(int_tok) = self.try_get_int() {
            int_tok
        } else if let Some(binary_op) = self.try_get_binary_op() {
            binary_op
        } else if let Some(paren) = self.try_get_paren() {
            paren
        } else {
            self.get_borked()
        };

        self.skip_whitespaces();
        token
    }

    fn consume_digit(&mut self) -> Option<char> {
        let maybe_char = self.code.chars().nth(self.pos);
        let maybe_digit = match maybe_char {
            None => None,
            Some(c) => match c.is_digit(10) {
                true => Some(c),
                false => None,
            },
        };
        match maybe_digit {
            None => return None,
            Some(d) => {
                self.pos += 1;
                return Some(d);
            }
        }
    }
    fn try_get_int(&mut self) -> Option<Token> {
        let mut num_str = "".to_owned();
        while let Some(digit) = self.consume_digit() {
            num_str.push(digit);
        }

        match num_str.is_empty() {
            true => None,
            false => Some(Token::from_int(&num_str)),
        }
    }

    fn try_get_binary_op(&mut self) -> Option<Token> {
        let maybe_char = self.code.chars().nth(self.pos);
        match maybe_char {
            Some(c) => match is_arithmetic_op(&c.to_string()) {
                true => ArithmeticOp::from_str(&c.to_string()).map(|o| {
                    self.pos += 1;
                    Token::ArithmeticOp(o)
                }),
                false => None,
            },
            None => None,
        }
    }

    fn get_borked(&mut self) -> Token {
        let token_str = self
            .code
            .chars()
            .take_while(|c| !c.is_whitespace())
            .collect::<String>();
        self.pos += token_str.len();

        Token::Borked(token_str)
    }

    fn try_get_paren(&mut self) -> Option<Token> {
        let maybe_char = self.code.chars().nth(self.pos);
        match maybe_char {
            None => None,
            Some(c) => match is_paren(&c) {
                false => None,
                true => {
                    self.pos += 1;
                    Some(paren_token(&c))
                }
            },
        }
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
