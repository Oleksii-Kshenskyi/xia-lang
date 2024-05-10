const TOKEN_STOPPERS: &str = "";

pub fn is_token_ender(prev_c: Option<char>, c: &char) -> bool {
    let is_arithmetic_stopper = match prev_c {
        Some(ac) => !ac.is_whitespace(),
        None => false,
    } && is_arithmetic_op(&c.to_string());

    c.is_whitespace() || is_arithmetic_stopper || TOKEN_STOPPERS.contains(*c)
}

pub fn is_arithmetic_op(c: &str) -> bool {
    c.len() == 1 && "+-*/".contains(c)
}
