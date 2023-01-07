use std::{ops::Deref, str::Chars};

use itertools::Itertools;

#[derive(Debug)]
pub enum BinaryOp {
    Add,
    Sub,
    Multi,
    Less,
}

#[derive(Debug)]
pub enum Token {
    Def,
    Extern,
    Identifier(String),
    Number(f64),
    LeftParenthesis,
    RightParenthesis,
    Comma,
    BinOp(BinaryOp),
}

pub struct Lexer<'a> {
    text: Chars<'a>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &str) -> Lexer {
        Lexer {
            text: input.chars(),
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let c = self.text.find(|x| !x.is_whitespace());
        match c {
            None => None,
            Some(c) if c.is_alphabetic() => {
                let mut s = String::from(c);
                s.push_str(
                    &self
                        .text
                        .peeking_take_while(|x| x.is_alphanumeric())
                        .collect::<String>(),
                );
                match s.deref() {
                    "def" => Some(Token::Def),
                    "extern" => Some(Token::Extern),
                    _ => Some(Token::Identifier(s)),
                }
            }
            Some(c) if c.is_numeric() || c == '.' => {
                let mut s = String::from(c);
                s.push_str(
                    &self
                        .text
                        .peeking_take_while(|x| x.is_numeric() || *x == '.')
                        .collect::<String>(),
                );
                let f: f64 = s.parse().unwrap_or_else(|e| {
                    panic!("{}: {}", e, s);
                });
                Some(Token::Number(f))
            }
            Some('#') => {
                self.text.find(|&x| x == '\n' || x == '\r');
                self.next()
            }
            Some(c) => match c {
                '(' => Some(Token::LeftParenthesis),
                ')' => Some(Token::RightParenthesis),
                ',' => Some(Token::Comma),
                '+' => Some(Token::BinOp(BinaryOp::Add)),
                '-' => Some(Token::BinOp(BinaryOp::Sub)),
                '*' => Some(Token::BinOp(BinaryOp::Multi)),
                '<' => Some(Token::BinOp(BinaryOp::Less)),
                c => panic!("invaild char: {}", c),
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new_lexer() {
        let s = "\
# Compute the x'th fibonacci number.
def fib(x)
    if x < 3 then
    1
    else
    fib(x-1)+fib(x-2)

# This expression will compute the 40th number.
fib(40)

(3.7+.9)*0.0 # hi
";
        let lexer = Lexer::new(s);
        for t in lexer {
            println!("{:?}", t);
        }
    }
}
