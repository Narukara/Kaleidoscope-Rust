use std::iter::Peekable;

mod lexer;

use lexer::{BinaryOp, Lexer, Token};

pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(input: &str) -> Parser {
        Parser {
            lexer: Lexer::new(input).peekable(),
        }
    }

    fn get_binop_precedence(b: &BinaryOp) -> i32 {
        match b {
            BinaryOp::Add => 20,
            BinaryOp::Sub => 20,
            BinaryOp::Multi => 40,
            BinaryOp::Less => 10,
        }
    }

    // number_expr ::= number
    fn parse_number(&mut self) -> ExprAST {
        match self.lexer.next() {
            Some(Token::Number(f)) => ExprAST::Number(f),
            _ => unreachable!(),
        }
    }

    // parentheses_expr ::= '(' expr ')'
    fn parse_parentheses(&mut self) -> ExprAST {
        self.lexer.next(); // eat '('
        let e = self.parse_expr();
        match self.lexer.next() {
            Some(Token::RightParenthesis) => e,
            t => panic!("expected ')', got {:?}", t),
        }
    }

    // identifier_expr ::= id
    //                 ::= id '(' expr* ')'
    fn parse_identifier(&mut self) -> ExprAST {
        let name = match self.lexer.next() {
            Some(Token::Identifier(n)) => n,
            _ => unreachable!(),
        };

        if let Some(Token::LeftParenthesis) = self.lexer.peek() {
            // function call
            self.lexer.next(); // eat '('
            let mut args = vec![];
            if let Some(Token::RightParenthesis) = self.lexer.peek() {
                self.lexer.next(); // eat ')'
            } else {
                loop {
                    args.push(self.parse_expr());
                    match self.lexer.next() {
                        Some(Token::RightParenthesis) => break,
                        Some(Token::Comma) => (),
                        t => panic!("expected ')' or ',', got {:?}", t),
                    }
                }
            }
            ExprAST::CallExpr { func: name, args }
        } else {
            // variable
            ExprAST::Variable { name }
        }
    }

    // primary ::= number_expr
    //         ::= identifier_expr
    //         ::= parentheses_expr
    fn parse_primary(&mut self) -> ExprAST {
        match self.lexer.peek() {
            Some(Token::Identifier(_)) => self.parse_identifier(),
            Some(Token::LeftParenthesis) => self.parse_parentheses(),
            Some(Token::Number(_)) => self.parse_number(),
            t => panic!("expected expression, got: {:?}", t),
        }
    }

    // bin_op_rhs ::= ('+' primary)*
    fn parse_bin_op_rhs(&mut self, expr_prec: i32, mut lhs: ExprAST) -> ExprAST {
        loop {
            let op_prec = match self.lexer.peek() {
                Some(Token::BinOp(op)) => Self::get_binop_precedence(op),
                _ => -1,
            };
            if op_prec < expr_prec {
                return lhs;
            }
            let op = match self.lexer.next() {
                Some(Token::BinOp(op)) => op,
                _ => unreachable!(),
            };

            let mut rhs = self.parse_primary();

            let next_prec = match self.lexer.peek() {
                Some(Token::BinOp(op)) => Self::get_binop_precedence(op),
                _ => -1,
            };

            if op_prec < next_prec {
                rhs = self.parse_bin_op_rhs(op_prec + 1, rhs);
            }

            lhs = ExprAST::BinaryExpr {
                left: Box::new(lhs),
                op,
                right: Box::new(rhs),
            };
        }
    }

    // expr ::= primary bin_op_rhs
    fn parse_expr(&mut self) -> ExprAST {
        let lhs = self.parse_primary();
        self.parse_bin_op_rhs(0, lhs)
    }

    // prototype ::= id '(' id* ')'
    fn parse_prototype(&mut self) -> PrototypeAST {
        let name = match self.lexer.next() {
            Some(Token::Identifier(n)) => n,
            _ => unreachable!(),
        };

        match self.lexer.next() {
            Some(Token::LeftParenthesis) => (),
            t => panic!("expected '(', got {:?}", t),
        }

        let mut args = vec![];
        match self.lexer.next() {
            Some(Token::RightParenthesis) => (),
            Some(Token::Identifier(id)) => {
                args.push(id);
                loop {
                    match self.lexer.next() {
                        Some(Token::RightParenthesis) => break,
                        Some(Token::Comma) => match self.lexer.next() {
                            Some(Token::Identifier(id)) => args.push(id),
                            t => panic!("expected identifier, got {:?}", t),
                        },
                        t => panic!("expected ')' or ',', got {:?}", t),
                    }
                }
            }
            t => panic!("expected ')' or identifier, got {:?}", t),
        }

        PrototypeAST { name, args }
    }

    // function ::= 'def' prototype expr
    fn parse_func(&mut self) -> FunctionAST {
        self.lexer.next(); // eat def
        let proto = self.parse_prototype();
        let body = self.parse_expr();
        FunctionAST { proto, body }
    }

    // external ::= 'extern' prototype
    fn parse_extern(&mut self) -> PrototypeAST {
        self.lexer.next(); // eat extern
        self.parse_prototype()
    }

    // top_level_expr ::= expr
    fn parse_top_level_expr(&mut self) -> FunctionAST {
        let proto = PrototypeAST {
            name: String::new(),
            args: vec![],
        };
        let body = self.parse_expr();
        FunctionAST { proto, body }
    }

    pub fn parse(&mut self) {
        loop {
            match self.lexer.peek() {
                Some(Token::Extern) => println!("{:?}", self.parse_extern()),
                Some(Token::Def) => println!("{:?}", self.parse_func()),
                None => break,
                Some(_) => println!("{:?}", self.parse_top_level_expr()),
            }
        }
    }
}

#[derive(Debug)]
enum ExprAST {
    Number(f64),
    Variable {
        name: String,
    },
    BinaryExpr {
        left: Box<ExprAST>,
        op: BinaryOp,
        right: Box<ExprAST>,
    },
    CallExpr {
        func: String,
        args: Vec<ExprAST>,
    },
}

#[derive(Debug)]
struct PrototypeAST {
    name: String,
    args: Vec<String>,
}

#[derive(Debug)]
struct FunctionAST {
    proto: PrototypeAST,
    body: ExprAST,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_expr1() {
        let s = "foo(1+a, bar()) * (b+fun(j))";
        let mut parser = Parser::new(s);
        println!("{:?}", parser.parse_expr());
    }

    #[test]
    #[should_panic]
    fn parse_expr2() {
        let s = "9+fun(1*)";
        let mut parser = Parser::new(s);
        println!("{:?}", parser.parse_expr());
    }

    #[test]
    fn parse_proto1() {
        let s = "fun(a,ffff)";
        let mut parser = Parser::new(s);
        println!("{:?}", parser.parse_prototype());
    }

    #[test]
    #[should_panic]
    fn parse_proto2() {
        let s = "fun(1)";
        let mut parser = Parser::new(s);
        println!("{:?}", parser.parse_prototype());
    }
}
