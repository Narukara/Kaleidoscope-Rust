use std::iter::Peekable;

use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    values::{BasicMetadataValueEnum, FloatValue},
    FloatPredicate,
};
use lexer::{BinOp, Lexer, Token};

mod lexer;

pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(input: &str) -> Parser {
        Parser {
            lexer: Lexer::new(input).peekable(),
        }
    }

    fn get_bin_op_precedence(b: &BinOp) -> i32 {
        match b {
            BinOp::Add => 20,
            BinOp::Sub => 20,
            BinOp::Multi => 40,
            BinOp::Less => 10,
        }
    }

    // number_expr ::= number
    fn parse_number(&mut self) -> ExprAST {
        match self.lexer.next() {
            Some(Token::Num(f)) => ExprAST::Num(f),
            _ => unreachable!(),
        }
    }

    // parentheses_expr ::= '(' expr ')'
    fn parse_parentheses(&mut self) -> ExprAST {
        self.lexer.next(); // eat '('
        let e = self.parse_expr();
        match self.lexer.next() {
            Some(Token::RParen) => e,
            t => panic!("expected ')', got {:?}", t),
        }
    }

    // identifier_expr ::= id
    //                 ::= id '(' expr* ')'
    fn parse_identifier(&mut self) -> ExprAST {
        let name = match self.lexer.next() {
            Some(Token::Id(n)) => n,
            _ => unreachable!(),
        };

        if let Some(Token::LParen) = self.lexer.peek() {
            // function call
            self.lexer.next(); // eat '('
            let mut args = vec![];
            if let Some(Token::RParen) = self.lexer.peek() {
                self.lexer.next(); // eat ')'
            } else {
                loop {
                    args.push(self.parse_expr());
                    match self.lexer.next() {
                        Some(Token::RParen) => break,
                        Some(Token::Comma) => (),
                        t => panic!("expected ')' or ',', got {:?}", t),
                    }
                }
            }
            ExprAST::CallExpr { name, args }
        } else {
            // variable
            ExprAST::Var { name }
        }
    }

    // primary ::= number_expr
    //         ::= identifier_expr
    //         ::= parentheses_expr
    fn parse_primary(&mut self) -> ExprAST {
        match self.lexer.peek() {
            Some(Token::Id(_)) => self.parse_identifier(),
            Some(Token::LParen) => self.parse_parentheses(),
            Some(Token::Num(_)) => self.parse_number(),
            t => panic!("expected expression, got: {:?}", t),
        }
    }

    // bin_op_rhs ::= ('+' primary)*
    fn parse_bin_op_rhs(&mut self, base_prec: i32, mut lhs: ExprAST) -> ExprAST {
        loop {
            let op_prec = match self.lexer.peek() {
                Some(Token::BinOp(op)) => Self::get_bin_op_precedence(op),
                _ => -1,
            };
            if base_prec > op_prec {
                return lhs;
            }
            let op = match self.lexer.next() {
                Some(Token::BinOp(op)) => op,
                _ => unreachable!(),
            };

            let mut rhs = self.parse_primary();

            let next_prec = match self.lexer.peek() {
                Some(Token::BinOp(op)) => Self::get_bin_op_precedence(op),
                _ => -1,
            };

            if op_prec < next_prec {
                rhs = self.parse_bin_op_rhs(op_prec + 1, rhs);
            }

            lhs = ExprAST::BinExpr {
                lhs: Box::new(lhs),
                op,
                rhs: Box::new(rhs),
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
            Some(Token::Id(n)) => n,
            _ => unreachable!(),
        };

        match self.lexer.next() {
            Some(Token::LParen) => (),
            t => panic!("expected '(', got {:?}", t),
        }

        let mut args = vec![];
        match self.lexer.next() {
            Some(Token::RParen) => (),
            Some(Token::Id(id)) => {
                args.push(id);
                loop {
                    match self.lexer.next() {
                        Some(Token::RParen) => break,
                        Some(Token::Comma) => match self.lexer.next() {
                            Some(Token::Id(id)) => args.push(id),
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
        FunctionAST {
            proto,
            body: Some(body),
        }
    }

    // external ::= 'extern' prototype
    fn parse_extern(&mut self) -> FunctionAST {
        self.lexer.next(); // eat extern
        let proto = self.parse_prototype();
        FunctionAST { proto, body: None }
    }

    // top_level_expr ::= expr
    fn parse_top_level_expr(&mut self) -> FunctionAST {
        let proto = PrototypeAST {
            name: String::new(),
            args: vec![],
        };
        let body = self.parse_expr();
        FunctionAST {
            proto,
            body: Some(body),
        }
    }
}

impl<'a> Iterator for Parser<'a> {
    type Item = FunctionAST;

    fn next(&mut self) -> Option<Self::Item> {
        match self.lexer.peek() {
            Some(Token::Extern) => Some(self.parse_extern()),
            Some(Token::Def) => Some(self.parse_func()),
            Some(_) => Some(self.parse_top_level_expr()),
            None => None,
        }
    }
}

#[derive(Debug)]
enum ExprAST {
    Num(f64),
    Var {
        name: String,
    },
    BinExpr {
        lhs: Box<ExprAST>,
        op: BinOp,
        rhs: Box<ExprAST>,
    },
    CallExpr {
        name: String,
        args: Vec<ExprAST>,
    },
}

#[derive(Debug)]
struct PrototypeAST {
    name: String,
    args: Vec<String>,
}

#[derive(Debug)]
pub struct FunctionAST {
    proto: PrototypeAST,
    body: Option<ExprAST>,
}

impl ExprAST {
    fn codegen<'a>(
        &'a self,
        context: &'a Context,
        module: &Module<'a>,
        builder: &'a Builder,
        params: &Vec<FloatValue<'a>>,
    ) -> FloatValue {
        match self {
            Self::Num(n) => context.f64_type().const_float(*n),
            Self::Var { name } => *params
                .iter()
                .find(|&p| p.get_name().to_str().unwrap() == name)
                .unwrap_or_else(|| panic!("undefined variable: {}", name)),
            Self::BinExpr { lhs, op, rhs } => {
                let lhs = lhs.codegen(context, module, builder, params);
                let rhs = rhs.codegen(context, module, builder, params);
                match op {
                    BinOp::Add => builder.build_float_add(lhs, rhs, "sum"),
                    BinOp::Sub => builder.build_float_sub(lhs, rhs, "diff"),
                    BinOp::Multi => builder.build_float_mul(lhs, rhs, "prod"),
                    BinOp::Less => {
                        let temp =
                            builder.build_float_compare(FloatPredicate::ULT, lhs, rhs, "temp");
                        builder.build_unsigned_int_to_float(temp, context.f64_type(), "cmp")
                    }
                }
            }
            Self::CallExpr { name, args } => {
                let func = module
                    .get_function(name)
                    .unwrap_or_else(|| panic!("undefined function: {}", name));
                if func.count_params() as usize != args.len() {
                    panic!("the number of parameters does not match");
                }
                let args: Vec<BasicMetadataValueEnum> = args
                    .iter()
                    .map(|a| a.codegen(context, module, builder, params).into())
                    .collect();
                builder
                    .build_call(func, &args, "call")
                    .try_as_basic_value()
                    .left()
                    .unwrap()
                    .into_float_value()
            }
        }
    }
}

impl FunctionAST {
    pub fn codegen<'a>(&'a self, context: &'a Context, module: &Module<'a>, builder: &'a Builder) {
        let f64_t = context.f64_type();
        let fn_t = f64_t.fn_type(&vec![f64_t.into(); self.proto.args.len()], false);
        let func = module.add_function(&self.proto.name, fn_t, None);

        let params: Vec<FloatValue> = func
            .get_param_iter()
            .map(|p| p.into_float_value())
            .collect();
        for (i, p) in params.iter().enumerate() {
            p.set_name(&self.proto.args[i]);
        }

        if let Some(expr) = &self.body {
            let bb = context.append_basic_block(func, "entry");
            builder.position_at_end(bb);
            builder.build_return(Some(&expr.codegen(context, module, builder, &params)));
        }
    }
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
    fn parse_expr2() {
        let s = "1+2*(3+4)+5*6";
        let mut parser = Parser::new(s);
        println!("{:?}", parser.parse_expr());
    }

    #[test]
    #[should_panic]
    fn parse_expr3() {
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
        let s = "fun(a, (b))";
        let mut parser = Parser::new(s);
        println!("{:?}", parser.parse_prototype());
    }
}
