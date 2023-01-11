use std::{collections::HashSet, iter::Peekable};

use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    values::{BasicMetadataValueEnum, FloatValue},
    FloatPredicate,
};
use lexer::{BinOp, Lexer, Token};

mod lexer;

pub const TARGET_FUNC_NAME: &str = "_find";

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
            BinOp::Less => 10,
            BinOp::Add => 20,
            BinOp::Sub => 20,
            BinOp::Multi => 40,
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
            t => panic!("expected ')', got '{:?}'", t),
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
                        t => panic!("expected ')' or ',', got '{:?}'", t),
                    }
                }
            }
            ExprAST::CallExpr { name, args }
        } else {
            // variable
            ExprAST::Var { name }
        }
    }

    // if_expr ::= 'if' expr 'then' expr 'else' expr
    fn parse_if(&mut self) -> ExprAST {
        self.lexer.next(); // eat 'if'
        let cond = Box::new(self.parse_expr());
        match self.lexer.next() {
            Some(Token::Then) => (),
            t => panic!("expected 'then', got '{:?}'", t),
        }
        let then = Box::new(self.parse_expr());
        match self.lexer.next() {
            Some(Token::Else) => (),
            t => panic!("expected 'else', got '{:?}'", t),
        }
        let els = Box::new(self.parse_expr());
        ExprAST::IfExpr { cond, then, els }
    }

    // primary ::= number_expr
    //         ::= identifier_expr
    //         ::= parentheses_expr
    fn parse_primary(&mut self) -> ExprAST {
        match self.lexer.peek() {
            Some(Token::Id(_)) => self.parse_identifier(),
            Some(Token::LParen) => self.parse_parentheses(),
            Some(Token::Num(_)) => self.parse_number(),
            Some(Token::If) => self.parse_if(),
            t => panic!("expected expression, got: '{:?}'", t),
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
        // get function name
        let name = match self.lexer.next() {
            Some(Token::Id(n)) => n,
            t => panic!("expected identifier, got '{:?}'", t),
        };

        // eat (
        match self.lexer.next() {
            Some(Token::LParen) => (),
            t => panic!("expected '(', got '{:?}'", t),
        }

        // get parameters
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
                            t => panic!("expected identifier, got '{:?}'", t),
                        },
                        t => panic!("expected ')' or ',', got '{:?}'", t),
                    }
                }
            }
            t => panic!("expected ')' or identifier, got '{:?}'", t),
        }

        // check for duplicate parameters
        let mut set = HashSet::new();
        if !args.iter().all(|a| set.insert(a)) {
            panic!("duplicate parameters are not allowed: function '{}'", name);
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

    // target ::= 'find' expr
    fn parse_target(&mut self) -> FunctionAST {
        self.lexer.next(); // eat find
        let proto = PrototypeAST {
            name: String::from(TARGET_FUNC_NAME),
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
            Some(Token::Find) => Some(self.parse_target()),
            Some(t) => panic!("expect 'extern', 'def' or 'find', got '{:?}'", t),
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
    IfExpr {
        cond: Box<ExprAST>,
        then: Box<ExprAST>,
        els: Box<ExprAST>,
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
    fn codegen_num(num: f64, context: &Context) -> FloatValue {
        context.f64_type().const_float(num)
    }

    fn codegen_var<'a>(name: &str, params: &Vec<FloatValue<'a>>) -> FloatValue<'a> {
        *params
            .iter()
            .find(|&p| p.get_name().to_str().unwrap() == name)
            .unwrap_or_else(|| panic!("undefined variable: '{}'", name))
    }

    fn codegen_bin<'a>(
        bin_expr: &'a ExprAST,
        context: &'a Context,
        module: &Module<'a>,
        builder: &'a Builder,
        params: &Vec<FloatValue<'a>>,
    ) -> FloatValue<'a> {
        let (lhs, op, rhs) = match bin_expr {
            ExprAST::BinExpr { lhs, op, rhs } => (lhs, op, rhs),
            _ => unreachable!(),
        };
        let lhs = lhs.codegen(context, module, builder, params);
        let rhs = rhs.codegen(context, module, builder, params);
        match op {
            BinOp::Add => builder.build_float_add(lhs, rhs, "sum"),
            BinOp::Sub => builder.build_float_sub(lhs, rhs, "diff"),
            BinOp::Multi => builder.build_float_mul(lhs, rhs, "prod"),
            BinOp::Less => {
                let temp = builder.build_float_compare(FloatPredicate::ULT, lhs, rhs, "temp");
                builder.build_unsigned_int_to_float(temp, context.f64_type(), "cmp")
            }
        }
    }

    fn codegen_call<'a>(
        call_expr: &'a ExprAST,
        context: &'a Context,
        module: &Module<'a>,
        builder: &'a Builder,
        params: &Vec<FloatValue<'a>>,
    ) -> FloatValue<'a> {
        let (name, args) = match call_expr {
            Self::CallExpr { name, args } => (name, args),
            _ => unreachable!(),
        };
        let func = module
            .get_function(name)
            .unwrap_or_else(|| panic!("undefined function '{}'", name));
        if func.count_params() as usize != args.len() {
            panic!("number of parameters does not match: function '{}'", name);
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

    fn codegen_if<'a>(
        if_expr: &'a ExprAST,
        context: &'a Context,
        module: &Module<'a>,
        builder: &'a Builder,
        params: &Vec<FloatValue<'a>>,
    ) -> FloatValue<'a> {
        let (cond, then, els) = match if_expr {
            ExprAST::IfExpr { cond, then, els } => (cond, then, els),
            _ => unreachable!(),
        };
        let cond = cond.codegen(context, module, builder, params);
        let cond = builder.build_float_compare(
            FloatPredicate::ONE,
            cond,
            context.f64_type().const_zero(),
            "if_cond",
        );

        let func = builder.get_insert_block().unwrap().get_parent().unwrap();
        let then_bb = context.append_basic_block(func, "then");
        let else_bb = context.append_basic_block(func, "else");
        let merge_bb = context.append_basic_block(func, "merge");
        builder.build_conditional_branch(cond, then_bb, else_bb);

        builder.position_at_end(then_bb);
        let then = then.codegen(context, module, builder, params);
        builder.build_unconditional_branch(merge_bb);
        let then_bb = builder.get_insert_block().unwrap();

        builder.position_at_end(else_bb);
        let els = els.codegen(context, module, builder, params);
        builder.build_unconditional_branch(merge_bb);
        let else_bb = builder.get_insert_block().unwrap();

        builder.position_at_end(merge_bb);
        let phi = builder.build_phi(context.f64_type(), "if_merge");
        phi.add_incoming(&[(&then, then_bb), (&els, else_bb)]);
        phi.as_basic_value().into_float_value()
    }

    fn codegen<'a>(
        &'a self,
        context: &'a Context,
        module: &Module<'a>,
        builder: &'a Builder,
        params: &Vec<FloatValue<'a>>,
    ) -> FloatValue {
        match self {
            Self::Num(n) => ExprAST::codegen_num(*n, context),
            Self::Var { name } => ExprAST::codegen_var(name, params),
            bin_expr @ Self::BinExpr { .. } => {
                ExprAST::codegen_bin(bin_expr, context, module, builder, params)
            }
            call_expr @ Self::CallExpr { .. } => {
                ExprAST::codegen_call(call_expr, context, module, builder, params)
            }
            if_expr @ Self::IfExpr { .. } => {
                ExprAST::codegen_if(if_expr, context, module, builder, params)
            }
        }
    }
}

impl FunctionAST {
    pub fn codegen<'a>(&'a self, context: &'a Context, module: &Module<'a>, builder: &'a Builder) {
        if module.get_function(&self.proto.name).is_some() {
            panic!("duplicate definition of function: {}", self.proto.name);
        }
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

    #[test]
    #[should_panic]
    fn parse_expr4() {
        let s = "if 1 then 2";
        let mut parser = Parser::new(s);
        println!("{:?}", parser.parse_expr());
    }

    #[test]
    #[should_panic]
    fn parse_expr5() {
        let s = "if 1 2";
        let mut parser = Parser::new(s);
        println!("{:?}", parser.parse_expr());
    }
}
