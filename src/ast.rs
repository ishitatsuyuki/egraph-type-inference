// Copyright 2021 Tatsuyuki Ishi <ishitatsuyuki@gmail.com>
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// https://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or https://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use std::borrow::Cow;
use std::fmt::{Display, Formatter};

use logos::{Lexer, Logos};

#[derive(Logos, Clone, Debug, PartialEq)]
pub enum Token {
    #[token("lambda")]
    #[token("\\lambda")]
    #[token("λ")]
    Lambda,
    #[token(":")]
    Comma,
    #[token(".")]
    Period,
    #[token("->")]
    Arrow,

    #[token("(")]
    LeftParen,
    #[token(")")]
    RightParen,

    #[regex("['_a-zA-Z][_a-zA-Z0-9]*")]
    Identifier,

    #[error]
    #[regex(r"[ \t\n\f]+", logos::skip)]
    Error,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Expression<'ast> {
    Lambda {
        arg: &'ast str,
        arg_ty: Option<Type<'ast>>,
        body: Box<Expression<'ast>>,
    },
    Variable(&'ast str),
    Application(Box<Expression<'ast>>, Box<Expression<'ast>>),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Type<'ast> {
    Name(Cow<'ast, str>),
    Function(Box<Type<'ast>>, Box<Type<'ast>>),
}

impl<'ast> Display for Expression<'ast> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Lambda { arg, arg_ty, body } => {
                if let Some(arg_ty) = arg_ty {
                    write!(f, "(lambda {}: {}.{})", arg, arg_ty, body)
                } else {
                    write!(f, "(lambda {}.{})", arg, body)
                }
            }
            Expression::Variable(x) => {
                write!(f, "{}", x)
            }
            Expression::Application(a, b) => {
                write!(f, "({} {})", a, b)
            }
        }
    }
}

impl<'ast> Display for Type<'ast> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Name(x) => {
                write!(f, "{}", x)
            }
            Type::Function(arg, ret) => {
                write!(f, "({} -> {})", arg, ret)
            }
        }
    }
}

/// A recursive-descent lambda expression parser.
pub fn parse(input: &str) -> Expression {
    let mut lex: Lexer<Token> = Token::lexer(input);
    let res = exp(&mut lex, true);
    assert_eq!(lex.next(), None);
    res
}

fn exp<'ast>(lex: &mut Lexer<'ast, Token>, parse_application: bool) -> Expression<'ast> {
    let next = lex.clone().next();
    if let Some(next) = next {
        let mut lhs = match next {
            Token::Lambda => lambda(lex),
            Token::LeftParen => {
                lex.next().unwrap();
                let res = exp(lex, true);
                assert_eq!(lex.next(), Some(Token::RightParen));
                res
            }
            Token::Identifier => {
                lex.next().unwrap();
                Expression::Variable(lex.slice())
            }
            _ => panic!("Unexpected token"),
        };
        loop {
            match lex.clone().next() {
                None | Some(Token::RightParen) => break,
                _ => if parse_application {
                    lhs = Expression::Application(Box::new(lhs), Box::new(exp(lex, false)))
                } else {
                    break;
                }
            }
        }
        lhs
    } else {
        panic!("Unexpected EOF");
    }
}

fn lambda<'ast>(lex: &mut Lexer<'ast, Token>) -> Expression<'ast> {
    let next = lex.next().unwrap();
    assert_eq!(next, Token::Lambda);
    let binding = lex.next().unwrap();
    assert_eq!(binding, Token::Identifier);
    let arg = lex.slice();
    let ty = if lex.clone().next() == Some(Token::Comma) {
        lex.next().unwrap();
        let ty = ty(lex);
        Some(ty)
    } else {
        None
    };
    let dot = lex.next().unwrap();
    assert_eq!(dot, Token::Period);

    Expression::Lambda {
        arg,
        arg_ty: ty,
        body: Box::new(exp(lex, true)),
    }
}

fn ty<'ast>(lex: &mut Lexer<'ast, Token>) -> Type<'ast> {
    let next = lex.clone().next();
    if let Some(next) = next {
        let mut lhs = match next {
            Token::LeftParen => {
                lex.next().unwrap();
                let res = ty(lex);
                assert_eq!(lex.next(), Some(Token::RightParen));
                res
            }
            Token::Identifier => {
                lex.next().unwrap();
                Type::Name(lex.slice().into())
            }
            _ => panic!("Unexpected token"),
        };
        loop {
            match lex.clone().next() {
                Some(Token::Arrow) => {
                    lex.next().unwrap();
                    lhs = Type::Function(Box::new(lhs), Box::new(ty(lex)))
                }
                _ => break,
            }
        }
        lhs
    } else {
        panic!("Unexpected EOF");
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::Expression::*;
    use crate::ast::parse;
    use crate::ast::Type::*;

    #[test]
    fn simple() {
        let res = parse("lambda x:A.lambda y:B.x y");
        assert_eq!(res, Lambda {
            arg: "x",
            arg_ty: Some(
                Name(
                    "A".into(),
                ),
            ),
            body: Box::new(Lambda {
                arg: "y",
                arg_ty: Some(
                    Name(
                        "B".into(),
                    ),
                ),
                body: Box::new(Application(
                    Box::new(Variable(
                        "x",
                    )),
                    Box::new(Variable(
                        "y",
                    )),
                )),
            }),
        })
    }

    #[test]
    fn unicode_lambda() {
        let res = parse("(λf. λx. f (f x)) (λf. λx. f (f x))");
        assert_eq!(res, Application(
            Box::new(Lambda {
                arg: "f",
                arg_ty: None,
                body: Box::new(Lambda {
                    arg: "x",
                    arg_ty: None,
                    body: Box::new(Application(
                        Box::new(Variable(
                            "f",
                        )),
                        Box::new(Application(
                            Box::new(Variable(
                                "f",
                            )),
                            Box::new(Variable(
                                "x",
                            )),
                        )),
                    )),
                }),
            }),
            Box::new(Lambda {
                arg: "f",
                arg_ty: None,
                body: Box::new(Lambda {
                    arg: "x",
                    arg_ty: None,
                    body: Box::new(Application(
                        Box::new(Variable(
                            "f",
                        )),
                        Box::new(Application(
                            Box::new(Variable(
                                "f",
                            )),
                            Box::new(Variable(
                                "x",
                            )),
                        )),
                    )),
                }),
            }),
        )
        )
    }

    #[test]
    fn typed_lambda() {
        let res = parse("lambda x: ( ( ( (A->B)->C ) -> (A->C) -> C) -> B) -> B.x");
        assert_eq!(res, Lambda {
            arg: "x",
            arg_ty: Some(
                Function(
                    Box::new(Function(
                        Box::new(Function(
                            Box::new(Function(
                                Box::new(Function(
                                    Box::new(Name(
                                        "A".into(),
                                    )),
                                    Box::new(Name(
                                        "B".into(),
                                    )),
                                )),
                                Box::new(Name(
                                    "C".into(),
                                )),
                            )),
                            Box::new(Function(
                                Box::new(Function(
                                    Box::new(Name(
                                        "A".into(),
                                    )),
                                    Box::new(Name(
                                        "C".into(),
                                    )),
                                )),
                                Box::new(Name(
                                    "C".into(),
                                )),
                            )),
                        )),
                        Box::new(Name(
                            "B".into(),
                        )),
                    )),
                    Box::new(Name(
                        "B".into(),
                    )),
                ),
            ),
            body: Box::new(Variable(
                "x",
            )),
        }
        )
    }
}
