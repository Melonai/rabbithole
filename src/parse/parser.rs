use crate::lex::token::Token;
use crate::lex::token::TokenVariant;
use crate::lex::token::TokenVariant::*;
use crate::parse::ast::Expression;
use anyhow::anyhow;
use anyhow::Result;
use std::iter::Peekable;

pub struct Parser<T: Iterator> {
    tokens: Peekable<T>,
}

impl<T: Iterator<Item = Token>> Parser<T> {
    pub fn new(tokens: T) -> Self {
        Parser {
            tokens: tokens.peekable(),
        }
    }

    pub fn parse(&mut self) -> Result<Expression> {
        self.expression()
    }

    fn expression(&mut self) -> Result<Expression> {
        self.term_expression()
    }

    fn term_expression(&mut self) -> Result<Expression> {
        let mut left = self.factor_expression()?;

        while matches!(
            self.tokens.peek(),
            Some(Token {
                variant: OpPlus | OpMinus,
                ..
            })
        ) {
            let op = self.tokens.next().unwrap();
            let right = self.factor_expression()?;

            left = Expression::Binary {
                left: Box::new(left),
                op,
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn factor_expression(&mut self) -> Result<Expression> {
        let mut left = self.unary_expression()?;

        while matches!(
            self.tokens.peek(),
            Some(Token {
                variant: OpSlash | OpStar,
                ..
            })
        ) {
            let op = self.tokens.next().unwrap();
            let right = self.unary_expression()?;

            left = Expression::Binary {
                left: Box::new(left),
                op,
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn unary_expression(&mut self) -> Result<Expression> {
        let expression = if let Some(Token {
            variant: OpPlus | OpMinus | OpNot,
            ..
        }) = self.tokens.peek()
        {
            Expression::Unary {
                op: self.tokens.next().unwrap(),
                right: Box::new(self.unary_expression()?),
            }
        } else {
            self.unit_expression()?
        };

        Ok(expression)
    }

    fn unit_expression(&mut self) -> Result<Expression> {
        if let Some(token) = self.tokens.next() {
            match token.variant {
                Int(_) | Float(_) => Ok(Expression::Literal(token)),
                GroupOpen => {
                    let expression = self.expression()?;
                    if let Some(Token {
                        variant: TokenVariant::GroupClose,
                        ..
                    }) = self.tokens.next()
                    {
                        Ok(Expression::Group(Box::new(expression)))
                    } else {
                        Err(anyhow!("Expected ')' after grouping."))
                    }
                }
                _ => Err(anyhow!("Unexpected token: {:?}", token.variant)),
            }
        } else {
            Err(anyhow!("Expected expression."))
        }
    }
}
