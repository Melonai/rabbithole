use super::ast::expression::Expression;
use super::ast::statement::Statement;
use super::ast::value::UnaryOperator;
use super::ast::Program;
use crate::check;
use crate::consume;
use crate::consume_if;
use crate::inner;
use crate::lex::token::Token;
use crate::lex::token::TokenVariant::*;
use crate::parse::ast::value::BinaryOperator;
use crate::parse::ast::value::Block;
use crate::parse::ast::value::ConditionalBlock;
use crate::parse::ast::value::FnHeader;
use crate::parse::ast::value::Literal;
use crate::parse::ast::value::TypedIdentifier;
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

    pub fn parse(&mut self) -> Result<Program> {
        let mut statements = Vec::new();

        while !check!(self, Eof) {
            statements.push(self.statement()?);
        }

        Ok(Program { statements })
    }

    fn statement(&mut self) -> Result<Statement> {
        let token = self.tokens.peek().expect("Expected token.");
        match token.variant {
            KeywordPrint => self.print_statement(),
            KeywordReturn => self.return_statement(),
            _ => self.expression_statement(),
        }
    }

    fn return_statement(&mut self) -> Result<Statement> {
        consume!(self, KeywordReturn)?;
        let expression = self.expression()?;
        consume!(self, SemiColon)?;
        Ok(Statement::Return(expression))
    }

    fn print_statement(&mut self) -> Result<Statement> {
        consume!(self, KeywordPrint)?;
        let expression = self.expression()?;
        consume!(self, SemiColon)?;
        Ok(Statement::Print(expression))
    }

    fn expression_statement(&mut self) -> Result<Statement> {
        let expression = self.expression()?;
        consume!(self, SemiColon)?;
        Ok(Statement::Expression(expression))
    }

    fn expression(&mut self) -> Result<Expression> {
        self.assignment_expression()
    }

    fn assignment_expression(&mut self) -> Result<Expression> {
        // Parse any expressions as l-values for now.
        let left = self.term_expression()?;

        if let Some(op) = consume_if!(self, Assign | ConstAssign) {
            let right = self.assignment_expression()?;

            Ok(Expression::Binary {
                left: Box::new(left),
                op: BinaryOperator::from_token(op),
                right: Box::new(right),
            })
        } else {
            Ok(left)
        }
    }

    fn term_expression(&mut self) -> Result<Expression> {
        let mut left = self.factor_expression()?;

        while let Some(op) = consume_if!(self, OpPlus | OpMinus) {
            let right = self.factor_expression()?;

            left = Expression::Binary {
                left: Box::new(left),
                op: BinaryOperator::from_token(op),
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn factor_expression(&mut self) -> Result<Expression> {
        let mut left = self.unary_expression()?;

        while let Some(op) = consume_if!(self, OpSlash | OpStar) {
            let right = self.unary_expression()?;

            left = Expression::Binary {
                left: Box::new(left),
                op: BinaryOperator::from_token(op),
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn unary_expression(&mut self) -> Result<Expression> {
        let expression = if check!(self, OpPlus | OpMinus | OpNot) {
            Expression::Unary {
                op: UnaryOperator::from_token(self.tokens.next().unwrap()),
                right: Box::new(self.unary_expression()?),
            }
        } else {
            self.unit_expression()?
        };

        Ok(expression)
    }

    fn unit_expression(&mut self) -> Result<Expression> {
        if let Some(token) = self.tokens.peek() {
            match token.variant {
                Int(_) | Float(_) | Str(_) => {
                    let literal = self.tokens.next().unwrap();
                    Ok(Expression::Literal(Literal::from_token(literal)))
                }
                // Identifier(_) => match self.tokens.next().unwrap().variant {
                //     Identifier(identifier) => Ok(Expression::Identifier(identifier)),
                //     _ => unreachable!(),
                // },
                Ident(_) => Ok(Expression::Identifier(inner!(
                    self.tokens.next().unwrap(),
                    Ident
                ))),
                GroupOpen => {
                    consume!(self, GroupOpen)?;
                    let expression = self.expression()?;
                    consume!(self, GroupClose)?;
                    Ok(Expression::Group(Box::new(expression)))
                }
                BlockOpen => Ok(Expression::Block(Box::new(self.block()?))),
                KeywordFn => {
                    consume!(self, KeywordFn)?;
                    let token = self.tokens.next().expect("Expected function header.");

                    let header = {
                        let has_self_receiver = if let KeywordSelf = token.variant {
                            consume_if!(self, Comma);
                            true
                        } else {
                            false
                        };

                        let mut parameters = Vec::new();
                        while let Some(token) = consume_if!(self, Ident(_)) {
                            let parameter_name = inner!(token, Ident);

                            let type_constraint = if consume_if!(self, Colon).is_some() {
                                Some(inner!(consume!(self, Ident(_))?, Ident))
                            } else {
                                None
                            };

                            parameters.push(TypedIdentifier {
                                identifier: parameter_name,
                                type_constraint,
                            });

                            if consume_if!(self, Comma).is_none() {
                                break;
                            }
                        }

                        let return_type = if consume_if!(self, Arrow).is_some() {
                            Some(inner!(consume!(self, Ident(_))?, Ident))
                        } else {
                            None
                        };

                        FnHeader {
                            has_self_receiver,
                            parameters,
                            return_type,
                        }
                    };

                    let body = self.block()?;

                    Ok(Expression::Fn {
                        header,
                        body: Box::new(body),
                    })
                }
                KeywordIf => {
                    consume!(self, KeywordIf)?;

                    let mut conditionals = Vec::new();

                    let if_condition = self.expression()?;
                    let if_block = self.block()?;

                    conditionals.push(ConditionalBlock {
                        condition: if_condition,
                        block: if_block,
                    });

                    // Elifs
                    while consume_if!(self, KeywordElif).is_some() {
                        let elif_condition = self.expression()?;
                        let elif_block = self.block()?;

                        conditionals.push(ConditionalBlock {
                            condition: elif_condition,
                            block: elif_block,
                        });
                    }

                    let else_conditional = if consume_if!(self, KeywordElse).is_some() {
                        Some(self.block()?)
                    } else {
                        None
                    };

                    Ok(Expression::If {
                        conditionals,
                        else_block: else_conditional.map(Box::new),
                    })
                }
                _ => Err(anyhow!("Unexpected token: {:?}", token.variant)),
            }
        } else {
            Err(anyhow!("Expected expression."))
        }
    }

    fn block(&mut self) -> Result<Block> {
        consume!(self, BlockOpen)?;

        let mut statements = Vec::new();
        let mut tail_expression = None;

        loop {
            let token = self.tokens.peek().expect("Unclosed block.");
            match token.variant {
                KeywordReturn | KeywordPrint => {
                    statements.push(self.statement()?);
                }
                BlockClose => {
                    break;
                }
                _ => {
                    let expression = self.expression()?;
                    if consume_if!(self, SemiColon).is_some() {
                        statements.push(Statement::Expression(expression));
                    } else {
                        tail_expression = Some(expression);
                        break;
                    }
                }
            }
        }

        consume!(self, BlockClose)?;

        Ok(Block {
            statements,
            tail_expression,
        })
    }
}
