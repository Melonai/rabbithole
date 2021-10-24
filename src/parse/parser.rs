use super::ast::expression::Expression;
use super::ast::nodes::{ArrayNode, LoopNode, UnaryOperator};
use super::ast::statement::Statement;
use super::ast::Program;
use crate::lex::token::TokenVariant::*;
use crate::parse::ast::nodes::{
    ArrayAccessNode, BinaryOperator, BlockNode, CallNode, ConditionalBlock, FnHeader, FnNode,
    IfNode, MemberAccessNode, SimpleLiteral, TypedIdentifier,
};
use crate::{check, consume, consume_if, inner, lex::token::Token};
use anyhow::{anyhow, Result};
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
            KeywordBreak => self.break_statement(),
            KeywordContinue => self.continue_statement(),
            _ => self.expression_statement(),
        }
    }

    fn return_statement(&mut self) -> Result<Statement> {
        consume!(self, KeywordReturn)?;
        let expression = self.expression()?;
        consume!(self, SemiColon)?;
        Ok(Statement::Return(expression))
    }

    fn break_statement(&mut self) -> Result<Statement> {
        consume!(self, KeywordBreak)?;
        let returned_on_break = if consume_if!(self, SemiColon).is_none() {
            let expression = self.expression()?;
            consume!(self, SemiColon)?;
            Some(expression)
        } else {
            None
        };
        Ok(Statement::Break(returned_on_break))
    }

    fn continue_statement(&mut self) -> Result<Statement> {
        consume!(self, KeywordContinue)?;
        consume!(self, SemiColon)?;
        Ok(Statement::Continue)
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

    pub fn expression(&mut self) -> Result<Expression> {
        self.assignment_expression()
    }

    fn assignment_expression(&mut self) -> Result<Expression> {
        // Parse any expressions as l-values for now.
        let left = self.equality_expression()?;

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

    fn equality_expression(&mut self) -> Result<Expression> {
        let mut left = self.comparison_expression()?;

        while let Some(op) = consume_if!(self, OpEq | OpNeq) {
            let right = self.comparison_expression()?;

            left = Expression::Binary {
                left: Box::new(left),
                op: BinaryOperator::from_token(op),
                right: Box::new(right),
            };
        }

        Ok(left)
    }

    fn comparison_expression(&mut self) -> Result<Expression> {
        let mut left = self.term_expression()?;

        while let Some(op) = consume_if!(self, OpGt | OpGte | OpLt | OpLte) {
            let right = self.term_expression()?;

            left = Expression::Binary {
                left: Box::new(left),
                op: BinaryOperator::from_token(op),
                right: Box::new(right),
            };
        }

        Ok(left)
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
            self.postfix_expression()?
        };

        Ok(expression)
    }

    fn postfix_expression(&mut self) -> Result<Expression> {
        let mut left = self.unit_expression()?;

        while let Some(token) = consume_if!(self, GroupOpen | ArrayOpen | Dot) {
            match token.variant {
                GroupOpen => loop {
                    let mut arguments = Vec::new();

                    loop {
                        arguments.push(self.expression()?);

                        if consume_if!(self, Comma).is_none() {
                            consume!(self, GroupClose)?;
                            break;
                        }
                    }

                    left = Expression::Call(Box::new(CallNode {
                        called: left,
                        arguments,
                    }))
                },
                ArrayOpen => {
                    let index = self.expression()?;
                    consume!(self, ArrayClose)?;

                    left = Expression::ArrayAccess(Box::new(ArrayAccessNode { array: left, index }))
                }
                Dot => {
                    let member_name = inner!(consume!(self, Ident(_))?, Ident);

                    left = Expression::MemberAccess(Box::new(MemberAccessNode {
                        object: left,
                        member_name,
                    }))
                }
                _ => unreachable!(),
            }
        }

        Ok(left)
    }

    fn unit_expression(&mut self) -> Result<Expression> {
        if let Some(token) = self.tokens.peek() {
            match token.variant {
                Int(_) | Float(_) | Str(_) | KeywordTrue | KeywordFalse => {
                    let literal = self.tokens.next().unwrap();
                    Ok(Expression::SimpleLiteral(SimpleLiteral::from_token(
                        literal,
                    )))
                }
                Ident(_) => Ok(Expression::Identifier(inner!(
                    self.tokens.next().unwrap(),
                    Ident
                ))),
                GroupOpen => Ok(Expression::Group(Box::new(self.group()?))),
                BlockOpen => Ok(Expression::Block(Box::new(self.generic_block()?))),
                ArrayOpen => Ok(Expression::ArrayLiteral(self.array()?)),
                KeywordFn => Ok(Expression::FnLiteral(Box::new(self.function()?))),
                KeywordIf => Ok(Expression::If(Box::new(self.conditional()?))),
                KeywordLoop => Ok(Expression::Loop(Box::new(self.repeating()?))),
                _ => Err(anyhow!("Unexpected token: {:?}", token.variant)),
            }
        } else {
            Err(anyhow!("Expected expression."))
        }
    }

    fn group(&mut self) -> Result<Expression> {
        consume!(self, GroupOpen)?;
        let expression = self.expression()?;
        consume!(self, GroupClose)?;
        Ok(expression)
    }

    fn array(&mut self) -> Result<ArrayNode> {
        consume!(self, ArrayOpen)?;
        let mut elements = Vec::new();

        loop {
            elements.push(self.expression()?);

            if consume_if!(self, Comma).is_none() {
                break;
            }
        }

        consume!(self, ArrayClose)?;
        Ok(ArrayNode { elements })
    }

    fn function(&mut self) -> Result<FnNode> {
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

        let body = self.generic_block()?;

        Ok(FnNode { header, body })
    }

    fn conditional(&mut self) -> Result<IfNode> {
        consume!(self, KeywordIf)?;

        let mut conditionals = Vec::new();

        let if_condition = self.expression()?;
        let if_block = self.generic_block()?;

        conditionals.push(ConditionalBlock {
            condition: if_condition,
            block: if_block,
        });

        // Elifs
        while consume_if!(self, KeywordElif).is_some() {
            let elif_condition = self.expression()?;
            let elif_block = self.generic_block()?;

            conditionals.push(ConditionalBlock {
                condition: elif_condition,
                block: elif_block,
            });
        }

        let else_block = if consume_if!(self, KeywordElse).is_some() {
            Some(self.generic_block()?)
        } else {
            None
        };

        Ok(IfNode {
            conditionals,
            else_block,
        })
    }

    fn repeating(&mut self) -> Result<LoopNode> {
        consume!(self, KeywordLoop)?;

        let condition = if consume_if!(self, KeywordIf).is_some() {
            let expression = self.expression()?;
            Some(expression)
        } else {
            None
        };

        let body = self.generic_block()?;
        Ok(LoopNode { body, condition })
    }

    fn generic_block(&mut self) -> Result<BlockNode> {
        consume!(self, BlockOpen)?;

        let mut statements = Vec::new();
        let mut tail_expression = None;

        loop {
            let token = self.tokens.peek().expect("Unclosed block.");
            match token.variant {
                KeywordReturn | KeywordPrint | KeywordContinue | KeywordBreak => {
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

        Ok(BlockNode {
            statements,
            tail_expression,
        })
    }
}
