use thiserror::Error;

use super::ast::expression::{Expression, ExpressionKind};
use super::ast::nodes::{
    ArrayExpression, BinaryOperatorNode, FnHeaderNode, LoopExpression, SimpleLiteralNode,
    StrExpression, StrPartKind, StrPartNode, TypedIdentifierNode, UnaryOperatorNode,
};
use super::ast::statement::{Statement, StatementKind};
use super::ast::Program;
use crate::lex::lexer::Lexer;
use crate::lex::token::Location;
use crate::lex::token::TokenKind::{self, *};
use crate::parse::ast::nodes::{
    ArrayAccessExpression, BlockExpression, CallExpression, ConditionalBlockNode, FnExpression,
    IfExpression, MemberAccessExpression,
};
use crate::{check, consume, consume_if, inner, lex::token::Token, merge_token_names};
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

    pub fn parse(&mut self) -> Result<Program, ParserError> {
        let mut statements = Vec::new();

        while !check!(self, Eof) {
            statements.push(self.statement()?);
        }

        Ok(Program { statements })
    }

    fn statement(&mut self) -> Result<Statement, ParserError> {
        let token = self.tokens.peek().expect("Expected token.");
        match token.kind {
            KeywordPrint => self.print_statement(),
            KeywordReturn => self.return_statement(),
            KeywordBreak => self.break_statement(),
            KeywordContinue => self.continue_statement(),
            _ => self.expression_statement(),
        }
    }

    fn return_statement(&mut self) -> Result<Statement, ParserError> {
        let return_token = consume!(self, KeywordReturn)?;
        let expression = self.expression()?;
        consume!(self, SemiColon)?;
        Ok(Statement {
            at: return_token.location,
            kind: StatementKind::Return(expression),
        })
    }

    fn break_statement(&mut self) -> Result<Statement, ParserError> {
        let break_token = consume!(self, KeywordBreak)?;
        let returned_on_break = if consume_if!(self, SemiColon).is_none() {
            let expression = self.expression()?;
            consume!(self, SemiColon)?;
            Some(expression)
        } else {
            None
        };
        Ok(Statement {
            at: break_token.location,
            kind: StatementKind::Break(returned_on_break),
        })
    }

    fn continue_statement(&mut self) -> Result<Statement, ParserError> {
        let continue_token = consume!(self, KeywordContinue)?;
        consume!(self, SemiColon)?;
        Ok(Statement {
            at: continue_token.location,
            kind: StatementKind::Continue,
        })
    }

    fn print_statement(&mut self) -> Result<Statement, ParserError> {
        let print_token = consume!(self, KeywordPrint)?;
        let expression = self.expression()?;
        consume!(self, SemiColon)?;
        Ok(Statement {
            at: print_token.location,
            kind: StatementKind::Print(expression),
        })
    }

    fn expression_statement(&mut self) -> Result<Statement, ParserError> {
        let expression = self.expression()?;
        consume!(self, SemiColon)?;
        Ok(Statement {
            at: expression.at,
            kind: StatementKind::Expression(expression),
        })
    }

    pub fn expression(&mut self) -> Result<Expression, ParserError> {
        self.assignment_expression()
    }

    fn assignment_expression(&mut self) -> Result<Expression, ParserError> {
        // Parse any expressions as l-values for now.
        let left = self.or_expression()?;

        if let Some(op) = consume_if!(self, Assign | ConstAssign) {
            let right = self.assignment_expression()?;

            Ok(Expression {
                at: left.at,
                kind: ExpressionKind::Binary {
                    left: Box::new(left),
                    op: BinaryOperatorNode::from_token(op),
                    right: Box::new(right),
                },
            })
        } else {
            Ok(left)
        }
    }

    fn or_expression(&mut self) -> Result<Expression, ParserError> {
        let mut left = self.and_expression()?;

        while let Some(op) = consume_if!(self, OpAnd) {
            let right = self.and_expression()?;

            left = Expression {
                at: left.at,
                kind: ExpressionKind::Binary {
                    left: Box::new(left),
                    op: BinaryOperatorNode::from_token(op),
                    right: Box::new(right),
                },
            };
        }

        Ok(left)
    }

    fn and_expression(&mut self) -> Result<Expression, ParserError> {
        let mut left = self.equality_expression()?;

        while let Some(op) = consume_if!(self, OpOr) {
            let right = self.equality_expression()?;

            left = Expression {
                at: left.at,
                kind: ExpressionKind::Binary {
                    left: Box::new(left),
                    op: BinaryOperatorNode::from_token(op),
                    right: Box::new(right),
                },
            };
        }

        Ok(left)
    }

    fn equality_expression(&mut self) -> Result<Expression, ParserError> {
        let mut left = self.comparison_expression()?;

        while let Some(op) = consume_if!(self, OpEq | OpNeq) {
            let right = self.comparison_expression()?;

            left = Expression {
                at: left.at,
                kind: ExpressionKind::Binary {
                    left: Box::new(left),
                    op: BinaryOperatorNode::from_token(op),
                    right: Box::new(right),
                },
            };
        }

        Ok(left)
    }

    fn comparison_expression(&mut self) -> Result<Expression, ParserError> {
        let mut left = self.term_expression()?;

        while let Some(op) = consume_if!(self, OpGt | OpGte | OpLt | OpLte) {
            let right = self.term_expression()?;

            left = Expression {
                at: left.at,
                kind: ExpressionKind::Binary {
                    left: Box::new(left),
                    op: BinaryOperatorNode::from_token(op),
                    right: Box::new(right),
                },
            };
        }

        Ok(left)
    }

    fn term_expression(&mut self) -> Result<Expression, ParserError> {
        let mut left = self.factor_expression()?;

        while let Some(op) = consume_if!(self, OpPlus | OpMinus) {
            let right = self.factor_expression()?;

            left = Expression {
                at: left.at,
                kind: ExpressionKind::Binary {
                    left: Box::new(left),
                    op: BinaryOperatorNode::from_token(op),
                    right: Box::new(right),
                },
            };
        }

        Ok(left)
    }

    fn factor_expression(&mut self) -> Result<Expression, ParserError> {
        let mut left = self.unary_expression()?;

        while let Some(op) = consume_if!(self, OpSlash | OpStar) {
            let right = self.unary_expression()?;

            left = Expression {
                at: left.at,
                kind: ExpressionKind::Binary {
                    left: Box::new(left),
                    op: BinaryOperatorNode::from_token(op),
                    right: Box::new(right),
                },
            };
        }

        Ok(left)
    }

    fn unary_expression(&mut self) -> Result<Expression, ParserError> {
        let expression = if check!(self, OpPlus | OpMinus | OpNot) {
            let op_token = self.tokens.next().unwrap();

            Expression {
                at: op_token.location,
                kind: ExpressionKind::Unary {
                    op: UnaryOperatorNode::from_token(op_token),
                    right: Box::new(self.unary_expression()?),
                },
            }
        } else {
            self.postfix_expression()?
        };

        Ok(expression)
    }

    fn postfix_expression(&mut self) -> Result<Expression, ParserError> {
        let mut left = self.unit_expression()?;

        while let Some(token) = consume_if!(self, GroupOpen | ArrayOpen | Dot) {
            let kind = match token.kind {
                GroupOpen => {
                    let mut arguments = Vec::new();

                    while consume_if!(self, GroupClose).is_none() {
                        arguments.push(self.expression()?);

                        if consume_if!(self, Comma).is_none() {
                            consume!(self, GroupClose)?;
                            break;
                        }
                    }

                    ExpressionKind::Call(Box::new(CallExpression {
                        at: token.location,
                        called: left,
                        arguments,
                    }))
                }
                ArrayOpen => {
                    let index = self.expression()?;
                    consume!(self, ArrayClose)?;

                    ExpressionKind::ArrayAccess(Box::new(ArrayAccessExpression {
                        at: token.location,
                        array: left,
                        index,
                    }))
                }
                Dot => {
                    let member_name = inner!(consume!(self, Ident(_))?, Ident);

                    ExpressionKind::MemberAccess(Box::new(MemberAccessExpression {
                        at: token.location,
                        object: left,
                        member_name,
                    }))
                }
                _ => unreachable!(),
            };

            left = Expression {
                at: token.location,
                kind,
            };
        }

        Ok(left)
    }

    fn unit_expression(&mut self) -> Result<Expression, ParserError> {
        if let Some(token) = self.tokens.peek() {
            let location = token.location;

            let kind = match token.kind {
                Int(_) | Float(_) | Str(_) | KeywordTrue | KeywordFalse => {
                    let literal = self.tokens.next().unwrap();
                    Ok(ExpressionKind::SimpleLiteral(
                        SimpleLiteralNode::from_token(literal),
                    ))
                }
                Ident(_) => Ok(ExpressionKind::Identifier(inner!(
                    self.tokens.next().unwrap(),
                    Ident
                ))),
                StrOpen => Ok(ExpressionKind::StrLiteral(Box::new(self.str()?))),
                GroupOpen => Ok(ExpressionKind::Group(Box::new(self.group()?))),
                BlockOpen => Ok(ExpressionKind::Block(Box::new(self.generic_block()?))),
                ArrayOpen => Ok(ExpressionKind::ArrayLiteral(self.array()?)),
                KeywordFn => Ok(ExpressionKind::FnLiteral(Box::new(self.function()?))),
                KeywordIf => Ok(ExpressionKind::If(Box::new(self.conditional()?))),
                KeywordLoop => Ok(ExpressionKind::Loop(Box::new(self.repeating()?))),
                _ => Err(ParserError::new(
                    ParserErrorLocation::Specific(token.location),
                    ParserErrorKind::UnexpectedToken {
                        received: token.kind.clone(),
                        expected: merge_token_names!(
                            Int(_),
                            Float(_),
                            Str(_),
                            KeywordTrue,
                            KeywordFalse,
                            Ident(_),
                            StrOpen,
                            GroupOpen,
                            BlockOpen,
                            ArrayOpen,
                            KeywordFn,
                            KeywordIf,
                            KeywordLoop
                        ),
                    },
                )),
            }?;

            Ok(Expression { at: location, kind })
        } else {
            Err(ParserError::new(
                ParserErrorLocation::Eof,
                ParserErrorKind::UnexpectedEof {
                    // Well sure this works.
                    expected: "expression".into(),
                },
            ))
        }
    }

    fn group(&mut self) -> Result<Expression, ParserError> {
        consume!(self, GroupOpen)?;
        let expression = self.expression()?;
        consume!(self, GroupClose)?;
        Ok(expression)
    }

    fn array(&mut self) -> Result<ArrayExpression, ParserError> {
        let array_token = consume!(self, ArrayOpen)?;
        let mut elements = Vec::new();

        loop {
            elements.push(self.expression()?);

            if consume_if!(self, Comma).is_none() {
                break;
            }
        }

        consume!(self, ArrayClose)?;
        Ok(ArrayExpression {
            at: array_token.location,
            elements,
        })
    }

    fn str(&mut self) -> Result<StrExpression, ParserError> {
        let mut parts = Vec::new();

        let str_token = consume!(self, StrOpen)?;

        loop {
            let token = self.tokens.next().expect("Unclosed str.");

            let part_kind = match token.kind {
                Str(literal) => StrPartKind::Literal(literal),
                StrEmbed(code) => {
                    let embed_lexer = Lexer::new(&code);
                    let mut embed_parser = Parser::new(embed_lexer);

                    let node = embed_parser.expression()?;

                    StrPartKind::Embed(node)
                }
                StrClose => break,
                _ => unreachable!(),
            };

            parts.push(StrPartNode {
                at: token.location,
                kind: part_kind,
            });
        }

        Ok(StrExpression {
            at: str_token.location,
            parts,
        })
    }

    fn function(&mut self) -> Result<FnExpression, ParserError> {
        let fn_token = consume!(self, KeywordFn)?;

        let header = {
            let has_self_receiver = if consume_if!(self, KeywordSelf).is_some() {
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

                parameters.push(TypedIdentifierNode {
                    identifier: parameter_name,
                    type_constraint,
                    at: token.location,
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

            FnHeaderNode {
                has_self_receiver,
                parameters,
                return_type,
                at: fn_token.location,
            }
        };

        let body = self.generic_block()?;

        Ok(FnExpression {
            header,
            body,
            // Duplicated ^
            at: fn_token.location,
        })
    }

    fn conditional(&mut self) -> Result<IfExpression, ParserError> {
        let if_token = consume!(self, KeywordIf)?;

        let mut conditionals = Vec::new();

        let if_condition = self.expression()?;
        let if_block = self.generic_block()?;

        conditionals.push(ConditionalBlockNode {
            at: if_condition.at,
            condition: if_condition,
            block: if_block,
        });

        // Elifs
        while consume_if!(self, KeywordElif).is_some() {
            let elif_condition = self.expression()?;
            let elif_block = self.generic_block()?;

            conditionals.push(ConditionalBlockNode {
                at: elif_condition.at,
                condition: elif_condition,
                block: elif_block,
            });
        }

        let else_block = if consume_if!(self, KeywordElse).is_some() {
            Some(self.generic_block()?)
        } else {
            None
        };

        Ok(IfExpression {
            conditionals,
            else_block,
            at: if_token.location,
        })
    }

    fn repeating(&mut self) -> Result<LoopExpression, ParserError> {
        let loop_token = consume!(self, KeywordLoop)?;

        let condition = if consume_if!(self, KeywordIf).is_some() {
            let expression = self.expression()?;
            Some(expression)
        } else {
            None
        };

        let body = self.generic_block()?;
        Ok(LoopExpression {
            body,
            condition,
            at: loop_token.location,
        })
    }

    fn generic_block(&mut self) -> Result<BlockExpression, ParserError> {
        let block_token = consume!(self, BlockOpen)?;

        let mut statements = Vec::new();
        let mut tail_expression = None;

        loop {
            let token = self.tokens.peek().expect("Unclosed block.");
            match token.kind {
                KeywordReturn | KeywordPrint | KeywordContinue | KeywordBreak => {
                    statements.push(self.statement()?);
                }
                BlockClose => {
                    break;
                }
                _ => {
                    let expression = self.expression()?;
                    if consume_if!(self, SemiColon).is_some() {
                        statements.push(Statement {
                            at: expression.at,
                            kind: StatementKind::Expression(expression),
                        });
                    } else {
                        tail_expression = Some(expression);
                        break;
                    }
                }
            }
        }

        consume!(self, BlockClose)?;

        Ok(BlockExpression {
            statements,
            tail_expression,
            at: block_token.location,
        })
    }
}

#[derive(Debug)]
pub enum ParserErrorLocation {
    Specific(Location),
    Eof,
}

#[derive(Debug)]
pub struct ParserError {
    pub kind: ParserErrorKind,
    pub at: ParserErrorLocation,
}

impl ParserError {
    fn new(at: ParserErrorLocation, kind: ParserErrorKind) -> Self {
        Self { at, kind }
    }
}

#[derive(Error, Debug)]
pub enum ParserErrorKind {
    #[error("Received unexpected '{received}', expected: {expected}.'")]
    UnexpectedToken {
        received: TokenKind,
        // Stringified expected tokens
        expected: String,
    },
    #[error("Unexpected end of file, expected: {expected}")]
    UnexpectedEof { expected: String },
}
