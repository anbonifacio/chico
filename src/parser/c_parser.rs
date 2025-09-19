use std::iter::Peekable;
use std::slice::Iter;

use crate::lexer::token::{Token, TokenType};
use crate::parser::c_ast::*;
use crate::parser::c_parser::FunctionDefinition::Function;

pub struct CParser<'expr> {
    expr_pool: &'expr mut ExprPool,
    tokens: &'expr [Token],
}

impl<'expr> CParser<'expr> {
    pub fn new(expr_pool: &'expr mut ExprPool, tokens: &'expr [Token]) -> Self {
        CParser { expr_pool, tokens }
    }

    pub fn parse_program(&mut self) -> std::io::Result<CProgram> {
        let tokens = self.tokens;
        log::debug!("Parsing {} tokens...", tokens.len());
        let tokens_iter = &mut tokens.iter().peekable();
        let program = self.parse_function(tokens_iter)?;
        let count = tokens_iter.count();
        if count > 0 {
            return Err(std::io::Error::new(
                std::io::ErrorKind::InvalidInput,
                format!("Error: {} tokens left unparsed", count),
            ));
        }
        Ok(CProgram::Program(program))
    }

    fn parse_function(
        &mut self,
        tokens_iter: &mut Peekable<Iter<'expr, Token>>,
    ) -> std::io::Result<FunctionDefinition> {
        self.expect(TokenType::IntKeyword, tokens_iter)?;
        let identifier = self.parse_identifier(tokens_iter)?;
        self.expect(TokenType::OpenParenthesis, tokens_iter)?;
        self.expect(TokenType::VoidKeyword, tokens_iter)?;
        self.expect(TokenType::CloseParenthesis, tokens_iter)?;
        self.expect(TokenType::OpenBrace, tokens_iter)?;
        let body = self.parse_statement(tokens_iter)?;
        self.expect(TokenType::CloseBrace, tokens_iter)?;
        Ok(Function(identifier, body))
    }

    fn parse_identifier(
        &self,
        tokens_iter: &mut Peekable<Iter<Token>>,
    ) -> std::io::Result<Identifier> {
        if let Some(token) = tokens_iter.next() {
            if token.token_type == TokenType::Identifier {
                Ok(Identifier::Name(token.value.clone()))
            } else {
                Err(std::io::Error::new(
                    std::io::ErrorKind::InvalidInput,
                    format!(
                        "Expected {:?}, found {:?}",
                        TokenType::Identifier,
                        token.token_type
                    ),
                ))
            }
        } else {
            Err(std::io::Error::new(
                std::io::ErrorKind::UnexpectedEof,
                "Unexpected end of input",
            ))
        }
    }

    fn parse_statement(
        &mut self,
        tokens_iter: &mut Peekable<Iter<'expr, Token>>,
    ) -> std::io::Result<Statement> {
        self.expect(TokenType::ReturnKeyword, tokens_iter)?;
        let expression = self.parse_expression(tokens_iter, 0)?;
        self.expect(TokenType::Semicolon, tokens_iter)?;
        Ok(Statement::Return(expression))
    }

    fn parse_factor(
        &mut self,
        tokens_iter: &mut Peekable<Iter<'expr, Token>>,
    ) -> std::io::Result<ExprRef> {
        if let Some(next_token) = tokens_iter.peek() {
            match next_token.token_type {
                TokenType::Constant => {
                    let token = self.extract_token(tokens_iter)?;
                    let expr_ref = self
                        .expr_pool
                        .add_expr(Expr::Constant(self.parse_as_i32(token)?));
                    log::debug!("Parsed constant: {}", self.expr_pool.get_expr(expr_ref));
                    Ok(expr_ref)
                }
                TokenType::Tilde | TokenType::Hyphen => {
                    let token = self.extract_token(tokens_iter)?;
                    let operator = self.parse_unop(token)?;
                    let inner_expr = self.parse_factor(tokens_iter)?;
                    let expr_ref = self.expr_pool.add_expr(Expr::Unary(operator, inner_expr));
                    log::debug!(
                        "Parsed unary expression: {}",
                        self.expr_pool.get_expr(expr_ref)
                    );
                    Ok(expr_ref)
                }
                TokenType::OpenParenthesis => {
                    self.expect(TokenType::OpenParenthesis, tokens_iter)?;
                    let expr_ref = self.parse_expression(tokens_iter, 0)?;
                    self.expect(TokenType::CloseParenthesis, tokens_iter)?;
                    log::debug!(
                        "Parsed parenthesized expression: ({})",
                        self.expr_pool.get_expr(expr_ref)
                    );
                    Ok(expr_ref)
                }
                _ => Err(std::io::Error::new(
                    std::io::ErrorKind::InvalidData,
                    format!("Malformed factor, found: {:?}", next_token.token_type),
                )),
            }
        } else {
            Err(std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                format!(
                    "Malformed factor, found: {:?}",
                    tokens_iter.peekable().peek()
                ),
            ))
        }
    }

    fn parse_expression(
        &mut self,
        tokens_iter: &mut Peekable<Iter<'expr, Token>>,
        min_prec: u8,
    ) -> std::io::Result<ExprRef> {
        let mut left = self.parse_factor(tokens_iter)?;
        log::debug!("Parsed left factor: {}", self.expr_pool.get_expr(left));
        if let Some(mut next_token) = tokens_iter.peek() {
            loop {
                let next_prec = next_token.token_type.precedence();
                if next_token.token_type.is_binop() && next_prec >= min_prec {
                    let operator = self.parse_binop(tokens_iter)?;
                    log::debug!("Parsed binary operator: {:?}", operator);
                    let right = self.parse_expression(tokens_iter, next_prec + 1)?;
                    log::debug!("Parsed right factor: {}", self.expr_pool.get_expr(right));
                    left = self.expr_pool.add_expr(Expr::Binary(operator, left, right));
                    next_token = if let Some(next_token) = tokens_iter.peek() {
                        log::debug!("Next token: {:?}", next_token);
                        next_token
                    } else {
                        break;
                    }
                } else {
                    break;
                }
            }
            log::debug!("Return expression: {:?}", self.expr_pool.get_expr(left));
            Ok(left)
        } else {
            Err(std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                format!(
                    "Malformed factor, found: {:?}",
                    tokens_iter.peekable().peek()
                ),
            ))
        }
    }

    fn parse_unop(&self, token: &Token) -> std::io::Result<UnaryOperator> {
        match token.token_type {
            TokenType::Tilde => Ok(UnaryOperator::Complement),
            TokenType::Hyphen => Ok(UnaryOperator::Negate),
            _ => Err(std::io::Error::new(
                std::io::ErrorKind::InvalidInput,
                format!("Expected unary operator, found {:?}", token.token_type),
            )),
        }
    }

    fn expect(
        &self,
        expected_type: TokenType,
        tokens_iter: &mut Peekable<Iter<Token>>,
    ) -> std::io::Result<()> {
        if let Some(token) = tokens_iter.next() {
            if token.token_type == expected_type {
                Ok(())
            } else {
                Err(std::io::Error::new(
                    std::io::ErrorKind::InvalidInput,
                    format!("Expected {:?}, found {:?}", expected_type, token.token_type),
                ))
            }
        } else {
            Err(std::io::Error::new(
                std::io::ErrorKind::UnexpectedEof,
                "Unexpected end of input",
            ))
        }
    }

    fn parse_as_i32(&self, token: &Token) -> std::io::Result<i32> {
        token
            .value
            .parse::<i32>()
            .map_err(|err| std::io::Error::new(std::io::ErrorKind::InvalidData, format!("{}", err)))
    }

    fn extract_token(
        &self,
        tokens_iter: &mut Peekable<Iter<'expr, Token>>,
    ) -> std::io::Result<&'expr Token> {
        tokens_iter.next().ok_or_else(|| {
            std::io::Error::new(std::io::ErrorKind::InvalidData, "Unexpected end of input")
        })
    }

    fn parse_binop(
        &self,
        tokens_iter: &mut Peekable<Iter<'expr, Token>>,
    ) -> std::io::Result<BinaryOperator> {
        match tokens_iter.next() {
            Some(token) => match token.token_type {
                TokenType::Plus => Ok(BinaryOperator::Add),
                TokenType::Hyphen => Ok(BinaryOperator::Subtract),
                TokenType::Asterisk => Ok(BinaryOperator::Multiply),
                TokenType::ForwardSlash => Ok(BinaryOperator::Divide),
                TokenType::Percent => Ok(BinaryOperator::Remainder),
                TokenType::BitwiseAnd => Ok(BinaryOperator::BitwiseAnd),
                TokenType::BitwiseOr => Ok(BinaryOperator::BitwiseOr),
                TokenType::BitwiseXor => Ok(BinaryOperator::BitwiseXor),
                TokenType::LeftShift => Ok(BinaryOperator::LeftShift),
                TokenType::RightShift => Ok(BinaryOperator::RightShift),
                _ => Err(std::io::Error::new(
                    std::io::ErrorKind::InvalidInput,
                    format!("Expected binary operator, found {:?}", token.token_type),
                )),
            },
            None => Err(std::io::Error::new(
                std::io::ErrorKind::UnexpectedEof,
                "Unexpected end of input",
            )),
        }
    }
}
