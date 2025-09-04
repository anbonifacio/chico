use std::slice::Iter;

use crate::lexer::token::{Token, TokenType};
use crate::parser::c_ast::*;
use crate::parser::parser::FunctionDefinition::Function;

pub struct CParser;

impl CParser {
    pub fn new() -> Self {
        CParser {}
    }

    pub fn parse_program(&self, tokens: &[Token]) -> std::io::Result<CProgram> {
        println!("Parsing {} tokens...", tokens.len());
        let tokens_iter = &mut tokens.iter();
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

    fn parse_function(&self, tokens_iter: &mut Iter<Token>) -> std::io::Result<FunctionDefinition> {
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

    fn parse_identifier(&self, tokens_iter: &mut Iter<Token>) -> std::io::Result<Identifier> {
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

    fn parse_statement(&self, tokens_iter: &mut Iter<Token>) -> std::io::Result<Statement> {
        self.expect(TokenType::ReturnKeyword, tokens_iter)?;
        let expression = self.parse_expression(tokens_iter)?;
        self.expect(TokenType::Semicolon, tokens_iter)?;
        Ok(Statement::Return(expression))
    }

    fn parse_expression(&self, tokens_iter: &mut Iter<Token>) -> std::io::Result<Exp> {
        if let Some(token) = tokens_iter.next() {
            if token.token_type == TokenType::Constant {
                Ok(Exp::Constant(token.value.parse::<i32>().map_err(
                    |err| {
                        std::io::Error::new(
                            std::io::ErrorKind::InvalidData,
                            format!("{}", err.to_string()),
                        )
                    },
                )?))
            } else {
                Err(std::io::Error::new(
                    std::io::ErrorKind::InvalidInput,
                    format!(
                        "Expected {:?}, found {:?}",
                        TokenType::Constant,
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

    fn expect(
        &self,
        expected_type: TokenType,
        tokens_iter: &mut Iter<Token>,
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
}
