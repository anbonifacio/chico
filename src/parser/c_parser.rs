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
        let mut function_body = vec![];
        while let Some(next_token) = tokens_iter.peek() {
            if let TokenType::CloseBrace = next_token.token_type {
                break;
            } else {
                let next_block_item = self.parse_block_item(tokens_iter)?;
                function_body.push(next_block_item);
            }
        }
        self.expect(TokenType::CloseBrace, tokens_iter)?;
        Ok(Function(identifier, function_body))
    }

    fn parse_block_item(
        &mut self,
        tokens_iter: &mut Peekable<Iter<'expr, Token>>,
    ) -> std::io::Result<BlockItem> {
        if let Some(next_token) = tokens_iter.peek() {
            match next_token.token_type {
                TokenType::IntKeyword => {
                    // This is a Declaration
                    log::debug!("Parsing declaration in block item...");
                    let declaration = self.parse_declaration(tokens_iter)?;
                    Ok(BlockItem::D(declaration))
                }
                _ => {
                    // This is a Statement
                    log::debug!("Parsing statement in block item...");
                    let statement = self.parse_statement(tokens_iter)?;
                    Ok(BlockItem::S(statement))
                }
            }
        } else {
            Err(std::io::Error::new(
                std::io::ErrorKind::UnexpectedEof,
                "Unexpected end of input",
            ))
        }
    }

    fn parse_declaration(
        &mut self,
        tokens_iter: &mut Peekable<Iter<'expr, Token>>,
    ) -> std::io::Result<Declaration> {
        self.expect(TokenType::IntKeyword, tokens_iter)?;
        let identifier = self.parse_identifier(tokens_iter)?;
        let expr = match tokens_iter.peek() {
            Some(next_token) => match next_token.token_type {
                TokenType::Assign => {
                    self.extract_token(tokens_iter)?;
                    log::debug!("Parsing initializer for declaration...");
                    let expr = self.parse_expression(tokens_iter, 0)?;
                    log::debug!(
                        "Parsed initializer expression: {}",
                        self.expr_pool.get_expr(expr)
                    );
                    Some(expr)
                }
                _ => None,
            },
            None => None,
        };
        self.expect(TokenType::Semicolon, tokens_iter)?;
        let declaration = Declaration::Declaration(identifier, expr);
        log::debug!("Parsed declaration: {}", declaration);
        Ok(declaration)
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
        let statement = if let Some(next_token) = tokens_iter.peek() {
            log::debug!("Next statement {:?}", next_token);
            match next_token.token_type {
                TokenType::ReturnKeyword => {
                    self.expect(TokenType::ReturnKeyword, tokens_iter)?;
                    let expression = self.parse_expression(tokens_iter, 0)?;
                    Statement::Return(expression)
                }
                t if t.is_assignment() => {
                    self.extract_token(tokens_iter)?;
                    let expression = self.parse_expression(tokens_iter, 0)?;
                    Statement::Expression(expression)
                }
                TokenType::Constant
                | TokenType::Identifier
                | TokenType::OpenParenthesis
                | TokenType::Tilde
                | TokenType::Hyphen
                | TokenType::Not
                | TokenType::DoublePlus
                | TokenType::DoubleHyphens => {
                    let expression = self.parse_expression(tokens_iter, 0)?;
                    Statement::Expression(expression)
                }
                TokenType::Semicolon => Statement::Null,
                _ => {
                    return Err(std::io::Error::new(
                        std::io::ErrorKind::InvalidData,
                        format!("Malformed statement, found: {:?}", next_token.token_type),
                    ));
                }
            }
        } else {
            return Err(std::io::Error::new(
                std::io::ErrorKind::UnexpectedEof,
                "Unexpected end of input",
            ));
        };
        self.expect(TokenType::Semicolon, tokens_iter)?;
        Ok(statement)
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
                    match next_token.token_type {
                        t if t.is_assignment() => {
                            log::debug!("Parsing assignment operator {:?}", next_token.token_type);
                            let operator = self.parse_binop(tokens_iter)?;
                            let compound_op = operator.get_compound_operator();
                            match compound_op {
                                Some(compound_op) => {
                                    // This is a compound assignment (e.g., +=, -=)
                                    let right =
                                        self.parse_expression(tokens_iter, next_prec + 1)?;
                                    log::debug!(
                                        "Parsed right factor: {}",
                                        self.expr_pool.get_expr(right)
                                    );
                                    let combined = self.expr_pool.add_expr(Expr::Binary(
                                        compound_op,
                                        left,
                                        right,
                                    ));
                                    left =
                                        self.expr_pool.add_expr(Expr::Assignment(combined, right));
                                }
                                None => {
                                    // This is a simple assignment (=)
                                    let right = self.parse_expression(tokens_iter, next_prec)?;
                                    log::debug!(
                                        "Parsed right factor: {}",
                                        self.expr_pool.get_expr(right)
                                    );
                                    left = self.expr_pool.add_expr(Expr::Assignment(left, right));
                                }
                            }
                        }
                        _ => {
                            let operator = self.parse_binop(tokens_iter)?;
                            log::debug!("Parsed binary operator: {:?}", operator);
                            let right = self.parse_expression(tokens_iter, next_prec + 1)?;
                            log::debug!("Parsed right factor: {}", self.expr_pool.get_expr(right));
                            left = self.expr_pool.add_expr(Expr::Binary(operator, left, right));
                        }
                    }
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
                    "Malformed expression, found: {:?}",
                    tokens_iter.peekable().peek()
                ),
            ))
        }
    }

    fn parse_factor(
        &mut self,
        tokens_iter: &mut Peekable<Iter<'expr, Token>>,
    ) -> std::io::Result<ExprRef> {
        let factor = if let Some(next_token) = tokens_iter.peek() {
            log::debug!("Parsing factor starting with {:?}", next_token);
            match next_token.token_type {
                TokenType::Constant => {
                    let token = self.extract_token(tokens_iter)?;
                    let expr_ref = self
                        .expr_pool
                        .add_expr(Expr::Constant(self.parse_as_i32(token)?));
                    log::debug!("Parsed constant: {}", self.expr_pool.get_expr(expr_ref));
                    Ok(expr_ref)
                }
                t if t.is_unop() => {
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
                TokenType::Identifier => {
                    let var = self.parse_identifier(tokens_iter)?;
                    let expr_ref = self.expr_pool.add_expr(Expr::Var(var));
                    log::debug!(
                        "Parsed Var expression: {}",
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
        };
        log::debug!("Checking for postfix operations...");
        self.parse_postfix_tokens(tokens_iter)?;
        factor
    }

    fn parse_postfix_tokens(
        &mut self,
        tokens_iter: &mut Peekable<Iter<'expr, Token>>,
    ) -> Result<(), std::io::Error> {
        Ok(loop {
            if let Some(next_token) = tokens_iter.peek() {
                match next_token.token_type {
                    t if t.is_postfix_op() => {
                        let token = self.extract_token(tokens_iter)?;
                        let expr_ref = self.parse_postfix_expression(&token.token_type)?;
                        log::debug!(
                            "Parsed postfix expression: {}",
                            self.expr_pool.get_expr(expr_ref)
                        );
                    }
                    t => {
                        log::debug!("No postfix operator found, continuing with token: {:?}", t);
                        break;
                    }
                }
            } else {
                break;
            }
        })
    }

    fn parse_unop(&self, token: &Token) -> std::io::Result<UnaryOperator> {
        match token.token_type {
            TokenType::Tilde => Ok(UnaryOperator::Complement),
            TokenType::Hyphen => Ok(UnaryOperator::Negate),
            TokenType::Not => Ok(UnaryOperator::Not),
            TokenType::DoublePlus => Ok(UnaryOperator::PrefixIncr),
            TokenType::DoubleHyphens => Ok(UnaryOperator::PrefixDecr),
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
                TokenType::And => Ok(BinaryOperator::And),
                TokenType::Or => Ok(BinaryOperator::Or),
                TokenType::Equal => Ok(BinaryOperator::Equal),
                TokenType::NotEqual => Ok(BinaryOperator::NotEqual),
                TokenType::LessThan => Ok(BinaryOperator::LessThan),
                TokenType::LessThanEq => Ok(BinaryOperator::LessOrEqual),
                TokenType::GreaterThan => Ok(BinaryOperator::GreaterThan),
                TokenType::GreaterThanEq => Ok(BinaryOperator::GreaterOrEqual),
                TokenType::Assign => Ok(BinaryOperator::Assign),
                TokenType::AssignPlus => Ok(BinaryOperator::AssignPlus),
                TokenType::AssignMinus => Ok(BinaryOperator::AssignMinus),
                TokenType::AssignMult => Ok(BinaryOperator::AssignMult),
                TokenType::AssignDiv => Ok(BinaryOperator::AssignDiv),
                TokenType::AssignMod => Ok(BinaryOperator::AssignMod),
                TokenType::AssignAnd => Ok(BinaryOperator::AssignAnd),
                TokenType::AssignOr => Ok(BinaryOperator::AssignOr),
                TokenType::AssignXor => Ok(BinaryOperator::AssignXor),
                TokenType::AssignLeftShift => Ok(BinaryOperator::AssignLeftShift),
                TokenType::AssignRightShift => Ok(BinaryOperator::AssignRightShift),
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

    fn parse_postfix_expression(&mut self, token_type: &TokenType) -> std::io::Result<ExprRef> {
        let last_exp = ExprRef::new(self.expr_pool.len().saturating_sub(1) as u32);
        match token_type {
            TokenType::DoublePlus => {
                log::debug!("Parsing postfix increment operator");
                let expr_ref = self.expr_pool.add_expr(Expr::Unary(UnaryOperator::PostfixIncr, last_exp));
                Ok(expr_ref)
            }
            TokenType::DoubleHyphens => {
                log::debug!("Parsing postfix decrement operator");
                let expr_ref = self.expr_pool.add_expr(Expr::Unary(UnaryOperator::PostfixDecr, last_exp));
                Ok(expr_ref)
            }
            _ => {
                return Err(std::io::Error::new(
                    std::io::ErrorKind::InvalidInput,
                    format!("Expected postfix operator, found {:?}", token_type),
                ));
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        lexer::token::{Token, TokenType},
        parser::{
            c_ast::{Expr, ExprPool, ExprRef, Statement, UnaryOperator},
            c_parser::CParser,
        },
    };

    #[test]
    fn test_parse_statement_prefix_increment() {
        let tokens = vec![
            Token::new("++", 6, TokenType::DoublePlus),
            Token::new("a", 8, TokenType::Identifier),
            Token::new(";", 9, TokenType::Semicolon),
        ];
        let mut pool = ExprPool::new();
        let mut parser = CParser::new(&mut pool, &tokens);
        let res = parser
            .parse_statement(&mut tokens.iter().peekable())
            .unwrap();
        assert!(matches!(
            res,
            Statement::Expression(_)
        ));
        assert_eq!(pool.len(), 2);
        assert!(matches!(
            pool.get_expr(ExprRef::new(0)),
            Expr::Var(_)
        ));
        assert!(matches!(
            pool.get_expr(ExprRef::new(1)),
            Expr::Unary(UnaryOperator::PrefixIncr, _)
        ));
    }

    #[test]
    fn test_parse_statement_postfix_increment() {
        let tokens = vec![
            Token::new("a", 6, TokenType::Identifier),
            Token::new("++", 7, TokenType::DoublePlus),
            Token::new(";", 9, TokenType::Semicolon),
        ];
        let mut pool = ExprPool::new();
        let mut parser = CParser::new(&mut pool, &tokens);
        let res = parser
            .parse_statement(&mut tokens.iter().peekable())
            .unwrap();
        assert!(matches!(
            res,
            crate::parser::c_ast::Statement::Expression(_)
        ));
        println!("ExprPool: {:?}", pool);
        println!("First Expr: {:?}", pool.get_expr(ExprRef::new(0)));
        println!("Second Expr: {:?}", pool.get_expr(ExprRef::new(1)));
        assert_eq!(pool.len(), 2);
        assert!(matches!(
            pool.get_expr(ExprRef::new(0)),
            crate::parser::c_ast::Expr::Var(_)
        ));
        assert!(matches!(
            pool.get_expr(ExprRef::new(1)),
            crate::parser::c_ast::Expr::Unary(UnaryOperator::PostfixIncr, _)
        ));
    }

    #[test]
    fn test_parse_statement_postfix_decr_non_lvalue() {
        let tokens = vec![
            Token::new("return", 1, TokenType::ReturnKeyword),
            Token::new("a", 2, TokenType::Identifier),
            Token::new("++", 4, TokenType::DoublePlus),
            Token::new("--", 6, TokenType::DoubleHyphens),
            Token::new(";", 7, TokenType::Semicolon),
        ];
        let mut pool = ExprPool::new();
        let mut parser = CParser::new(&mut pool, &tokens);
        let res = parser
            .parse_statement(&mut tokens.iter().peekable())
            .unwrap();
        assert!(matches!(res, crate::parser::c_ast::Statement::Return(_)));
        println!("ExprPool: {:?}", pool);
        assert_eq!(pool.len(), 3);
        assert!(matches!(
            pool.get_expr(ExprRef::new(0)),
            crate::parser::c_ast::Expr::Var(_)
        ));
        assert!(matches!(
            pool.get_expr(ExprRef::new(1)),
            Expr::Unary(UnaryOperator::PostfixIncr, _)
        ));
        assert!(matches!(
            pool.get_expr(ExprRef::new(2)),
            Expr::Unary(UnaryOperator::PostfixDecr, _)
        ));
    }
}
