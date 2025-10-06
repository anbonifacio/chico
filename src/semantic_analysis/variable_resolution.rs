use std::collections::HashMap;

use crate::parser::c_ast::{Declaration, ExprPool, ExprRef, Identifier, Statement};

pub(crate) struct VariableResolver<'expr> {
    expr_pool: &'expr mut ExprPool,
    variable_map: HashMap<String, String>,
    counter: usize,
}

impl<'expr> VariableResolver<'expr> {
    pub fn new(expr_pool: &'expr mut ExprPool) -> Self {
        Self {
            expr_pool,
            variable_map: HashMap::new(),
            counter: 0,
        }
    }

    pub fn resolve_declaration(
        &mut self,
        declaration: &Declaration,
    ) -> std::io::Result<Declaration> {
        let name = declaration.name();
        log::debug!("Resolving declaration: {}", name);
        if self.variable_map.contains_key(name) {
            Err(std::io::Error::other(format!(
                "Duplicate variable declaration: {}",
                name
            )))
        } else {
            let init = declaration.initializer();
            let unique_name = self.make_temporary(name);
            self.variable_map
                .insert(name.to_string(), unique_name.clone());
            let init = if let Some(init) = init {
                Some(self.resolve_exp(&init)?)
            } else {
                None
            };

            Ok(Declaration::Declaration(
                Identifier::Name(unique_name),
                init,
            ))
        }
    }

    pub fn resolve_statement(
        &mut self,
        statement: &crate::parser::c_ast::Statement,
    ) -> std::io::Result<Statement> {
        match statement {
            Statement::Return(expr_ref) => {
                log::debug!("Resolving return statement: {:?}", expr_ref);
                Ok(Statement::Return(self.resolve_exp(expr_ref)?))
            }
            Statement::Expression(expr_ref) => {
                log::debug!("Resolving expression statement: {:?}", expr_ref);
                Ok(Statement::Expression(self.resolve_exp(expr_ref)?))
            }
            Statement::Null => Ok(Statement::Null),
        }
    }

    pub fn resolve_exp(
        &mut self,
        expr_ref: &crate::parser::c_ast::ExprRef,
    ) -> std::io::Result<ExprRef> {
        // Clone the expression out of the pool to avoid holding a borrow across recursion
        let expr = self.expr_pool.get_expr(expr_ref.id()).clone();
        log::debug!("Resolving expr: {:?}", expr);
        match expr {
            crate::parser::c_ast::Expr::Var(Identifier::Name(var_name)) => {
                log::debug!("Resolving Var: {}", var_name);
                if let Some(resolved_name) = self.variable_map.get(&var_name) {
                    let new_expr =
                        crate::parser::c_ast::Expr::Var(Identifier::Name(resolved_name.clone()));
                    self.expr_pool.update_expr(expr_ref, new_expr);
                    Ok(*expr_ref)
                } else {
                    Err(std::io::Error::other(format!(
                        "Undeclared variable: {}",
                        var_name
                    )))
                }
            }
            crate::parser::c_ast::Expr::Assignment(left_ref, right_ref) => {
                log::debug!("Resolving Assignment: {:?} = {:?}", left_ref, right_ref);
                // Clone left expression out of the pool
                let left = self.expr_pool.get_expr(left_ref.id()).clone();
                if !left.is_lvalue() {
                    return Err(std::io::Error::other(format!(
                        "Invalid lvalue: {:?}[{}]",
                        left.get_type(),
                        left
                    )));
                }
                let new_left = self.resolve_exp(&left_ref)?;
                let new_right = self.resolve_exp(&right_ref)?;
                let new_expr = crate::parser::c_ast::Expr::Assignment(new_left, new_right);
                self.expr_pool.update_expr(expr_ref, new_expr);
                Ok(*expr_ref)
            }
            crate::parser::c_ast::Expr::Constant(c) => {
                log::debug!("Resolving Constant: {:?}", c);
                let new_expr = crate::parser::c_ast::Expr::Constant(c);
                self.expr_pool.update_expr(expr_ref, new_expr);
                Ok(*expr_ref)
            }
            crate::parser::c_ast::Expr::Unary(unary_operator, inner_expr_ref) => {
                log::debug!("Resolving Unary: {:?}", inner_expr_ref);
                if unary_operator.is_lvalue_op() {
                    let inner = self.expr_pool.get_expr(inner_expr_ref.id()).clone();
                    log::debug!("Inner expr for lvalue check: {:?}", inner);
                    if !inner.is_lvalue() {
                        return Err(std::io::Error::other(format!(
                            "Invalid lvalue: {:?}[{}]",
                            inner.get_type(),
                            inner
                        )));
                    }
                }
                let new_inner = self.resolve_exp(&inner_expr_ref)?;
                let new_expr = crate::parser::c_ast::Expr::Unary(unary_operator, new_inner);
                self.expr_pool.update_expr(expr_ref, new_expr);
                Ok(*expr_ref)
            }
            crate::parser::c_ast::Expr::Binary(binary_operator, left_ref, right_ref) => {
                let new_left = self.resolve_exp(&left_ref)?;
                let new_right = self.resolve_exp(&right_ref)?;
                let new_expr =
                    crate::parser::c_ast::Expr::Binary(binary_operator, new_left, new_right);
                self.expr_pool.update_expr(expr_ref, new_expr);
                Ok(*expr_ref)
            }
        }
    }

    fn make_temporary(&mut self, name: &str) -> String {
        let count = self.counter;
        self.counter += 1;
        format!("{}:{}", name, count)
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::c_lexer;
    use crate::parser::c_ast::ExprPool;
    use crate::parser::c_parser::CParser;
    use crate::semantic_analysis::SemanticAnalysis;

    #[test]
    fn test_postfix_chain_invalid_lvalue() {
        // C code that should fail semantic analysis
        let code = r#"
            int main(void) {
                int a = 10;
                return a++--;
            }
        "#;

        // Set up lexer
        let mut lexer = c_lexer::Lexer::from_bytes(code.as_bytes());
        let tokens = lexer.tokenize().unwrap();

        // Set up parser and expression pool
        let mut expr_pool = ExprPool::new();
        let mut parser = CParser::new(&mut expr_pool, &tokens);

        // Parse the program
        let program = parser.parse_program().unwrap();

        // Run semantic analysis
        let mut semantic = SemanticAnalysis::new(&mut expr_pool);
        let result = semantic.analyze_program(program);

        // Assert that semantic analysis fails with "invalid lvalue"
        assert!(result.is_err(), "Semantic analysis should fail for a++--");
        let err = result.unwrap_err();
        let msg = format!("{}", err);
        assert!(
            msg.contains("Invalid lvalue"),
            "Error message should mention 'Invalid lvalue', got: {}",
            msg
        );
    }

    #[test]
    fn test_postfix_incr_on_assignment_invalid_lvalue() {
        let code = r#"
            int main(void) {
                int a = 0;
                (a = 4)++;
            }
        "#;

        // Set up lexer
        let mut lexer = c_lexer::Lexer::from_bytes(code.as_bytes());
        let tokens = lexer.tokenize().unwrap();

        // Set up parser and expression pool
        let mut expr_pool = ExprPool::new();
        let mut parser = CParser::new(&mut expr_pool, &tokens);

        // Parse the program
        let program = parser.parse_program().unwrap();

        let mut semantic = SemanticAnalysis::new(&mut expr_pool);
        let result = semantic.analyze_program(program);

        assert!(
            result.is_err(),
            "Semantic analysis should fail for (a = 4)++"
        );
        let err = result.unwrap_err();
        let msg = format!("{}", err);
        assert!(
            msg.contains("Invalid lvalue"),
            "Error message should mention 'Invalid lvalue', got: {}",
            msg
        );
    }
}
