use std::fmt::{Display, Formatter};

use crate::parser::c_ast::{BinaryOperator, Expr, Statement, UnaryOperator};

#[derive(Debug)]
pub struct NodesPool {
    expr_pool: ExprPool,
    statements_pool: StatementsPool,
}

impl NodesPool {
    pub fn new() -> Self {
        NodesPool {
            expr_pool: ExprPool::new(),
            statements_pool: StatementsPool::new(),
        }
    }

    pub fn expr_pool(&self) -> &ExprPool {
        &self.expr_pool
    }

    pub fn statements_pool(&self) -> &StatementsPool {
        &self.statements_pool
    }

    pub fn expr_pool_mut(&mut self) -> &mut ExprPool {
        &mut self.expr_pool
    }

    pub fn statements_pool_mut(&mut self) -> &mut StatementsPool {
        &mut self.statements_pool
    }
}

#[derive(Debug)]
pub struct StatementsPool(Vec<Statement>);

impl StatementsPool {
    pub fn new() -> Self {
        StatementsPool(Vec::new())
    }

    #[cfg(test)]
    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn get_statement(&self, id: u32) -> &Statement {
        &self.0[id as usize]
    }

    pub fn add_statement(&mut self, statement: Statement) -> StatementRef {
        let id = self.0.len() as u32;
        self.0.push(statement);
        StatementRef { id }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct StatementRef {
    id: u32,
}

impl StatementRef {
    pub fn new(id: u32) -> Self {
        StatementRef { id }
    }

    pub fn id(&self) -> u32 {
        self.id
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ExprRef {
    id: u32,
    expr_type: ExprType,
}

impl ExprRef {
    pub fn new(id: u32, expr_type: ExprType) -> Self {
        ExprRef { id, expr_type }
    }

    pub fn id(&self) -> u32 {
        self.id
    }

    pub fn expr_type(&self) -> ExprType {
        self.expr_type
    }
}

impl Display for ExprRef {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "ExprRef[{}: {:?}]", self.id, self.expr_type)
    }
}

#[derive(Debug)]
pub struct ExprPool(Vec<Expr>);

impl ExprPool {
    pub fn new() -> Self {
        ExprPool(Vec::new())
    }

    #[cfg(test)]
    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn get_expr(&self, id: u32) -> &Expr {
        &self.0[id as usize]
    }

    pub fn add_expr(&mut self, expr: Expr) -> ExprRef {
        let id = self.0.len() as u32;
        self.0.push(expr.clone());
        ExprRef {
            id,
            expr_type: expr.get_type(),
        }
    }

    pub fn update_expr(&mut self, id: &ExprRef, expr: Expr) {
        self.0[id.id as usize] = expr;
    }

    pub(crate) fn last_expr(&self) -> std::io::Result<ExprRef> {
        match self.0.iter().enumerate().next_back() {
            Some((idx, expr)) => Ok(ExprRef {
                id: idx as u32,
                expr_type: expr.get_type(),
            }),
            None => Err(std::io::Error::other(
                "Expression pool is empty, cannot get last expression",
            )),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ExprType {
    Constant,
    Var,
    Unary(UnaryOperator),
    Binary(BinaryOperator),
    Assignment,
}
