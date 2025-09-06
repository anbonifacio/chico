use crate::parser::c_ast::ExprPool;

pub struct TackyGenerator<'expr> {
    expr_pool: &'expr ExprPool,
}

impl<'expr> TackyGenerator<'expr> {
    pub fn new(expr_pool: &'expr ExprPool) -> Self {
        TackyGenerator { expr_pool }
    }
}
