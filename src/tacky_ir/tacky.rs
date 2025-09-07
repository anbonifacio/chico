use crate::parser::c_ast::{CProgram, ExprPool, ExprRef, Statement};
use crate::tacky_ir::tacky_ast::{
    FunctionDefinition, Identifier, Instruction, Instructions, TackyIR, Val,
};

pub struct TackyGenerator<'expr> {
    expr_pool: &'expr ExprPool,
}

impl<'expr> TackyGenerator<'expr> {
    pub fn new(expr_pool: &'expr ExprPool) -> Self {
        TackyGenerator { expr_pool }
    }

    pub fn generate_ir(&self, ast: &CProgram) -> std::io::Result<TackyIR> {
        let ir = self.generate_function_ir(ast)?;
        Ok(TackyIR::Program(ir))
    }

    pub fn generate_function_ir(&self, ast: &CProgram) -> std::io::Result<FunctionDefinition> {
        let identifier = ast.get_fn().get_identifier().get_name();
        let body = ast.get_fn().get_body();
        let instructions = self.generate_instructions(body)?;

        Ok(FunctionDefinition::Function(
            Identifier::Name(identifier.to_string()),
            instructions,
        ))
    }

    fn generate_instructions(&self, body: &Statement) -> std::io::Result<Instructions> {
        let mut instructions = Instructions::new();
        match body {
            Statement::Return(expr_ref) => self.emit_tacky(expr_ref, &mut instructions)?,
        }
        Ok(instructions)
    }

    fn emit_tacky(
        &self,
        expr_ref: &ExprRef,
        instructions: &mut Instructions,
    ) -> std::io::Result<()> {
        let expr = self.expr_pool.get_expr(*expr_ref);
        match expr {
            crate::parser::c_ast::Expr::Constant(c) => {
                instructions.append(Instruction::Return(Val::Constant(*c)));
            }
            crate::parser::c_ast::Expr::Unary(unary_operator, inner_expr_ref) => todo!(),
        }
        Ok(())
    }
}
