use crate::parser::c_ast;
use crate::parser::c_ast::{CProgram, ExprPool, ExprRef, Statement};
use crate::tacky_ir::tacky_ast;
use crate::tacky_ir::tacky_ast::*;

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
        let identifier = ast.fn_def().identifier().name();
        let body = ast.fn_def().body();
        let instructions = self.generate_instructions(body)?;

        Ok(FunctionDefinition::Function(
            Identifier::Name(identifier.to_string()),
            instructions,
        ))
    }

    fn generate_instructions(&self, body: &Statement) -> std::io::Result<Instructions> {
        let mut instructions = Instructions::new();
        match body {
            Statement::Return(expr_ref) => {
                let _ = self.emit_tacky(expr_ref, &mut instructions)?;
            }
        }
        Ok(instructions)
    }

    fn emit_tacky(
        &self,
        expr_ref: &ExprRef,
        instructions: &mut Instructions,
    ) -> std::io::Result<Val> {
        let expr = self.expr_pool.get_expr(*expr_ref);
        let dst = match expr {
            crate::parser::c_ast::Expr::Constant(c) => {
                instructions.append(Instruction::Return(Val::Constant(*c)));
                Val::Constant(*c)
            }
            crate::parser::c_ast::Expr::Unary(operator, inner_expr_ref) => {
                let src = self.emit_tacky(inner_expr_ref, instructions)?;
                let dst_name = self.make_temporary(inner_expr_ref)?;
                let dst = Val::Var(Identifier::Name(dst_name.clone()));
                let tacky_op = self.convert_unop(operator)?;
                instructions.append(Instruction::Unary(tacky_op, src, dst));
                Val::Var(Identifier::Name(dst_name))
            }
        };
        Ok(dst)
    }

    fn make_temporary(&self, expr_ref: &ExprRef) -> std::io::Result<String> {
        Ok(format!("tmp.{}", expr_ref.id()))
    }

    fn convert_unop(
        &self,
        operator: &c_ast::UnaryOperator,
    ) -> std::io::Result<tacky_ast::UnaryOperator> {
        match operator {
            c_ast::UnaryOperator::Negate => Ok(tacky_ast::UnaryOperator::Negate),
            c_ast::UnaryOperator::Complement => Ok(tacky_ast::UnaryOperator::Complement),
        }
    }
}
