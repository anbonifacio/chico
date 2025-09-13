use crate::parser::c_ast;
use crate::parser::c_ast::{CProgram, ExprPool, ExprRef, Statement};
use crate::tacky_ir::tacky_ast;
use crate::tacky_ir::tacky_ast::*;

pub struct TackyGenerator<'expr> {
    expr_pool: &'expr ExprPool,
}

impl<'expr> TackyGenerator<'expr> {
    pub fn new(expr_pool: &'expr ExprPool) -> Self {
        log::debug!("expr pool: {:?}", expr_pool);
        TackyGenerator { expr_pool }
    }

    pub fn generate_ir(&self, ast: &CProgram) -> TackyIR {
        let ir = self.generate_function_ir(ast);
        TackyIR::Program(ir)
    }

    pub fn generate_function_ir(&self, ast: &CProgram) -> FunctionDefinition {
        let identifier = ast.fn_def().identifier().name();
        let body = ast.fn_def().body();
        let instructions = self.generate_instructions(body);

        FunctionDefinition::Function(Identifier::Name(identifier.to_string()), instructions)
    }

    fn generate_instructions(&self, body: &Statement) -> Instructions {
        let mut instructions = Instructions::new();
        match body {
            Statement::Return(expr_ref) => {
                let expr = self.emit_tacky(expr_ref, &mut instructions);
                instructions.append(Instruction::Return(expr));
            }
        }
        instructions
    }

    fn emit_tacky(&self, expr_ref: &ExprRef, instructions: &mut Instructions) -> Val {
        let expr = self.expr_pool.get_expr(*expr_ref);
        match expr {
            c_ast::Expr::Unary(operator, inner_expr_ref) => {
                let src = self.emit_tacky(inner_expr_ref, instructions);
                let dst_name = self.make_temporary(inner_expr_ref);
                let dst = Val::Var(Identifier::Name(dst_name.clone()));
                let tacky_op = self.convert_unop(operator);
                instructions.append(Instruction::Unary(tacky_op, src, dst.clone()));
                dst
            }
            c_ast::Expr::Binary(binary_operator, left, right) => {
                todo!()
            }
            c_ast::Expr::Constant(c) => Val::Constant(*c),
        }
    }

    fn make_temporary(&self, expr_ref: &ExprRef) -> String {
        format!("tmp.{}", expr_ref.id())
    }

    fn convert_unop(&self, operator: &c_ast::UnaryOperator) -> tacky_ast::UnaryOperator {
        match operator {
            c_ast::UnaryOperator::Negate => tacky_ast::UnaryOperator::Negate,
            c_ast::UnaryOperator::Complement => tacky_ast::UnaryOperator::Complement,
        }
    }
}
