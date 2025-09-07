use crate::codegen::asm_ast::{
    AsmProgram, FunctionDefinition, Identifier, Instruction, Operand, RegisterType,
};
use crate::parser::c_ast::{CProgram, Expr, ExprPool, ExprRef, Statement};

pub struct Codegen<'expr> {
    expr_pool: &'expr mut ExprPool,
}

impl<'expr> Codegen<'expr> {
    pub fn new(expr_pool: &'expr mut ExprPool) -> Self {
        Codegen { expr_pool }
    }

    pub fn generate_asm_ast(&self, c_program: &CProgram) -> std::io::Result<AsmProgram> {
        let asm_function = self.generate_asm_function(c_program)?;
        Ok(AsmProgram::Program(asm_function))
    }

    fn generate_asm_function(&self, c_program: &CProgram) -> std::io::Result<FunctionDefinition> {
        let c_function = c_program.get_fn();
        let name = c_function.get_identifier().get_name();
        let body = c_function.get_body();
        let instructions = self.generate_asm_instructions(body)?;
        Ok(FunctionDefinition::new(
            Identifier::Name(name.to_string()),
            instructions,
        ))
    }

    fn generate_asm_instructions(&self, body: &Statement) -> std::io::Result<Vec<Instruction>> {
        let mut instructions = Vec::<Instruction>::new();
        match body {
            Statement::Return(expr_ref) => {
                self.generate_asm_instructions_for_expr(expr_ref, &mut instructions)?
            }
        };
        Ok(instructions)
    }

    fn generate_asm_instructions_for_expr(
        &self,
        expr_ref: &ExprRef,
        instructions: &mut Vec<Instruction>,
    ) -> std::io::Result<()> {
        let expr = self.expr_pool.get_expr(*expr_ref);
        match expr {
            Expr::Constant(int) => {
                instructions.push(Instruction::Mov(
                    Operand::Imm(*int),
                    Operand::Register(RegisterType::Eax),
                ));
                instructions.push(Instruction::Ret);
            }
            Expr::Unary(_, _) => todo!("Implement unary expression"),
        }
        Ok(())
    }
}
