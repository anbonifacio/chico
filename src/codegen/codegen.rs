use crate::codegen::asm_ast::{AsmProgram, FunctionDefinition, Identifier, Instruction, Operand};
use crate::parser::c_ast::{CProgram, Exp, Statement};

pub struct Codegen;

impl Codegen {
    pub fn new() -> Self {
        Codegen {}
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
        let instructions = match body {
            Statement::Return(expr) => self.generate_asm_instructions_for_return(expr)?,
        };
        Ok(instructions)
    }

    fn generate_asm_instructions_for_return(
        &self,
        expr: &Exp,
    ) -> std::io::Result<Vec<Instruction>> {
        let mut instructions = Vec::new();
        match expr {
            Exp::Constant(int) => {
                instructions.push(Instruction::Mov(Operand::Imm(*int), Operand::Register));
                instructions.push(Instruction::Ret);
            }
        }
        Ok(instructions)
    }
}
