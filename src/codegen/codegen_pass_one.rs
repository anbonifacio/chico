use crate::codegen::asm_ast::*;
use crate::tacky_ir::tacky_ast;
use crate::tacky_ir::tacky_ast::{Instructions, TackyIR};

pub struct CodegenPassOne;

impl CodegenPassOne {
    pub fn new() -> Self {
        CodegenPassOne {}
    }

    pub fn generate_asm_ast(&self, tacky_ir: &TackyIR) -> std::io::Result<AsmProgram> {
        let asm_function = self.generate_asm_function(tacky_ir)?;
        Ok(AsmProgram::Program(asm_function))
    }

    fn generate_asm_function(&self, tacky_ir: &TackyIR) -> std::io::Result<FunctionDefinition> {
        let c_function = tacky_ir.fn_def();
        let name = c_function.identifier().name();
        let body = c_function.body();
        let instructions = self.generate_asm_instructions(body)?;
        Ok(FunctionDefinition::new(
            Identifier::Name(name.to_string()),
            instructions,
        ))
    }

    fn generate_asm_instructions(&self, body: &Instructions) -> std::io::Result<Vec<Instruction>> {
        let mut asm_instructions = Vec::<Instruction>::new();
        for instruction in body.as_slice().iter() {
            self.generate_asm_instructions_for_val(instruction, &mut asm_instructions)?
        }
        Ok(asm_instructions)
    }

    fn generate_asm_instructions_for_val(
        &self,
        instruction: &tacky_ast::Instruction,
        asm_instructions: &mut Vec<Instruction>,
    ) -> std::io::Result<()> {
        match instruction {
            tacky_ast::Instruction::Return(val) => {
                asm_instructions.push(Instruction::Mov(
                    Operand::Imm(val.constant()?),
                    Operand::Reg(RegisterType::AX),
                ));
                asm_instructions.push(Instruction::Ret);
            }
            tacky_ast::Instruction::Unary(operator, src, dst) => {
                let src = if let Some(src) = src.var().ok() {
                    Operand::Pseudo(Identifier::Name(src))
                } else {
                    Operand::Imm(src.constant()?)
                };
                asm_instructions.push(Instruction::Mov(
                    src,
                    Operand::Pseudo(Identifier::Name(dst.var()?)),
                ));
                let operator = match operator {
                    tacky_ast::UnaryOperator::Negate => UnaryOperator::Neg,
                    tacky_ast::UnaryOperator::Complement => UnaryOperator::Not,
                };
                asm_instructions.push(Instruction::Unary(
                    operator,
                    Operand::Pseudo(Identifier::Name(dst.var()?)),
                ));
            }
        }
        Ok(())
    }
}
