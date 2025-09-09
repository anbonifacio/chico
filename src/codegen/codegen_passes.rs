use std::collections::HashMap;

use crate::codegen::asm_ast::*;
use crate::tacky_ir::tacky_ast;
use crate::tacky_ir::tacky_ast::{Instructions, TackyIR};

#[derive(Debug)]
struct StackOffset(i32);

pub struct Codegen {
    stack_offset: i32,
    pseudo_reg_map: HashMap<Operand, StackOffset>,
}

impl Codegen {
    pub fn new() -> Self {
        Codegen {
            stack_offset: -4,
            pseudo_reg_map: HashMap::new(),
        }
    }

    pub fn generate_asm_ast(&mut self, tacky_ir: &TackyIR) -> std::io::Result<AsmProgram> {
        // Pass 1: generate asm AST from tacky IR
        let asm_function = self.generate_asm_function(tacky_ir)?;

        // Pass 2: replace pseudo registers
        let asm_program_and_stack =
            self.replace_pseudo_registers(&AsmProgram::Program(asm_function))?;

        // Pass 3: fixing up instructions and return asm_program
        self.fixup_instructions(&asm_program_and_stack.0, &asm_program_and_stack.1)
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
                let src = if let Ok(src) = src.var() {
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

    fn replace_pseudo_registers(
        &mut self,
        asm_program: &AsmProgram,
    ) -> std::io::Result<(AsmProgram, StackOffset)> {
        let fn_def = asm_program.fn_def();
        let identifier = Identifier::Name(fn_def.name());
        let instructions = fn_def.instructions();
        let mut new_instructions = Vec::new();
        for instruction in instructions {
            let new_instruction = match instruction {
                Instruction::Mov(src, dst) => {
                    let src = self.match_operand(src)?;
                    let dst = self.match_operand(dst)?;
                    Instruction::Mov(src, dst)
                }
                Instruction::Unary(operator, pseudo_reg) => {
                    let operator = match operator {
                        UnaryOperator::Neg => UnaryOperator::Neg,
                        UnaryOperator::Not => UnaryOperator::Not,
                    };
                    let pseudo_reg = pseudo_reg.get_pseudo_identifier()?;
                    let stack = self.calculate_stack_location(pseudo_reg)?;
                    Instruction::Unary(operator, stack)
                }
                Instruction::AllocateStack(int) => Instruction::AllocateStack(*int),
                Instruction::Ret => Instruction::Ret,
            };
            new_instructions.push(new_instruction);
        }
        let stack_offset = self.get_stack_offset()?;
        Ok((
            AsmProgram::Program(FunctionDefinition::new(identifier, new_instructions)),
            StackOffset(stack_offset),
        ))
    }

    fn match_operand(&mut self, operand: &Operand) -> std::io::Result<Operand> {
        let op = match operand.get_type()? {
            Operand::Imm(int) => Operand::Imm(int),
            Operand::Reg(register_type) => Operand::Reg(register_type),
            Operand::Pseudo(identifier) => {
                self.calculate_stack_location(Operand::Pseudo(identifier))?
            }
            Operand::Stack(offset) => Operand::Stack(offset),
        };
        Ok(op)
    }

    fn calculate_stack_location(&mut self, operand: Operand) -> std::io::Result<Operand> {
        if self.pseudo_reg_map.contains_key(&operand) {
            let stack_location = self
                .pseudo_reg_map
                .get(&operand)
                .ok_or_else(|| std::io::Error::other("Pseudo register not found"))?;
            Ok(Operand::Stack(stack_location.0))
        } else {
            let offset = self.stack_offset;
            let new_offset = self.set_stack_offset(offset);
            self.pseudo_reg_map.insert(operand, StackOffset(new_offset));
            Ok(Operand::Stack(offset))
        }
    }

    fn get_stack_offset(&self) -> std::io::Result<i32> {
        Ok(self.stack_offset)
    }

    fn set_stack_offset(&mut self, offset: i32) -> i32 {
        self.stack_offset = offset - 4;
        self.stack_offset
    }

    fn fixup_instructions(
        &self,
        asm_program: &AsmProgram,
        stack_offset: &StackOffset,
    ) -> std::io::Result<AsmProgram> {
        let fn_def = asm_program.fn_def();
        let identifier = Identifier::Name(fn_def.name());
        let instructions = fn_def.instructions();
        let mut new_instructions = Vec::new();
        new_instructions.push(Instruction::AllocateStack(stack_offset.0));

        for instruction in instructions {
            match instruction {
                Instruction::Mov(src, dst) => {
                    // check if both src and dst are Stack, if so split in 2 movs using register R10
                    match (src, dst) {
                        (Operand::Stack(_), Operand::Stack(_)) => {
                            let r10 = Operand::Reg(RegisterType::R10);
                            new_instructions.push(Instruction::Mov(src.clone(), r10.clone()));
                            new_instructions.push(Instruction::Mov(r10.clone(), dst.clone()));
                        }
                        _ => new_instructions.push(Instruction::Mov(src.clone(), dst.clone())),
                    }
                }
                _ => new_instructions.push(instruction.clone()),
            };
        }

        Ok(AsmProgram::Program(FunctionDefinition::new(
            identifier,
            new_instructions,
        )))
    }
}
