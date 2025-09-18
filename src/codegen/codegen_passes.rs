use std::collections::HashMap;
use std::hint::unreachable_unchecked;

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
                match val {
                    tacky_ast::Val::Constant(int) => {
                        asm_instructions.push(Instruction::Mov(
                            Operand::Imm(*int),
                            Operand::Reg(RegisterType::AX),
                        ));
                    }
                    tacky_ast::Val::Var(val) => {
                        asm_instructions.push(Instruction::Mov(
                            Operand::Pseudo(Identifier::Name(val.name().to_string())),
                            Operand::Reg(RegisterType::AX),
                        ));
                    }
                }
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
            tacky_ast::Instruction::Binary(tacky_ast::BinaryOperator::Divide, src1, src2, dst) => {
                let src1 = if let Ok(src) = src1.var() {
                    Operand::Pseudo(Identifier::Name(src))
                } else {
                    Operand::Imm(src1.constant()?)
                };
                asm_instructions.push(Instruction::Mov(src1, Operand::Reg(RegisterType::AX)));
                asm_instructions.push(Instruction::Cdq);
                let src2 = if let Ok(src) = src2.var() {
                    Operand::Pseudo(Identifier::Name(src))
                } else {
                    Operand::Imm(src2.constant()?)
                };
                asm_instructions.push(Instruction::Idiv(src2));
                asm_instructions.push(Instruction::Mov(
                    Operand::Reg(RegisterType::AX),
                    Operand::Pseudo(Identifier::Name(dst.var()?)),
                ));
            }
            tacky_ast::Instruction::Binary(
                tacky_ast::BinaryOperator::Reminder,
                src1,
                src2,
                dst,
            ) => {
                let src1 = if let Ok(src) = src1.var() {
                    Operand::Pseudo(Identifier::Name(src))
                } else {
                    Operand::Imm(src1.constant()?)
                };
                asm_instructions.push(Instruction::Mov(src1, Operand::Reg(RegisterType::AX)));
                asm_instructions.push(Instruction::Cdq);
                let src2 = if let Ok(src) = src2.var() {
                    Operand::Pseudo(Identifier::Name(src))
                } else {
                    Operand::Imm(src2.constant()?)
                };
                asm_instructions.push(Instruction::Idiv(src2));
                asm_instructions.push(Instruction::Mov(
                    Operand::Reg(RegisterType::DX),
                    Operand::Pseudo(Identifier::Name(dst.var()?)),
                ));
            }
            tacky_ast::Instruction::Binary(operator, src1, src2, dst) => {
                let src1 = if let Ok(src) = src1.var() {
                    Operand::Pseudo(Identifier::Name(src))
                } else {
                    Operand::Imm(src1.constant()?)
                };
                asm_instructions.push(Instruction::Mov(
                    src1,
                    Operand::Pseudo(Identifier::Name(dst.var()?)),
                ));
                let src2 = if let Ok(src) = src2.var() {
                    Operand::Pseudo(Identifier::Name(src))
                } else {
                    Operand::Imm(src2.constant()?)
                };
                asm_instructions.push(Instruction::Mov(
                    src2.clone(),
                    Operand::Pseudo(Identifier::Name("tmp.BINOP".to_string())),
                ));
                let op = match operator {
                    tacky_ast::BinaryOperator::Add => BinaryOperator::Add,
                    tacky_ast::BinaryOperator::Subtract => BinaryOperator::Sub,
                    tacky_ast::BinaryOperator::Multiply => BinaryOperator::Mult,
                    _ => unsafe {
                        // Safety: Divide and Reminder are already matched on their own
                        unreachable_unchecked()
                    },
                };
                asm_instructions.push(Instruction::Binary(
                    op,
                    src2,
                    Operand::Pseudo(Identifier::Name(dst.var()?)),
                ))
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
                    let stack = self.match_operand(pseudo_reg)?;
                    Instruction::Unary(operator, stack)
                }
                Instruction::AllocateStack(int) => Instruction::AllocateStack(*int),
                Instruction::Ret => Instruction::Ret,
                Instruction::Binary(operator, src, dst) => {
                    let operator = match operator {
                        BinaryOperator::Add => BinaryOperator::Add,
                        BinaryOperator::Sub => BinaryOperator::Sub,
                        BinaryOperator::Mult => BinaryOperator::Mult,
                    };
                    let stack1 = self.match_operand(src)?;
                    let stack2 = self.match_operand(dst)?;
                    Instruction::Binary(operator, stack1, stack2)
                }
                Instruction::Idiv(operand) => {
                    let op = self.match_operand(operand)?;
                    Instruction::Idiv(op)
                }
                Instruction::Cdq => Instruction::Cdq,
            };
            new_instructions.push(new_instruction);
        }
        let stack_offset = self.get_current_stack_offset();
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
        match operand {
            Operand::Pseudo(_) => {
                if self.pseudo_reg_map.contains_key(&operand) {
                    let stack_location = self
                        .pseudo_reg_map
                        .get(&operand)
                        .ok_or_else(|| std::io::Error::other("Pseudo register not found"))?;
                    Ok(Operand::Stack(stack_location.0))
                } else {
                    let offset = self.get_current_stack_offset();
                    log::debug!("Set {:?} at offset {}", &operand, offset);
                    self.pseudo_reg_map.insert(operand, StackOffset(offset));
                    self.increase_stack_offset();
                    Ok(Operand::Stack(offset))
                }
            }
            _ => unimplemented!("Operand {:?} is not a pseudo register", operand),
        }
    }

    fn get_current_stack_offset(&self) -> i32 {
        self.stack_offset
    }

    fn increase_stack_offset(&mut self) -> i32 {
        self.stack_offset -= 4;
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
                // fixup Add, Sub, Mult instructions
                Instruction::Binary(binop, src, dst) => match binop {
                    BinaryOperator::Add => {
                        let r10 = Operand::Reg(RegisterType::R10);
                        new_instructions.push(Instruction::Mov(src.clone(), r10.clone()));
                        new_instructions.push(Instruction::Binary(
                            BinaryOperator::Add,
                            r10.clone(),
                            dst.clone(),
                        ));
                    }
                    BinaryOperator::Sub => {
                        let r10 = Operand::Reg(RegisterType::R10);
                        new_instructions.push(Instruction::Mov(src.clone(), r10.clone()));
                        new_instructions.push(Instruction::Binary(
                            BinaryOperator::Sub,
                            r10.clone(),
                            dst.clone(),
                        ));
                    }
                    BinaryOperator::Mult => {
                        let r11 = Operand::Reg(RegisterType::R11);
                        new_instructions.push(Instruction::Mov(dst.clone(), r11.clone()));
                        new_instructions.push(Instruction::Binary(
                            BinaryOperator::Mult,
                            src.clone(),
                            r11.clone(),
                        ));
                        new_instructions.push(Instruction::Mov(r11.clone(), dst.clone()));
                    }
                },
                // fixup Idiv instructions
                Instruction::Idiv(op) => {
                    let r10 = Operand::Reg(RegisterType::R10);
                    new_instructions.push(Instruction::Mov(op.clone(), r10.clone()));
                    new_instructions.push(Instruction::Idiv(r10.clone()));
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
