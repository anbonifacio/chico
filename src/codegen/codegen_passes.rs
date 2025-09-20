use std::collections::HashMap;
use std::hint::unreachable_unchecked;

use crate::codegen::asm_ast::*;
use crate::tacky_ir::tacky_ast;
use crate::tacky_ir::tacky_ast::BinaryOperator::{
    Divide, Equal, GreaterOrEqual, GreaterThan, LessOrEqual, LessThan, NotEqual, Remainder,
};
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
                            Operand::Reg(RegisterType::AX(RegisterSize::Four)),
                        ));
                    }
                    tacky_ast::Val::Var(val) => {
                        asm_instructions.push(Instruction::Mov(
                            Operand::Pseudo(Identifier::Name(val.name().to_string())),
                            Operand::Reg(RegisterType::AX(RegisterSize::Four)),
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
                match operator {
                    tacky_ast::UnaryOperator::Negate => {
                        asm_instructions.push(Instruction::Mov(
                            src,
                            Operand::Pseudo(Identifier::Name(dst.var()?)),
                        ));
                        asm_instructions.push(Instruction::Unary(
                            UnaryOperator::Neg,
                            Operand::Pseudo(Identifier::Name(dst.var()?)),
                        ));
                    }
                    tacky_ast::UnaryOperator::Complement => {
                        asm_instructions.push(Instruction::Mov(
                            src,
                            Operand::Pseudo(Identifier::Name(dst.var()?)),
                        ));
                        asm_instructions.push(Instruction::Unary(
                            UnaryOperator::Not,
                            Operand::Pseudo(Identifier::Name(dst.var()?)),
                        ));
                    }
                    tacky_ast::UnaryOperator::Not => {
                        let dst = if let Ok(dst) = dst.var() {
                            Operand::Pseudo(Identifier::Name(dst))
                        } else {
                            Operand::Imm(dst.constant()?)
                        };
                        asm_instructions.push(Instruction::Cmp(Operand::Imm(0), src));
                        asm_instructions.push(Instruction::Mov(Operand::Imm(0), dst.clone()));
                        asm_instructions.push(Instruction::SetCC(CondCode::E, dst));
                    }
                }
            }
            tacky_ast::Instruction::Binary(Equal, src1, src2, dst) => {
                self.generate_relational_operator(asm_instructions, CondCode::E, src1, src2, dst)?;
            }
            tacky_ast::Instruction::Binary(NotEqual, src1, src2, dst) => {
                self.generate_relational_operator(asm_instructions, CondCode::NE, src1, src2, dst)?;
            }
            tacky_ast::Instruction::Binary(GreaterThan, src1, src2, dst) => {
                self.generate_relational_operator(asm_instructions, CondCode::G, src1, src2, dst)?;
            }
            tacky_ast::Instruction::Binary(GreaterOrEqual, src1, src2, dst) => {
                self.generate_relational_operator(asm_instructions, CondCode::GE, src1, src2, dst)?;
            }
            tacky_ast::Instruction::Binary(LessThan, src1, src2, dst) => {
                self.generate_relational_operator(asm_instructions, CondCode::L, src1, src2, dst)?;
            }
            tacky_ast::Instruction::Binary(LessOrEqual, src1, src2, dst) => {
                self.generate_relational_operator(asm_instructions, CondCode::LE, src1, src2, dst)?;
            }
            tacky_ast::Instruction::Binary(Divide, src1, src2, dst) => {
                let src1 = if let Ok(src) = src1.var() {
                    Operand::Pseudo(Identifier::Name(src))
                } else {
                    Operand::Imm(src1.constant()?)
                };
                asm_instructions.push(Instruction::Mov(
                    src1,
                    Operand::Reg(RegisterType::AX(RegisterSize::Four)),
                ));
                asm_instructions.push(Instruction::Cdq);
                let src2 = if let Ok(src) = src2.var() {
                    Operand::Pseudo(Identifier::Name(src))
                } else {
                    Operand::Imm(src2.constant()?)
                };
                asm_instructions.push(Instruction::Idiv(src2));
                asm_instructions.push(Instruction::Mov(
                    Operand::Reg(RegisterType::AX(RegisterSize::Four)),
                    Operand::Pseudo(Identifier::Name(dst.var()?)),
                ));
            }
            tacky_ast::Instruction::Binary(Remainder, src1, src2, dst) => {
                let src1 = if let Ok(src) = src1.var() {
                    Operand::Pseudo(Identifier::Name(src))
                } else {
                    Operand::Imm(src1.constant()?)
                };
                asm_instructions.push(Instruction::Mov(
                    src1,
                    Operand::Reg(RegisterType::AX(RegisterSize::Four)),
                ));
                asm_instructions.push(Instruction::Cdq);
                let src2 = if let Ok(src) = src2.var() {
                    Operand::Pseudo(Identifier::Name(src))
                } else {
                    Operand::Imm(src2.constant()?)
                };
                asm_instructions.push(Instruction::Idiv(src2));
                asm_instructions.push(Instruction::Mov(
                    Operand::Reg(RegisterType::DX(RegisterSize::Four)),
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
                    tacky_ast::BinaryOperator::BitwiseAnd => BinaryOperator::BitwiseAnd,
                    tacky_ast::BinaryOperator::BitwiseOr => BinaryOperator::BitwiseOr,
                    tacky_ast::BinaryOperator::BitwiseXor => BinaryOperator::BitwiseXor,
                    tacky_ast::BinaryOperator::LeftShift => BinaryOperator::LeftShift,
                    tacky_ast::BinaryOperator::RightShift => BinaryOperator::RightShift,
                    _ => unsafe {
                        // Safety: Other operators are already matched on their own
                        unreachable_unchecked()
                    },
                };
                asm_instructions.push(Instruction::Binary(
                    op,
                    src2,
                    Operand::Pseudo(Identifier::Name(dst.var()?)),
                ))
            }
            tacky_ast::Instruction::JumpIfZero(val, target) => {
                let val = match val {
                    tacky_ast::Val::Constant(int) => Operand::Imm(*int),
                    tacky_ast::Val::Var(identifier) => {
                        Operand::Pseudo(Identifier::Name(identifier.name().to_string()))
                    }
                };
                asm_instructions.push(Instruction::Cmp(Operand::Imm(0), val));
                asm_instructions.push(Instruction::JmpCC(
                    CondCode::E,
                    Identifier::Name(target.name().to_string()),
                ));
            }
            tacky_ast::Instruction::JumpIfNotZero(val, target) => {
                let val = match val {
                    tacky_ast::Val::Constant(int) => Operand::Imm(*int),
                    tacky_ast::Val::Var(identifier) => {
                        Operand::Pseudo(Identifier::Name(identifier.name().to_string()))
                    }
                };
                asm_instructions.push(Instruction::Cmp(Operand::Imm(0), val));
                asm_instructions.push(Instruction::JmpCC(
                    CondCode::NE,
                    Identifier::Name(target.name().to_string()),
                ));
            }
            tacky_ast::Instruction::Copy(src, dst) => {
                let src = if let Ok(src) = src.var() {
                    Operand::Pseudo(Identifier::Name(src))
                } else {
                    Operand::Imm(src.constant()?)
                };
                let dst = if let Ok(dst) = dst.var() {
                    Operand::Pseudo(Identifier::Name(dst))
                } else {
                    Operand::Imm(dst.constant()?)
                };
                asm_instructions.push(Instruction::Mov(src, dst));
            }
            tacky_ast::Instruction::Jump(target) => {
                asm_instructions.push(Instruction::Jmp(Identifier::Name(
                    target.name().to_string(),
                )));
            }
            tacky_ast::Instruction::Label(target) => {
                asm_instructions.push(Instruction::Label(Identifier::Name(
                    target.name().to_string(),
                )));
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
                        BinaryOperator::BitwiseAnd => BinaryOperator::BitwiseAnd,
                        BinaryOperator::BitwiseOr => BinaryOperator::BitwiseOr,
                        BinaryOperator::BitwiseXor => BinaryOperator::BitwiseXor,
                        BinaryOperator::LeftShift => BinaryOperator::LeftShift,
                        BinaryOperator::RightShift => BinaryOperator::RightShift,
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
                Instruction::Jmp(identifier) => {
                    Instruction::Jmp(Identifier::Name(identifier.name().to_string()))
                }
                Instruction::JmpCC(cond_code, identifier) => {
                    Instruction::JmpCC(*cond_code, Identifier::Name(identifier.name().to_string()))
                }
                Instruction::SetCC(cond_code, operand) => {
                    let stack = self.match_operand(operand)?;
                    Instruction::SetCC(*cond_code, stack)
                }
                Instruction::Label(identifier) => {
                    Instruction::Label(Identifier::Name(identifier.name().to_string()))
                }
                Instruction::Cmp(src2, src1) => {
                    let stack1 = self.match_operand(src1)?;
                    let stack2 = self.match_operand(src2)?;
                    Instruction::Cmp(stack2, stack1)
                }
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
                            let r10 = Operand::Reg(RegisterType::R10(RegisterSize::Four));
                            new_instructions.push(Instruction::Mov(src.clone(), r10.clone()));
                            new_instructions.push(Instruction::Mov(r10.clone(), dst.clone()));
                        }
                        _ => new_instructions.push(Instruction::Mov(src.clone(), dst.clone())),
                    }
                }
                // fixup Add, Sub, Mult, BitwiseAnd, BitwiseOr instructions
                Instruction::Binary(binop, src, dst) => match binop {
                    BinaryOperator::Add => {
                        let r10 = Operand::Reg(RegisterType::R10(RegisterSize::Four));
                        new_instructions.push(Instruction::Mov(src.clone(), r10.clone()));
                        new_instructions.push(Instruction::Binary(
                            BinaryOperator::Add,
                            r10.clone(),
                            dst.clone(),
                        ));
                    }
                    BinaryOperator::Sub => {
                        let r10 = Operand::Reg(RegisterType::R10(RegisterSize::Four));
                        new_instructions.push(Instruction::Mov(src.clone(), r10.clone()));
                        new_instructions.push(Instruction::Binary(
                            BinaryOperator::Sub,
                            r10.clone(),
                            dst.clone(),
                        ));
                    }
                    BinaryOperator::Mult => {
                        let r11 = Operand::Reg(RegisterType::R11(RegisterSize::Four));
                        new_instructions.push(Instruction::Mov(dst.clone(), r11.clone()));
                        new_instructions.push(Instruction::Binary(
                            BinaryOperator::Mult,
                            src.clone(),
                            r11.clone(),
                        ));
                        new_instructions.push(Instruction::Mov(r11.clone(), dst.clone()));
                    }
                    BinaryOperator::BitwiseAnd => {
                        let r10 = Operand::Reg(RegisterType::R10(RegisterSize::Four));
                        new_instructions.push(Instruction::Mov(src.clone(), r10.clone()));
                        new_instructions.push(Instruction::Binary(
                            BinaryOperator::BitwiseAnd,
                            r10.clone(),
                            dst.clone(),
                        ));
                    }
                    BinaryOperator::BitwiseOr => {
                        let r10 = Operand::Reg(RegisterType::R10(RegisterSize::Four));
                        new_instructions.push(Instruction::Mov(src.clone(), r10.clone()));
                        new_instructions.push(Instruction::Binary(
                            BinaryOperator::BitwiseOr,
                            r10.clone(),
                            dst.clone(),
                        ));
                    }
                    BinaryOperator::BitwiseXor => {
                        let r10 = Operand::Reg(RegisterType::R10(RegisterSize::Four));
                        new_instructions.push(Instruction::Mov(src.clone(), r10.clone()));
                        new_instructions.push(Instruction::Binary(
                            BinaryOperator::BitwiseXor,
                            r10.clone(),
                            dst.clone(),
                        ));
                    }
                    // fixup shifts
                    BinaryOperator::LeftShift => {
                        let eax = Operand::Reg(RegisterType::AX(RegisterSize::Four));
                        let cx = Operand::Reg(RegisterType::CX);
                        let cl = Operand::Reg(RegisterType::CL);
                        new_instructions.push(Instruction::Mov(src.clone(), cx.clone()));
                        new_instructions.push(Instruction::Mov(dst.clone(), eax.clone()));
                        new_instructions.push(Instruction::Binary(
                            BinaryOperator::LeftShift,
                            cl.clone(),
                            eax.clone(),
                        ));
                        new_instructions.push(Instruction::Mov(eax.clone(), dst.clone()));
                    }
                    BinaryOperator::RightShift => {
                        let eax = Operand::Reg(RegisterType::AX(RegisterSize::Four));
                        let cx = Operand::Reg(RegisterType::CX);
                        let cl = Operand::Reg(RegisterType::CL);
                        new_instructions.push(Instruction::Mov(src.clone(), cx.clone()));
                        new_instructions.push(Instruction::Mov(dst.clone(), eax.clone()));
                        new_instructions.push(Instruction::Binary(
                            BinaryOperator::RightShift,
                            cl.clone(),
                            eax.clone(),
                        ));
                        new_instructions.push(Instruction::Mov(eax.clone(), dst.clone()));
                    }
                },
                // fixup Idiv instructions
                Instruction::Idiv(op) => {
                    let r10 = Operand::Reg(RegisterType::R10(RegisterSize::Four));
                    new_instructions.push(Instruction::Mov(op.clone(), r10.clone()));
                    new_instructions.push(Instruction::Idiv(r10.clone()));
                }
                // fixup Cmp
                Instruction::Cmp(src2, src1) => {
                    match (src2, src1) {
                        // check if both sources are Stacks, if so split using register R10
                        (Operand::Stack(_), Operand::Stack(_)) => {
                            let r10 = Operand::Reg(RegisterType::R10(RegisterSize::Four));
                            new_instructions.push(Instruction::Mov(src2.clone(), r10.clone()));
                            new_instructions.push(Instruction::Cmp(r10.clone(), src1.clone()));
                        }
                        // check if second op is a constant, if so split using register R11
                        (_, Operand::Imm(_)) => {
                            let r11 = Operand::Reg(RegisterType::R11(RegisterSize::Four));
                            new_instructions.push(Instruction::Mov(src2.clone(), r11.clone()));
                            new_instructions.push(Instruction::Cmp(r11.clone(), src1.clone()));
                        }
                        _ => new_instructions.push(Instruction::Cmp(src2.clone(), src1.clone())),
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

    fn generate_relational_operator(
        &self,
        asm_instructions: &mut Vec<Instruction>,
        cond_code: CondCode,
        src1: &tacky_ast::Val,
        src2: &tacky_ast::Val,
        dst: &tacky_ast::Val,
    ) -> Result<(), std::io::Error> {
        let src1 = if let Ok(src) = src1.var() {
            Operand::Pseudo(Identifier::Name(src))
        } else {
            Operand::Imm(src1.constant()?)
        };
        let src2 = if let Ok(src) = src2.var() {
            Operand::Pseudo(Identifier::Name(src))
        } else {
            Operand::Imm(src2.constant()?)
        };
        asm_instructions.push(Instruction::Cmp(src2, src1));
        asm_instructions.push(Instruction::Mov(
            Operand::Imm(0),
            Operand::Pseudo(Identifier::Name(dst.var()?)),
        ));
        asm_instructions.push(Instruction::SetCC(
            cond_code,
            Operand::Pseudo(Identifier::Name(dst.var()?)),
        ));
        Ok(())
    }
}
