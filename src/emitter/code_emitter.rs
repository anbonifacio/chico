use std::{
    fs::File,
    io::{BufWriter, Write},
    path::PathBuf,
};

use crate::codegen::asm_ast::*;

pub struct CodeEmitter {
    output: BufWriter<File>,
}

impl CodeEmitter {
    pub fn new(asm_file: &PathBuf) -> std::io::Result<Self> {
        let file = File::create(asm_file)?;
        let output = BufWriter::new(file);
        Ok(CodeEmitter { output })
    }

    pub fn emit_asm(&mut self, asm: &AsmProgram) -> std::io::Result<()> {
        match asm {
            AsmProgram::Program(function_definition) => {
                #[cfg(target_os = "macos")]
                {
                    self.output.write_all(
                        format!("    .globl _{}\n", function_definition.name()).as_bytes(),
                    )?;
                }
                #[cfg(target_os = "linux")]
                {
                    self.output.write_all(
                        format!("    .globl {}\n", function_definition.name()).as_bytes(),
                    )?;
                }
                match function_definition {
                    FunctionDefinition::Function(identifier, instructions) => {
                        match identifier {
                            Identifier::Name(name) => {
                                self.output.write_all(format!("{}:\n", name).as_bytes())?;
                            }
                        }
                        self.write_prologue()?;
                        for instruction in instructions {
                            match instruction {
                                Instruction::Mov(src, dst) => {
                                    let src = match_operand(src);
                                    let dst = match_operand(dst);
                                    self.output.write_all(
                                        format!("    movl   {}, {}\n", src, dst).as_bytes(),
                                    )?;
                                }
                                Instruction::Unary(unary_operator, operand) => match unary_operator
                                {
                                    UnaryOperator::Neg => {
                                        let operand = match_operand(operand);
                                        self.output.write_all(
                                            format!("    negl   {}\n", operand).as_bytes(),
                                        )?;
                                    }
                                    UnaryOperator::Not => {
                                        let operand = match_operand(operand);
                                        self.output.write_all(
                                            format!("    notl   {}\n", operand).as_bytes(),
                                        )?;
                                    }
                                },
                                Instruction::AllocateStack(int) => {
                                    self.output.write_all(
                                        format!("    subq   ${}, %rsp\n", int).as_bytes(),
                                    )?;
                                }
                                Instruction::Ret => {
                                    self.write_epilogue()?;
                                }
                                Instruction::Binary(binary_operator, src2, dst) => todo!(),
                                Instruction::Idiv(operand) => todo!(),
                                Instruction::Cdq => todo!(),
                            }
                        }
                    }
                }
            }
        }

        #[cfg(target_os = "linux")]
        {
            self.output
                .write_all(b".section .note.GNU-stack,\"\",@progbits\n")?;
        }

        self.output.flush()
    }

    fn write_prologue(&mut self) -> std::io::Result<()> {
        self.output.write_all(b"    pushq   %rbp\n")?;
        self.output.write_all(b"    movq    %rsp, %rbp\n")?;
        Ok(())
    }

    fn write_epilogue(&mut self) -> std::io::Result<()> {
        self.output.write_all(b"    movq    %rbp, %rsp\n")?;
        self.output.write_all(b"    popq    %rbp\n")?;
        self.output.write_all(b"    ret\n")?;
        Ok(())
    }
}

fn match_operand(src: &Operand) -> String {
    match src {
        Operand::Imm(int) => {
            format!("${}", int)
        }
        Operand::Reg(reg) => match reg {
            RegisterType::AX => "%eax".to_string(),
            RegisterType::DX => todo!(),
            RegisterType::R10 => "%r10d".to_string(),
            RegisterType::R11 => todo!(),
        },
        Operand::Pseudo(_) => unimplemented!(),
        Operand::Stack(int) => {
            format!("{}(%rbp)", int)
        }
    }
}
