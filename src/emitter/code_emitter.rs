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
                    self.output.write_all(b"    .globl _main\n")?;
                }
                #[cfg(target_os = "linux")]
                {
                    self.output.write_all(b"    .globl main\n")?;
                }
                match function_definition {
                    FunctionDefinition::Function(identifier, instructions) => {
                        match identifier {
                            Identifier::Name(name) => {
                                self.output.write_all(format!("{}:\n", name).as_bytes())?;
                            }
                        }
                        for instruction in instructions {
                            match instruction {
                                Instruction::Mov(src, dst) => {
                                    let src = match src {
                                        Operand::Imm(int) => {
                                            format!("${}", int)
                                        }
                                        Operand::Reg(_) => unimplemented!(),
                                        Operand::Pseudo(identifier) => todo!(),
                                        Operand::Stack(_) => todo!(),
                                    };
                                    let dst = match dst {
                                        Operand::Imm(_) => unimplemented!(),
                                        Operand::Reg(reg) => {
                                            format!("%{}", reg)
                                        }
                                        Operand::Pseudo(identifier) => todo!(),
                                        Operand::Stack(_) => todo!(),
                                    };
                                    self.output.write_all(
                                        format!("    movl   {}, {}\n", src, dst).as_bytes(),
                                    )?;
                                }
                                Instruction::Ret => {
                                    self.output.write_all(b"    ret\n")?;
                                }
                                Instruction::Unary(unary_operator, operand) => todo!(),
                                Instruction::AllocateStack(_) => todo!(),
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
}
