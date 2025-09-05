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
                    self.output.write(b"    .globl _main\n")?;
                }
                #[cfg(target_os = "linux")]
                {
                    self.output.write(b"    .globl main\n")?;
                }
                match function_definition {
                    FunctionDefinition::Function(identifier, instructions) => {
                        match identifier {
                            Identifier::Name(name) => {
                                self.output.write(format!("{}:\n", name).as_bytes())?;
                            }
                        }
                        for instruction in instructions {
                            match instruction {
                                Instruction::Mov(src, dst) => {
                                    let src = match src {
                                        Operand::Imm(int) => {
                                            format!("${}", int)
                                        }
                                        Operand::Register(_) => unimplemented!(),
                                    };
                                    let dst = match dst {
                                        Operand::Imm(_) => unimplemented!(),
                                        Operand::Register(reg) => {
                                            format!("%{}", reg)
                                        }
                                    };
                                    self.output.write(
                                        format!("    movl    {}, {}\n", src, dst).as_bytes(),
                                    )?;
                                }
                                Instruction::Ret => {
                                    self.output.write(b"    ret\n")?;
                                }
                            }
                        }
                    }
                }
            }
        }

        #[cfg(target_os = "linux")]
        {
            self.output
                .write(b".section .note.GNU-stack,\"\",@progbits\n")?;
        }

        self.output.flush()
    }
}
