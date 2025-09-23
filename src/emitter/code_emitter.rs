use std::{
    fs::File,
    hint::unreachable_unchecked,
    io::{BufWriter, Write},
    path::PathBuf,
};

use crate::codegen::asm_ast::*;

const LABEL_PREFIX: &'static str = get_label_prefix();
const fn get_label_prefix() -> &'static str {
    #[cfg(target_os = "macos")]
    {
        "L"
    }
    #[cfg(target_os = "linux")]
    {
        ".L"
    }
}

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
                                        format!("    movl    {}, {}\n", src, dst).as_bytes(),
                                    )?;
                                }
                                Instruction::Unary(unary_operator, operand) => match unary_operator
                                {
                                    UnaryOperator::Neg => {
                                        let operand = match_operand(operand);
                                        self.output.write_all(
                                            format!("    negl    {}\n", operand).as_bytes(),
                                        )?;
                                    }
                                    UnaryOperator::Not => {
                                        let operand = match_operand(operand);
                                        self.output.write_all(
                                            format!("    notl    {}\n", operand).as_bytes(),
                                        )?;
                                    }
                                },
                                Instruction::AllocateStack(int) => {
                                    self.output.write_all(
                                        format!("    subq    ${}, %rsp\n", int).as_bytes(),
                                    )?;
                                }
                                Instruction::Ret => {
                                    self.write_epilogue()?;
                                }
                                Instruction::Binary(binop, src, dst) => match binop {
                                    BinaryOperator::Add => {
                                        self.output.write_all(
                                            format!(
                                                "    addl    {}, {}\n",
                                                match_operand(src),
                                                match_operand(dst)
                                            )
                                            .as_bytes(),
                                        )?;
                                    }
                                    BinaryOperator::Sub => {
                                        self.output.write_all(
                                            format!(
                                                "    subl    {}, {}\n",
                                                match_operand(src),
                                                match_operand(dst)
                                            )
                                            .as_bytes(),
                                        )?;
                                    }
                                    BinaryOperator::Mult => {
                                        self.output.write_all(
                                            format!(
                                                "    imull   {}, {}\n",
                                                match_operand(src),
                                                match_operand(dst)
                                            )
                                            .as_bytes(),
                                        )?;
                                    }
                                    BinaryOperator::BitwiseAnd => self.output.write_all(
                                        format!(
                                            "    andl    {}, {}\n",
                                            match_operand(src),
                                            match_operand(dst)
                                        )
                                        .as_bytes(),
                                    )?,
                                    BinaryOperator::BitwiseOr => self.output.write_all(
                                        format!(
                                            "    orl     {}, {}\n",
                                            match_operand(src),
                                            match_operand(dst)
                                        )
                                        .as_bytes(),
                                    )?,
                                    BinaryOperator::BitwiseXor => self.output.write_all(
                                        format!(
                                            "    xorl    {}, {}\n",
                                            match_operand(src),
                                            match_operand(dst)
                                        )
                                        .as_bytes(),
                                    )?,
                                    BinaryOperator::LeftShift => self.output.write_all(
                                        format!(
                                            "    shll    {}, {}\n",
                                            match_operand(src),
                                            match_operand(dst)
                                        )
                                        .as_bytes(),
                                    )?,
                                    BinaryOperator::RightShift => self.output.write_all(
                                        format!(
                                            "    sarl    {}, {}\n",
                                            match_operand(src),
                                            match_operand(dst)
                                        )
                                        .as_bytes(),
                                    )?,
                                },
                                Instruction::Idiv(operand) => self.output.write_all(
                                    format!("    idivl   {}\n", match_operand(operand)).as_bytes(),
                                )?,
                                Instruction::Cdq => {
                                    self.output.write_all(b"    cdq\n")?;
                                }
                                Instruction::Jmp(identifier) => self.output.write_all(
                                    format!("    jmp     {}{}\n", LABEL_PREFIX, identifier.name())
                                        .as_bytes(),
                                )?,
                                Instruction::JmpCC(cond_code, identifier) => {
                                    self.output.write_all(
                                        format!(
                                            "    j{}     {}{}\n",
                                            cond_code,
                                            LABEL_PREFIX,
                                            identifier.name()
                                        )
                                        .as_bytes(),
                                    )?
                                }
                                Instruction::SetCC(cond_code, op) => self.output.write_all(
                                    format!("    set{}    {}\n", cond_code, match_operand(op))
                                        .as_bytes(),
                                )?,
                                Instruction::Label(identifier) => self.output.write_all(
                                    format!("{}{}:\n", LABEL_PREFIX, identifier.name()).as_bytes(),
                                )?,
                                Instruction::Cmp(op2, op1) => self.output.write_all(
                                    format!(
                                        "    cmpl    {}, {}\n",
                                        match_operand(op2),
                                        match_operand(op1)
                                    )
                                    .as_bytes(),
                                )?,
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
        Operand::Reg(reg) => {
            format!("%{}", reg)
        }
        Operand::Pseudo(_) => unsafe {
            // Safety: Pseudo Registers are already fixed up here.
            unreachable_unchecked()
        },
        Operand::Stack(int) => {
            format!("{}(%rbp)", int)
        }
    }
}
