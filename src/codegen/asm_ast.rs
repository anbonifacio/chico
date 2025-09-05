use std::fmt::{Display, Formatter};

#[derive(Debug)]
pub enum AsmProgram {
    Program(FunctionDefinition),
}

#[derive(Debug)]
pub enum FunctionDefinition {
    Function(Identifier, Vec<Instruction>),
}

impl FunctionDefinition {
    pub fn new(identifier: Identifier, instructions: Vec<Instruction>) -> Self {
        FunctionDefinition::Function(identifier, instructions)
    }
}

#[derive(Debug)]
pub enum Identifier {
    Name(String),
}

#[derive(Debug)]
pub enum Instruction {
    Mov(Operand, Operand),
    Ret,
}

#[derive(Debug)]
pub enum Operand {
    Imm(i32),
    Register(RegisterType),
}

#[derive(Debug)]
pub enum RegisterType {
    Eax,
}

impl Display for RegisterType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            RegisterType::Eax => write!(f, "eax"),
        }
    }
}
