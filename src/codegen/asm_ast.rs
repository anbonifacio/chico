use std::fmt::{Display, Formatter};

#[derive(Debug)]
pub enum AsmProgram {
    Program(FunctionDefinition),
}

impl AsmProgram {
    pub fn fn_def(&self) -> &FunctionDefinition {
        match self {
            AsmProgram::Program(function_definition) => function_definition,
        }
    }
}

#[derive(Debug)]
pub enum FunctionDefinition {
    Function(Identifier, Vec<Instruction>),
}

impl FunctionDefinition {
    pub fn new(identifier: Identifier, instructions: Vec<Instruction>) -> Self {
        FunctionDefinition::Function(identifier, instructions)
    }

    pub fn instructions(&self) -> &[Instruction] {
        match self {
            FunctionDefinition::Function(_, instructions) => instructions,
        }
    }
}

#[derive(Debug)]
pub enum Identifier {
    Name(String),
}

#[derive(Debug)]
pub enum Instruction {
    Mov(Operand, Operand),
    Unary(UnaryOperator, Operand),
    AllocateStack(i32),
    Ret,
}

#[derive(Debug)]
pub enum UnaryOperator {
    Neg,
    Not,
}

#[derive(Debug)]
pub enum Operand {
    Imm(i32),
    Reg(RegisterType),
    Pseudo(Identifier),
    Stack(i32),
}

#[derive(Debug)]
pub enum RegisterType {
    AX,
    R10,
}

impl Display for RegisterType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            RegisterType::AX => write!(f, "eax"),
            RegisterType::R10 => write!(f, "r10"),
        }
    }
}
