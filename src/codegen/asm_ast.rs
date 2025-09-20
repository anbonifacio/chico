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

    pub fn name(&self) -> String {
        match self {
            FunctionDefinition::Function(identifier, _) => identifier.name(),
        }
    }

    pub fn instructions(&self) -> &[Instruction] {
        match self {
            FunctionDefinition::Function(_, instructions) => instructions,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Identifier {
    Name(String),
}

impl Identifier {
    pub fn name(&self) -> String {
        match self {
            Identifier::Name(name) => name.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Instruction {
    Mov(Operand, Operand),
    Unary(UnaryOperator, Operand),
    Binary(BinaryOperator, Operand, Operand),
    Idiv(Operand),
    Cdq,
    AllocateStack(i32),
    Ret,
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOperator {
    Neg,
    Not,
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mult,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    LeftShift,
    RightShift,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Operand {
    Imm(i32),
    Reg(RegisterType),
    Pseudo(Identifier),
    Stack(i32),
}

impl Operand {
    pub fn get_type(&self) -> std::io::Result<Operand> {
        match self {
            Operand::Imm(value) => Ok(Operand::Imm(*value)),
            Operand::Reg(reg) => match reg {
                RegisterType::AX => Ok(Operand::Reg(RegisterType::AX)),
                RegisterType::DX => Ok(Operand::Reg(RegisterType::DX)),
                RegisterType::R10 => Ok(Operand::Reg(RegisterType::R10)),
                RegisterType::R11 => Ok(Operand::Reg(RegisterType::R11)),
                RegisterType::CX => Ok(Operand::Reg(RegisterType::CL)),
                RegisterType::CL => Ok(Operand::Reg(RegisterType::CX)),
            },
            Operand::Pseudo(identifier) => Ok(Operand::Pseudo(Identifier::Name(identifier.name()))),
            Operand::Stack(offset) => Ok(Operand::Stack(*offset)),
        }
    }

    pub fn get_pseudo_identifier(&self) -> std::io::Result<Operand> {
        match self {
            Operand::Pseudo(identifier) => Ok(Operand::Pseudo(Identifier::Name(identifier.name()))),
            _ => Err(std::io::Error::new(
                std::io::ErrorKind::InvalidInput,
                "Not a Pseudo Register",
            )),
        }
    }

    pub fn get_imm(&self) -> std::io::Result<Operand> {
        match self {
            Operand::Imm(value) => Ok(Operand::Imm(*value)),
            _ => Err(std::io::Error::new(
                std::io::ErrorKind::InvalidInput,
                "Not an immediate value",
            )),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum RegisterType {
    AX,
    DX,
    R10,
    R11,
    CX,
    CL,
}

impl Display for RegisterType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            RegisterType::AX => write!(f, "eax"),
            RegisterType::DX => write!(f, "edx"),
            RegisterType::R10 => write!(f, "r10d"),
            RegisterType::R11 => write!(f, "r11d"),
            RegisterType::CX => write!(f, "ecx"),
            RegisterType::CL => write!(f, "cl"),
        }
    }
}
