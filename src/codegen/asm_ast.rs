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
    // TODO: refactor identifiers to use something more efficient than String
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
    Cmp(Operand, Operand),
    Idiv(Operand),
    Cdq,
    Jmp(Identifier),
    JmpCC(CondCode, Identifier),
    SetCC(CondCode, Operand),
    Label(Identifier),
    AllocateStack(i32),
    Ret,
}

#[derive(Debug, Clone, Copy)]
pub enum CondCode {
    E,
    NE,
    G,
    GE,
    L,
    LE,
}

impl Display for CondCode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            CondCode::E => write!(f, "e"),
            CondCode::NE => write!(f, "ne"),
            CondCode::G => write!(f, "g"),
            CondCode::GE => write!(f, "ge"),
            CondCode::L => write!(f, "l"),
            CondCode::LE => write!(f, "le"),
        }
    }
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
                RegisterType::AX(size) => Ok(Operand::Reg(RegisterType::AX(*size))),
                RegisterType::DX(size) => Ok(Operand::Reg(RegisterType::DX(*size))),
                RegisterType::R10(size) => Ok(Operand::Reg(RegisterType::R10(*size))),
                RegisterType::R11(size) => Ok(Operand::Reg(RegisterType::R11(*size))),
                RegisterType::CX(size) => Ok(Operand::Reg(RegisterType::CX(*size))),
            },
            Operand::Pseudo(identifier) => Ok(Operand::Pseudo(Identifier::Name(identifier.name()))),
            Operand::Stack(offset) => Ok(Operand::Stack(*offset)),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum RegisterType {
    AX(RegisterSize),
    DX(RegisterSize),
    R10(RegisterSize),
    R11(RegisterSize),
    CX(RegisterSize),
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum RegisterSize {
    OneByte,
    FourBytes,
}

impl Display for RegisterType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            RegisterType::AX(RegisterSize::FourBytes) => write!(f, "eax"),
            RegisterType::AX(RegisterSize::OneByte) => write!(f, "al"),
            RegisterType::DX(RegisterSize::FourBytes) => write!(f, "edx"),
            RegisterType::DX(RegisterSize::OneByte) => write!(f, "dl"),
            RegisterType::R10(RegisterSize::FourBytes) => write!(f, "r10d"),
            RegisterType::R10(RegisterSize::OneByte) => write!(f, "r10b"),
            RegisterType::R11(RegisterSize::FourBytes) => write!(f, "r11d"),
            RegisterType::R11(RegisterSize::OneByte) => write!(f, "r11b"),
            RegisterType::CX(RegisterSize::FourBytes) => write!(f, "ecx"),
            RegisterType::CX(RegisterSize::OneByte) => write!(f, "cl"),
        }
    }
}
