use std::fmt::{Display, Formatter};

pub enum TackyIR {
    Program(FunctionDefinition),
}

impl TackyIR {
    pub fn fn_def(&self) -> &FunctionDefinition {
        match self {
            TackyIR::Program(fn_def) => fn_def,
        }
    }
}

impl Display for TackyIR {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TackyIR::Program(fn_def) => write!(f, "\nProgram:\n{}", fn_def),
        }
    }
}

pub struct Instructions(Vec<Instruction>);

impl Instructions {
    pub fn new() -> Self {
        Instructions(Vec::new())
    }

    pub fn append(&mut self, instruction: Instruction) {
        self.0.push(instruction);
    }

    pub fn as_slice(&self) -> &[Instruction] {
        self.0.as_slice()
    }
}

impl Display for Instructions {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Instructions:")?;
        for (i, instruction) in self.0.iter().enumerate() {
            write!(f, "    ({}) {}", i, instruction)?;
        }
        write!(f, "")
    }
}

pub enum FunctionDefinition {
    Function(Identifier, Instructions),
}

impl FunctionDefinition {
    pub fn identifier(&self) -> &Identifier {
        match self {
            FunctionDefinition::Function(name, _) => name,
        }
    }

    pub fn body(&self) -> &Instructions {
        match self {
            FunctionDefinition::Function(_, body) => body,
        }
    }
}

impl Display for FunctionDefinition {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            FunctionDefinition::Function(name, body) => write!(
                f,
                "  Function:\n   name=\"{}\",\n   body={}",
                name.name(),
                body
            ),
        }
    }
}

#[derive(Clone)]
pub enum Identifier {
    Name(String),
}

impl Identifier {
    pub fn name(&self) -> &str {
        match self {
            Identifier::Name(name) => name,
        }
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Identifier::Name(name) => write!(f, "{}", name),
        }
    }
}

pub enum Instruction {
    Return(Val),
    Unary(UnaryOperator, Val, Val),
    Binary(BinaryOperator, Val, Val, Val),
    Copy(Val, Val),
    Jump(Identifier),
    JumpIfZero(Val, Identifier),
    JumpIfNotZero(Val, Identifier),
    Label(Identifier),
}

impl Display for Instruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::Return(val) => writeln!(f, "Return: {}", val),
            Instruction::Unary(unary_operator, src, dst) => writeln!(
                f,
                "Unary:\n      operator={}\n      src={}\n      dst={}",
                unary_operator, src, dst
            ),
            Instruction::Binary(binary_operator, src1, src2, dst) => writeln!(
                f,
                "Binary:\n      operator={}\n      src1={}\n      src2={}\n      dst={}",
                binary_operator, src1, src2, dst
            ),
            Instruction::Copy(src, dst) => {
                writeln!(f, "Copy:\n      src={}\n      dst={}", src, dst)
            }
            Instruction::Jump(target) => writeln!(f, "Jump to: {}", target),
            Instruction::JumpIfZero(condition, target) => {
                writeln!(f, "JumpIfZero: if {} == 0 jump to {}", condition, target)
            }
            Instruction::JumpIfNotZero(condition, target) => {
                writeln!(f, "JumpIfNotZero: if {} != 0 jump to {}", condition, target)
            }
            Instruction::Label(target) => writeln!(f, "Label: {}", target),
        }
    }
}

pub enum UnaryOperator {
    Complement,
    Negate,
    Not,
}

impl Display for UnaryOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            UnaryOperator::Complement => write!(f, "'~'"),
            UnaryOperator::Negate => write!(f, "'-'"),
            UnaryOperator::Not => write!(f, "'!'"),
        }
    }
}

pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    LeftShift,
    RightShift,
    And,
    Or,
    Equal,
    NotEqual,
    LessThan,
    LessOrEqual,
    GreaterThan,
    GreaterOrEqual,
    Assign,
    AssignPlus,
    AssignMinus,
    AssignMult,
    AssignDiv,
    AssignMod,
    AssignAnd,
    AssignOr,
    AssignXor,
    AssignLeftShift,
    AssignRightShift,
}

impl Display for BinaryOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryOperator::Add => write!(f, "'+'"),
            BinaryOperator::Subtract => write!(f, "'-'"),
            BinaryOperator::Multiply => write!(f, "'*'"),
            BinaryOperator::Divide => write!(f, "'/'"),
            BinaryOperator::Remainder => write!(f, "'%'"),
            BinaryOperator::BitwiseAnd => write!(f, "'&'"),
            BinaryOperator::BitwiseOr => write!(f, "'|'"),
            BinaryOperator::BitwiseXor => write!(f, "'^'"),
            BinaryOperator::LeftShift => write!(f, "'<<'"),
            BinaryOperator::RightShift => write!(f, "'>>'"),
            BinaryOperator::And => write!(f, "'&&'"),
            BinaryOperator::Or => write!(f, "'||'"),
            BinaryOperator::Equal => write!(f, "'=='"),
            BinaryOperator::NotEqual => write!(f, "'!='"),
            BinaryOperator::LessThan => write!(f, "'<'"),
            BinaryOperator::LessOrEqual => write!(f, "'<='"),
            BinaryOperator::GreaterThan => write!(f, "'>'"),
            BinaryOperator::GreaterOrEqual => write!(f, "'>='"),
            BinaryOperator::Assign => write!(f, "'='"),
            BinaryOperator::AssignPlus => write!(f, "'+='"),
            BinaryOperator::AssignMinus => write!(f, "'-='"),
            BinaryOperator::AssignMult => write!(f, "'*='"),
            BinaryOperator::AssignDiv => write!(f, "'/='"),
            BinaryOperator::AssignMod => write!(f, "'%='"),
            BinaryOperator::AssignAnd => write!(f, "'&='"),
            BinaryOperator::AssignOr => write!(f, "'|='"),
            BinaryOperator::AssignXor => write!(f, "'^='"),
            BinaryOperator::AssignLeftShift => write!(f, "'<<='"),
            BinaryOperator::AssignRightShift => write!(f, "'>>='"),
        }
    }
}

#[derive(Clone)]
pub enum Val {
    Constant(i32),
    Var(Identifier),
}

impl Val {
    pub fn constant(&self) -> std::io::Result<i32> {
        match self {
            Val::Constant(value) => Ok(*value),
            _ => Err(std::io::Error::other("Not a Constant")),
        }
    }

    pub fn var(&self) -> std::io::Result<String> {
        match self {
            Val::Var(Identifier::Name(name)) => Ok(name.clone()),
            _ => Err(std::io::Error::other("Not a Var")),
        }
    }
}

impl Display for Val {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Val::Constant(value) => write!(f, "Constant({})", value),
            Val::Var(Identifier::Name(name)) => write!(f, "Var({})", name),
        }
    }
}
