use std::fmt::{Display, Formatter};

pub enum TackyIR {
    Program(FunctionDefinition),
}

impl TackyIR {
    pub fn get_fn(&self) -> &FunctionDefinition {
        match self {
            TackyIR::Program(fn_def) => fn_def,
        }
    }
}

impl Display for TackyIR {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TackyIR::Program(fn_def) => write!(f, "Program(\n{}\n)", fn_def),
        }
    }
}

pub struct Instructions(Vec<Instruction>);

impl Display for Instructions {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Instructions(")?;
        for (i, instruction) in self.0.iter().enumerate() {
            writeln!(f, "  {} {}", i, instruction)?;
        }
        write!(f, ")")
    }
}

pub enum FunctionDefinition {
    Function(Identifier, Instructions),
}

impl FunctionDefinition {
    pub fn get_identifier(&self) -> &Identifier {
        match self {
            FunctionDefinition::Function(name, _) => name,
        }
    }

    pub fn get_body(&self) -> &Instructions {
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
                " Function(\n  name=\"{}\",\n  body={}\n )",
                name.get_name(),
                body
            ),
        }
    }
}

pub enum Identifier {
    Name(String),
}

impl Identifier {
    pub fn get_name(&self) -> &str {
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
}

impl Display for Instruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::Return(val) => write!(f, "Return(\n  {}\n  )", val),
            Instruction::Unary(unary_operator, src, dst) => write!(
                f,
                "Unary(\n  operator={}\n  src={}\n  dst={}\n  )",
                unary_operator, src, dst
            ),
        }
    }
}

pub enum UnaryOperator {
    Complement,
    Negate,
}

impl Display for UnaryOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            UnaryOperator::Complement => write!(f, "~"),
            UnaryOperator::Negate => write!(f, "-"),
        }
    }
}

pub enum Val {
    Constant(i32),
    Var(Identifier),
}

impl Display for Val {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Val::Constant(value) => write!(f, "Constant({})", value),
            Val::Var(Identifier::Name(name)) => write!(f, "Var({})", name),
        }
    }
}
