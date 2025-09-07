use std::fmt::{Display, Formatter};

pub enum CProgram {
    Program(FunctionDefinition),
}

impl CProgram {
    pub fn fn_def(&self) -> &FunctionDefinition {
        match self {
            CProgram::Program(fn_def) => fn_def,
        }
    }
}

impl Display for CProgram {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            CProgram::Program(fn_def) => write!(f, "\nProgram:\n{}", fn_def),
        }
    }
}

pub enum FunctionDefinition {
    Function(Identifier, Statement),
}

impl FunctionDefinition {
    pub fn identifier(&self) -> &Identifier {
        match self {
            FunctionDefinition::Function(name, _) => name,
        }
    }

    pub fn body(&self) -> &Statement {
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
                "  Function:\n    name=\"{}\",\n    body={}",
                name.name(),
                body
            ),
        }
    }
}

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

pub enum Statement {
    Return(ExprRef),
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Return(expr_ref) => write!(f, "Return: {}", expr_ref),
        }
    }
}

#[derive(Clone, Copy)]
pub struct ExprRef(u32);

impl ExprRef {
    pub fn id(&self) -> u32 {
        self.0
    }
}

impl Display for ExprRef {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        // FIXME: how to get the real Expr from ExprPool for each index [0..self.0]?
        write!(f, "ExprRef[{}]", self.0 + 1)
    }
}

pub struct ExprPool(Vec<Expr>);

impl ExprPool {
    pub fn new() -> Self {
        ExprPool(Vec::new())
    }

    pub fn add_expr(&mut self, expr: Expr) -> ExprRef {
        let id = self.0.len() as u32;
        self.0.push(expr);
        ExprRef(id)
    }

    pub fn get_expr(&self, id: ExprRef) -> &Expr {
        &self.0[id.0 as usize]
    }
}

pub enum Expr {
    Constant(i32),
    Unary(UnaryOperator, ExprRef),
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Constant(value) => write!(f, "{}", value),
            Expr::Unary(op, expr) => write!(f, "{}({})", op, expr),
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
            UnaryOperator::Complement => write!(f, "'~'"),
            UnaryOperator::Negate => write!(f, "'-'"),
        }
    }
}
