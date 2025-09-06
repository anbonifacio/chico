use std::fmt::{Display, Formatter};

pub enum CProgram {
    Program(FunctionDefinition),
}

impl CProgram {
    pub fn get_fn(&self) -> &FunctionDefinition {
        match self {
            CProgram::Program(fn_def) => fn_def,
        }
    }
}

impl Display for CProgram {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            CProgram::Program(fn_def) => write!(f, "Program(\n{}\n)", fn_def),
        }
    }
}

pub enum FunctionDefinition {
    Function(Identifier, Statement),
}

impl FunctionDefinition {
    pub fn get_identifier(&self) -> &Identifier {
        match self {
            FunctionDefinition::Function(name, _) => name,
        }
    }

    pub fn get_body(&self) -> &Statement {
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

pub enum Statement {
    Return(ExprRef),
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Return(expr_ref) => write!(f, "Return(\n  {}\n  )", expr_ref),
        }
    }
}

#[derive(Clone, Copy)]
pub struct ExprRef(u32);

impl Display for ExprRef {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "  ExprRef({})", self.0)
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
            UnaryOperator::Complement => write!(f, "~"),
            UnaryOperator::Negate => write!(f, "-"),
        }
    }
}
