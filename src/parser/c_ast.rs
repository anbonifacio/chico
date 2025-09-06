#[derive(Debug)]
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

#[derive(Debug)]
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

#[derive(Debug)]
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

#[derive(Debug)]
pub enum Statement {
    Return(ExprRef),
}

#[derive(Debug, Clone, Copy)]
pub struct ExprRef(u32);

#[derive(Debug)]
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

#[derive(Debug)]
pub enum Expr {
    Constant(i32),
    Unary(UnaryOperator, ExprRef),
}

#[derive(Debug)]
pub enum UnaryOperator {
    Complement,
    Negate,
}
