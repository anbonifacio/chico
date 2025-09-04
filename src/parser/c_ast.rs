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
    Return(Exp),
}

impl Statement {
    pub fn get_return_exp(&self) -> Option<&Exp> {
        match self {
            Statement::Return(exp) => Some(exp),
        }
    }
}

#[derive(Debug)]
pub enum Exp {
    Constant(i32),
}

impl Exp {
    pub fn get_value(&self) -> i32 {
        match self {
            Exp::Constant(value) => *value,
        }
    }
}
