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
    Function(Identifier, Vec<BlockItem>),
}

impl FunctionDefinition {
    pub fn identifier(&self) -> &Identifier {
        match self {
            FunctionDefinition::Function(name, _) => name,
        }
    }

    pub fn body(&self) -> &[BlockItem] {
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
                "  Function:\n    name=\"{}\",\n    body={:?}",
                name.name(),
                body
            ),
        }
    }
}

#[derive(Debug, Clone)]
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

#[derive(Debug)]
pub enum BlockItem {
    S(Statement),
    D(Declaration),
}

#[derive(Debug)]
pub enum Statement {
    Return(ExprRef),
    Expression(ExprRef),
    Null,
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Return(expr_ref) => write!(f, "Return: {}", expr_ref),
            Statement::Expression(expr_ref) => write!(f, "Expression: {}", expr_ref),
            Statement::Null => write!(f, "Null;"),
        }
    }
}

#[derive(Debug)]
pub enum Declaration {
    Declaration(Identifier, Option<ExprRef>),
}

impl Display for Declaration {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Declaration::Declaration(identifier, Some(init)) => {
                write!(f, "Declaration: {} = {}", identifier.name(), init)
            }
            Declaration::Declaration(identifier, None) => {
                write!(f, "Declaration: {}", identifier.name())
            }
        }
    }
}

impl Declaration {
    pub fn name(&self) -> &str {
        match self {
            Declaration::Declaration(identifier, _) => identifier.name(),
        }
    }

    pub fn initializer(&self) -> Option<ExprRef> {
        match self {
            Declaration::Declaration(_, init) => *init,
        }
    }
}

#[derive(Debug, Clone, Copy)]
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

#[derive(Debug)]
pub struct ExprPool(Vec<Expr>);

impl ExprPool {
    pub fn new() -> Self {
        ExprPool(Vec::new())
    }

    pub fn get_expr(&self, id: ExprRef) -> &Expr {
        &self.0[id.0 as usize]
    }

    pub fn add_expr(&mut self, expr: Expr) -> ExprRef {
        let id = self.0.len() as u32;
        self.0.push(expr);
        ExprRef(id)
    }

    pub fn update_expr(&mut self, id: &ExprRef, expr: Expr) {
        self.0[id.0 as usize] = expr;
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Constant(i32),
    Var(Identifier),
    Unary(UnaryOperator, ExprRef),
    Binary(BinaryOperator, ExprRef, ExprRef),
    Assignment(ExprRef, ExprRef),
}

impl Expr {
    pub fn is_lvalue(&self) -> bool {
        matches!(self, Expr::Var(_))
    }

    pub fn var(&self) -> std::io::Result<String> {
        match self {
            Expr::Var(identifier) => Ok(identifier.name().to_string()),
            _ => Err(std::io::Error::other("Not a variable")),
        }
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Constant(value) => write!(f, "{}", value),
            Expr::Unary(op, expr) => write!(f, "{}({})", op, expr),
            Expr::Binary(op, left, right) => write!(f, "({} {} {})", left, op, right),
            Expr::Var(identifier) => write!(f, "{}", identifier.name()),
            Expr::Assignment(lvalue, rvalue) => write!(f, "{} = {}", lvalue, rvalue),
        }
    }
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone, PartialEq)]
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
        }
    }
}
