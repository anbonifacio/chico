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
pub struct ExprRef {
    id: u32,
    expr_type: ExprType,
}

impl ExprRef {
    pub fn new(id: u32, expr_type: ExprType) -> Self {
        ExprRef { id, expr_type }
    }

    pub fn id(&self) -> u32 {
        self.id
    }

    pub fn expr_type(&self) -> ExprType {
        self.expr_type
    }
}

impl Display for ExprRef {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "ExprRef[{}: {:?}]", self.id, self.expr_type)
    }
}

#[derive(Debug)]
pub struct ExprPool(Vec<Expr>);

impl ExprPool {
    pub fn new() -> Self {
        ExprPool(Vec::new())
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn get_expr(&self, id: u32) -> &Expr {
        &self.0[id as usize]
    }

    pub fn add_expr(&mut self, expr: Expr) -> ExprRef {
        let id = self.0.len() as u32;
        self.0.push(expr.clone());
        ExprRef {
            id,
            expr_type: expr.get_type(),
        }
    }

    pub fn update_expr(&mut self, id: &ExprRef, expr: Expr) {
        self.0[id.id as usize] = expr;
    }

    pub(crate) fn last_expr(&self) -> std::io::Result<ExprRef> {
        match self.0.iter().enumerate().next_back() {
            Some((idx, expr)) => Ok(ExprRef {
                id: idx as u32,
                expr_type: expr.get_type(),
            }),
            None => Err(std::io::Error::other(
                "Expression pool is empty, cannot get last expression",
            )),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ExprType {
    Constant,
    Var,
    Unary,
    Binary,
    Assignment,
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

    pub fn get_type(&self) -> ExprType {
        match self {
            Expr::Constant(_) => ExprType::Constant,
            Expr::Var(_) => ExprType::Var,
            Expr::Unary(_, _) => ExprType::Unary,
            Expr::Binary(_, _, _) => ExprType::Binary,
            Expr::Assignment(_, _) => ExprType::Assignment,
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
    PrefixIncr,
    PrefixDecr,
    PostfixIncr,
    PostfixDecr,
}

impl UnaryOperator {
    pub fn is_lvalue_op(&self) -> bool {
        matches!(
            self,
            UnaryOperator::PrefixIncr
                | UnaryOperator::PrefixDecr
                | UnaryOperator::PostfixIncr
                | UnaryOperator::PostfixDecr
        )
    }
}

impl Display for UnaryOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            UnaryOperator::Complement => write!(f, "'~'"),
            UnaryOperator::Negate => write!(f, "'-'"),
            UnaryOperator::Not => write!(f, "'!'"),
            UnaryOperator::PrefixIncr | UnaryOperator::PostfixIncr => write!(f, "'++'"),
            UnaryOperator::PrefixDecr | UnaryOperator::PostfixDecr => write!(f, "'--'"),
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

impl BinaryOperator {
    pub fn is_assignment(&self) -> bool {
        matches!(
            self,
            BinaryOperator::Assign
                | BinaryOperator::AssignPlus
                | BinaryOperator::AssignMinus
                | BinaryOperator::AssignMult
                | BinaryOperator::AssignDiv
                | BinaryOperator::AssignMod
                | BinaryOperator::AssignAnd
                | BinaryOperator::AssignOr
                | BinaryOperator::AssignXor
                | BinaryOperator::AssignLeftShift
                | BinaryOperator::AssignRightShift
        )
    }

    pub fn get_compound_operator(&self) -> Option<BinaryOperator> {
        match self {
            BinaryOperator::AssignPlus => Some(BinaryOperator::Add),
            BinaryOperator::AssignMinus => Some(BinaryOperator::Subtract),
            BinaryOperator::AssignMult => Some(BinaryOperator::Multiply),
            BinaryOperator::AssignDiv => Some(BinaryOperator::Divide),
            BinaryOperator::AssignMod => Some(BinaryOperator::Remainder),
            BinaryOperator::AssignAnd => Some(BinaryOperator::BitwiseAnd),
            BinaryOperator::AssignOr => Some(BinaryOperator::BitwiseOr),
            BinaryOperator::AssignXor => Some(BinaryOperator::BitwiseXor),
            BinaryOperator::AssignLeftShift => Some(BinaryOperator::LeftShift),
            BinaryOperator::AssignRightShift => Some(BinaryOperator::RightShift),
            _ => None,
        }
    }
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
