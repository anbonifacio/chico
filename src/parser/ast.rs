#[derive(Debug)]
pub enum AST {
    Program(FunctionDefinition)
}

#[derive(Debug)]
pub enum FunctionDefinition {
    Function(Identifier, Statement)
}

#[derive(Debug)]
pub enum Identifier {
    Name(String)
}

#[derive(Debug)]
pub enum Statement {
    Return(Exp)
}

#[derive(Debug)]
pub enum Exp {
    Constant(i32)
}
