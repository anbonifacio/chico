use crate::parser::c_ast;
use crate::parser::c_ast::{CProgram, ExprPool, ExprRef, Statement};
use crate::tacky_ir::tacky_ast;
use crate::tacky_ir::tacky_ast::*;

pub struct TackyGenerator<'expr> {
    expr_pool: &'expr ExprPool,
}

impl<'expr> TackyGenerator<'expr> {
    pub fn new(expr_pool: &'expr ExprPool) -> Self {
        log::debug!("expr pool: {:?}", expr_pool);
        TackyGenerator { expr_pool }
    }

    pub fn generate_ir(&self, ast: &CProgram) -> TackyIR {
        let ir = self.generate_function_ir(ast);
        TackyIR::Program(ir)
    }

    pub fn generate_function_ir(&self, ast: &CProgram) -> FunctionDefinition {
        let identifier = ast.fn_def().identifier().name();
        let body = ast.fn_def().body();
        let instructions = self.generate_instructions(body);

        FunctionDefinition::Function(Identifier::Name(identifier.to_string()), instructions)
    }

    fn generate_instructions(&self, body: &Statement) -> Instructions {
        let mut instructions = Instructions::new();
        match body {
            Statement::Return(expr_ref) => {
                let expr = self.emit_tacky(expr_ref, &mut instructions);
                instructions.append(Instruction::Return(expr));
            }
        }
        instructions
    }

    fn emit_tacky(&self, expr_ref: &ExprRef, instructions: &mut Instructions) -> Val {
        let expr = self.expr_pool.get_expr(*expr_ref);
        match expr {
            c_ast::Expr::Unary(operator, inner_expr_ref) => {
                let src = self.emit_tacky(inner_expr_ref, instructions);
                let dst_name = self.make_temporary(inner_expr_ref);
                let dst = Val::Var(Identifier::Name(dst_name.clone()));
                let tacky_op = self.convert_unop(operator);
                instructions.append(Instruction::Unary(tacky_op, src, dst.clone()));
                dst
            }
            c_ast::Expr::Binary(c_ast::BinaryOperator::And, left, right) => {
                let dst_name = self.make_temporary(left);
                let false_label = self.make_label("and_false", left);
                let end_label = self.make_label("and_end", left);
                let dst = Val::Var(Identifier::Name(dst_name.clone()));
                let v1 = self.emit_tacky(left, instructions);
                instructions.append(Instruction::JumpIfZero(
                    v1,
                    Identifier::Name(false_label.clone()),
                ));
                let v2 = self.emit_tacky(right, instructions);
                instructions.append(Instruction::JumpIfZero(
                    v2,
                    Identifier::Name(false_label.clone()),
                ));
                instructions.append(Instruction::Copy(dst.clone(), Val::Constant(1)));
                instructions.append(Instruction::Jump(Identifier::Name(end_label.clone())));
                instructions.append(Instruction::Label(Identifier::Name(false_label)));
                instructions.append(Instruction::Copy(dst.clone(), Val::Constant(0)));
                instructions.append(Instruction::Label(Identifier::Name(end_label)));
                dst
            }
            c_ast::Expr::Binary(c_ast::BinaryOperator::Or, left, right) => {
                let dst_name = self.make_temporary(left);
                let false_label = self.make_label("or_false", left);
                let end_label = self.make_label("or_end", left);
                let dst = Val::Var(Identifier::Name(dst_name.clone()));
                let v1 = self.emit_tacky(left, instructions);
                instructions.append(Instruction::JumpIfNotZero(
                    v1,
                    Identifier::Name(false_label.clone()),
                ));
                let v2 = self.emit_tacky(right, instructions);
                instructions.append(Instruction::JumpIfNotZero(
                    v2,
                    Identifier::Name(false_label.clone()),
                ));
                instructions.append(Instruction::Copy(dst.clone(), Val::Constant(1)));
                instructions.append(Instruction::Jump(Identifier::Name(end_label.clone())));
                instructions.append(Instruction::Label(Identifier::Name(false_label)));
                instructions.append(Instruction::Copy(dst.clone(), Val::Constant(0)));
                instructions.append(Instruction::Label(Identifier::Name(end_label)));
                dst
            }
            c_ast::Expr::Binary(operator, left, right) => {
                let v1 = self.emit_tacky(left, instructions);
                let v2 = self.emit_tacky(right, instructions);
                let dst_name = self.make_temporary(left);
                let dst = Val::Var(Identifier::Name(dst_name.clone()));
                let tacky_op = self.convert_binop(operator);
                instructions.append(Instruction::Binary(tacky_op, v1, v2, dst.clone()));
                dst
            }
            c_ast::Expr::Constant(c) => Val::Constant(*c),
        }
    }

    fn make_temporary(&self, expr_ref: &ExprRef) -> String {
        format!("tmp.{}", expr_ref.id())
    }

    fn make_label(&self, label: &str, expr_ref: &ExprRef) -> String {
        format!("{}{}", label, expr_ref.id())
    }

    fn convert_unop(&self, operator: &c_ast::UnaryOperator) -> tacky_ast::UnaryOperator {
        match operator {
            c_ast::UnaryOperator::Negate => tacky_ast::UnaryOperator::Negate,
            c_ast::UnaryOperator::Complement => tacky_ast::UnaryOperator::Complement,
            c_ast::UnaryOperator::Not => tacky_ast::UnaryOperator::Not,
        }
    }

    fn convert_binop(&self, operator: &c_ast::BinaryOperator) -> BinaryOperator {
        match operator {
            c_ast::BinaryOperator::Add => BinaryOperator::Add,
            c_ast::BinaryOperator::Subtract => BinaryOperator::Subtract,
            c_ast::BinaryOperator::Multiply => BinaryOperator::Multiply,
            c_ast::BinaryOperator::Divide => BinaryOperator::Divide,
            c_ast::BinaryOperator::Remainder => BinaryOperator::Remainder,
            c_ast::BinaryOperator::BitwiseAnd => BinaryOperator::BitwiseAnd,
            c_ast::BinaryOperator::BitwiseOr => BinaryOperator::BitwiseOr,
            c_ast::BinaryOperator::BitwiseXor => BinaryOperator::BitwiseXor,
            c_ast::BinaryOperator::LeftShift => BinaryOperator::LeftShift,
            c_ast::BinaryOperator::RightShift => BinaryOperator::RightShift,
            c_ast::BinaryOperator::And => BinaryOperator::And,
            c_ast::BinaryOperator::Or => BinaryOperator::Or,
            c_ast::BinaryOperator::Equal => BinaryOperator::Equal,
            c_ast::BinaryOperator::NotEqual => BinaryOperator::NotEqual,
            c_ast::BinaryOperator::LessThan => BinaryOperator::LessThan,
            c_ast::BinaryOperator::LessOrEqual => BinaryOperator::LessOrEqual,
            c_ast::BinaryOperator::GreaterThan => BinaryOperator::GreaterThan,
            c_ast::BinaryOperator::GreaterOrEqual => BinaryOperator::GreaterOrEqual,
        }
    }
}
