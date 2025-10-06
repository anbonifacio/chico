use crate::parser::c_ast::{self, BlockItem};
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

    pub fn generate_ir(&self, ast: &CProgram) -> std::io::Result<TackyIR> {
        let ir = self.generate_function_ir(ast)?;
        Ok(TackyIR::Program(ir))
    }

    pub fn generate_function_ir(&self, ast: &CProgram) -> std::io::Result<FunctionDefinition> {
        let identifier = ast.fn_def().identifier().name();
        let body = ast.fn_def().body();
        let mut instructions = self.generate_instructions(body)?;
        instructions.append(Instruction::Return(Val::Constant(0)));

        Ok(FunctionDefinition::Function(
            Identifier::Name(identifier.to_string()),
            instructions,
        ))
    }

    fn generate_instructions(&self, body: &[BlockItem]) -> std::io::Result<Instructions> {
        log::debug!("Generating instructions for body: {:?}", body);
        let mut instructions = Instructions::new();
        for item in body.iter() {
            match item {
                BlockItem::S(statement) => match statement {
                    Statement::Return(expr_ref) => {
                        log::debug!("Emitting Return statement: {:?}", statement);
                        let expr = self.emit_tacky(expr_ref, &mut instructions)?;
                        instructions.append(Instruction::Return(expr));
                    }
                    Statement::Expression(expr_ref) => {
                        self.emit_tacky(expr_ref, &mut instructions)?;
                    }
                    Statement::Null => {}
                },
                BlockItem::D(declaration) => {
                    if let Some(init) = declaration.initializer() {
                        log::debug!("Emitting declaration with initializer: {:?}", declaration);
                        let var = Val::Var(Identifier::Name(declaration.name().to_string()));
                        let result = self.emit_tacky(&init, &mut instructions)?;
                        instructions.append(Instruction::Copy(result, var.clone()));
                    }
                }
            }
        }
        Ok(instructions)
    }

    fn emit_tacky(
        &self,
        expr_ref: &ExprRef,
        instructions: &mut Instructions,
    ) -> std::io::Result<Val> {
        let expr = self.expr_pool.get_expr(expr_ref.id());
        match expr {
            c_ast::Expr::Unary(c_ast::UnaryOperator::PrefixIncr, inner_expr_ref) => {
                // Prefix increment: ++a
                log::debug!("Emitting PrefixIncr expression: {:?}", expr);
                let v1 = self.emit_tacky(inner_expr_ref, instructions)?;
                let v2 = Val::Constant(1);
                instructions.append(Instruction::Binary(
                    BinaryOperator::Add,
                    v1.clone(),
                    v2,
                    v1.clone(),
                ));
                Ok(v1)
            }
            c_ast::Expr::Unary(c_ast::UnaryOperator::PostfixIncr, inner_expr_ref) => {
                log::debug!("Emitting PostfixIncr expression: {:?}", expr);
                // Postfix increment: a++
                let v1 = self.emit_tacky(inner_expr_ref, instructions)?;
                let v2 = Val::Constant(1);
                let tmp_name = self.make_temporary(inner_expr_ref);
                let tmp = Val::Var(Identifier::Name(tmp_name.clone()));
                // Save original value
                instructions.append(Instruction::Copy(v1.clone(), tmp.clone()));
                // Increment variable
                instructions.append(Instruction::Binary(
                    BinaryOperator::Add,
                    v1.clone(),
                    v2,
                    v1.clone(),
                ));
                // Return original value
                Ok(tmp)
            }
            c_ast::Expr::Unary(c_ast::UnaryOperator::PrefixDecr, inner_expr_ref) => {
                // Prefix decrement: --a
                log::debug!("Emitting PrefixDecr expression: {:?}", expr);
                let v1 = self.emit_tacky(inner_expr_ref, instructions)?;
                let v2 = Val::Constant(1);
                instructions.append(Instruction::Binary(
                    BinaryOperator::Subtract,
                    v1.clone(),
                    v2,
                    v1.clone(),
                ));
                Ok(v1)
            }
            c_ast::Expr::Unary(c_ast::UnaryOperator::PostfixDecr, inner_expr_ref) => {
                log::debug!("Emitting PostfixDecr expression: {:?}", expr);
                // Postfix decrement: a--
                let v1 = self.emit_tacky(inner_expr_ref, instructions)?;
                let v2 = Val::Constant(1);
                let tmp_name = self.make_temporary(inner_expr_ref);
                let tmp = Val::Var(Identifier::Name(tmp_name.clone()));
                // Save original value
                instructions.append(Instruction::Copy(v1.clone(), tmp.clone()));
                // Decrement variable
                instructions.append(Instruction::Binary(
                    BinaryOperator::Subtract,
                    v1.clone(),
                    v2,
                    v1.clone(),
                ));
                // Return original value
                Ok(tmp)
            }
            c_ast::Expr::Unary(operator, inner_expr_ref) => {
                log::debug!("Emitting Unary expression: {:?}", expr);
                let src = self.emit_tacky(inner_expr_ref, instructions)?;
                let dst_name = self.make_temporary(inner_expr_ref);
                let dst = Val::Var(Identifier::Name(dst_name.clone()));
                let tacky_op = self.convert_unop(operator)?;
                instructions.append(Instruction::Unary(tacky_op, src, dst.clone()));
                Ok(dst)
            }
            c_ast::Expr::Binary(c_ast::BinaryOperator::And, left, right) => {
                let dst_name = self.make_temporary(left);
                let false_label = self.make_label("and_false", left);
                let end_label = self.make_label("and_end", left);
                let dst = Val::Var(Identifier::Name(dst_name.clone()));
                let v1 = self.emit_tacky(left, instructions)?;
                instructions.append(Instruction::JumpIfZero(
                    v1,
                    Identifier::Name(false_label.clone()),
                ));
                let v2 = self.emit_tacky(right, instructions)?;
                instructions.append(Instruction::JumpIfZero(
                    v2,
                    Identifier::Name(false_label.clone()),
                ));
                instructions.append(Instruction::Copy(Val::Constant(1), dst.clone()));
                instructions.append(Instruction::Jump(Identifier::Name(end_label.clone())));
                instructions.append(Instruction::Label(Identifier::Name(false_label)));
                instructions.append(Instruction::Copy(Val::Constant(0), dst.clone()));
                instructions.append(Instruction::Label(Identifier::Name(end_label)));
                Ok(dst)
            }
            c_ast::Expr::Binary(c_ast::BinaryOperator::Or, left, right) => {
                let dst_name = self.make_temporary(left);
                let true_label = self.make_label("or_true", left);
                let end_label = self.make_label("or_end", left);
                let dst = Val::Var(Identifier::Name(dst_name.clone()));
                let v1 = self.emit_tacky(left, instructions)?;
                instructions.append(Instruction::JumpIfNotZero(
                    v1,
                    Identifier::Name(true_label.clone()),
                ));
                let v2 = self.emit_tacky(right, instructions)?;
                instructions.append(Instruction::JumpIfNotZero(
                    v2,
                    Identifier::Name(true_label.clone()),
                ));
                instructions.append(Instruction::Copy(Val::Constant(0), dst.clone()));
                instructions.append(Instruction::Jump(Identifier::Name(end_label.clone())));
                instructions.append(Instruction::Label(Identifier::Name(true_label)));
                instructions.append(Instruction::Copy(Val::Constant(1), dst.clone()));
                instructions.append(Instruction::Label(Identifier::Name(end_label)));
                Ok(dst)
            }
            // Handle compound assignments
            c_ast::Expr::Binary(operator, var_ref, rhs) if operator.is_compound_assignment() => {
                log::debug!("Emitting compound assignment expression: {:?}", expr);
                let expr = self.expr_pool.get_expr(var_ref.id());
                let var = Val::Var(Identifier::Name(expr.var()?));
                let result = self.emit_tacky(rhs, instructions)?;
                let bin_op = self.convert_binop(operator);
                // Perform the binary operation and store the result back in the variable
                instructions.append(Instruction::Binary(
                    bin_op,
                    var.clone(),
                    result,
                    var.clone(),
                ));
                Ok(var)
            }
            c_ast::Expr::Binary(operator, left, right) => {
                log::debug!("Emitting binary expression: {:?}", expr);
                let v1 = self.emit_tacky(left, instructions)?;
                let v2 = self.emit_tacky(right, instructions)?;
                let dst_name = self.make_temporary(left);
                let dst = Val::Var(Identifier::Name(dst_name.clone()));
                let tacky_op = self.convert_binop(operator);
                instructions.append(Instruction::Binary(tacky_op, v1, v2, dst.clone()));
                Ok(dst)
            }
            c_ast::Expr::Constant(c) => Ok(Val::Constant(*c)),
            c_ast::Expr::Var(v) => Ok(Val::Var(Identifier::Name(v.name().to_string()))),
            c_ast::Expr::Assignment(var_ref, rhs) => {
                let expr = self.expr_pool.get_expr(var_ref.id());
                let var = Val::Var(Identifier::Name(expr.var()?));
                let result = self.emit_tacky(rhs, instructions)?;
                instructions.append(Instruction::Copy(result, var.clone()));
                Ok(var)
            }
        }
    }

    fn make_temporary(&self, expr_ref: &ExprRef) -> String {
        format!("tmp.{}", expr_ref.id())
    }

    fn make_label(&self, label: &str, expr_ref: &ExprRef) -> String {
        format!("{}{}", label, expr_ref.id())
    }

    fn convert_unop(
        &self,
        operator: &c_ast::UnaryOperator,
    ) -> std::io::Result<tacky_ast::UnaryOperator> {
        match operator {
            c_ast::UnaryOperator::Negate => Ok(tacky_ast::UnaryOperator::Negate),
            c_ast::UnaryOperator::Complement => Ok(tacky_ast::UnaryOperator::Complement),
            c_ast::UnaryOperator::Not => Ok(tacky_ast::UnaryOperator::Not),
            c_ast::UnaryOperator::PrefixIncr | c_ast::UnaryOperator::PostfixIncr => {
                Err(std::io::Error::new(
                    std::io::ErrorKind::InvalidInput,
                    "Increment operators should be handled as Binary operators",
                ))
            }
            c_ast::UnaryOperator::PrefixDecr | c_ast::UnaryOperator::PostfixDecr => {
                Err(std::io::Error::new(
                    std::io::ErrorKind::InvalidInput,
                    "Decrement operators should be handled as Binary operators",
                ))
            }
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
            c_ast::BinaryOperator::Assign => BinaryOperator::Assign,
            c_ast::BinaryOperator::AssignPlus => BinaryOperator::Add,
            c_ast::BinaryOperator::AssignMinus => BinaryOperator::Subtract,
            c_ast::BinaryOperator::AssignMult => BinaryOperator::Multiply,
            c_ast::BinaryOperator::AssignDiv => BinaryOperator::Divide,
            c_ast::BinaryOperator::AssignMod => BinaryOperator::Remainder,
            c_ast::BinaryOperator::AssignAnd => BinaryOperator::BitwiseAnd,
            c_ast::BinaryOperator::AssignOr => BinaryOperator::BitwiseOr,
            c_ast::BinaryOperator::AssignXor => BinaryOperator::BitwiseXor,
            c_ast::BinaryOperator::AssignLeftShift => BinaryOperator::LeftShift,
            c_ast::BinaryOperator::AssignRightShift => BinaryOperator::RightShift,
        }
    }
}
