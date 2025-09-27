use crate::{
    parser::c_ast::{BlockItem, CProgram, ExprPool},
    semantic_analysis::variable_resolution::VariableResolver,
};

mod variable_resolution;

pub struct SemanticAnalysis<'expr> {
    variable_resolver: VariableResolver<'expr>,
}

impl<'expr> SemanticAnalysis<'expr> {
    pub fn new(expr_pool: &'expr mut ExprPool) -> Self {
        let variable_resolver = VariableResolver::new(expr_pool);
        Self { variable_resolver }
    }

    pub(crate) fn analyze_program(
        &mut self,
        c_program: crate::parser::c_ast::CProgram,
    ) -> std::io::Result<CProgram> {
        let body = c_program.fn_def().body();
        let new_body = body
            .iter()
            .map(|block_item| match block_item {
                crate::parser::c_ast::BlockItem::S(statement) => Ok(BlockItem::S(
                    self.variable_resolver.resolve_statement(statement)?,
                )),
                crate::parser::c_ast::BlockItem::D(declaration) => Ok(BlockItem::D(
                    self.variable_resolver.resolve_declaration(declaration)?,
                )),
            })
            .collect::<Result<Vec<_>, std::io::Error>>()?;
        let new_fndef = crate::parser::c_ast::FunctionDefinition::Function(
            c_program.fn_def().identifier().clone(),
            new_body,
        );

        Ok(CProgram::Program(new_fndef))
    }
}
