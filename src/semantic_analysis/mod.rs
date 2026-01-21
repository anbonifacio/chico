use crate::{
    parser::{
        c_ast::{BlockItem, CProgram},
        nodes_pool::NodesPool,
    },
    semantic_analysis::variable_resolution::VariableResolver,
};

mod variable_resolution;

pub struct SemanticAnalysis<'pool> {
    variable_resolver: VariableResolver<'pool>,
}

impl<'pool> SemanticAnalysis<'pool> {
    pub fn new(nodes_pool: &'pool mut NodesPool) -> Self {
        let variable_resolver = VariableResolver::new(nodes_pool);
        Self { variable_resolver }
    }

    pub(crate) fn analyze_program(
        &mut self,
        c_program: crate::parser::c_ast::CProgram,
    ) -> std::io::Result<CProgram> {
        let body = c_program.fn_def().body();
        log::debug!("Analyzing block items: {:?}", body);
        let new_body = body
            .iter()
            .map(|block_item| match block_item {
                crate::parser::c_ast::BlockItem::D(declaration) => Ok(BlockItem::D(
                    self.variable_resolver.resolve_declaration(declaration)?,
                )),
                crate::parser::c_ast::BlockItem::S(statement) => Ok(BlockItem::S(
                    self.variable_resolver.resolve_statement(statement)?,
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
