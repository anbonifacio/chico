use crate::codegen::asm_ast::AsmProgram;

pub struct CodegenPassTwo;

impl CodegenPassTwo {
    pub fn new() -> Self {
        CodegenPassTwo {}
    }

    pub fn replace_pseudo_reg(&self, asm_program: &AsmProgram) -> std::io::Result<AsmProgram> {
        todo!()
    }
}
