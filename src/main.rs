use clap::ArgAction;
use clap::Parser;
use std::path::PathBuf;
use std::process::Command;

mod codegen;
mod emitter;
mod lexer;
mod parser;
use crate::codegen::codegen::Codegen;
use crate::emitter::code_emitter::CodeEmitter;
use crate::lexer::lexer::Lexer;
use crate::parser::c_ast::ExprPool;
use crate::parser::parser::CParser;

#[derive(Parser)]
struct Cli {
    input_file: PathBuf,
    #[arg(long, action = ArgAction::SetTrue, conflicts_with_all = ["parse", "codegen", "tacky"])]
    lex: Option<bool>,
    #[arg(long, action = ArgAction::SetTrue, conflicts_with_all = ["lex", "codegen", "tacky"])]
    parse: Option<bool>,
    #[arg(long, action = ArgAction::SetTrue, conflicts_with_all = ["lex", "parse", "tacky"])]
    codegen: Option<bool>,
    #[arg(long, action = ArgAction::SetTrue, conflicts_with_all = ["lex", "parse", "codegen"])]
    tacky: Option<bool>,
    #[arg(long, action = ArgAction::SetTrue)]
    keep_generated: Option<bool>,
}

#[derive(Debug, PartialEq)]
enum Stage {
    All,
    Lex,
    Parse,
    Codegen,
    Tacky,
}

fn main() -> std::io::Result<()> {
    env_logger::init();

    let cli = Cli::parse();
    let stage = if let Some(true) = cli.lex {
        Stage::Lex
    } else if let Some(true) = cli.parse {
        Stage::Parse
    } else if let Some(true) = cli.codegen {
        Stage::Codegen
    } else if let Some(true) = cli.tacky {
        Stage::Tacky
    } else {
        Stage::All
    };

    let preprocessed = cli.input_file.with_extension("i");
    let assembled = cli.input_file.with_extension("s");
    let compiled = cli.input_file.with_extension("");

    // Generate the preprocessed source file with gcc
    let preprocessed_status = Command::new("gcc")
        .args([
            "-E",
            "-P",
            cli.input_file.to_str().unwrap(),
            "-o",
            preprocessed.to_str().unwrap(),
        ])
        .status()
        .expect("Failed to run gcc");

    if !preprocessed_status.success() {
        log::error!("gcc preprocessing failed");
        return Err(std::io::Error::other("gcc preprocessing failed"));
    }

    let mut lexer = Lexer::new(&preprocessed)?;
    let tokens = lexer.tokenize()?;
    log::debug!("Tokens: {:?}", tokens);

    if stage == Stage::Lex {
        cleanup(&cli, &stage, &preprocessed, &assembled);
        return Ok(());
    }

    let mut expr_pool = ExprPool::new();
    let mut parser = CParser::new(&mut expr_pool);
    let c_program = parser.parse_program(&tokens)?;
    log::debug!("Program: {:?}", c_program);

    if stage == Stage::Parse {
        cleanup(&cli, &stage, &preprocessed, &assembled);
        return Ok(());
    }

    let codegen = Codegen::new(&mut expr_pool);
    let code = codegen.generate_asm_ast(&c_program)?;
    log::debug!("Generated ASM AST: {:?}", code);

    if stage == Stage::Codegen {
        cleanup(&cli, &stage, &preprocessed, &assembled);
        return Ok(());
    }

    if stage == Stage::Tacky {
        cleanup(&cli, &stage, &preprocessed, &assembled);
        return Ok(());
    }

    let mut emitter = CodeEmitter::new(&assembled)?;
    emitter.emit_asm(&code)?;

    let compiled_status = Command::new("gcc")
        .args([
            assembled.to_str().unwrap(),
            "-o",
            compiled.to_str().unwrap(),
        ])
        .status()
        .expect("Failed to run gcc");

    if !compiled_status.success() {
        log::error!("compiling failed");
        return Err(std::io::Error::other("gcc compilation failed"));
    }

    cleanup(&cli, &stage, &preprocessed, &assembled);

    Ok(())
}

fn cleanup(cli: &Cli, stage: &Stage, preprocessed: &PathBuf, assembled: &PathBuf) {
    let keep = cli.keep_generated.unwrap_or(false);
    if !keep {
        match stage {
            Stage::All => {
                std::fs::remove_file(preprocessed)
                    .unwrap_or_else(|_| panic!("Failed to remove {}", preprocessed.display()));

                std::fs::remove_file(assembled)
                    .unwrap_or_else(|_| panic!("Failed to remove {}", assembled.display()));
            }
            _ => {
                std::fs::remove_file(preprocessed)
                    .unwrap_or_else(|_| panic!("Failed to remove {}", preprocessed.display()));
            }
        }
    }
}
