use clap::ArgAction;
use clap::Parser;
use std::path::PathBuf;
use std::process::Command;

mod lexer;
mod parser;
use crate::lexer::lexer::Lexer;
use crate::parser::parser::CParser;

#[derive(Parser)]
struct Cli {
    input_file: PathBuf,
    #[arg(long, action = ArgAction::SetTrue, conflicts_with_all = ["parse", "codegen"])]
    lex: Option<bool>,
    #[arg(long, action = ArgAction::SetTrue, conflicts_with_all = ["lex", "codegen"])]
    parse: Option<bool>,
    #[arg(long, action = ArgAction::SetTrue, conflicts_with_all = ["lex", "parse"])]
    codegen: Option<bool>,
    #[arg(long, action = ArgAction::SetTrue)]
    keep_generated: Option<bool>,
}

#[derive(Debug, PartialEq)]
enum Stage {
    All,
    Lex,
    Parse,
    Codegen,
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
        return Err(std::io::Error::new(
            std::io::ErrorKind::Other,
            "gcc preprocessing failed",
        ));
    }

    let mut lexer = Lexer::new(&preprocessed)?;
    let tokens = lexer.tokenize()?;
    log::debug!("Tokens: {:?}", tokens);

    if stage == Stage::Lex {
        cleanup(&cli, &stage, &preprocessed, &assembled);
        return Ok(());
    }
    
    let parser = CParser::new();
    let function = parser.parse_program(&tokens)?;
    log::debug!("Program: {:?}", function);
    
    if stage == Stage::Parse {
        cleanup(&cli, &stage, &preprocessed, &assembled);
        return Ok(());
    }

    let assembled_status = Command::new("gcc")
        .args([
            preprocessed.to_str().unwrap(),
            "-S",
            "-O",
            "-fno-asynchronous-unwind-tables",
            "-fcf-protection=none",
            "-o",
            assembled.to_str().unwrap(),
        ])
        .status()
        .expect("Failed to run gcc -S");

    if !assembled_status.success() {
        eprintln!("assembling failed");
        return Err(std::io::Error::new(
            std::io::ErrorKind::Other,
            "gcc assembling failed",
        ));
    }

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
        return Err(std::io::Error::new(
            std::io::ErrorKind::Other,
            "gcc compilation failed",
        ));
    }

    cleanup(&cli, &stage, &preprocessed, &assembled);

    Ok(())
}

fn cleanup(cli: &Cli, stage: &Stage, preprocessed: &PathBuf, assembled: &PathBuf) {
    let keep = cli.keep_generated.unwrap_or(false);
    if !keep {
        match stage {
            Stage::Lex | Stage::Parse => {
                std::fs::remove_file(&preprocessed)
                    .expect(&format!("Failed to remove {}", preprocessed.display()));
            }
            _ => {
                std::fs::remove_file(&preprocessed)
                    .expect(&format!("Failed to remove {}", preprocessed.display()));

                std::fs::remove_file(&assembled)
                    .expect(&format!("Failed to remove {}", assembled.display()));
            }
        }
    }
}
