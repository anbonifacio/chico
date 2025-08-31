use clap::ArgAction;
use clap::Parser;
use std::path::PathBuf;
use std::process::Command;

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

#[derive(Debug)]
enum Stage {
    All,
    Lex,
    Parse,
    Codegen,
}

fn main() {
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
        eprintln!("gcc preprocessing failed");
        std::process::exit(1);
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
        std::process::exit(1);
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
        eprintln!("compiling failed");
        std::process::exit(1);
    }

    if cli.keep_generated.is_none() {
        std::fs::remove_file(&preprocessed)
            .expect(&format!("Failed to remove {}", preprocessed.display()));
        std::fs::remove_file(&assembled)
            .expect(&format!("Failed to remove {}", assembled.display()));
    }
}
