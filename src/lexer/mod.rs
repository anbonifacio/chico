use std::sync::LazyLock;

use regex::Regex;

use crate::lexer::token::TokenType;

pub(crate) mod lexer;
pub(crate) mod token;

static IDENTIFIER: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"[a-zA-Z_]\w*\b").unwrap());
static CONSTANT: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"[0-9]+\b").unwrap());
static INT_KEYWORD: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"int\b").unwrap());
static VOID_KEYWORD: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"void\b").unwrap());
static RETURN_KEYWORD: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"return\b").unwrap());
static OPEN_PARENTHESIS: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"\(").unwrap());
static CLOSE_PARENTHESIS: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"\)").unwrap());
static OPEN_BRACE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"\{").unwrap());
static CLOSE_BRACE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"\}").unwrap());
static SEMICOLON: LazyLock<Regex> = LazyLock::new(|| Regex::new(r";").unwrap());
static TILDE: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"~").unwrap());
static HYPHEN: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"-").unwrap());
static TWO_HYPHENS: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"--").unwrap());

static PATTERNS: LazyLock<[(&Regex, TokenType); 13]> = LazyLock::new(|| {
    [
        (&*INT_KEYWORD, TokenType::IntKeyword),
        (&*VOID_KEYWORD, TokenType::VoidKeyword),
        (&*RETURN_KEYWORD, TokenType::ReturnKeyword),
        (&*IDENTIFIER, TokenType::Identifier),
        (&*CONSTANT, TokenType::Constant),
        (&*OPEN_PARENTHESIS, TokenType::OpenParenthesis),
        (&*CLOSE_PARENTHESIS, TokenType::CloseParenthesis),
        (&*OPEN_BRACE, TokenType::OpenBrace),
        (&*CLOSE_BRACE, TokenType::CloseBrace),
        (&*SEMICOLON, TokenType::Semicolon),
        (&*TILDE, TokenType::Tilde),
        (&*HYPHEN, TokenType::Hyphen),
        (&*TWO_HYPHENS, TokenType::TwoHyphens),
    ]
});
