use std::sync::LazyLock;

use regex::Regex;

use crate::lexer::token::TokenType;

pub(crate) mod c_lexer;
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
static PLUS: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"\+").unwrap());
static PLUS_PLUS: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"(?:\+\+)").unwrap());
static ASTERISK: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"\*").unwrap());
static FORWARD_SLASH: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"/").unwrap());
static PERCENT: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"%").unwrap());
static BITWISE_AND: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"&").unwrap());
static BITWISE_OR: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"\|").unwrap());
static BITWISE_XOR: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"\^").unwrap());
static LEFT_SHIFT: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"<<").unwrap());
static RIGHT_SHIFT: LazyLock<Regex> = LazyLock::new(|| Regex::new(r">>").unwrap());
static NOT: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"!").unwrap());
static AND: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"&&").unwrap());
static OR: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"\|\|").unwrap());
static EQUAL: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"==").unwrap());
static NOT_EQUAL: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"!=").unwrap());
static LESS_THEN: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"<").unwrap());
static GREATER_THEN: LazyLock<Regex> = LazyLock::new(|| Regex::new(r">").unwrap());
static LESS_THEN_EQ: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"<=").unwrap());
static GREATER_THEN_EQ: LazyLock<Regex> = LazyLock::new(|| Regex::new(r">=").unwrap());

static PATTERNS: LazyLock<Vec<(&Regex, TokenType)>> = LazyLock::new(|| {
    vec![
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
        (&*PLUS, TokenType::Plus),
        (&*PLUS_PLUS, TokenType::PlusPlus),
        (&*ASTERISK, TokenType::Asterisk),
        (&*FORWARD_SLASH, TokenType::ForwardSlash),
        (&*PERCENT, TokenType::Percent),
        (&*BITWISE_AND, TokenType::BitwiseAnd),
        (&*BITWISE_OR, TokenType::BitwiseOr),
        (&*BITWISE_XOR, TokenType::BitwiseXor),
        (&*LEFT_SHIFT, TokenType::LeftShift),
        (&*RIGHT_SHIFT, TokenType::RightShift),
        (&*NOT, TokenType::Not),
        (&*AND, TokenType::And),
        (&*OR, TokenType::Or),
        (&*EQUAL, TokenType::Equal),
        (&*NOT_EQUAL, TokenType::NotEqual),
        (&*LESS_THEN_EQ, TokenType::LessThanEq),
        (&*GREATER_THEN_EQ, TokenType::GreaterThanEq),
        (&*LESS_THEN, TokenType::LessThan),
        (&*GREATER_THEN, TokenType::GreaterThan),
    ]
});
