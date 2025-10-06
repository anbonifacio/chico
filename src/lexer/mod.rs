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
static LESS_THAN: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"<").unwrap());
static GREATER_THAN: LazyLock<Regex> = LazyLock::new(|| Regex::new(r">").unwrap());
static LESS_OR_EQ: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"<=").unwrap());
static GREATER_OR_EQ: LazyLock<Regex> = LazyLock::new(|| Regex::new(r">=").unwrap());
static ASSIGN: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"=").unwrap());
static ASSIGN_PLUS: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"\+=").unwrap());
static ASSIGN_MINUS: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"-=").unwrap());
static ASSIGN_MULT: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"\*=").unwrap());
static ASSIGN_DIV: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"/=").unwrap());
static ASSIGN_MOD: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"%=").unwrap());
static ASSIGN_AND: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"&=").unwrap());
static ASSIGN_OR: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"\|=").unwrap());
static ASSIGN_XOR: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"\^=").unwrap());
static ASSIGN_LEFT_SHIFT: LazyLock<Regex> = LazyLock::new(|| Regex::new(r"<<=").unwrap());
static ASSIGN_RIGHT_SHIFT: LazyLock<Regex> = LazyLock::new(|| Regex::new(r">>=").unwrap());

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
        (&*TWO_HYPHENS, TokenType::DoubleHyphens),
        (&*PLUS, TokenType::Plus),
        (&*PLUS_PLUS, TokenType::DoublePlus),
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
        (&*LESS_OR_EQ, TokenType::LessThanEq),
        (&*GREATER_OR_EQ, TokenType::GreaterThanEq),
        (&*LESS_THAN, TokenType::LessThan),
        (&*GREATER_THAN, TokenType::GreaterThan),
        (&*ASSIGN, TokenType::Assign),
        (&*ASSIGN_PLUS, TokenType::AssignPlus),
        (&*ASSIGN_MINUS, TokenType::AssignMinus),
        (&*ASSIGN_MULT, TokenType::AssignMult),
        (&*ASSIGN_DIV, TokenType::AssignDiv),
        (&*ASSIGN_MOD, TokenType::AssignMod),
        (&*ASSIGN_AND, TokenType::AssignAnd),
        (&*ASSIGN_OR, TokenType::AssignOr),
        (&*ASSIGN_XOR, TokenType::AssignXor),
        (&*ASSIGN_LEFT_SHIFT, TokenType::AssignLeftShift),
        (&*ASSIGN_RIGHT_SHIFT, TokenType::AssignRightShift),
    ]
});
