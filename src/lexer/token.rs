#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenType {
    CloseBrace,
    CloseParenthesis,
    Constant,
    Identifier,
    IntKeyword,
    OpenBrace,
    OpenParenthesis,
    ReturnKeyword,
    Semicolon,
    VoidKeyword,
    Tilde,
    Hyphen,
    TwoHyphens,
    Plus,
    PlusPlus,
    Asterisk,
    ForwardSlash,
    Percent,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    LeftShift,
    RightShift,
}

impl TokenType {
    pub fn precedence(&self) -> u8 {
        match self {
            TokenType::Plus | TokenType::Hyphen => 45,
            TokenType::Asterisk | TokenType::ForwardSlash | TokenType::Percent => 50,
            TokenType::LeftShift | TokenType::RightShift => 40,
            TokenType::BitwiseAnd => 35,
            TokenType::BitwiseXor => 30,
            TokenType::BitwiseOr => 25,
            _ => 0,
        }
    }

    pub fn is_binop(&self) -> bool {
        matches!(
            self,
            TokenType::Plus
                | TokenType::Hyphen
                | TokenType::Asterisk
                | TokenType::ForwardSlash
                | TokenType::Percent
                | TokenType::BitwiseAnd
                | TokenType::BitwiseOr
                | TokenType::BitwiseXor
                | TokenType::LeftShift
                | TokenType::RightShift
        )
    }
}

#[derive(Debug, PartialEq)]
pub struct Token {
    pub value: String,
    pub token_type: TokenType,
    end: usize,
}

impl Token {
    pub fn new(token: &str, end: usize, token_type: TokenType) -> Self {
        Token {
            value: token.to_string(),
            token_type,
            end,
        }
    }

    pub fn end(&self) -> usize {
        self.end
    }
}
