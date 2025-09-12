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
