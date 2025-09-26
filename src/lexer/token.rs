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
    Not,
    And,
    Or,
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
    LessThanEq,
    GreaterThanEq,
    Assign,
}

impl TokenType {
    pub fn precedence(&self) -> u8 {
        match self {
            TokenType::Asterisk | TokenType::ForwardSlash | TokenType::Percent => 50,
            TokenType::Plus | TokenType::Hyphen => 45,
            TokenType::LeftShift | TokenType::RightShift => 40,
            TokenType::GreaterThan
            | TokenType::GreaterThanEq
            | TokenType::LessThan
            | TokenType::LessThanEq => 35,
            TokenType::Equal | TokenType::NotEqual => 30,
            TokenType::BitwiseAnd => 25,
            TokenType::BitwiseXor => 20,
            TokenType::BitwiseOr => 15,
            TokenType::And => 10,
            TokenType::Or => 5,
            TokenType::Assign => 1,
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
                | TokenType::And
                | TokenType::Or
                | TokenType::NotEqual
                | TokenType::LessThan
                | TokenType::GreaterThan
                | TokenType::LessThanEq
                | TokenType::GreaterThanEq
                | TokenType::Equal
                | TokenType::Assign
        )
    }

    pub fn is_exp(&self) -> bool {
        matches!(
            self,
            TokenType::Constant
                | TokenType::Identifier
                | TokenType::OpenParenthesis
                | TokenType::Tilde
                | TokenType::Hyphen
                | TokenType::Not
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
