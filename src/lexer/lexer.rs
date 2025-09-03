use std::io::{BufRead, Read};
use std::{fs::File, io::BufReader, path::PathBuf};

use crate::lexer::PATTERNS;
use crate::lexer::token::{Token, TokenType};

pub struct Lexer {
    reader: BufReader<File>,
}

impl Lexer {
    pub fn new(file_path: &PathBuf) -> std::io::Result<Self> {
        let file = File::open(file_path)?;
        let reader = BufReader::new(file);
        Ok(Self { reader })
    }

    pub fn tokenize(&mut self) -> std::io::Result<Vec<Token>> {
        let mut tokens = Vec::new();
        for line in self.reader.by_ref().lines() {
            let mut position = 0;

            // trim whitespaces at beginning of line
            let line = line?;
            let trimmed_line = line.trim_start();

            while position < trimmed_line.len() {
                // Skip whitespace
                if trimmed_line
                    .chars()
                    .nth(position)
                    .map_or(false, |c| c.is_whitespace())
                {
                    position += 1;
                    continue;
                }

                if let Some(token) = find_match(trimmed_line, position) {
                    position = token.end();
                    tokens.push(token);
                } else {
                    return Err(std::io::Error::new(
                        std::io::ErrorKind::InvalidData,
                        "Invalid token",
                    ));
                }
            }
        }
        Ok(tokens)
    }
}

fn find_match(line: &str, position: usize) -> Option<Token> {
    log::debug!("Finding match at position {} for line '{}'", position, line);
    if position >= line.len() {
        return None;
    }

    let text = &line[position..];
    let mut best_match: Option<(usize, &str, usize, &TokenType)> = None;

    for (re, token_type) in PATTERNS.iter() {
        if let Some(matched) = re.find(text) {
            if matched.start() == 0 {
                let matched_len = matched.len();
                if best_match.map_or(true, |(len, _, _, _)| matched_len > len) {
                    best_match = Some((matched_len, matched.as_str(), matched.end(), token_type));
                }
            }
        }
    }

    best_match.map(|(_, value, end, token_type)| {
        log::debug!("Creating token '{}' for position {}", value, position);
        Token::new(value, end + position, *token_type)
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::TokenType;

    #[test]
    fn test_find_match() {
        let line = "int main(void) {";
        let token = find_match(line, 0).unwrap();
        assert_eq!(token, Token::new("int", 3, TokenType::IntKeyword));
    }
    
    #[test]
    fn test_match_none() {
        let line = "a@b";
        let res = find_match(line, 1);
        assert!(res.is_none());
    }
}
