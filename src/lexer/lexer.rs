use std::io::{BufRead, Read};
use std::{fs::File, io::BufReader, path::PathBuf};

use crate::lexer::PATTERNS;
use crate::lexer::token::Token;

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
            let line = line?.trim_start().to_string();

            while position < line.len() {
                // Skip whitespace
                if line
                    .chars()
                    .nth(position)
                    .map_or(false, |c| c.is_whitespace())
                {
                    position += 1;
                    continue;
                }

                if let Some(token) = find_match(&line, position) {
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
    let mut matches = Vec::new();
    for (re, token_type) in PATTERNS.iter() {
        if let Some(matched) = re.find(text) {
            if matched.start() == 0 {
                matches.push((
                    matched.len(),
                    matched.as_str(),
                    matched.end(),
                    token_type.clone(),
                ));
            }
        }
    }
    matches.sort_by_key(|&(len, _, _, _)| std::cmp::Reverse(len));

    matches.first().map(|(_, value, end, token_type)| {
        log::debug!("Creating token '{}' for position {}", value, position);
        Token::new(value, *end + position, token_type.clone())
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
}
