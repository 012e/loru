use thiserror::Error;

use crate::token::Token;

#[derive(Debug, Error)]
pub enum Error {
  #[error("[Line {line}]Unexpected character: {character}")]
  UnexpectedCharacter { character: char, line: u32 },

  #[error("[Line {line}] Unterminated string")]
  UnterminatedString { line: u32 },
}

pub struct Scanner<'a> {
  source: &'a Vec<char>,
  current_line: u32,
  current: usize,
  start: usize,
}

impl<'a> Scanner<'a> {
  pub fn new(source: &'a Vec<char>) -> Self {
    Self {
      source,
      current_line: 1,
      current: 0,
      start: 0,
    }
  }

  fn is_at_end(&self) -> bool {
    self.current >= self.source.len()
  }

  fn peek(&self) -> Option<&char> {
    if self.is_at_end() {
      return None;
    }
    Some(&self.source[self.current])
  }

  fn advance(&mut self) -> Option<&char> {
    if self.is_at_end() {
      return None;
    }
    let c = &self.source[self.current];
    self.current += 1;
    Some(c)
  }

  fn handle_number(&mut self) -> Result<Option<Token>, Error> {
    while let Some(c) = self.peek()
      && c.is_ascii_digit()
    {
      self.advance();
    }
    if let Some(c) = self.peek()
      && *c == '.'
    {
      self.advance();
    }
    while let Some(c) = self.peek()
      && c.is_ascii_digit()
    {
      self.advance();
    }
    Ok(Some(Token::Number(
      self.source[self.start..self.current]
        .iter()
        .collect::<String>()
        .parse::<f64>()
        .unwrap(),
    )))
  }

  fn handle_identifier(&mut self) -> Result<Option<Token>, Error> {
    while let Some(c) = self.peek()
      && c.is_alphabetic()
    {
      self.advance();
    }
    let iden = self.source[self.start..self.current]
      .iter()
      .collect::<String>();
    match &iden[..] {
      "and" => Ok(Some(Token::And)),
      "class" => Ok(Some(Token::Class)),
      "else" => Ok(Some(Token::Else)),
      "false" => Ok(Some(Token::False)),
      "for" => Ok(Some(Token::For)),
      "fun" => Ok(Some(Token::Fun)),
      "if" => Ok(Some(Token::If)),
      "nil" => Ok(Some(Token::Nil)),
      "or" => Ok(Some(Token::Or)),
      "print" => Ok(Some(Token::Print)),
      "return" => Ok(Some(Token::Return)),
      "super" => Ok(Some(Token::Super)),
      "this" => Ok(Some(Token::This)),
      "true" => Ok(Some(Token::True)),
      "var" => Ok(Some(Token::Var)),
      "while" => Ok(Some(Token::While)),
      "break" => Ok(Some(Token::Break)),
      name => Ok(Some(Token::Identifier(name.into()))),
    }
  }

  fn handle_string(&mut self) -> Result<Option<Token>, Error> {
    while let Some(c) = self.peek() {
      if *c == '"' {
        self.advance();
        return Ok(Some(Token::String(
          self.source[self.start + 1..self.current - 1]
            .iter()
            .collect::<String>(),
        )));
      } else if *c == '\n' {
        self.current_line += 1;
      }
      self.advance();
    }
    Err(Error::UnterminatedString {
      line: self.current_line,
    })
  }

  fn next_to_be(&mut self, expected: char) -> bool {
    match self.peek() {
      Some(c) => {
        if *c == expected {
          self.advance();
          true
        } else {
          false
        }
      }
      None => false,
    }
  }

  pub fn scan_next(&mut self) -> Result<Option<Token>, Error> {
    self.start = self.current;
    let c = match self.advance() {
      Some(c) => c,
      None => return Ok(None),
    };
    match c {
      ' ' | '\r' | '\t' => Ok(None),
      '\n' => {
        self.current_line += 1;
        Ok(None)
      }
      '(' => Ok(Some(Token::LeftParen)),
      ')' => Ok(Some(Token::RightParen)),
      '{' => Ok(Some(Token::LeftBrace)),
      '}' => Ok(Some(Token::RightBrace)),
      ',' => Ok(Some(Token::Comma)),
      '.' => Ok(Some(Token::Dot)),
      '-' => Ok(Some(Token::Minus)),
      '+' => Ok(Some(Token::Plus)),
      ';' => Ok(Some(Token::Semicolon)),
      '*' => Ok(Some(Token::Star)),
      '!' => {
        if self.next_to_be('=') {
          Ok(Some(Token::BangEqual))
        } else {
          Ok(Some(Token::Bang))
        }
      }
      '=' => {
        if self.next_to_be('=') {
          Ok(Some(Token::EqualEqual))
        } else {
          Ok(Some(Token::Equal))
        }
      }
      '<' => {
        if self.next_to_be('=') {
          Ok(Some(Token::LessEqual))
        } else {
          Ok(Some(Token::Less))
        }
      }
      '>' => {
        if self.next_to_be('=') {
          Ok(Some(Token::GreaterEqual))
        } else {
          Ok(Some(Token::Greater))
        }
      }
      '/' => {
        if self.next_to_be('/') {
          while self.peek().is_some() {
            self.advance();
          }
          Ok(None)
        } else {
          Ok(Some(Token::Slash))
        }
      }
      '"' => self.handle_string(),
      _ => {
        if c.is_ascii_digit() {
          self.handle_number()
        } else if c.is_alphabetic() {
          return self.handle_identifier();
        } else {
          return Err(Error::UnexpectedCharacter {
            character: *c,
            line: self.current_line,
          });
        }
      }
    }
  }

  pub fn scan(&mut self) -> (Vec<Token>, Vec<Error>) {
    let mut tokens: Vec<Token> = Vec::new();
    let mut errors: Vec<Error> = Vec::new();
    while !self.is_at_end() {
      let token = self.scan_next();
      match token {
        Ok(None) => (),
        Ok(Some(token)) => tokens.push(token),
        Err(error) => errors.push(error),
      }
    }
    tokens.push(Token::Eof);
    (tokens, errors)
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  fn string_to_vec_char(s: &str) -> Vec<char> {
    s.chars().collect::<Vec<_>>()
  }

  #[test]
  fn test_scan() {
    let string_to_vec_char = string_to_vec_char("1 + 2");
    let mut scanner = Scanner::new(&string_to_vec_char);
    let (tokens, errors) = scanner.scan();
    assert_eq!(tokens.len(), 4);
    assert_eq!(errors.len(), 0);
  }

  #[test]
  fn test_scan_errors() {
    let string_to_vec_char = string_to_vec_char("1 + 2 + &");
    let mut scanner = Scanner::new(&string_to_vec_char);
    let (tokens, errors) = scanner.scan();
    assert_eq!(tokens.len(), 5);
    assert_eq!(errors.len(), 1);
  }

  #[test]
  fn test_scan_string() {
    let string_to_vec_char = string_to_vec_char("\"hello world\"");
    let mut scanner = Scanner::new(&string_to_vec_char);
    let (tokens, errors) = scanner.scan();
    assert_eq!(tokens.len(), 2);
    assert_eq!(errors.len(), 0);
  }

  #[test]
  fn test_scan_string_error() {
    let string_to_vec_char = string_to_vec_char("\"hello world");
    let mut scanner = Scanner::new(&string_to_vec_char);
    let (tokens, errors) = scanner.scan();
    assert_eq!(tokens.len(), 1);
    assert_eq!(errors.len(), 1);
  }

  #[test]
  fn test_number() {
    let string_to_vec_char = string_to_vec_char("1 + 2 + 3.14");
    let mut scanner = Scanner::new(&string_to_vec_char);
    let (tokens, errors) = scanner.scan();
    let expected_tokens: Vec<Token> = {
      use Token::*;
      vec![Number(1.0), Plus, Number(2.00), Plus, Number(3.14), Eof]
    };
    assert_eq!(tokens, expected_tokens);
    assert_eq!(errors.len(), 0);
  }

  #[test]
  fn test_generic() {
    let (tokens, errors) = Scanner::new(&string_to_vec_char(
      "-=+!/*<> <= == != =;
var i = 10;
if (a != b) {
	print \"a is not equal to b\";
	print \"xin chào thế giới❤️❤️\";
}",
    ))
    .scan();
    assert!(errors.is_empty());
    let expected_token: Vec<Token> = {
      use Token::*;
      vec![
        Minus,
        Equal,
        Plus,
        Bang,
        Slash,
        Star,
        Less,
        Greater,
        LessEqual,
        EqualEqual,
        BangEqual,
        Equal,
        Semicolon,
        Var,
        Identifier("i".into()),
        Equal,
        Number(10.0),
        Semicolon,
        If,
        LeftParen,
        Identifier("a".into()),
        BangEqual,
        Identifier("b".into()),
        RightParen,
        LeftBrace,
        Print,
        String("a is not equal to b".to_string()),
        Semicolon,
        Print,
        String("xin chào thế giới❤️❤️".to_string()),
        Semicolon,
        RightBrace,
        Eof,
      ]
    };
    // assert_eq!(tokens.len(), expected_token.len());
    for (i, _) in tokens.iter().enumerate() {
      println!("Testing token number {i}");
      assert_eq!(tokens[i], expected_token[i]);
      println!("Token {:?} passed", tokens[i]);
    }
  }
}
