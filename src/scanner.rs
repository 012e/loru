use paste::paste;
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

macro_rules! get_token {
	($self:ident, $token:ty) => {
		paste!(Ok(Some($token {
			line: $self.current_line
		})))
	};
}

macro_rules! get_token_with_value {
	($self:ident, $token:ty, $value:ident) => {
		paste!(Ok(Some($token {
			line: $self.current_line,
			value: $value,
		})))
	};
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
		let value = self.source[self.start..self.current]
			.iter()
			.collect::<String>()
			.parse::<f64>()
			.unwrap();

		get_token_with_value!(self, Token::Number, value)
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
			"and" => get_token!(self, Token::And),
			"class" => get_token!(self, Token::Class),
			"else" => get_token!(self, Token::Else),
			"false" => get_token!(self, Token::False),
			"for" => get_token!(self, Token::For),
			"fun" => get_token!(self, Token::Fun),
			"if" => get_token!(self, Token::If),
			"nil" => get_token!(self, Token::Nil),
			"or" => get_token!(self, Token::Or),
			"print" => get_token!(self, Token::Print),
			"return" => get_token!(self, Token::Return),
			"super" => get_token!(self, Token::Super),
			"this" => get_token!(self, Token::This),
			"true" => get_token!(self, Token::True),
			"var" => get_token!(self, Token::Var),
			"while" => get_token!(self, Token::While),
			_ => get_token!(self, Token::Identifier),
		}
	}

	fn handle_string(&mut self) -> Result<Option<Token>, Error> {
		while let Some(c) = self.peek() {
			if *c == '"' {
				self.advance();
				let value = self.source[self.start + 1..self.current - 1]
					.iter()
					.collect::<String>();
				return get_token_with_value!(self, Token::String, value);
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
			'(' => get_token!(self, Token::LeftParen),
			')' => get_token!(self, Token::RightParen),
			'{' => get_token!(self, Token::LeftBrace),
			'}' => get_token!(self, Token::RightBrace),
			',' => get_token!(self, Token::Comma),
			'.' => get_token!(self, Token::Dot),
			'-' => get_token!(self, Token::Minus),
			'+' => get_token!(self, Token::Plus),
			';' => get_token!(self, Token::Semicolon),
			'*' => get_token!(self, Token::Star),
			'!' => {
				if self.next_to_be('=') {
					get_token!(self, Token::BangEqual)
				} else {
					get_token!(self, Token::Bang)
				}
			}
			'=' => {
				if self.next_to_be('=') {
					get_token!(self, Token::EqualEqual)
				} else {
					get_token!(self, Token::Equal)
				}
			}
			'<' => {
				if self.next_to_be('=') {
					get_token!(self, Token::LessEqual)
				} else {
					get_token!(self, Token::Less)
				}
			}
			'>' => {
				if self.next_to_be('=') {
					get_token!(self, Token::GreaterEqual)
				} else {
					get_token!(self, Token::Greater)
				}
			}
			'/' => {
				if self.next_to_be('/') {
					while self.peek().is_some() {
						self.advance();
					}
					Ok(None)
				} else {
					get_token!(self, Token::Slash)
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
		tokens.push(Token::Eof {
			line: self.current_line,
		});
		(tokens, errors)
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	fn string_to_vec_char(s: &str) -> Vec<char> {
		s.chars().collect::<Vec<_>>()
	}

	macro_rules! token {
		($token:ty, $line:expr) => {
			paste! {
					$token {
						line: $line
					}
			}
		};
		($token:ty, $value:expr, $line:expr) => {
			paste! {
				$token {
					value: $value,
					line: $line,
				}
			}
		};
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
			vec![
				token!(Number, 1.0, 1),
				token!(Plus, 1),
				token!(Number, 2.0, 1),
				token!(Plus, 1),
				token!(Number, 322.0, 1),
				token!(Eof, 1),
			]
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
		let expected_tokens: Vec<Token> = {
			use Token::*;
			vec![
				// first line
				token!(Minus, 1),
				token!(Equal, 1),
				token!(Plus, 1),
				token!(Bang, 1),
				token!(Slash, 1),
				token!(Star, 1),
				token!(Less, 1),
				token!(Greater, 1),
				token!(LessEqual, 1),
				token!(EqualEqual, 1),
				token!(BangEqual, 1),
				token!(Equal, 1),
				token!(Semicolon, 1),
				// second line
				token!(Var, 2),
				token!(Identifier, 2),
				token!(Equal, 2),
				token!(Number, 10.0, 2),
				// third line
				token!(Semicolon, 3),
				token!(If, 3),
				token!(LeftParen, 3),
				// fourth line
				token!(Identifier, 4),
				token!(BangEqual, 4),
				token!(Identifier, 4),
				token!(RightParen, 4),
				token!(LeftBrace, 4),
				token!(Print, 4),
				token!(String, "a is not equal to b".to_string(), 4),
				token!(Semicolon, 4),
				// fifth line
				token!(Print, 5),
				token!(String, "xin chào thế giới❤️❤️".to_string(), 5),
				token!(Semicolon, 5),
				// sixth line
				token!(RightBrace, 6),
				token!(Eof, 6),
			]
		};
		// assert_eq!(tokens.len(), expected_token.len());
		for (i, _) in tokens.iter().enumerate() {
			println!("Testing token number {i}");
			assert_eq!(tokens[i], expected_tokens[i]);
			println!("Token {:?} passed", tokens[i]);
		}
	}
}
