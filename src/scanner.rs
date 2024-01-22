use thiserror::Error;

use crate::token::{GetTokenInfo, Info, Token, TokenInfo};

#[derive(Debug, Error)]
pub enum Error {
	#[error("[Line {line}] Unexpected character: {character}")]
	UnexpectedCharacter { character: char, line: usize },

	#[error("[Line {line}] Unterminated string")]
	UnterminatedString { line: usize },
}

pub struct Scanner<'a> {
	source: &'a Vec<char>,
	current_line: usize,
	current: usize,
	start: usize,
}

pub type ScanResult<T> = Result<T, Error>;

impl<'a> GetTokenInfo for Scanner<'a> {
	fn get_token(&self, token: Token) -> TokenInfo {
		TokenInfo(
			token,
			Info {
				line: self.current_line,
			},
		)
	}
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

	fn handle_number(&mut self) -> Result<Option<TokenInfo>, Error> {
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
		Ok(Some(
			self.get_token(Token::Number(
				self.source[self.start..self.current]
					.iter()
					.collect::<String>()
					.parse::<f64>()
					.unwrap(),
			)),
		))
	}

	fn handle_identifier(&mut self) -> Result<Option<TokenInfo>, Error> {
		while let Some(c) = self.peek()
			&& c.is_alphabetic()
		{
			self.advance();
		}
		let iden = self.source[self.start..self.current]
			.iter()
			.collect::<String>();
		match &iden[..] {
			"and" => Ok(Some(self.get_token(Token::And))),
			"class" => Ok(Some(self.get_token(Token::Class))),
			"else" => Ok(Some(self.get_token(Token::Else))),
			"false" => Ok(Some(self.get_token(Token::False))),
			"for" => Ok(Some(self.get_token(Token::For))),
			"fun" => Ok(Some(self.get_token(Token::Fun))),
			"if" => Ok(Some(self.get_token(Token::If))),
			"nil" => Ok(Some(self.get_token(Token::Nil))),
			"or" => Ok(Some(self.get_token(Token::Or))),
			"print" => Ok(Some(self.get_token(Token::Print))),
			"return" => Ok(Some(self.get_token(Token::Return))),
			"super" => Ok(Some(self.get_token(Token::Super))),
			"this" => Ok(Some(self.get_token(Token::This))),
			"true" => Ok(Some(self.get_token(Token::True))),
			"var" => Ok(Some(self.get_token(Token::Var))),
			"while" => Ok(Some(self.get_token(Token::While))),
			_ => Ok(Some(self.get_token(Token::Identifier))),
		}
	}

	fn handle_string(&mut self) -> Result<Option<TokenInfo>, Error> {
		while let Some(c) = self.peek() {
			if *c == '"' {
				self.advance();
				return Ok(Some(
					self.get_token(Token::String(
						self.source[self.start + 1..self.current - 1]
							.iter()
							.collect::<String>(),
					)),
				));
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

	pub fn scan_next(&mut self) -> Result<Option<TokenInfo>, Error> {
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
			'(' => Ok(Some(self.get_token(Token::LeftParen))),
			')' => Ok(Some(self.get_token(Token::RightParen))),
			'{' => Ok(Some(self.get_token(Token::LeftBrace))),
			'}' => Ok(Some(self.get_token(Token::RightBrace))),
			',' => Ok(Some(self.get_token(Token::Comma))),
			'.' => Ok(Some(self.get_token(Token::Dot))),
			'-' => Ok(Some(self.get_token(Token::Minus))),
			'+' => Ok(Some(self.get_token(Token::Plus))),
			';' => Ok(Some(self.get_token(Token::Semicolon))),
			'*' => Ok(Some(self.get_token(Token::Star))),
			'!' => {
				if self.next_to_be('=') {
					Ok(Some(self.get_token(Token::BangEqual)))
				} else {
					Ok(Some(self.get_token(Token::Bang)))
				}
			}
			'=' => {
				if self.next_to_be('=') {
					Ok(Some(self.get_token(Token::EqualEqual)))
				} else {
					Ok(Some(self.get_token(Token::Equal)))
				}
			}
			'<' => {
				if self.next_to_be('=') {
					Ok(Some(self.get_token(Token::LessEqual)))
				} else {
					Ok(Some(self.get_token(Token::Less)))
				}
			}
			'>' => {
				if self.next_to_be('=') {
					Ok(Some(self.get_token(Token::GreaterEqual)))
				} else {
					Ok(Some(self.get_token(Token::Greater)))
				}
			}
			'/' => {
				if self.next_to_be('/') {
					while self.peek().is_some() {
						self.advance();
					}
					Ok(None)
				} else {
					Ok(Some(self.get_token(Token::Slash)))
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

	pub fn scan(&mut self) -> (Vec<TokenInfo>, Vec<Error>) {
		let mut tokens: Vec<TokenInfo> = Vec::new();
		let mut errors: Vec<Error> = Vec::new();
		while !self.is_at_end() {
			let token = self.scan_next();
			match token {
				Ok(None) => (),
				Ok(Some(token)) => tokens.push(token),
				Err(error) => errors.push(error),
			}
		}
		tokens.push(self.get_token(Token::Eof));
		(tokens, errors)
	}
}

#[cfg(test)]
mod tests {
	use paste::paste;

	use super::*;
	use crate::token::Token::*;

	macro_rules! t {
		($token:expr, $line:expr) => {
			paste! {
				TokenInfo(
					$token,
					Info {
						line: $line,
					},
				)
			}
		};
	}

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
		let expected_tokens: Vec<TokenInfo> = {
			vec![
				t!(Number(1.0), 1),
				t!(Plus, 1),
				t!(Number(2.00), 1),
				t!(Plus, 1),
				t!(Number(3.14), 1),
				t!(Eof, 1),
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
		let expected_token: Vec<TokenInfo> = {
			use Token::*;
			vec![
				t!(Minus, 1),
				t!(Equal, 1),
				t!(Plus, 1),
				t!(Bang, 1),
				t!(Slash, 1),
				t!(Star, 1),
				t!(Less, 1),
				t!(Greater, 1),
				t!(LessEqual, 1),
				t!(EqualEqual, 1),
				t!(BangEqual, 1),
				t!(Equal, 1),
				t!(Semicolon, 1),
				t!(Var, 2),
				t!(Identifier, 2),
				t!(Equal, 2),
				t!(Number(10.0), 2),
				t!(Semicolon, 2),
				t!(If, 3),
				t!(LeftParen, 3),
				t!(Identifier, 3),
				t!(BangEqual, 3),
				t!(Identifier, 3),
				t!(RightParen, 3),
				t!(LeftBrace, 3),
				t!(Print, 4),
				t!(String("a is not equal to b".to_string()), 4),
				t!(Semicolon, 4),
				t!(Print, 5),
				t!(String("xin chào thế giới❤️❤️".to_string()), 5),
				t!(Semicolon, 5),
				t!(RightBrace, 6),
				t!(Eof, 6),
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
