use thiserror::Error;

use crate::token::{GetInfo, Info, Token, TokenInfo};

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

impl<'a> GetInfo for Scanner<'a> {
	type Source = Token;
	type Target = TokenInfo;

	fn get_info(&self, token: Token) -> TokenInfo {
		TokenInfo::new(token, Info::new(self.current_line))
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
			self.get_info(Token::Number(
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
			"and" => Ok(Some(self.get_info(Token::And))),
			"class" => Ok(Some(self.get_info(Token::Class))),
			"else" => Ok(Some(self.get_info(Token::Else))),
			"false" => Ok(Some(self.get_info(Token::False))),
			"for" => Ok(Some(self.get_info(Token::For))),
			"fun" => Ok(Some(self.get_info(Token::Fun))),
			"if" => Ok(Some(self.get_info(Token::If))),
			"nil" => Ok(Some(self.get_info(Token::Nil))),
			"or" => Ok(Some(self.get_info(Token::Or))),
			"print" => Ok(Some(self.get_info(Token::Print))),
			"return" => Ok(Some(self.get_info(Token::Return))),
			"super" => Ok(Some(self.get_info(Token::Super))),
			"this" => Ok(Some(self.get_info(Token::This))),
			"true" => Ok(Some(self.get_info(Token::True))),
			"var" => Ok(Some(self.get_info(Token::Var))),
			"while" => Ok(Some(self.get_info(Token::While))),
			_ => Ok(Some(self.get_info(Token::Identifier))),
		}
	}

	fn handle_string(&mut self) -> Result<Option<TokenInfo>, Error> {
		while let Some(c) = self.peek() {
			if *c == '"' {
				self.advance();
				return Ok(Some(
					self.get_info(Token::String(
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
			'(' => Ok(Some(self.get_info(Token::LeftParen))),
			')' => Ok(Some(self.get_info(Token::RightParen))),
			'{' => Ok(Some(self.get_info(Token::LeftBrace))),
			'}' => Ok(Some(self.get_info(Token::RightBrace))),
			',' => Ok(Some(self.get_info(Token::Comma))),
			'.' => Ok(Some(self.get_info(Token::Dot))),
			'-' => Ok(Some(self.get_info(Token::Minus))),
			'+' => Ok(Some(self.get_info(Token::Plus))),
			';' => Ok(Some(self.get_info(Token::Semicolon))),
			'*' => Ok(Some(self.get_info(Token::Star))),
			'!' => {
				if self.next_to_be('=') {
					Ok(Some(self.get_info(Token::BangEqual)))
				} else {
					Ok(Some(self.get_info(Token::Bang)))
				}
			}
			'=' => {
				if self.next_to_be('=') {
					Ok(Some(self.get_info(Token::EqualEqual)))
				} else {
					Ok(Some(self.get_info(Token::Equal)))
				}
			}
			'<' => {
				if self.next_to_be('=') {
					Ok(Some(self.get_info(Token::LessEqual)))
				} else {
					Ok(Some(self.get_info(Token::Less)))
				}
			}
			'>' => {
				if self.next_to_be('=') {
					Ok(Some(self.get_info(Token::GreaterEqual)))
				} else {
					Ok(Some(self.get_info(Token::Greater)))
				}
			}
			'/' => {
				if self.next_to_be('/') {
					while self.peek().is_some() {
						self.advance();
					}
					Ok(None)
				} else {
					Ok(Some(self.get_info(Token::Slash)))
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
		tokens.push(self.get_info(Token::Eof));
		(tokens, errors)
	}
}

#[cfg(test)]
mod tests {
	use paste::paste;

	use super::*;
	use crate::token::Token::*;

	macro_rules! token {
		($token:expr, $line:expr) => {
			paste! {
				TokenInfo::new(
					$token,
					Info::new ($line),
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
				token!(Number(1.0), 1),
				token!(Plus, 1),
				token!(Number(2.00), 1),
				token!(Plus, 1),
				token!(Number(3.14), 1),
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
		let expected_token: Vec<TokenInfo> = {
			use Token::*;
			vec![
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
				token!(Var, 2),
				token!(Identifier, 2),
				token!(Equal, 2),
				token!(Number(10.0), 2),
				token!(Semicolon, 2),
				token!(If, 3),
				token!(LeftParen, 3),
				token!(Identifier, 3),
				token!(BangEqual, 3),
				token!(Identifier, 3),
				token!(RightParen, 3),
				token!(LeftBrace, 3),
				token!(Print, 4),
				token!(String("a is not equal to b".to_string()), 4),
				token!(Semicolon, 4),
				token!(Print, 5),
				token!(String("xin chào thế giới❤️❤️".to_string()), 5),
				token!(Semicolon, 5),
				token!(RightBrace, 6),
				token!(Eof, 6),
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
