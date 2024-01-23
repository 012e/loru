use thiserror::Error;

use crate::token::Token;

#[derive(Debug, Error, PartialEq)]
pub enum Error {
	#[error("[Line {line}]Unexpected character: {character}")]
	UnexpectedCharacter { character: char, line: u32 },

	#[error("[Line {line}] Unterminated string")]
	UnterminatedString { line: u32 },
}

#[derive(Debug, PartialEq)]
pub struct Position {
	pub line: u32,
}

impl Position {
	pub fn new(line: u32) -> Self {
		Self { line }
	}
}

pub struct Scanner<'a> {
	source: &'a Vec<char>,
	current_line: u32,
	current: usize,
	start: usize,
	errors: Vec<Error>,
	tokens: Vec<Token>,
	positions: Vec<Position>,
}

impl<'a> Scanner<'a> {
	pub fn new(source: &'a Vec<char>) -> Self {
		Self {
			source,
			current_line: 1,
			current: 0,
			start: 0,
			errors: Vec::new(),
			tokens: Vec::new(),
			positions: Vec::new(),
		}
	}

	fn is_at_end(&self) -> bool {
		self.current >= self.source.len()
	}

	fn peek(&self) -> Option<&char> {
		self.source.get(self.current)
	}

	fn advance(&mut self) -> Option<&char> {
		let c = self.source.get(self.current);
		self.current += 1;
		c
	}

	fn add_token(&mut self, token: Token) {
		self.tokens.push(token);
		self.positions.push(Position {
			line: self.current_line,
		});
	}

	fn add_error(&mut self, error: Error) {
		self.errors.push(error);
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
}

fn handle_number(scanner: &mut Scanner) {
	while let Some(c) = scanner.peek()
		&& c.is_ascii_digit()
	{
		scanner.advance();
	}
	if let Some(c) = scanner.peek()
		&& *c == '.'
	{
		scanner.advance();
	}
	while let Some(c) = scanner.peek()
		&& c.is_ascii_digit()
	{
		scanner.advance();
	}
	scanner.add_token(Token::Number(
		scanner.source[scanner.start..scanner.current]
			.iter()
			.collect::<String>()
			.parse::<f64>()
			.unwrap(),
	))
}

fn handle_identifier(scanner: &mut Scanner) {
	while let Some(c) = scanner.peek()
		&& c.is_alphabetic()
	{
		scanner.advance();
	}
	let iden = scanner.source[scanner.start..scanner.current]
		.iter()
		.collect::<String>();
	match &iden[..] {
		"and" => scanner.add_token(Token::And),
		"class" => scanner.add_token(Token::Class),
		"else" => scanner.add_token(Token::Else),
		"false" => scanner.add_token(Token::False),
		"for" => scanner.add_token(Token::For),
		"fun" => scanner.add_token(Token::Fun),
		"if" => scanner.add_token(Token::If),
		"nil" => scanner.add_token(Token::Nil),
		"or" => scanner.add_token(Token::Or),
		"print" => scanner.add_token(Token::Print),
		"return" => scanner.add_token(Token::Return),
		"super" => scanner.add_token(Token::Super),
		"this" => scanner.add_token(Token::This),
		"true" => scanner.add_token(Token::True),
		"var" => scanner.add_token(Token::Var),
		"while" => scanner.add_token(Token::While),
		_ => scanner.add_token(Token::Identifier),
	}
}

fn handle_string(scanner: &mut Scanner) {
	while let Some(c) = scanner.peek() {
		if *c == '"' {
			scanner.advance();
			return scanner.add_token(Token::String(
				scanner.source[scanner.start + 1..scanner.current - 1]
					.iter()
					.collect::<String>(),
			));
		} else if *c == '\n' {
			scanner.current_line += 1;
		}
		scanner.advance();
	}
	scanner.add_error(Error::UnterminatedString {
		line: scanner.current_line,
	});
}

pub fn scan_next(scanner: &mut Scanner) {
	scanner.start = scanner.current;
	let c = match scanner.advance() {
		Some(c) => c,
		None => return,
	};
	match c {
		' ' | '\r' | '\t' => (),
		'\n' => {
			scanner.current_line += 1;
		}
		'(' => scanner.add_token(Token::LeftParen),
		')' => scanner.add_token(Token::RightParen),
		'{' => scanner.add_token(Token::LeftBrace),
		'}' => scanner.add_token(Token::RightBrace),
		',' => scanner.add_token(Token::Comma),
		'.' => scanner.add_token(Token::Dot),
		'-' => scanner.add_token(Token::Minus),
		'+' => scanner.add_token(Token::Plus),
		';' => scanner.add_token(Token::Semicolon),
		'*' => scanner.add_token(Token::Star),
		'!' => {
			if scanner.next_to_be('=') {
				scanner.add_token(Token::BangEqual);
			} else {
				scanner.add_token(Token::Bang);
			}
		}
		'=' => {
			if scanner.next_to_be('=') {
				scanner.add_token(Token::EqualEqual);
			} else {
				scanner.add_token(Token::Equal);
			}
		}
		'<' => {
			if scanner.next_to_be('=') {
				scanner.add_token(Token::LessEqual);
			} else {
				scanner.add_token(Token::Less);
			}
		}
		'>' => {
			if scanner.next_to_be('=') {
				scanner.add_token(Token::GreaterEqual);
			} else {
				scanner.add_token(Token::Greater);
			}
		}
		'/' => {
			if scanner.next_to_be('/') {
				while scanner.peek().is_some() {
					scanner.advance();
				}
			} else {
				scanner.add_token(Token::Slash);
			}
		}
		'"' => handle_string(scanner),
		_ => {
			if c.is_ascii_digit() {
				handle_number(scanner);
			} else if c.is_alphabetic() {
				handle_identifier(scanner);
			} else {
				let c = c.clone();
				scanner.add_error(Error::UnexpectedCharacter {
					character: c,
					line: scanner.current_line,
				});
			}
		}
	}
}

pub fn scan(source: Vec<char>) -> (Vec<Token>, Vec<Position>, Vec<Error>) {
	let mut scanner = Scanner::new(&source);
	while !scanner.is_at_end() {
		scan_next(&mut scanner);
	}
	scanner.add_token(Token::Eof);
	(scanner.tokens, scanner.positions, scanner.errors)
}

#[cfg(test)]
mod tests {
	use super::*;

	fn compare_vecs<T: PartialEq + std::fmt::Debug>(source: Vec<T>, expected: Vec<T>) {
		for (i, _) in expected.iter().enumerate() {
			println!("Testing token number {i}");
			assert_eq!(source[i], expected[i]);
			println!("Token {:?} passed", source[i]);
		}
	}

	fn string_to_vec_char(s: &str) -> Vec<char> {
		s.chars().collect::<Vec<_>>()
	}

	#[test]
	fn test_scan() {
		let string_to_vec_char = string_to_vec_char("1 + 2");
		let (tokens, positions, errors) = scan(string_to_vec_char);
		let expectes_tokens: Vec<Token> = {
			use Token::*;
			vec![Number(1.0), Plus, Number(2.00), Eof]
		};
		let expected_positions: Vec<Position> = vec![
			Position { line: 1 },
			Position { line: 1 },
			Position { line: 1 },
			Position { line: 1 },
		];
		assert_eq!(positions.len(), 4);
		assert_eq!(tokens.len(), 4);
		assert_eq!(errors.len(), 0);
		compare_vecs(tokens, expectes_tokens);
		compare_vecs(positions, expected_positions);
	}

	#[test]
	fn test_scan_errors() {
		let string_to_vec_char = string_to_vec_char("1 + 2 + &");
		let (tokens, positions, errors) = scan(string_to_vec_char);
		assert_eq!(tokens.len(), 5);
		assert_eq!(positions.len(), 5);
		assert_eq!(errors.len(), 1);
		assert_eq!(
			errors[0],
			Error::UnexpectedCharacter {
				character: '&',
				line: 1,
			}
		);
	}

	#[test]
	fn test_scan_string() {
		let string_to_vec_char = string_to_vec_char("\"hello world\"");
		let (tokens, positions, errors) = scan(string_to_vec_char);
		let expected_tokens: Vec<Token> = {
			use Token::*;
			vec![String("hello world".to_string()), Eof]
		};
		let expected_positions: Vec<Position> =
			vec![Position { line: 1 }, Position { line: 1 }];
		assert_eq!(positions.len(), 2);
		assert_eq!(tokens.len(), 2);
		assert_eq!(errors.len(), 0);

		compare_vecs(tokens, expected_tokens);
		compare_vecs(positions, expected_positions);
	}

	#[test]
	fn test_scan_string_error() {
		let string_to_vec_char = string_to_vec_char("\"hello world");
		let (tokens, positions, errors) = scan(string_to_vec_char);
		assert_eq!(tokens.len(), 1);
		assert_eq!(positions.len(), 1);
		assert_eq!(errors.len(), 1);
		assert_eq!(errors[0], Error::UnterminatedString { line: 1 });
	}

	#[test]
	fn test_number() {
		let string_to_vec_char = string_to_vec_char("1 + 2 + 3.14");
		let (tokens, _, errors) = scan(string_to_vec_char);
		let expected_tokens: Vec<Token> = {
			use Token::*;
			vec![Number(1.0), Plus, Number(2.00), Plus, Number(3.14), Eof]
		};
		assert_eq!(tokens, expected_tokens);
		assert_eq!(errors.len(), 0);
	}

	#[test]
	fn test_generic() {
		let (tokens, positions, errors) = scan(string_to_vec_char(
			"-=+!/*<> <= == != =;
var i = 10;
if (a != b) {
	print \"a is not equal to b\";
	print \"xin chào thế giới❤️❤️\";
}",
		));
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
				Identifier,
				Equal,
				Number(10.0),
				Semicolon,
				If,
				LeftParen,
				Identifier,
				BangEqual,
				Identifier,
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
		let expected_position: Vec<Position> = vec![
			Position::new(1),
			Position::new(1),
			Position::new(1),
			Position::new(1),
			Position::new(1),
			Position::new(1),
			Position::new(1),
			Position::new(1),
			Position::new(1),
			Position::new(1),
			Position::new(1),
			Position::new(1),
			Position::new(1),
			Position::new(2),
			Position::new(2),
			Position::new(2),
			Position::new(2),
			Position::new(2),
			Position::new(3),
			Position::new(3),
			Position::new(3),
			Position::new(3),
			Position::new(3),
			Position::new(3),
			Position::new(3),
			Position::new(4),
			Position::new(4),
			Position::new(4),
			Position::new(5),
			Position::new(5),
			Position::new(5),
			Position::new(6),
			Position::new(6),
		];

		compare_vecs(tokens, expected_token);
		compare_vecs(positions, expected_position);
	}
}
