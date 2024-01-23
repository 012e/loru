use thiserror::Error;

use crate::{
	ast::Expr,
	ast::{Literal, Stmt},
	token::Token,
};

#[derive(Error, Debug, PartialEq)]
pub enum Error {
	#[error("Unexpected token {0:?}")]
	UnexpectedToken(Token),
	#[error("Missing ')' after expression")]
	MissingRightParen,
	#[error("Missing ';' after expression")]
	MissingSemicolon,
}

pub struct Parser {
	tokens: Vec<Token>,
	current: usize,
}

impl Parser {
	fn new(tokens: Vec<Token>) -> Parser {
		Parser { tokens, current: 0 }
	}

	fn is_at_end(&self) -> bool {
		self.current >= self.tokens.len() || self.tokens[self.current] == Token::Eof
	}

	fn advance(&mut self) -> Token {
		let current = self.peek();
		self.current += 1;
		current
	}

	fn peek(&self) -> Token {
		self.tokens[self.current].clone()
	}
}

pub type ParseResult<T> = Result<T, Error>;

pub fn parse(tokens: Vec<Token>) -> ParseResult<Vec<Stmt>> {
	let mut parser = Parser::new(tokens);
	let mut statements: Vec<Stmt> = vec![];
	while !parser.is_at_end() {
		statements.push(parse_statement(&mut parser)?);
	}
	Ok(statements)
}

fn parse_statement(parser: &mut Parser) -> ParseResult<Stmt> {
	match parser.peek() {
		Token::Print => parse_print_statement(parser),
		_ => parse_expression_statement(parser),
	}
}

fn parse_expression_statement(parser: &mut Parser) -> ParseResult<Stmt> {
	let expr = parse_expression(parser)?;
	let t = parser.advance();
	match t {
		Token::Semicolon => {
			parser.advance();
			Ok(Stmt::Expression(expr))
		}
		_ => Err(Error::MissingSemicolon),
	}
}

fn parse_print_statement(parser: &mut Parser) -> ParseResult<Stmt> {
	let expr = parse_expression(parser)?;
	let t = parser.advance();
	match t {
		Token::Semicolon => {
			parser.advance();
			Ok(Stmt::Print(expr))
		}
		_ => Err(Error::MissingSemicolon),
	}
}

fn parse_expression(parser: &mut Parser) -> ParseResult<Expr> {
	parse_equality(parser)
}

fn parse_primary(parser: &mut Parser) -> ParseResult<Expr> {
	match parser.advance() {
		Token::False => Ok(Expr::Literal(Literal::False)),
		Token::True => Ok(Expr::Literal(Literal::True)),
		Token::Nil => Ok(Expr::Literal(Literal::Nil)),
		Token::Number(n) => Ok(Expr::Literal(Literal::Number(n))),
		Token::String(s) => Ok(Expr::Literal(Literal::String(s))),
		Token::LeftParen => {
			let expr = parse_expression(parser)?;
			match parser.advance() {
				Token::RightParen => Ok(Expr::Grouping(Box::new(expr))),
				_ => Err(Error::MissingRightParen),
			}
		}
		_ => Err(Error::UnexpectedToken(parser.peek())),
	}
}

fn parse_unary(parser: &mut Parser) -> ParseResult<Expr> {
	if let Some(operator) = match parser.peek() {
		Token::Bang | Token::Minus => Some(parser.advance()),
		_ => None,
	} {
		let right = parse_unary(parser)?;
		return Ok(Expr::Unary(operator.try_into()?, Box::new(right)));
	}
	parse_primary(parser)
}

fn parse_factor(parser: &mut Parser) -> ParseResult<Expr> {
	let mut expr = parse_unary(parser)?;
	while let Some(operator) = match parser.peek() {
		Token::Slash | Token::Star => Some(parser.advance()),
		_ => None,
	} {
		let right = parse_unary(parser)?;
		expr = Expr::Binary(Box::new(expr), operator.try_into()?, Box::new(right));
	}
	Ok(expr)
}

fn parse_term(parser: &mut Parser) -> ParseResult<Expr> {
	let mut expr = parse_factor(parser)?;

	while let Some(operator) = match parser.peek() {
		Token::Minus | Token::Plus => Some(parser.advance()),
		_ => None,
	} {
		let right = parse_factor(parser)?;
		expr = Expr::Binary(Box::new(expr), operator.try_into()?, Box::new(right));
	}

	Ok(expr)
}

fn parse_comparison(parser: &mut Parser) -> ParseResult<Expr> {
	let mut expr = parse_term(parser)?;

	while let Some(operator) = match parser.peek() {
		Token::Greater
		| Token::GreaterEqual
		| Token::Less
		| Token::LessEqual
		| Token::BangEqual
		| Token::EqualEqual => Some(parser.advance()),
		_ => None,
	} {
		let right = parse_term(parser)?;
		expr = Expr::Binary(Box::new(expr), operator.try_into()?, Box::new(right));
	}
	Ok(expr)
}

fn parse_equality(parser: &mut Parser) -> ParseResult<Expr> {
	let mut expr = parse_comparison(parser)?;

	while let Some(operator) = match parser.peek() {
		Token::BangEqual | Token::EqualEqual => Some(parser.advance()),
		_ => None,
	} {
		let right = parse_comparison(parser)?;
		expr = Expr::Binary(Box::new(expr), operator.try_into()?, Box::new(right));
	}

	Ok(expr)
}

#[cfg(test)]
mod tests {
	use super::*;
	use crate::{ast::Operator, token::Token};

	#[test]
	fn test_simple_expr_parse() {
		let tokens = vec![
			Token::Number(1.0),
			Token::Plus,
			Token::Number(2.0),
			Token::Star,
			Token::Number(3.0),
			Token::Semicolon,
			Token::Eof,
		];
		let mut stmts = parse(tokens).unwrap();
		assert_eq!(stmts.len(), 1);
		let expr = match stmts.remove(0) {
			Stmt::Expression(expr) => expr,
			_ => panic!("Expected expression statement"),
		};

		let expected = Expr::Binary(
			Box::new(Expr::Literal(Literal::Number(1.0))),
			Operator::Plus,
			Box::new(Expr::Binary(
				Box::new(Expr::Literal(Literal::Number(2.0))),
				Operator::Star,
				Box::new(Expr::Literal(Literal::Number(3.0))),
			)),
		);
		assert_eq!(expr, expected);
	}

	#[test]
	fn test_nested_expression() {
		// 1 + (2 * (9 + 9) * 3);
		let tokens = vec![
			Token::Number(1.0),
			Token::Plus,
			Token::LeftParen,
			Token::Number(2.0),
			Token::Star,
			Token::LeftParen,
			Token::Number(9.0),
			Token::Plus,
			Token::Number(9.0),
			Token::RightParen,
			Token::Star,
			Token::Number(3.0),
			Token::RightParen,
			Token::Semicolon,
			Token::Eof,
		];
		let mut statements = parse(tokens).unwrap();
		assert_eq!(statements.len(), 1);
		let expr = match statements.remove(0) {
			Stmt::Expression(expr) => expr,
			_ => panic!("Expected expression statement"),
		};

		// (9 + 9)
		let inner1 = Expr::Grouping(Box::new(Expr::Binary(
			Box::new(Expr::Literal(Literal::Number(9.0))),
			Operator::Plus,
			Box::new(Expr::Literal(Literal::Number(9.0))),
		)));

		// (2 * (9 + 9) * 3)
		let inner2 = Expr::Grouping(Box::new(Expr::Binary(
			Box::new(Expr::Binary(
				Box::new(Expr::Literal(Literal::Number(2.0))),
				Operator::Star,
				Box::new(inner1),
			)),
			Operator::Star,
			Box::new(Expr::Literal(Literal::Number(3.0))),
		)));

		// 1 + (2 * (9 + 9) * 3)
		let expected = Expr::Binary(
			Box::new(Expr::Literal(Literal::Number(1.0))),
			Operator::Plus,
			Box::new(inner2),
		);

		assert_eq!(expr, expected);
	}

	#[test]
	fn test_expr_missing_right_paren() {
		let tokens = vec![Token::LeftParen, Token::Number(1.0), Token::Eof];
		let expr = parse(tokens);
		assert_eq!(expr.unwrap_err(), Error::MissingRightParen);
	}
}
