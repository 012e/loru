use thiserror::Error;

use crate::{
	ast::Expr,
	ast::Literal,
	token::{Token, TokenInfo},
};

#[derive(Error, Debug, PartialEq)]
pub enum Error {
	#[error("[line {}] Unexpected token {}", .0.info.line, .0.token)]
	UnexpectedToken(TokenInfo),

	// will match the last token in the expression
	#[error("[line {}] Missing ')' after expression", .0.info.line)]
	MissingRightParen(TokenInfo),
}

pub struct Parser {
	tokens: Vec<TokenInfo>,
	current_token: TokenInfo,
}

impl Parser {
	fn new(mut tokens: Vec<TokenInfo>) -> Parser {
		let current_token = tokens.remove(0);
		Parser {
			tokens,
			current_token,
		}
	}

	fn advance(&mut self) -> TokenInfo {
		let current = self.peek();
		if !self.tokens.is_empty() {
			self.current_token = self.tokens.remove(0);
		}
		current
	}

	fn peek(&self) -> TokenInfo {
		unsafe {
			// This is safe because we know that the current_token is always valid.
			// If something went wrong, we could have just `self.current_token.clone()`.
			// hehehehe...
			std::ptr::read(&self.current_token as *const TokenInfo)
		}
	}
}

pub type ParseResult<T> = Result<T, Error>;

pub fn parse(tokens: Vec<TokenInfo>) -> ParseResult<Expr> {
	let mut parser = Parser::new(tokens);
	parse_expression(&mut parser)
}

fn parse_expression(parser: &mut Parser) -> ParseResult<Expr> {
	parse_equality(parser)
}

fn parse_primary(parser: &mut Parser) -> ParseResult<Expr> {
	match parser.advance().token {
		Token::False => Ok(Expr::Literal(Literal::False)),
		Token::True => Ok(Expr::Literal(Literal::True)),
		Token::Nil => Ok(Expr::Literal(Literal::Nil)),
		Token::Number(n) => Ok(Expr::Literal(Literal::Number(n))),
		Token::String(s) => Ok(Expr::Literal(Literal::String(s))),
		Token::LeftParen => {
			let expr = parse_expression(parser)?;
			let current = parser.advance();
			match current.token {
				Token::RightParen => Ok(Expr::Grouping(Box::new(expr))),
				_ => Err(Error::MissingRightParen(current)),
			}
		}
		_ => Err(Error::UnexpectedToken(parser.peek())),
	}
}

fn parse_unary(parser: &mut Parser) -> ParseResult<Expr> {
	if let Some(operator) = match parser.peek().token {
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
	while let Some(operator) = match parser.peek().token {
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

	while let Some(operator) = match parser.peek().token {
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

	while let Some(operator) = match parser.peek().token {
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

	while let Some(operator) = match parser.peek().token {
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
	use paste::paste;

	use super::*;
	use crate::token;
	use crate::token::Info;
	use crate::{ast::Operator, token::Token};

	#[test]
	fn test_simple_expr_parse() {
		let tokens = vec![
			token!(Token::Number(1.0), 1),
			token!(Token::Plus, 1),
			token!(Token::Number(2.0), 1),
			token!(Token::Star, 1),
			token!(Token::Number(3.0), 1),
			token!(Token::Eof, 1),
		];
		let expr = parse(tokens);
		assert!(expr.is_ok());
		let expr = expr.unwrap();

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
		// 1 + (2 * (9 + 9) * 3)
		let tokens = vec![
			token!(Token::Number(1.0), 1),
			token!(Token::Plus, 1),
			token!(Token::LeftParen, 1),
			token!(Token::Number(2.0), 1),
			token!(Token::Star, 1),
			token!(Token::LeftParen, 1),
			token!(Token::Number(9.0), 1),
			token!(Token::Plus, 1),
			token!(Token::Number(9.0), 1),
			token!(Token::RightParen, 1),
			token!(Token::Star, 1),
			token!(Token::Number(3.0), 1),
			token!(Token::RightParen, 1),
			token!(Token::Eof, 1),
		];
		let expr = parse(tokens);
		assert!(expr.is_ok());
		let expr = expr.unwrap();

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
		let tokens = vec![
			token!(Token::LeftParen, 1),
			token!(Token::Number(1.0), 1),
			token!(Token::Semicolon, 1),
		];
		let expr = parse(tokens);
		assert!(expr.is_err());
		assert_eq!(
			expr.unwrap_err(),
			Error::MissingRightParen(TokenInfo::new(Token::Semicolon, Info::new(1)))
		);
	}
}
