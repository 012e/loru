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

	fn advance(&mut self) -> Option<Token> {
		let current = self.peek();
		self.current += 1;
		current
	}

	fn peek(&self) -> Option<Token> {
		let token = self.tokens.get(self.current);
		match token {
			Some(Token::Eof) => None,
			None => None,
			Some(token) => Some(token.clone()),
		}
	}

	fn sync(&mut self) {
		self.advance();
		while let Some(token) = self.peek() {
			match token {
				Token::Semicolon => {
					self.advance();
					return;
				}
				Token::Class
				| Token::Fun
				| Token::Var
				| Token::For
				| Token::If
				| Token::While
				| Token::Print
				| Token::Return => return,
				_ => (),
			}
			self.advance();
		}
	}
}

pub type ParseResult<T> = Result<T, Error>;

pub fn parse(tokens: Vec<Token>) -> (Vec<Stmt>, Vec<Error>) {
	let mut parser = Parser::new(tokens);
	let mut statements: Vec<Stmt> = vec![];
	let mut errors: Vec<Error> = vec![];
	while !parser.is_at_end() {
		match parse_statement(&mut parser) {
			Ok(statement) => statements.push(statement),
			Err(error) => {
				errors.push(error);
				parser.sync();
			}
		}
	}
	(statements, errors)
}

fn parse_statement(parser: &mut Parser) -> ParseResult<Stmt> {
	match parser.peek() {
		Some(Token::Print) => parse_print_statement(parser),
		Some(_) => parse_expression_statement(parser),
		None => panic!("unexpected None"),
	}
}

fn parse_expression_statement(parser: &mut Parser) -> ParseResult<Stmt> {
	let expr = parse_expression(parser)?;
	match parser.peek() {
		Some(Token::Semicolon) => {
			parser.advance();
			Ok(Stmt::Expression(expr))
		}
		Some(_) => Err(Error::MissingSemicolon),
		None => Err(Error::UnexpectedToken(Token::Eof)),
	}
}

fn parse_print_statement(parser: &mut Parser) -> ParseResult<Stmt> {
	// advance through the print token
	parser.advance();
	let expr = parse_expression(parser)?;
	match parser.peek() {
		Some(Token::Semicolon) => {
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
	let current = parser.peek();
	match current {
		Some(Token::False) => {
			parser.advance();
			Ok(Expr::Literal(Literal::False))
		}
		Some(Token::True) => {
			parser.advance();
			Ok(Expr::Literal(Literal::True))
		}
		Some(Token::Nil) => {
			parser.advance();
			Ok(Expr::Literal(Literal::Nil))
		}
		Some(Token::Number(n)) => {
			parser.advance();
			Ok(Expr::Literal(Literal::Number(n)))
		}
		Some(Token::String(s)) => {
			parser.advance();
			Ok(Expr::Literal(Literal::String(s)))
		}
		Some(Token::LeftParen) => {
			parser.advance();
			let expr = parse_expression(parser)?;
			match parser.peek() {
				Some(Token::RightParen) => {
					parser.advance();
					Ok(Expr::Grouping(Box::new(expr)))
				}
				_ => Err(Error::MissingRightParen),
			}
		}
		Some(token) => Err(Error::UnexpectedToken(token)),
		None => Err(Error::UnexpectedToken(Token::Eof)),
	}
}

fn parse_unary(parser: &mut Parser) -> ParseResult<Expr> {
	match parser.peek() {
		Some(Token::Bang) | Some(Token::Minus) => {
			let operator = parser
				.peek()
				.expect("must be able to advance (we have just peeked)");
			parser.advance();
			let right = parse_unary(parser)?;
			Ok(Expr::Unary(operator.try_into()?, Box::new(right)))
		}
		Some(_) => parse_primary(parser),
		None => Err(Error::UnexpectedToken(Token::Eof)),
	}
}

fn parse_factor(parser: &mut Parser) -> ParseResult<Expr> {
	let mut expr = parse_unary(parser)?;
	while matches!(parser.peek(), Some(Token::Slash) | Some(Token::Star)) {
		let operator = parser.peek().expect("must be able to advance");
		parser.advance();
		let right = parse_unary(parser)?;
		expr = Expr::Binary(Box::new(expr), operator.try_into()?, Box::new(right));
	}

	Ok(expr)
}

fn parse_term(parser: &mut Parser) -> ParseResult<Expr> {
	let mut expr = parse_factor(parser)?;

	while matches!(parser.peek(), Some(Token::Minus) | Some(Token::Plus)) {
		let operator = parser.peek().expect("must be able to advance");
		parser.advance();
		let right = parse_factor(parser)?;
		expr = Expr::Binary(Box::new(expr), operator.try_into()?, Box::new(right));
	}

	Ok(expr)
}

fn parse_comparison(parser: &mut Parser) -> ParseResult<Expr> {
	let mut expr = parse_term(parser)?;
	while matches!(
		parser.peek(),
		Some(Token::Greater)
			| Some(Token::GreaterEqual)
			| Some(Token::Less)
			| Some(Token::LessEqual)
	) {
		let operator = parser.peek().expect("must be able to advance");
		parser.advance();
		let right = parse_term(parser)?;
		expr = Expr::Binary(Box::new(expr), operator.try_into()?, Box::new(right));
	}

	Ok(expr)
}

fn parse_equality(parser: &mut Parser) -> ParseResult<Expr> {
	let mut expr = parse_comparison(parser)?;

	while matches!(
		parser.peek(),
		Some(Token::BangEqual) | Some(Token::EqualEqual)
	) {
		let operator = parser.peek().expect("must be able to advance");
		parser.advance();
		let right = parse_comparison(parser)?;
		expr = Expr::Binary(Box::new(expr), operator.try_into()?, Box::new(right));
	}

	Ok(expr)
}

#[cfg(test)]
mod tests {
	use assert2::assert;

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
		let (stmts, errors) = parse(tokens);
		dbg!(&errors);
		assert!(stmts.len() == 1);
		assert!(errors.len() == 0);
		let expr = match &stmts[0] {
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
		assert!(*expr == expected);
	}

	#[test]
	fn test_simple_nested_expression() {
		let tokens = vec![
			Token::LeftParen,
			Token::Number(2.0),
			Token::Star,
			Token::Number(3.0),
			Token::RightParen,
			Token::Semicolon,
			Token::Eof,
		];
		let (stmts, errors) = parse(tokens);
		assert!(stmts.len() == 1);
		assert!(errors.is_empty());
		let expected_stmt = Stmt::Expression(Expr::Grouping(Box::new(Expr::Binary(
			Box::new(Expr::Literal(Literal::Number(2.0))),
			Operator::Star,
			Box::new(Expr::Literal(Literal::Number(3.0))),
		))));
		assert!(stmts == vec![expected_stmt]);
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
		let (stmts, errors) = parse(tokens);
		assert!(stmts.len() == 1);
		assert!(errors.len() == 0);
		let expr = match &stmts[0] {
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

		assert!(expr == &expected);
	}

	#[test]
	fn test_expr_missing_right_paren() {
		let tokens = vec![Token::LeftParen, Token::Number(1.0), Token::Eof];
		let (stmts, errors) = parse(tokens);
		assert!(stmts.is_empty());
		assert!(errors.len() == 1);
		assert!(errors[0] == Error::MissingRightParen);
	}

	#[test]
	fn test_multiple_statement() {
		let tokens = vec![
			Token::Number(1.0),
			Token::Semicolon,
			Token::Number(2.0),
			Token::Semicolon,
			Token::Eof,
		];
		let (stmts, errors) = parse(tokens);
		assert!(stmts.len() == 2);
		assert!(errors.len() == 0);
		let expected_statement = vec![
			Stmt::Expression(Expr::Literal(Literal::Number(1.0))),
			Stmt::Expression(Expr::Literal(Literal::Number(2.0))),
		];
		assert!(stmts == expected_statement);
	}

	#[test]
	fn test_print_statement() {
		let tokens = vec![
			Token::Print,
			Token::Number(1.0),
			Token::Semicolon,
			Token::Print,
			Token::Number(2.0),
			Token::Semicolon,
			Token::Eof,
		];
		let (stmts, errors) = parse(tokens);
		assert!(stmts.len() == 2);
		assert!(errors.len() == 0);
		let expected_statement = vec![
			Stmt::Print(Expr::Literal(Literal::Number(1.0))),
			Stmt::Print(Expr::Literal(Literal::Number(2.0))),
		];
		assert!(stmts == expected_statement);
	}

	#[test]
	fn test_sync() {
		// print 1 + 2;
		// print -;
		// print 4;
		// print var;
		let tokens = vec![
			Token::Print,
			Token::Number(1.0),
			Token::Plus,
			Token::Number(2.0),
			Token::Semicolon,
			Token::Print,
			Token::Minus,
			Token::Semicolon,
			Token::Print,
			Token::Number(4.0),
			Token::Semicolon,
			Token::Print,
			Token::Var,
			Token::Semicolon,
			Token::Eof,
		];
		let (stmts, errors) = parse(tokens);
		assert!(stmts.len() == 2, "checking statement len");
		assert!(errors.len() == 2, "check errors len");
		let expected_statement = vec![
			Stmt::Print(Expr::Binary(
				Box::new(Expr::Literal(Literal::Number(1.0))),
				Operator::Plus,
				Box::new(Expr::Literal(Literal::Number(2.0))),
			)),
			Stmt::Print(Expr::Literal(Literal::Number(4.0))),
		];
		let expected_error = vec![
			Error::UnexpectedToken(Token::Semicolon),
			Error::UnexpectedToken(Token::Var),
		];
		assert!(stmts == expected_statement, "checking statements");
		assert!(errors == expected_error, "checking statements");
	}
}
