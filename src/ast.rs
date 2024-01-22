use enum_display::EnumDisplay;

use crate::{parser, token::Token};

#[derive(Debug, PartialEq, Clone, EnumDisplay)]
pub enum Literal {
	False,
	True,
	Nil,
	Number(f64),
	String(String),
}

#[derive(Debug, PartialEq, Clone, EnumDisplay)]
pub enum Operator {
	Plus,
	Minus,
	Star,
	Slash,
	BangEqual,
	EqualEqual,
	Greater,
	GreaterEqual,
	Less,
	LessEqual,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
	Binary(Box<Expr>, Operator, Box<Expr>),
	Grouping(Box<Expr>),
	Literal(Literal),
	Unary(Token, Box<Expr>),
}

impl TryFrom<Token> for Operator {
	type Error = parser::Error;

	fn try_from(token: Token) -> Result<Self, Self::Error> {
		match token {
			Token::Minus => Ok(Operator::Minus),
			Token::Plus => Ok(Operator::Plus),
			Token::Slash => Ok(Operator::Slash),
			Token::Star => Ok(Operator::Star),
			Token::BangEqual => Ok(Operator::BangEqual),
			Token::EqualEqual => Ok(Operator::EqualEqual),
			Token::Greater => Ok(Operator::Greater),
			Token::GreaterEqual => Ok(Operator::GreaterEqual),
			Token::Less => Ok(Operator::Less),
			Token::LessEqual => Ok(Operator::LessEqual),
			_ => Err(Self::Error::UnexpectedToken(token)),
		}
	}
}

impl std::fmt::Display for Expr {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		match self {
			Expr::Binary(left, operator, right) => {
				write!(f, "({} {} {})", operator, left, right)
			}
			Expr::Grouping(expr) => {
				write!(f, "(group {})", expr)
			}
			Expr::Literal(token) => {
				write!(f, "{}", token)
			}
			Expr::Unary(operator, right) => {
				write!(f, "({} {})", operator, right)
			}
		}
	}
}
