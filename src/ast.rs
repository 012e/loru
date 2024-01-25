use crate::{parser, token::Token};

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
	False,
	True,
	Nil,
	Number(f64),
	String(String),
}

#[derive(Debug, PartialEq, Clone)]
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
pub enum UnaryOperator {
	Minus,
	Bang,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
	Binary(Box<Expr>, Operator, Box<Expr>),
	Grouping(Box<Expr>),
	Literal(Literal),
	Unary(UnaryOperator, Box<Expr>),
}

#[derive(Debug, PartialEq)]
pub enum Stmt {
	Expression(Expr),
	Print(Expr),
}

impl std::fmt::Display for Literal {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Literal::False => write!(f, "false"),
			Literal::True => write!(f, "true"),
			Literal::Nil => write!(f, "nil"),
			Literal::Number(n) => write!(f, "{}", n),
			Literal::String(s) => write!(f, "{}", s),
		}
	}
}

impl std::fmt::Display for Operator {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Operator::Plus => write!(f, "+"),
			Operator::Minus => write!(f, "-"),
			Operator::Star => write!(f, "*"),
			Operator::Slash => write!(f, "/"),
			Operator::BangEqual => write!(f, "!="),
			Operator::EqualEqual => write!(f, "=="),
			Operator::Greater => write!(f, ">"),
			Operator::GreaterEqual => write!(f, ">="),
			Operator::Less => write!(f, "<"),
			Operator::LessEqual => write!(f, "<="),
		}
	}
}

impl std::fmt::Display for UnaryOperator {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			UnaryOperator::Minus => write!(f, "-"),
			UnaryOperator::Bang => write!(f, "!"),
		}
	}
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

impl TryFrom<Token> for UnaryOperator {
	type Error = parser::Error;

	fn try_from(token: Token) -> Result<Self, Self::Error> {
		match token {
			Token::Minus => Ok(UnaryOperator::Minus),
			Token::Bang => Ok(UnaryOperator::Bang),
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
