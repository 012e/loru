use crate::ast::{self, Literal, Operator, Stmt, UnaryOperator};

#[derive(Debug, thiserror::Error, PartialEq)]
pub enum Error {
	#[error("Unmatched type {0} {1} {2}")]
	UnmatchedType(Literal, Operator, Literal),

	#[error("Can't apply unary operator on {0:?}")]
	CantApplyUnaryOperator(Literal),

	#[error("Unsupported operator {0:?} on {1:?}")]
	UnsupportedOperator(Operator, Literal),

	#[error("Divide by zero")]
	DivideByZero,
}

pub type Result<T, E = Error> = std::result::Result<T, E>;

pub trait Evalable {
	fn eval(self) -> Result<Literal, Error>;
}

trait TryApplyBinary {
	type Error;
	fn try_apply_binary(
		self,
		op: Operator,
		other: Literal,
	) -> Result<Literal, Self::Error>;
}

trait TryApplyUnary {
	type Error;
	fn try_apply_unary(self, op: UnaryOperator) -> Result<Literal, Self::Error>;
}

impl Evalable for ast::Literal {
	fn eval(self) -> Result<Literal, Error> {
		Ok(self)
	}
}

impl TryApplyUnary for Literal {
	type Error = Error;

	fn try_apply_unary(self, op: UnaryOperator) -> Result<Literal, Self::Error> {
		match (op, &self) {
			(UnaryOperator::Minus, Literal::Number(n)) => Ok(Literal::Number(-n)),
			(UnaryOperator::Bang, Literal::False) => Ok(Literal::True),
			(UnaryOperator::Bang, Literal::True) => Ok(Literal::False),
			(UnaryOperator::Bang, Literal::Nil) => Ok(Literal::True),
			_ => Err(Error::CantApplyUnaryOperator(self)),
		}
	}
}

impl TryApplyBinary for Literal {
	type Error = Error;

	fn try_apply_binary(
		self,
		op: Operator,
		other: Literal,
	) -> Result<Literal, Self::Error> {
		match (&self, op, &other) {
			(Literal::False, Operator::EqualEqual, Literal::True) => Ok(Literal::False),
			(Literal::True, Operator::EqualEqual, Literal::False) => Ok(Literal::False),
			(Literal::True, Operator::EqualEqual, Literal::True) => Ok(Literal::True),
			(Literal::False, Operator::EqualEqual, Literal::False) => Ok(Literal::True),
			(Literal::False, Operator::BangEqual, Literal::True) => Ok(Literal::True),
			(Literal::True, Operator::BangEqual, Literal::False) => Ok(Literal::True),
			(Literal::True, Operator::BangEqual, Literal::True) => Ok(Literal::False),
			(Literal::False, Operator::BangEqual, Literal::False) => Ok(Literal::False),
			(Literal::Number(l), Operator::Plus, Literal::Number(r)) => {
				Ok(Literal::Number(l + r))
			}
			(Literal::Number(l), Operator::Minus, Literal::Number(r)) => {
				Ok(Literal::Number(l - r))
			}
			(Literal::Number(l), Operator::Star, Literal::Number(r)) => {
				Ok(Literal::Number(l * r))
			}
			(Literal::Number(l), Operator::Slash, Literal::Number(r)) => {
				if *r == 0.0 {
					return Err(Error::DivideByZero);
				}
				Ok(Literal::Number(l / r))
			}
			(Literal::String(l), Operator::Plus, Literal::String(r)) => {
				Ok(Literal::String(l.to_owned() + r))
			}
			(Literal::Number(l), Operator::EqualEqual, Literal::Number(r)) => {
				if l == r {
					Ok(Literal::True)
				} else {
					Ok(Literal::False)
				}
			}
			(Literal::Number(l), Operator::BangEqual, Literal::Number(r)) => {
				if l == r {
					Ok(Literal::False)
				} else {
					Ok(Literal::True)
				}
			}
			(Literal::String(l), Operator::EqualEqual, Literal::String(r)) => {
				if l == r {
					Ok(Literal::True)
				} else {
					Ok(Literal::False)
				}
			}
			(Literal::String(l), Operator::BangEqual, Literal::String(r)) => {
				if l == r {
					Ok(Literal::False)
				} else {
					Ok(Literal::True)
				}
			}
			(l, op, r) => {
				if std::mem::discriminant(l) != std::mem::discriminant(r) {
					return Err(Error::UnmatchedType(self, op, other));
				}
				Err(Error::UnsupportedOperator(op, other))
			}
		}
	}
}

impl Evalable for ast::Expr {
	fn eval(self) -> Result<Literal, Error> {
		match self {
			ast::Expr::Literal(lit) => lit.eval(),
			ast::Expr::Binary(lexpr, op, rexpr) => {
				let left_result = lexpr.eval()?;
				let right_result = rexpr.eval()?;
				left_result.try_apply_binary(op, right_result)
			}
			ast::Expr::Unary(op, expr) => {
				let result = expr.eval()?;
				result.try_apply_unary(op)
			}
			ast::Expr::Grouping(expr) => expr.eval(),
		}
	}
}

impl Evalable for Stmt {
	fn eval(self) -> crate::eval::Result<Literal, crate::eval::Error> {
		match self {
			Stmt::Expression(expr) => expr.eval(),
			Stmt::Print(expr) => {
				let literal = expr.eval()?;
				return Ok(Literal::String(format!("{}", literal)));
			}
		}
	}
}

#[cfg(test)]
mod tests {
	use super::*;
	use crate::ast::Expr;

	#[test]
	fn test_eval() {
		let statement = Stmt::Expression(Expr::Binary(
			Box::new(Expr::Literal(Literal::Number(14.0))),
			Operator::Slash,
			Box::new(Expr::Literal(Literal::Number(2.0))),
		));
		assert_eq!(statement.eval().unwrap(), Literal::Number(7.0));
	}
	#[test]
	fn test_boolean() {
		let statement1 = Stmt::Expression(Expr::Binary(
			Box::new(Expr::Literal(Literal::True)),
			Operator::EqualEqual,
			Box::new(Expr::Literal(Literal::True)),
		));
		let statement2 = Stmt::Expression(Expr::Binary(
			Box::new(Expr::Literal(Literal::Number(1.0))),
			Operator::EqualEqual,
			Box::new(Expr::Literal(Literal::Number(2.0))),
		));
		let statement3 = Stmt::Expression(Expr::Binary(
			Box::new(Expr::Literal(Literal::Number(1.0))),
			Operator::BangEqual,
			Box::new(Expr::Literal(Literal::Number(2.0))),
		));
		assert_eq!(statement1.eval().unwrap(), Literal::True);
		assert_eq!(statement2.eval().unwrap(), Literal::False);
		assert_eq!(statement3.eval().unwrap(), Literal::True);
	}

	#[test]
	fn test_invalid_eval() {
		let statement = Stmt::Expression(ast::Expr::Binary(
			Box::new(ast::Expr::Literal(Literal::Number(8.0))),
			Operator::Star,
			Box::new(ast::Expr::Literal(Literal::String("2".into()))),
		));
		assert_eq!(
			statement.eval().unwrap_err(),
			Error::UnmatchedType(
				Literal::Number(8.0),
				Operator::Star,
				Literal::String("2".to_owned())
			)
		);
	}

	#[test]
	fn test_divide_by_zero() {
		let statement = Stmt::Expression(ast::Expr::Binary(
			Box::new(ast::Expr::Literal(Literal::Number(8.0))),
			Operator::Slash,
			Box::new(ast::Expr::Literal(Literal::Number(0.0))),
		));
		assert_eq!(statement.eval().unwrap_err(), Error::DivideByZero);
	}

	#[test]
	fn test_print() {
		let stmt = ast::Stmt::Print(ast::Expr::Literal(Literal::Number(1.0)));
		assert_eq!(stmt.eval().unwrap(), Literal::String("1".to_owned()));
	}

	#[test]
	fn test_unsupported_operation() {
		let statement = Stmt::Expression(ast::Expr::Binary(
			Box::new(ast::Expr::Literal(Literal::True)),
			Operator::Plus,
			Box::new(ast::Expr::Literal(Literal::True)),
		));
		assert_eq!(
			statement.eval().unwrap_err(),
			Error::UnsupportedOperator(Operator::Plus, Literal::True)
		);
	}
}
