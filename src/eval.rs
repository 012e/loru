use crate::ast::{self, Literal, Operator, UnaryOperator};

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

fn compare_numbers(left: f64, op: Operator, right: f64) -> Literal {
	match op {
		Operator::BangEqual => {
			if left != right {
				Literal::True
			} else {
				Literal::False
			}
		}
		Operator::EqualEqual => {
			if left == right {
				Literal::True
			} else {
				Literal::False
			}
		}
		Operator::Greater => {
			if left > right {
				Literal::True
			} else {
				Literal::False
			}
		}
		Operator::GreaterEqual => {
			if left >= right {
				Literal::True
			} else {
				Literal::False
			}
		}
		Operator::Less => {
			if left < right {
				Literal::True
			} else {
				Literal::False
			}
		}
		Operator::LessEqual => {
			if left <= right {
				Literal::True
			} else {
				Literal::False
			}
		}
		_ => unreachable!(),
	}
}

fn compare_strings(left: String, op: Operator, right: String) -> Literal {
	match op {
		Operator::BangEqual => {
			if left != right {
				Literal::True
			} else {
				Literal::False
			}
		}
		Operator::EqualEqual => {
			if left == right {
				Literal::True
			} else {
				Literal::False
			}
		}
		_ => unreachable!(),
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
		// Check if literals are of the same type.
		// WARN: `Literal::False` and `Literal::True` are different types.
		if (!matches!(self, Literal::True | Literal::False)
			&& !matches!(other, Literal::True | Literal::False))
			&& (std::mem::discriminant(&self) != std::mem::discriminant(&other))
		{
			return Err(Error::UnmatchedType(
				self.clone(),
				op.clone(),
				other.clone(),
			));
		}

		match (&op, self) {
			(Operator::Plus, Literal::Number(n)) => match other {
				Literal::Number(r) => Ok(Literal::Number(n + r)),
				_ => unreachable!(),
			},
			(Operator::Minus, Literal::Number(n)) => match other {
				Literal::Number(r) => Ok(Literal::Number(n - r)),
				_ => unreachable!(),
			},
			(Operator::Star, Literal::Number(n)) => match other {
				Literal::Number(r) => Ok(Literal::Number(n * r)),
				_ => unreachable!(),
			},
			(Operator::Slash, Literal::Number(n)) => match other {
				// TODO: this is a hack, fix it, rust is getting away from using `match`
				// with floating point literals
				#[allow(illegal_floating_point_literal_pattern)]
				Literal::Number(0f64) => Err(Error::DivideByZero),
				Literal::Number(r) => Ok(Literal::Number(n / r)),
				_ => unreachable!(),
			},
			(Operator::BangEqual, Literal::Number(l))
			| (Operator::EqualEqual, Literal::Number(l))
			| (Operator::Greater, Literal::Number(l))
			| (Operator::GreaterEqual, Literal::Number(l))
			| (Operator::Less, Literal::Number(l))
			| (Operator::LessEqual, Literal::Number(l)) => match other {
				Literal::Number(r) => Ok(compare_numbers(l, op, r)),
				_ => unreachable!(),
			},
			(Operator::Plus, Literal::String(s)) => match other {
				Literal::String(r) => Ok(Literal::String(s + &r)),
				_ => unreachable!(),
			},
			(Operator::EqualEqual, Literal::String(l))
			| (Operator::BangEqual, Literal::String(l)) => match other {
				Literal::String(r) => Ok(compare_strings(l, op, r)),
				_ => unreachable!(),
			},
			(op, lit) => Err(Error::UnsupportedOperator(op.clone(), lit.clone())),
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

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn test_eval() {
		let expr = ast::Expr::Binary(
			Box::new(ast::Expr::Literal(Literal::Number(8.0))),
			Operator::Star,
			Box::new(ast::Expr::Literal(Literal::Number(2.0))),
		);
		assert_eq!(expr.eval().unwrap(), Literal::Number(16.0));
	}

	#[test]
	fn test_invalid_eval() {
		let expr = ast::Expr::Binary(
			Box::new(ast::Expr::Literal(Literal::Number(8.0))),
			Operator::Star,
			Box::new(ast::Expr::Literal(Literal::String("2".into()))),
		);
		let result = expr.eval();
		assert!(result.is_err());
		assert_eq!(
			result.unwrap_err(),
			Error::UnmatchedType(
				Literal::Number(8.0),
				Operator::Star,
				Literal::String("2".to_owned())
			)
		);
	}

	#[test]
	fn test_unsupported_operation() {
		let expr = ast::Expr::Binary(
			Box::new(ast::Expr::Literal(Literal::True)),
			Operator::Plus,
			Box::new(ast::Expr::Literal(Literal::True)),
		);
		let result = expr.eval();
		assert!(result.is_err());
		assert_eq!(
			result.unwrap_err(),
			Error::UnsupportedOperator(Operator::Plus, Literal::True)
		);
	}
}
