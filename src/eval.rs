use std::rc::Rc;

use crate::{
	ast::{self, Literal, Operator, Stmt, UnaryOperator},
	environment::Environment,
};

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

	#[error("Undefined variable {0}")]
	UndefinedVariable(String),
}

pub type Result<T, E = Error> = std::result::Result<T, E>;

pub trait Evalable {
	type Output;
	fn eval(self, environment: &Environment) -> Result<Self::Output, Error>;
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
	type Output = Literal;

	fn eval(self, _: &Environment) -> Result<Literal, Error> {
		Ok(self)
	}
}

impl Evalable for ast::Expr {
	type Output = Literal;

	fn eval(self, env: &Environment) -> Result<Literal, Error> {
		match self {
			ast::Expr::Literal(lit) => lit.eval(env),
			ast::Expr::Binary(lexpr, op, rexpr) => {
				let left_result = lexpr.eval(env)?;
				let right_result = rexpr.eval(env)?;
				left_result.try_apply_binary(op, right_result)
			}
			ast::Expr::Unary(op, expr) => {
				let result = expr.eval(env)?;
				result.try_apply_unary(op)
			}
			ast::Expr::Grouping(expr) => expr.eval(env),
			ast::Expr::Variable(name) => match env.get(&name) {
				Some(value) => Ok(value),
				None => Err(Error::UndefinedVariable(name)),
			},
			ast::Expr::Assign(name, value) => {
				let value = value.eval(env)?;
				env.assign(&name, value.clone())?;
				Ok(value)
			}
		}
	}
}

impl Evalable for Stmt {
	type Output = ();

	fn eval(self, env: &Environment) -> Result<(), crate::eval::Error> {
		match self {
			Stmt::Expression(expr) => {
				expr.eval(env)?;
			}
			Stmt::Print(expr) => {
				let literal = expr.eval(env)?;
				println!("{}", literal);
			}
			Stmt::Var(name, maybe_expr) => match maybe_expr {
				Some(expr) => {
					let value = expr.eval(env)?;
					env.define(&name, value);
				}
				None => {
					env.define(&name, Literal::Nil);
				}
			},
			Stmt::Block(stmts) => {
				let new_env = env.new_enclosed();
				for stmt in stmts {
					stmt.eval(&new_env)?;
				}
			}
		};
		Ok(())
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

#[cfg(test)]
mod tests {
	use assert2::{assert, let_assert};

	use super::*;
	use crate::{ast::Expr, environment::Environment};

	#[test]
	fn test_eval() {
		let statement = Stmt::Expression(Expr::Binary(
			Box::new(Expr::Literal(Literal::Number(14.0))),
			Operator::Slash,
			Box::new(Expr::Literal(Literal::Number(2.0))),
		));
		assert_eq!(statement.eval(&Environment::new()).unwrap(), ());
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
		assert_eq!(statement1.eval(&Environment::new()).unwrap(), ());
		assert_eq!(statement2.eval(&Environment::new()).unwrap(), ());
		assert_eq!(statement3.eval(&Environment::new()).unwrap(), ());
	}

	#[test]
	fn test_invalid_eval() {
		let statement = Stmt::Expression(ast::Expr::Binary(
			Box::new(ast::Expr::Literal(Literal::Number(8.0))),
			Operator::Star,
			Box::new(ast::Expr::Literal(Literal::String("2".into()))),
		));
		assert_eq!(
			statement.eval(&Environment::new()).unwrap_err(),
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
		assert_eq!(
			statement.eval(&Environment::new()).unwrap_err(),
			Error::DivideByZero
		);
	}

	#[test]
	fn test_print() {
		let stmt = ast::Stmt::Print(ast::Expr::Literal(Literal::Number(1.0)));
		assert_eq!(stmt.eval(&Environment::new()).unwrap(), ());
	}

	#[test]
	fn test_unsupported_operation() {
		let statement = Stmt::Expression(ast::Expr::Binary(
			Box::new(ast::Expr::Literal(Literal::True)),
			Operator::Plus,
			Box::new(ast::Expr::Literal(Literal::True)),
		));
		assert_eq!(
			statement.eval(&Environment::new()).unwrap_err(),
			Error::UnsupportedOperator(Operator::Plus, Literal::True)
		);
	}

	#[test]
	fn test_assign() {
		let env = Environment::new();
		Stmt::Var("a".to_owned(), Some(Expr::Literal(Literal::Number(1.0))))
			.eval(&env)
			.unwrap();
		Stmt::Block(vec![Stmt::Expression(Expr::Assign(
			"a".to_owned(),
			Box::new(Expr::Literal(Literal::Number(2.0))),
		))])
		.eval(&env)
		.unwrap();
		assert!(env.get("a").unwrap() == Literal::Number(2.0));
	}
}
