use crate::{
  ast::{self, Expr, Literal, LogicalOperator, Operator, Stmt, UnaryOperator},
  environment::Environment,
};

#[derive(Debug, thiserror::Error, PartialEq, Clone)]
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

  #[error("Invalid condition, found {0}")]
  InvalidCondition(Literal),
}

pub type Result<T, E = Error> = std::result::Result<T, E>;

pub trait Evalable {
  type Output;
  fn eval(&self, environment: &Environment) -> Result<Self::Output, Error>;
}

trait TryApplyBinary {
  type Error;
  fn try_apply_binary(
    &self,
    op: &Operator,
    other: &Literal,
  ) -> Result<Literal, Self::Error>;
}

trait TryApplyUnary {
  type Error;
  fn try_apply_unary(&self, op: &UnaryOperator) -> Result<Literal, Self::Error>;
}

impl Evalable for ast::Literal {
  type Output = Literal;

  fn eval(&self, _: &Environment) -> Result<Literal, Error> {
    Ok(self.clone())
  }
}

impl Evalable for ast::Expr {
  type Output = Literal;

  fn eval(&self, env: &Environment) -> Result<Literal, Error> {
    match self {
      ast::Expr::Literal(lit) => lit.eval(env),
      ast::Expr::Binary(lexpr, op, rexpr) => {
        let left_result = lexpr.eval(env)?;
        let right_result = rexpr.eval(env)?;
        left_result.try_apply_binary(op, &right_result).clone()
      }
      ast::Expr::Unary(op, expr) => {
        let result = expr.eval(env)?;
        result.try_apply_unary(op)
      }
      ast::Expr::Grouping(expr) => expr.eval(env),
      ast::Expr::Variable(name) => match env.get(&name) {
        Some(value) => Ok(value),
        None => Err(Error::UndefinedVariable(name.to_owned())),
      },
      ast::Expr::Assign(name, value) => {
        let value = value.eval(env)?;
        env.assign(&name, value.clone())?;
        Ok(value)
      }
      ast::Expr::Logical(l, op, r) => match op {
        LogicalOperator::Or => {
          let left = l.eval(env)?;
          if left == Literal::True {
            return Ok(Literal::True);
          }
          let right = r.eval(env)?;
          if right == Literal::True {
            return Ok(Literal::True);
          }
          Ok(Literal::False)
        }
        LogicalOperator::And => {
          let left = l.eval(env)?;
          if left == Literal::False {
            return Ok(Literal::False);
          }
          let right = r.eval(env)?;
          if right == Literal::False {
            return Ok(Literal::False);
          }
          Ok(Literal::True)
        }
      },
      Expr::Binary(_, _, _) => todo!(),
      Expr::Grouping(_) => todo!(),
      Expr::Literal(_) => todo!(),
      Expr::Unary(_, _) => todo!(),
      Expr::Variable(_) => todo!(),
      Expr::Assign(_, _) => todo!(),
      Expr::Logical(_, _, _) => todo!(),
      Expr::Call(_, _) => todo!(),
    }
  }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Flow {
  // the same as Return(Litertal)
  Return(Literal),
  Break,
  Continue,
  None,
}

impl Evalable for Stmt {
  type Output = Flow;

  fn eval(&self, env: &Environment) -> Result<Flow, crate::eval::Error> {
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
          match stmt.eval(&new_env)? {
            Flow::Break => return Ok(Flow::Break),
            Flow::Continue => return Ok(Flow::Continue),
            Flow::Return(literal) => return Ok(Flow::Return(literal)),
            _ => (),
          };
        }
      }
      Stmt::If(cond, true_branch, false_branch) => {
        let cond = cond.eval(env)?;
        match cond {
          Literal::True => {
            match true_branch.eval(env)? {
              Flow::Break => return Ok(Flow::Break),
              Flow::Continue => return Ok(Flow::Continue),
              Flow::Return(literal) => return Ok(Flow::Return(literal)),
              _ => (),
            };
          }
          Literal::False => match false_branch {
            Some(stmt) => match stmt.eval(env)? {
              Flow::Break => return Ok(Flow::Break),
              Flow::Continue => return Ok(Flow::Continue),
              Flow::Return(literal) => return Ok(Flow::Return(literal)),
              _ => (),
            },
            None => (),
          },
          literal => return Err(Error::InvalidCondition(literal)),
        }
      }
      Stmt::While(expr, stmt) => {
        while let Literal::True = expr.eval(env)? {
          match stmt.eval(env)? {
            // consume break
            Flow::Break => return Ok(Flow::None),
            // consume continue
            Flow::Continue => (),
            Flow::Return(literal) => return Ok(Flow::Return(literal)),
            _ => (),
          };
        }
      }
      Stmt::For(oinit, ocond, oincre, stmt) => {
        let for_env = env.new_enclosed();
        let mut stmts = vec![stmt];
        if let Some(init) = oinit {
          init.eval(&for_env)?;
        }
        let incre = if let Some(incre) = oincre {
          Box::new(Stmt::Expression(incre.clone()))
        } else {
          Box::new(Stmt::Expression(Expr::Literal(Literal::Nil)))
        };
        stmts.push(&incre);
        if let Some(cond) = ocond {
          while let Literal::True = cond.eval(&for_env)? {
            for stmt in &stmts {
              match stmt.eval(&for_env)? {
                // consume break
                Flow::Break => return Ok(Flow::None),
                // consume continue
                Flow::Continue => (),
                Flow::Return(literal) => return Ok(Flow::Return(literal)),
                _ => (),
              };
            }
          }
        } else {
          loop {
            match stmt.eval(&for_env)? {
              // consume break
              Flow::Break => return Ok(Flow::None),
              // consume continue
              Flow::Continue => (),
              Flow::Return(literal) => return Ok(Flow::Return(literal)),
              _ => (),
            };
          }
        }
      }
      Stmt::Break => return Ok(Flow::Break),
      Stmt::Continue => return Ok(Flow::Continue),
      Stmt::Function(_, _, _) => todo!(),
      Stmt::Return(_) => todo!(),
    };
    Ok(Flow::None)
  }
}

impl TryApplyUnary for Literal {
  type Error = Error;

  fn try_apply_unary(&self, op: &UnaryOperator) -> Result<Literal, Self::Error> {
    match (op, &self) {
      (UnaryOperator::Minus, Literal::Number(n)) => Ok(Literal::Number(-n)),
      (UnaryOperator::Bang, Literal::False) => Ok(Literal::True),
      (UnaryOperator::Bang, Literal::True) => Ok(Literal::False),
      (UnaryOperator::Bang, Literal::Nil) => Ok(Literal::True),
      _ => Err(Error::CantApplyUnaryOperator(self.clone())),
    }
  }
}

impl TryApplyBinary for Literal {
  type Error = Error;

  fn try_apply_binary(
    &self,
    op: &Operator,
    other: &Literal,
  ) -> Result<Literal, Self::Error> {
    match (self, op, &other) {
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
      (Literal::Number(l), Operator::Less, Literal::Number(r)) => {
        if l < r {
          Ok(Literal::True)
        } else {
          Ok(Literal::False)
        }
      }
      (Literal::Number(l), Operator::LessEqual, Literal::Number(r)) => {
        if l <= r {
          Ok(Literal::True)
        } else {
          Ok(Literal::False)
        }
      }
      (Literal::Number(l), Operator::Greater, Literal::Number(r)) => {
        if l > r {
          Ok(Literal::True)
        } else {
          Ok(Literal::False)
        }
      }
      (Literal::Number(l), Operator::GreaterEqual, Literal::Number(r)) => {
        if l >= r {
          Ok(Literal::True)
        } else {
          Ok(Literal::False)
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
          return Err(Error::UnmatchedType(
            self.clone(),
            op.clone(),
            other.clone(),
          ));
        }
        Err(Error::UnsupportedOperator(op.clone(), other.clone()))
      }
    }
  }
}

#[cfg(test)]
mod tests {
  use assert2::{assert, let_assert};

  use super::*;
  use crate::{
    ast::{Expr, Identifier},
    environment::Environment,
  };

  #[test]
  fn test_eval() {
    let statement = Stmt::Expression(Expr::Binary(
      Box::new(Expr::Literal(Literal::Number(14.0))),
      Operator::Slash,
      Box::new(Expr::Literal(Literal::Number(2.0))),
    ));
    assert_eq!(statement.eval(&Environment::new()).unwrap(), Flow::None);
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
    assert_eq!(statement1.eval(&Environment::new()).unwrap(), Flow::None);
    assert_eq!(statement2.eval(&Environment::new()).unwrap(), Flow::None);
    assert_eq!(statement3.eval(&Environment::new()).unwrap(), Flow::None);
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
    assert_eq!(stmt.eval(&Environment::new()).unwrap(), Flow::None);
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

  #[test]
  fn test_if() {
    let env = Environment::new();
    // var a = 1;
    // if (a == 1)
    //   a = 2;
    Stmt::Var("a".into(), Some(Expr::Literal(Literal::Number(1.0))))
      .eval(&env)
      .unwrap();
    Stmt::If(
      Expr::Binary(
        Box::new(Expr::Variable("a".into())),
        Operator::EqualEqual,
        Box::new(Expr::Literal(Literal::Number(1.0))),
      ),
      Box::new(Stmt::Expression(Expr::Assign(
        "a".into(),
        Box::new(Expr::Literal(Literal::Number(2.0))),
      ))),
      None,
    )
    .eval(&env)
    .unwrap();
    assert!(env.get("a").unwrap() == Literal::Number(2.0));
  }
  #[test]
  fn test_if_else() {
    let env = Environment::new();
    // var a = 1;
    // if (a == 2)
    //   a = 2;
    // else
    //   a = 3;
    Stmt::Var("a".into(), Some(Expr::Literal(Literal::Number(1.0))))
      .eval(&env)
      .unwrap();
    let then_branch = Stmt::Expression(Expr::Assign(
      "a".into(),
      Box::new(Expr::Literal(Literal::Number(2.0))),
    ));
    let else_branch = Stmt::Expression(Expr::Assign(
      "a".into(),
      Box::new(Expr::Literal(Literal::Number(3.0))),
    ));
    Stmt::If(
      Expr::Binary(
        Box::new(Expr::Variable("a".into())),
        Operator::EqualEqual,
        Box::new(Expr::Literal(Literal::Number(2.0))),
      ),
      Box::new(then_branch),
      Some(Box::new(else_branch)),
    )
    .eval(&env)
    .unwrap();
    assert!(env.get("a").unwrap() == Literal::Number(3.0));
  }

  #[test]
  fn test_and() {
    let env = Environment::new();
    // var a = 1;
    // true or a = 2;
    Stmt::Var("a".into(), Some(Expr::Literal(Literal::Number(1.0))))
      .eval(&env)
      .unwrap();
    Stmt::Expression(Expr::Logical(
      Box::new(Expr::Literal(Literal::True)),
      LogicalOperator::Or,
      Box::new(Expr::Assign(
        "a".into(),
        Box::new(Expr::Literal(Literal::Number(2.0))),
      )),
    ))
    .eval(&env)
    .unwrap();
    assert!(env.get("a").unwrap() == Literal::Number(1.0));
  }

  #[test]
  fn test_while() -> Result<(), Error> {
    let env = Environment::new();
    // var a = 1;
    // while (a < 3)
    //   a = a + 1;
    Stmt::Var("a".into(), Some(Expr::Literal(Literal::Number(1.0)))).eval(&env)?;
    let cond = Expr::Binary(
      Box::new(Expr::Variable("a".into())),
      Operator::Less,
      Box::new(Expr::Literal(Literal::Number(3.0))),
    );
    let assign = Stmt::Expression(Expr::Assign(
      "a".into(),
      Box::new(Expr::Binary(
        Box::new(Expr::Variable("a".into())),
        Operator::Plus,
        Box::new(Expr::Literal(Literal::Number(1.0))),
      )),
    ));
    let flow = Stmt::While(cond, Box::new(assign)).eval(&env)?;
    assert!(flow == Flow::None);
    assert!(env.get("a").unwrap() == Literal::Number(3.0));
    Ok(())
  }

  #[test]
  fn test_for() -> Result<(), Error> {
    let env = Environment::new();
    // var result = 0;
    // for (var a = 1; a <= 3; a = a + 1) result += a;
    let init = Stmt::Var("a".into(), Some(Expr::Literal(Literal::Number(1.0))));
    let cond = Expr::Binary(
      Box::new(Expr::Variable("a".into())),
      Operator::LessEqual,
      Box::new(Expr::Literal(Literal::Number(3.0))),
    );
    let assign = Expr::Assign(
      "a".into(),
      Box::new(Expr::Binary(
        Box::new(Expr::Variable("a".into())),
        Operator::Plus,
        Box::new(Expr::Literal(Literal::Number(1.0))),
      )),
    );
    let stmt = Stmt::Expression(Expr::Assign(
      "result".into(),
      Box::new(Expr::Binary(
        Box::new(Expr::Variable("result".into())),
        Operator::Plus,
        Box::new(Expr::Variable("a".into())),
      )),
    ));
    Stmt::Var("result".into(), Some(Expr::Literal(Literal::Number(0.0)))).eval(&env)?;
    let flow = Stmt::For(
      Some(Box::new(init)),
      Some(cond),
      Some(assign),
      Box::new(stmt),
    )
    .eval(&env)?;
    assert!(flow == Flow::None);
    assert!(env.get("result").unwrap() == Literal::Number(6.0));
    Ok(())
  }

  #[test]
  fn test_break() {
    let env = Environment::new();
    // for (var a = 1; a <= 5; a = a + 1) {
    //   result += a;
    //   if (a == 3) break;
    // }
    Stmt::Var("result".into(), Some(Expr::Literal(Literal::Number(0.0))))
      .eval(&env)
      .unwrap();
    let init = Stmt::Var("a".into(), Some(Expr::Literal(Literal::Number(1.0))));
    let cond = Expr::Binary(
      Box::new(Expr::Variable("a".into())),
      Operator::LessEqual,
      Box::new(Expr::Literal(Literal::Number(5.0))),
    );
    let increment = Expr::Assign(
      "a".into(),
      Box::new(Expr::Binary(
        Box::new(Expr::Variable("a".into())),
        Operator::Plus,
        Box::new(Expr::Literal(Literal::Number(1.0))),
      )),
    );
    let stmt = Stmt::Block(vec![
      Stmt::Expression(Expr::Assign(
        "result".into(),
        Box::new(Expr::Binary(
          Box::new(Expr::Variable("result".into())),
          Operator::Plus,
          Box::new(Expr::Variable("a".into())),
        )),
      )),
      Stmt::If(
        Expr::Binary(
          Box::new(Expr::Variable("a".into())),
          Operator::EqualEqual,
          Box::new(Expr::Literal(Literal::Number(3.0))),
        ),
        Box::new(Stmt::Break),
        None,
      ),
    ]);
    let flow = Stmt::For(
      Some(Box::new(init)),
      Some(cond),
      Some(increment),
      Box::new(stmt),
    )
    .eval(&env)
    .unwrap();

    assert!(flow == Flow::None);
    assert!(env.get("result").unwrap() == Literal::Number(6.0));
  }

  #[test]
  fn test_continue() {
    let env = Environment::new();
    // var result = 0;
    // for (var a = 1; a <= 5; a = a + 1) {
    //   if (a == 3) continue;
    //   result += a;
    // }
    Stmt::Var("result".into(), Some(Expr::Literal(Literal::Number(0.0))))
      .eval(&env)
      .unwrap();
    let init = Stmt::Var("a".into(), Some(Expr::Literal(Literal::Number(1.0))));
    let cond = Expr::Binary(
      Box::new(Expr::Variable("a".into())),
      Operator::LessEqual,
      Box::new(Expr::Literal(Literal::Number(5.0))),
    );
    let increment = Expr::Assign(
      "a".into(),
      Box::new(Expr::Binary(
        Box::new(Expr::Variable("a".into())),
        Operator::Plus,
        Box::new(Expr::Literal(Literal::Number(1.0))),
      )),
    );
    let stmt = Stmt::Block(vec![
      Stmt::If(
        Expr::Binary(
          Box::new(Expr::Variable("a".into())),
          Operator::EqualEqual,
          Box::new(Expr::Literal(Literal::Number(3.0))),
        ),
        Box::new(Stmt::Continue),
        None,
      ),
      Stmt::Expression(Expr::Assign(
        "result".into(),
        Box::new(Expr::Binary(
          Box::new(Expr::Variable("result".into())),
          Operator::Plus,
          Box::new(Expr::Variable("a".into())),
        )),
      )),
    ]);
    let flow = Stmt::For(
      Some(Box::new(init)),
      Some(cond),
      Some(increment),
      Box::new(stmt),
    )
    .eval(&env)
    .unwrap();

    assert!(flow == Flow::None);
    assert!(env.get("result").unwrap() == Literal::Number(12.0));
  }
}
