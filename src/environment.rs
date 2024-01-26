use std::{borrow::Borrow, cell::RefCell, collections::HashMap, rc::Rc};

use crate::{ast::Literal, eval};

#[derive(Clone)]
struct EnvironmentValue {
  values: HashMap<String, Literal>,
  enclosing: Option<Environment>,
}

#[derive(Clone)]
pub struct Environment(Rc<RefCell<EnvironmentValue>>);

impl EnvironmentValue {
  pub fn default() -> EnvironmentValue {
    EnvironmentValue {
      values: HashMap::new(),
      enclosing: None,
    }
  }
}

impl Environment {
  pub fn new() -> Environment {
    Environment(Rc::new(RefCell::new(EnvironmentValue::default())))
  }

  pub fn new_enclosed(&self) -> Environment {
    Environment(Rc::new(RefCell::new(EnvironmentValue {
      values: HashMap::new(),
      enclosing: Some(Environment(Rc::clone(&self.0))),
    })))
  }

  pub fn get(&self, name: &str) -> Option<Literal> {
    let env = (*self.0).borrow();
    match env.borrow().values.get(name) {
      Some(value) => Some(value.clone()),
      None => match &env.enclosing {
        Some(env) => env.get(name),
        None => None,
      },
    }
  }

  /// Soft define a variable in the current scope.
  /// This **will override** the variable if exists.
  pub fn define(&self, name: &str, value: Literal) {
    let mut env = (*self.0).borrow_mut();
    env.values.insert(name.to_string(), value);
  }

  pub fn assign(&self, name: &str, value: Literal) -> Result<(), eval::Error> {
    let mut env = (*self.0).borrow_mut();
    match env.values.get_mut(name) {
      Some(v) => {
        *v = value;
        Ok(())
      }
      None => {
        // check if the variable is already defined in the enclosing scopes
        // TODO: optimize
        if let Some(env) = &env.enclosing {
          env.assign(name, value)
        } else {
          Err(eval::Error::UndefinedVariable(name.into()))
        }
      }
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::{
    ast::{Expr, Stmt},
    eval::{Error, Evalable},
  };

  #[test]
  fn test_define() {
    let env = Environment::new();
    env.define("foo", Literal::Number(42.0));
    assert_eq!(env.get("foo"), Some(Literal::Number(42.0)));
  }

  #[test]
  fn test_define_enclosed() {
    let env = Environment::new();
    let env2 = env.new_enclosed();
    env2.define("foo", Literal::Number(42.0));
    assert_eq!(env2.get("foo"), Some(Literal::Number(42.0)));
    assert_eq!(env.get("foo"), None);
  }

  #[test]
  fn test_define_enclosed_override() {
    let env1 = Environment::new();
    let env2 = env1.new_enclosed();
    let env3 = env2.new_enclosed();
    env1.define("foo", Literal::Number(42.0));
    env2.define("foo", Literal::Number(9.0));
    env3.define("foo", Literal::String("fafa".into()));
    assert_eq!(env1.get("foo"), Some(Literal::Number(42.0)));
    assert_eq!(env2.get("foo"), Some(Literal::Number(9.0)));
    assert_eq!(env3.get("foo"), Some(Literal::String("fafa".into())));
  }

  #[test]
  fn test_global_scope() {
    let env = Environment::new();
    // var a = 1;
    // var b = 2;
    // var c = "something";
    // var IHateWritingTests;
    Stmt::Var("a".into(), Some(Expr::Literal(Literal::Number(1.0))))
      .eval(&env)
      .unwrap();
    Stmt::Var("b".into(), Some(Expr::Literal(Literal::Number(2.0))))
      .eval(&env)
      .unwrap();
    Stmt::Var(
      "c".into(),
      Some(Expr::Literal(Literal::String("something".into()))),
    )
    .eval(&env)
    .unwrap();
    Stmt::Var("IHateWritingTests".into(), None)
      .eval(&env)
      .unwrap();
    assert!(Some(Literal::Number(1.0)) == env.get("a"));
    assert!(Some(Literal::Number(2.0)) == env.get("b"));
    assert!(Some(Literal::String("something".into())) == env.get("c"));
    assert!(Some(Literal::Nil) == env.get("IHateWritingTests"));

    let stmt = Stmt::Print(Expr::Variable("notexist".into()));
    assert!(Err(Error::UndefinedVariable("notexist".into())) == stmt.eval(&env));
  }
}
