use crate::{parser, token::Token};

#[derive(PartialEq, Clone)]
pub enum Literal {
  False,
  True,
  Nil,
  Number(f64),
  String(String),
}

#[derive(PartialEq, Clone)]
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

#[derive(PartialEq, Clone)]
pub enum UnaryOperator {
  Minus,
  Bang,
}

#[derive(PartialEq, Clone)]
pub enum LogicalOperator {
  And,
  Or,
}

#[derive(PartialEq, Clone)]
pub enum Expr {
  Binary(Box<Expr>, Operator, Box<Expr>),
  Grouping(Box<Expr>),
  Literal(Literal),
  Unary(UnaryOperator, Box<Expr>),
  Variable(Identifier),
  Assign(Identifier, Box<Expr>),
  Logical(Box<Expr>, LogicalOperator, Box<Expr>),
}

pub type Identifier = String;

#[derive(PartialEq)]
pub enum Stmt {
  Expression(Expr),
  Print(Expr),
  Var(Identifier, Option<Expr>),
  Block(Vec<Stmt>),
  If(Expr, Box<Stmt>, Option<Box<Stmt>>),
  While(Expr, Box<Stmt>),
}

impl std::fmt::Debug for Literal {
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

impl std::fmt::Debug for Operator {
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

impl std::fmt::Debug for UnaryOperator {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      UnaryOperator::Minus => write!(f, "-"),
      UnaryOperator::Bang => write!(f, "!"),
    }
  }
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

impl std::fmt::Display for LogicalOperator {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      LogicalOperator::And => write!(f, "and"),
      LogicalOperator::Or => write!(f, "or"),
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
      Expr::Variable(name) => write!(f, "{}", name),
      Expr::Assign(name, value) => write!(f, "(assign {} = {})", name, value),
      Expr::Logical(l, op, r) => write!(f, "({} {} {})", op, r, l),
    }
  }
}

impl std::fmt::Debug for Expr {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Expr::Binary(left, operator, right) => {
        write!(f, "({:?} {:?} {:?})", left, operator, right)
      }
      Expr::Grouping(expr) => {
        write!(f, "(group {:?})", expr)
      }
      Expr::Literal(token) => {
        write!(f, "{:?}", token)
      }
      Expr::Unary(operator, right) => {
        write!(f, "({:?} {:?})", operator, right)
      }
      Expr::Variable(name) => write!(f, "{:?}", name),
      Expr::Assign(ident, value) => write!(f, "(assign {:?} `{:?}`)", ident, value),
      Expr::Logical(_, _, _) => todo!(),
    }
  }
}

impl std::fmt::Debug for Stmt {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Stmt::Expression(expr) => write!(f, "{:?}", expr),
      Stmt::Print(expr) => write!(f, "(print {:?})", expr),
      Stmt::Var(name, expr) => match expr {
        Some(expr) => write!(f, "(var {:?} `{:?}`)", name, expr),
        None => write!(f, "(var {:?})", name),
      },
      Stmt::Block(stmts) => {
        write!(f, "(block {{")?;
        for stmt in stmts {
          write!(f, " {:?}", stmt)?;
        }
        write!(f, " }})")?;
        Ok(())
      }
      Stmt::If(_, _, _) => todo!(),
      Stmt::While(_, _) => todo!(),
    }
  }
}

impl TryFrom<Expr> for Identifier {
  type Error = parser::Error;

  fn try_from(expr: Expr) -> Result<Self, parser::Error> {
    match expr {
      Expr::Variable(name) => Ok(name),
      // TODO: Reconsider this, could need some refactoring.
      // Is there any different errors could come from this function?
      _ => Err(parser::Error::InvalidAssignmentTarget),
    }
  }
}
