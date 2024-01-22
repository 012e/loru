#[derive(Debug, PartialEq, Clone)]
pub enum Token {
	LeftParen { line: u32 },
	RightParen { line: u32 },
	LeftBrace { line: u32 },
	RightBrace { line: u32 },
	Comma { line: u32 },
	Dot { line: u32 },
	Minus { line: u32 },
	Plus { line: u32 },
	Semicolon { line: u32 },
	Slash { line: u32 },
	Star { line: u32 },

	Bang { line: u32 },
	BangEqual { line: u32 },
	Equal { line: u32 },
	EqualEqual { line: u32 },
	Greater { line: u32 },
	GreaterEqual { line: u32 },
	Less { line: u32 },
	LessEqual { line: u32 },

	Identifier { line: u32 },
	String { value: String, line: u32 },
	Number { value: f64, line: u32 },

	And { line: u32 },
	Class { line: u32 },
	Else { line: u32 },
	False { line: u32 },
	Fun { line: u32 },
	For { line: u32 },
	If { line: u32 },
	Nil { line: u32 },
	Or { line: u32 },

	Print { line: u32 },
	Return { line: u32 },
	Super { line: u32 },
	This { line: u32 },
	True { line: u32 },
	Var { line: u32 },
	While { line: u32 },

	Eof { line: u32 },
}

#[allow(dead_code)]
impl Token {
	pub fn is_literal(&self) -> bool {
		matches!(self, Token::String { .. } | Token::Number { .. })
	}

	pub fn is_unary(&self) -> bool {
		matches!(self, Token::Bang { .. } | Token::Minus { .. })
	}

	pub fn is_binary(&self) -> bool {
		matches!(
			self,
			Token::BangEqual { .. }
				| Token::EqualEqual { .. }
				| Token::Greater { .. }
				| Token::GreaterEqual { .. }
				| Token::Less { .. }
				| Token::LessEqual { .. }
				| Token::Minus { .. }
				| Token::Plus { .. }
				| Token::Slash { .. }
				| Token::Star { .. }
		)
	}

	pub fn is_statement(&self) -> bool {
		matches!(
			self,
			Token::Print { .. }
				| Token::Var { .. }
				| Token::If { .. }
				| Token::While { .. }
				| Token::For { .. }
				| Token::Return { .. }
		)
	}
}

impl std::fmt::Display for Token {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Token::Eof { .. } => f.write_str("EOF"),
			Token::LeftBrace { .. } => f.write_str("{"),
			Token::RightBrace { .. } => f.write_str("}"),
			Token::LeftParen { .. } => f.write_str("("),
			Token::RightParen { .. } => f.write_str(")"),
			Token::Comma { .. } => f.write_str(","),
			Token::Dot { .. } => f.write_str("."),
			Token::Minus { .. } => f.write_str("-"),
			Token::Plus { .. } => f.write_str("+"),
			Token::Semicolon { .. } => f.write_str(";"),
			Token::Slash { .. } => f.write_str("/"),
			Token::Star { .. } => f.write_str("*"),
			Token::Bang { .. } => f.write_str("!"),
			Token::BangEqual { .. } => f.write_str("!="),
			Token::Equal { .. } => f.write_str("="),
			Token::EqualEqual { .. } => f.write_str("=="),
			Token::Greater { .. } => f.write_str(">"),
			Token::GreaterEqual { .. } => f.write_str(">="),
			Token::Less { .. } => f.write_str("<"),
			Token::LessEqual { .. } => f.write_str("<="),
			Token::Identifier { .. } => f.write_str("identifier"),
			Token::String { value, .. } => f.write_str(value),
			Token::Number { value, .. } => f.write_str(&value.to_string()),
			Token::And { .. } => f.write_str("and"),
			Token::Class { .. } => f.write_str("class"),
			Token::Else { .. } => f.write_str("else"),
			Token::False { .. } => f.write_str("false"),
			Token::Fun { .. } => f.write_str("fun"),
			Token::For { .. } => f.write_str("for"),
			Token::If { .. } => f.write_str("if"),
			Token::Nil { .. } => f.write_str("nil"),
			Token::Or { .. } => f.write_str("or"),
			Token::Print { .. } => f.write_str("print"),
			Token::Return { .. } => f.write_str("return"),
			Token::Super { .. } => f.write_str("super"),
			Token::This { .. } => f.write_str("this"),
			Token::True { .. } => f.write_str("true"),
			Token::Var { .. } => f.write_str("var"),
			Token::While { .. } => f.write_str("while"),
		}
	}
}
