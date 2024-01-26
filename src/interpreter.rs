use std::{io::Write, path::Path};

use crate::{ast::Stmt, environment::Environment, eval::Evalable};

pub fn run_string(source: String, env: &Environment) {
	let source: &Vec<char> = &source.chars().collect::<Vec<_>>();
	let (tokens, errors) = crate::scanner::Scanner::new(source).scan();
	if errors.len() > 0 {
		for error in errors {
			println!("{}", error);
		}
	} else {
		let (stmts, errors) = crate::parser::parse(tokens);
		if errors.len() > 0 {
			for error in errors {
				println!("{}", error);
			}
		} else {
			if let Err(e) = run(stmts, env) {
				println!("runtime error: {}", e);
			}
		}
	}
}

fn run(stmts: Vec<Stmt>, env: &Environment) -> Result<(), crate::eval::Error> {
	for stmt in stmts.into_iter() {
		stmt.eval(env)?;
	}
	Ok(())
}

pub fn run_file(path: &Path) {
	let source = std::fs::read_to_string(path).unwrap();
	let env = Environment::new();
	run_string(source, &env);
}

pub fn repl() -> ! {
	let mut line: String = String::new();
	let env = Environment::new();
	println!("Entering REPL");
	loop {
		line.clear();
		print!(">>> ");
		std::io::stdout().flush().unwrap();
		std::io::stdin().read_line(&mut line).unwrap();
		run_string(line.clone(), &env);
	}
}
