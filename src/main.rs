#![feature(let_chains)]
mod ast;
mod parser;
mod scanner;
mod token;

use std::io::Write;

fn repl() {
	let mut line: String = String::new();
	loop {
		line.clear();
		print!(">>> ");
		std::io::stdout().flush().unwrap();
		std::io::stdin().read_line(&mut line).unwrap();
		let source: &Vec<char> = &line.chars().collect::<Vec<_>>();
		let (tokens, errors) = crate::scanner::Scanner::new(source).scan();
		assert!(errors.is_empty());
		println!("{:?}", parser::parse(tokens));
	}
}

fn main() {
	repl();
}
