#![feature(let_chains)]
mod ast;
mod eval;
mod parser;
mod scanner;
mod token;

use std::{
	fs::File,
	io::{prelude::*, BufReader, Write},
};

use eval::Evalable;

fn repl() -> ! {
	let mut line: String = String::new();
	loop {
		line.clear();
		print!(">>> ");
		std::io::stdout().flush().unwrap();
		std::io::stdin().read_line(&mut line).unwrap();
		let source: &Vec<char> = &line.chars().collect::<Vec<_>>();
		let (tokens, errors) = crate::scanner::Scanner::new(source).scan();
		assert!(errors.is_empty());
		match parser::parse(tokens) {
			Ok(expr) => println!("{:?}", expr.eval()),
			Err(e) => println!("parsing error: {:?}", e),
		}
	}
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
	let args = std::env::args().collect::<Vec<_>>();
	if let Some(path) = args.get(1) {
		let file = File::open(path)?;
		let reader = BufReader::new(file);

		for line in reader.lines() {
			let source: &Vec<char> = &line.unwrap().chars().collect::<Vec<_>>();
			let (tokens, errors) = crate::scanner::Scanner::new(source).scan();
			assert!(errors.is_empty());
			match parser::parse(tokens) {
				Ok(expr) => (),
				Err(e) => panic!("{}", e),
			}
		}
	} else {
		repl();
	}
	Ok(())
}
