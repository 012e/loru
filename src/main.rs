#![feature(let_chains)]
mod ast;
mod eval;
mod parser;
mod scanner;
mod token;

// fn repl() -> ! {
// let mut line: String = String::new();
// loop {
// 	line.clear();
// 	print!(">>> ");
// 	std::io::stdout().flush().unwrap();
// 	std::io::stdin().read_line(&mut line).unwrap();
// 	let source: &Vec<char> = &line.chars().collect::<Vec<_>>();
// 	let (tokens, errors) = crate::scanner::Scanner::new(source).scan();
// 	assert!(errors.is_empty());
// 	match parser::parse(tokens) {
// 		Ok(expr) => println!("{:?}", expr.eval()),
// 		Err(e) => println!("parsing error: {:?}", e),
// 	}
// }
// }

fn main() {
	// repl();
}
