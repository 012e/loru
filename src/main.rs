#![feature(let_chains)]
mod ast;
mod environment;
mod eval;
mod interpreter;
mod parser;
mod scanner;
mod token;

use std::path::Path;

use clap::Parser;

#[derive(Parser)]
struct Cli {
  path: Option<String>,
}

fn main() {
  let cli = Cli::parse();
  match cli.path {
    Some(s) => {
      let path = Path::new(&s);
      interpreter::run_file(path);
    }
    None => interpreter::repl(),
  }
}
