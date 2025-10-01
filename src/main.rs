mod ast;
mod builtins;
mod callable;
mod environment;
mod error;
mod function;
mod interpreter;
mod lexer;
mod parser;
mod resolver;
mod token;

use crate::interpreter::Interpreter;
use crate::resolver::Resolver;
use lexer::Lexer;
use parser::Parser;
use std::env;
use std::fs;
use std::io;
use std::io::{BufRead, Write};
use std::path::PathBuf;
use std::process::exit;
use std::str::FromStr;

#[derive(Debug, Clone, Default)]
pub struct Lox {
    had_error: bool,
    had_runtime_error: bool,
    interpreter: Interpreter,
}

impl Lox {
    fn new() -> Self {
        Self {
            had_error: false,
            had_runtime_error: false,
            interpreter: Interpreter::new(),
        }
    }

    fn main(&mut self, args: Vec<String>) {
        match args.len() {
            0 => self.run_prompt().unwrap(),
            1 => self.run_file(&args[0]).unwrap(),
            _ => {
                println!("Usage: rlox [script]");
            }
        }
    }

    fn run<S: Into<String>>(&mut self, source: S) {
        let mut scanner = Lexer::new(source.into());
        scanner.scan_tokens();
        // println!("tokens: {:?}", scanner.tokens());
        // for token in scanner.tokens(){
        //     println!("{token:?}");
        // }
        let mut parser = Parser::new(scanner.tokens().clone());
        let tree = parser.parse().unwrap_or_else(|e| {
            eprintln!("{e}");
            self.had_error = true;
            vec![]
        });

        let mut resolver = Resolver::new(&mut self.interpreter);
        resolver.resolve(&tree).unwrap_or_else(|e| {
            eprintln!("{e}");
            self.had_error = true;
        });

        self.interpreter.interpret(tree).unwrap_or_else(|e| {
            eprintln!("{e}");
            self.had_runtime_error = true;
        });
    }

    fn run_file(&mut self, str_path: &str) -> io::Result<()> {
        let bytes = fs::read_to_string(PathBuf::from_str(str_path).expect("invalid path"))?;
        self.run(bytes);
        if self.had_error {
            exit(65);
        }
        if self.had_runtime_error {
            exit(70);
        }
        Ok(())
    }

    fn run_prompt(&mut self) -> io::Result<()> {
        let stdin = io::stdin();
        let mut stdin_lock = stdin.lock();
        let stdout = io::stdout();
        let mut stdout_lock = stdout.lock();

        loop {
            stdout_lock.write_all(b">")?;
            stdout_lock.flush()?;

            let mut input = String::new();
            let bytes_read = stdin_lock.read_line(&mut input)?;

            if bytes_read == 0 {
                break;
            }

            let line = input.trim_end();
            self.run(line);
            self.had_error = false;
        }
        Ok(())
    }
}

fn main() {
    let args: Vec<String> = env::args().skip(1).collect();
    let mut lox = Lox::new();
    lox.main(args)
}
