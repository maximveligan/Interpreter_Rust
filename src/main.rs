//Maxim Veligan
//Programming Languages
//Assignment number 5, parser

#[macro_use]
extern crate nom;

use std::io::{self, Read};
use token::read_tokens;
use token::get_tokens;
use token::Token;
use parser::parse_program;
use evaluator::evaluate_program;

mod token;
mod parser;
mod evaluator;
mod ast;

fn main() {
    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer).expect("Read error");
    let tokens: Vec<Token> = get_tokens(&buffer);

    match parse_program(&tokens) {
        Ok(program) => {
            println!(
                "Program parsed successfuly, here is the tree {:#?}",
                program
            );
            match evaluate_program(program) {
                Ok(()) => println!("Evaluated successfuly"),
                Err(msg) => println!("Program failed at {:?}", msg),
            }
        }
        Err(messages) => {
            for errors in messages {
                println!("Parsing failed due to following errors: {}", errors);
            }
        }
    }
}
