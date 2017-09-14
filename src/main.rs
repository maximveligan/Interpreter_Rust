//  Maxim Veligan
//  Programming Languages Comp 149
//

#[macro_use]
extern crate nom;
use std::io::{self, Read};

#[derive(Debug)]
enum Token{
    Integer(i64),
    Identifier(String),
    Boolean(bool),
    Real(f64),
    Punctuation(Punctuation),
    Keyword(Keyword),
    Invalid(String),
}

#[derive(Clone, Copy, Debug)]
enum Punctuation {
    OpenParen,
    CloseParen,
    OpenCurly,
    CloseCurly,
    Coma,
    Plus,
    Minus,
    Multiply,
    Divide,
    Modulo,
    ColonEquals,
    NotEquals,
    LesserThan,
    GreaterThan,
    LesserEq,
    GreaterEq,
}

#[derive(Clone, Debug)]
enum Keyword {
    Var,
    Fun,
    If,
    Else,
    Return,
    Read,
    Write,
    Not,
    Or,
    And,
}

named!(invalid(&str) -> Token, map!(take_until_s!("\n"), |inv| Token::Invalid(inv.to_string())));

named!(key_word(&str) -> Keyword, alt!(
            preceded!(tag!("var"), value!(Keyword::Var)) | preceded!(tag!("fun"), value!(Keyword::Fun)) | 
            preceded!(tag!("if"), value!(Keyword:: If))| preceded!(tag!("else"), value!(Keyword::Else)) | 
            preceded!(tag!("return"), value!(Keyword::Return))| preceded!(tag!("read"), value!(Keyword::Read))| 
            preceded!(tag!("write"), value!(Keyword::Write))| preceded!(tag!("not"), value!(Keyword::Not))| 
            preceded!(tag!("or"), value!(Keyword::Or))| preceded!(tag!("and"), value!(Keyword::And)))); 

named!(identifier(&str) -> Token, 
                map!(recognize!(tuple!(one_of!(
                                "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"),
                                take_while_s!(char::is_alphanumeric))),
                                |name| Token::Identifier(name.to_string())));

named!(boolean(&str) -> Token, alt!(
            preceded!(tag!("#t"), value!(Token::Boolean(true))) | preceded!(tag!("#f"), value!(Token::Boolean(false)))));

named!(real_num(&str) -> Token,
        map_res!(
            alt!(
                  recognize!(
                    tuple!(
                        take_while_s!(apply!(char::is_digit, 10)),
                        char!('.'),
                        opt!(take_while_s!(apply!(char::is_digit, 10)))))
                | recognize!(
                    tuple!(
                        char!('.'),
                        take_while_s!(apply!(char::is_digit, 10))))),
            |float: &str| float.parse::<f64>().map(Token::Real)));

named!(punctuation(&str) -> Punctuation, ws!(alt!(
            preceded!(tag!("("), value!(Punctuation::OpenParen)) | preceded!(tag!(")"), value!(Punctuation::CloseParen)) |  
            preceded!(tag!("{"), value!(Punctuation::OpenCurly)) | preceded!(tag!("}"), value!(Punctuation::CloseCurly)) |  
            preceded!(tag!(","), value!(Punctuation::Coma)) | preceded!(tag!("+"), value!(Punctuation::Plus)) |  
            preceded!(tag!("-"), value!(Punctuation::Minus)) | preceded!(tag!("*"), value!(Punctuation::Multiply)) |  
            preceded!(tag!("/"), value!(Punctuation::Divide)) | preceded!(tag!("%"), value!(Punctuation::Modulo)) |  
            preceded!(tag!(":="), value!(Punctuation::ColonEquals)) | preceded!(tag!("!="), value!(Punctuation::NotEquals)) |  
            preceded!(tag!("<"), value!(Punctuation::LesserThan)) | preceded!(tag!(">"), value!(Punctuation::GreaterThan)) |  
            preceded!(tag!(">="), value!(Punctuation::LesserEq)) | preceded!(tag!("<="), value!(Punctuation::GreaterEq))))); 

named!(integer(&str) -> Token, map_res!(take_while_s!(apply!(char::is_digit, 10)), |int: &str| int.parse::<i64>().map(Token::Integer)));

named!(scanner(&str) -> Vec<Token>, ws!(many0!(alt_complete!(map!(
                    key_word, |kw| Token::Keyword(kw))| 
                    boolean | 
                    identifier |
                    real_num | 
                    integer | 
                    map!(punctuation, |p| Token::Punctuation(p)) |
                    invalid))));

fn main() {
    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer).expect("Read error");
    let tokens: Vec<Token> = get_tokens(&buffer);
    read_tokens(&tokens);
}

fn get_tokens(text: &str) -> Vec<Token> {
    match scanner(text) {
        nom::IResult::Done(_, result) => result,
        _ => panic!("error"),
    }
}

fn read_tokens(tokens: &Vec<Token>) -> () {
    for token in tokens {
        match token {
            &Token::Integer(i) => println!("Integer: {}", i),
            &Token::Identifier(ref s) => println!("Identifier: {}", s),
            &Token::Boolean(tf) => println!("Boolean: {}", tf),
            &Token::Real(f) => println!("Real number: {}", f),
            &Token::Punctuation(Punctuation::OpenParen) => println!("OpenParen"),
            &Token::Punctuation(Punctuation::CloseParen) => println!("CloseParen"), 
            &Token::Punctuation(Punctuation::OpenCurly) => println!("OpenCurly"),
            &Token::Punctuation(Punctuation::CloseCurly) => println!("CloseCurly"),
            &Token::Punctuation(Punctuation::Coma) => println!("Coma"),
            &Token::Punctuation(Punctuation::Plus) => println!("Plus"),
            &Token::Punctuation(Punctuation::Minus) => println!("Minus"),
            &Token::Punctuation(Punctuation::Multiply) => println!("Multiply)"),
            &Token::Punctuation(Punctuation::Divide) => println!("Divide"),
            &Token::Punctuation(Punctuation::Modulo) => println!("Modulo"),
            &Token::Punctuation(Punctuation::ColonEquals) => println!("ColonEquals"),
            &Token::Punctuation(Punctuation::NotEquals) => println!("NotEquals"),
            &Token::Punctuation(Punctuation::LesserThan) => println!("LesserThan"),
            &Token::Punctuation(Punctuation::GreaterThan) => println!("GreaterThan"),
            &Token::Punctuation(Punctuation::LesserEq) => println!("LesserEq"),
            &Token::Punctuation(Punctuation::GreaterEq) => println!("GreaterEq"),
            &Token::Keyword(Keyword::Var) => println!("Keyword: Var"),
            &Token::Keyword(Keyword::Fun) => println!("Keyword: Fun"),
            &Token::Keyword(Keyword::If) => println!("Keyword: If"),
            &Token::Keyword(Keyword::Else) => println!("Keyword: Else"),
            &Token::Keyword(Keyword::Return) => println!("Keyword: Return"),
            &Token::Keyword(Keyword::Read) => println!("Keyword: Read"),
            &Token::Keyword(Keyword::Write) => println!("Keyword: Write"),
            &Token::Keyword(Keyword::Not) => println!("Keyword: Not"),
            &Token::Keyword(Keyword::Or) => println!("Keyword: Or"),
            &Token::Keyword(Keyword::And) => println!("Keyword: And"),
            &Token::Invalid(ref s) => println!("Invalid token {}", s),
        }
    }
}

