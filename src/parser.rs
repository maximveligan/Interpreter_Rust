//Maxim Veligan
//Programming Languages
//Assignmnent number 5, parser

use std::collections::HashSet;
use token::Token;
use token::Keyword;
use token::Punctuation;

#[derive(Hash, Eq, PartialEq, Debug)]
pub struct Id(String);

#[derive(Debug)]
pub struct Function {
    id: Id,
    parameters: HashSet<Id>,
    block: Block,
    value: Option<Expr>,
}

#[derive(Debug)]
pub struct Block {
    vars: HashSet<Id>,
    cmds: Vec<Cmd>,
}

#[derive(Debug)]
pub enum Cmd {
    Read(Id),
    Write(Id),
    Block(Block),
    Return(Expr),
    While { expr: Expr, block: Block },
    If { expr: Expr, block: Block },
    Assignment { var: Id, expr: Expr },
    FunCall { var: Id, fun: Id, exprs: Vec<Expr> },
}

#[derive(Debug)]
pub struct Expr {
    pub simple_expr: SimpleExpr,
    pub relation: Option<(RelOp, SimpleExpr)>,
}

#[derive(Debug)]
pub struct SimpleExpr {
    pub sign: SignOp,
    pub term: Term,
    pub term_chain: Vec<(AddOp, Term)>,
}

#[derive(Debug)]
pub struct Term {
    pub factor: Factor,
    pub fac_chain: Vec<(MulOp, Factor)>,
}

#[derive(Debug)]
pub enum Factor {
    Constant(Constant),
    Id(Id),
    NotFac(Box<Factor>),
    Expr(Box<Expr>),
}

#[derive(Debug)]
pub enum Constant {
    Bool(bool),
    Int(i64),
    Real(f64),
}

#[derive(Debug)]
pub struct Program {
    pub vars: HashSet<Id>,
    pub funcs: Vec<Function>,
    pub cmds: Vec<Cmd>,
}

#[derive(Debug)]
pub enum AddOp {
    Add,
    Sub,
    Or,
}

#[derive(Debug)]
pub enum SignOp {
    Pos,
    Neg,
}

#[derive(Debug)]
pub enum MulOp {
    Mul,
    Div,
    Mod,
    And,
}

#[derive(Debug)]
pub enum RelOp {
    Equal,
    NEqual,
    Greater,
    Lesser,
    GreaterEq,
    LesserEq,
}

pub fn parse_program(input: &[Token]) -> Result<Program, Vec<String>> {
    let (rest, vars, var_err) = parse_vars(input);
    let (tail, funcs, fun_err) = parse_funcs(rest);
    let (tok_tail, cmds, cmd_err) = parse_cmds(tail);
    if tok_tail.is_empty() {
        Ok(Program {
            vars: vars,
            funcs: funcs,
            cmds: cmds,
        })
    } else {
        if cmds.is_empty() && funcs.is_empty() {
            Err(vec![var_err, fun_err, cmd_err])
        } else if cmds.is_empty() {
            Err(vec![fun_err, cmd_err])
        } else {
            Err(vec![cmd_err])
        }
    }
}

fn parse_vars(mut input: &[Token]) -> (&[Token], HashSet<Id>, String) {
    let mut vars = HashSet::new();
    let err;
    loop {
        match parse_var(input) {
            Ok((rest, idents)) => {
                vars.extend(idents);
                input = rest;
            }
            Err(msg) => {
                err = msg;
                break;
            }
        }
    }
    (input, vars, err)
}

fn parse_var(mut input: &[Token]) -> Result<(&[Token], HashSet<Id>), String> {
    let id_list;
    match input.split_first() {
        Some((&Token::Keyword(Keyword::Var), rest)) => {
            let (tail, vars) = parse_id_list(rest)?;
            input = tail;
            id_list = vars;
        }
        Some((token, _)) => return Err(format!("Did not find the var keyword. Found {:?}", token)),
        _ => return Err("Unexpected eof".to_string()),
    }
    match input.split_first() {
        Some((&Token::Punctuation(Punctuation::Semicolon), rest)) => Ok((rest, id_list)),
        Some((token, _)) => Err(format!("Syntax error. Expected ; token. Found {:?}", token)),
        _ => Err("Unexpected eof".to_string())
    }
}

fn parse_id_list(mut input: &[Token]) -> Result<(&[Token], HashSet<Id>), String> {
    let mut identifiers = HashSet::new();
    let (rest, id) = parse_id(input)?;
    identifiers.insert(id);
    input = rest;
    loop {
        match input.split_first() {
            Some((&Token::Punctuation(Punctuation::Coma), rest)) => {
                let (tail, ident) = parse_id(rest)?;
                input = tail;
                identifiers.insert(ident);
            }
            _ => break,
        }
    }
    Ok((input, identifiers))
}

fn parse_id(input: &[Token]) -> Result<(&[Token], Id), String> {
    match input.split_first() {
        Some((&Token::Identifier(ref s), rest)) => Ok((rest, Id(s.clone()))),
        Some((token, _)) => Err(format!("Expected an identifier. Found {:?}", token)),
        _ => {
            return Err("Unexpected eof".to_string())
        }
    }
}

fn parse_funcs(mut input: &[Token]) -> (&[Token], Vec<Function>, String) {
    let mut functions: Vec<Function> = Vec::new();
    let err;
    loop {
        match parse_func(input) {
            Ok((rest, funcs)) => {
                input = rest;
                functions.push(funcs);
            }
            Err(msg) => {
                err = msg;
                break;
            }
        }
    }
    (input, functions, err)
}

fn parse_func(mut input: &[Token]) -> Result<(&[Token], Function), String> {
    match input.split_first() {
        Some((&Token::Keyword(Keyword::Fun), rest)) => input = rest,
        Some((token, _)) => return Err(format!("Did not find the fun keyword. Found {:?}", token)),
        _ => {
            return Err("Unexpected eof".to_string())
        }
    }

    let (rest, fun_id) = parse_id(input)?;

    match rest.split_first() {
        Some((&Token::Punctuation(Punctuation::OpenParen), tail)) => input = tail,
        Some((token, _)) => return Err(format!("Did not find a delimeter ).Found {:?}", token)),
        _ => {
            return Err("Unexpected eof".to_string())
        }
    }

    let (rest, parameters) = parse_id_list(input)?;

    match rest.split_first() {
        Some((&Token::Punctuation(Punctuation::CloseParen), tail)) => input = tail,
        Some((token, _)) => return Err(format!("Did not find a delimeter ). Found {:?}", token)),
        _ => {
            return Err("Unexpected eof".to_string())
        }
    }

    let (rest, block) = parse_block(input)?;
    Ok((
        rest,
        Function {
            id: fun_id,
            parameters: parameters,
            block: block,
            value: None,
        },
    ))
}

fn parse_block(mut input: &[Token]) -> Result<(&[Token], Block), String> {
    match input.split_first() {
        Some((&Token::Punctuation(Punctuation::OpenCurly), rest)) => input = rest,
        Some((token, _)) => return Err(format!("Syntax error, expected {{. Found {:?}", token)),
        _ => {
            return Err("Unexpected eof".to_string())
        }
    }
    let (rest, vars, _) = parse_vars(input);
    let (tail, cmds, _) = parse_cmds(rest);
    match tail.split_first() {
        Some((&Token::Punctuation(Punctuation::CloseCurly), tail)) => Ok((
            tail,
            Block {
                vars: vars,
                cmds: cmds,
            },
        )),
        Some((token, _)) => return Err(format!("Syntax error, expected }}. Found {:?}", token)),
        _ => {
            return Err("Unexpected eof".to_string())
        }
    }
}

fn parse_cmds(mut input: &[Token]) -> (&[Token], Vec<Cmd>, String) {
    let mut cmds: Vec<Cmd> = Vec::new();
    let err;
    loop {
        match parse_cmd(input) {
            Ok((rest, cmd)) => {
                input = rest;
                cmds.push(cmd);
            }
            Err(msg) => {
                err = msg;
                break;
            }
        }
    }
    (input, cmds, err)
}

fn parse_cmd(mut input: &[Token]) -> Result<(&[Token], Cmd), String> {
    match input.split_first() {
        Some((&Token::Identifier(ref _s), _)) => {
            let (tail, assign) = parse_id_cmd(input)?;
            Ok((tail, assign))
        }
        Some((&Token::Keyword(Keyword::If), _)) => {
            let (rest, expr, block) = parse_if(input)?;
            Ok((rest, 
                Cmd::If {
                    expr: expr,
                    block: block,
                }
            ))
        }
        Some((&Token::Keyword(Keyword::While), _)) => {
            let (rest, expr, block) = parse_while(input)?;
            Ok((rest, 
                Cmd::While {
                    expr: expr,
                    block: block,
                }
            ))
        }
        Some((&Token::Keyword(Keyword::Return), rest)) => {
            let (tail, expr) = parse_expr(rest)?;
            input = tail;
            match input.split_first() {
                Some((&Token::Punctuation(Punctuation::Semicolon), rest)) => Ok((rest, {
                    Cmd::Return(expr)
                })),
                Some((token, _)) => return Err(format!("Missing token ;. Found {:?}", token)),
                _ => {
                    return Err("Unexpected eof".to_string())
                }
            }
        }

        Some((&Token::Keyword(Keyword::Read), rest)) => {
            let (tail, id) = parse_id(rest)?;
            input = tail;
            match input.split_first() {
                Some((&Token::Punctuation(Punctuation::Semicolon), rest)) => Ok((rest, {
                    Cmd::Read(id)
                })),
                Some((token, _)) => return Err(format!("Missing token ;. Found {:?}", token)),
                _ => {
                    return Err("Unexpected eof".to_string())
                }
            }
        }

        Some((&Token::Keyword(Keyword::Write), rest)) => {
            let (tail, id) = parse_id(rest)?;
            input = tail;
            match input.split_first() {
                Some((&Token::Punctuation(Punctuation::Semicolon), rest)) => Ok((rest, {
                    Cmd::Write(id)
                })),
                Some((token, _)) => return Err(format!("Missing token ;. Found {:?}", token)),
                _ => {
                    return Err("Unexpected eof".to_string())
                }
            }
        }

        Some((_, _)) => {
            match parse_block(input) {
                Ok((rest, block)) => Ok((rest, Cmd::Block(block))),
                Err(msg) => Err(msg),
            }
        }
        _ => Err("Unexpected eof".to_string())
    }
}

fn parse_exprs(mut input: &[Token]) -> Result<(&[Token], Vec<Expr>), String> {
    let mut exprs: Vec<Expr> = Vec::new();
    let (rest, expr) = parse_expr(input)?;
    exprs.push(expr);
    input = rest;
    loop {
        match input.split_first() {
            Some((&Token::Punctuation(Punctuation::Coma), rest)) => {
                let (tail, expression) = parse_expr(rest)?;
                input = tail;
                exprs.push(expression);
            }
            _ => break,
        }
    }
    Ok((input, exprs))
}

fn parse_expr(mut input: &[Token]) -> Result<(&[Token], Expr), String> {
    let (rest, simple_expr) = parse_simple_expr(input)?;
    input = rest;
    match input.split_first() {
        Some((&Token::Punctuation(Punctuation::Equals), rest)) => {
            let (tail, simple_ex) = parse_simple_expr(rest)?;
            Ok((
                tail,
                Expr {
                    simple_expr: simple_expr,
                    relation: Some((RelOp::Equal, simple_ex)),
                },
            ))
        } 
        Some((&Token::Punctuation(Punctuation::NotEquals), rest)) => {
            let (tail, simple_ex) = parse_simple_expr(rest)?;
            Ok((
                tail,
                Expr {
                    simple_expr: simple_expr,
                    relation: Some((RelOp::NEqual, simple_ex)),
                },
            ))
        }
        Some((&Token::Punctuation(Punctuation::LesserThan), rest)) => {
            let (tail, simple_ex) = parse_simple_expr(rest)?;
            Ok((
                tail,
                Expr {
                    simple_expr: simple_expr,
                    relation: Some((RelOp::Lesser, simple_ex)),
                },
            ))
        }
        Some((&Token::Punctuation(Punctuation::GreaterThan), rest)) => {
            let (tail, simple_ex) = parse_simple_expr(rest)?;
            Ok((
                tail,
                Expr {
                    simple_expr: simple_expr,
                    relation: Some((RelOp::Greater, simple_ex)),
                },
            ))
        }
        Some((&Token::Punctuation(Punctuation::LesserEq), rest)) => {
            let (tail, simple_ex) = parse_simple_expr(rest)?;
            Ok((
                tail,
                Expr {
                    simple_expr: simple_expr,
                    relation: Some((RelOp::LesserEq, simple_ex)),
                },
            ))
        }
        Some((&Token::Punctuation(Punctuation::GreaterEq), rest)) => {
            let (tail, simple_ex) = parse_simple_expr(rest)?;
            Ok((
                tail,
                Expr {
                    simple_expr: simple_expr,
                    relation: Some((RelOp::GreaterEq, simple_ex)),
                },
            ))
        }
        _ => Ok((
            input,
            Expr {
                simple_expr: simple_expr,
                relation: None,
            },
        )),
    }

}

fn parse_factor(input: &[Token]) -> Result<(&[Token], Factor), String> {
    match input.split_first() {
        Some((&Token::Boolean(b), rest)) => Ok((rest, Factor::Constant(Constant::Bool(b)))),
        Some((&Token::Integer(i), rest)) => Ok((rest, Factor::Constant(Constant::Int(i)))),
        Some((&Token::Real(r), rest)) => Ok((rest, Factor::Constant(Constant::Real(r)))),
        Some((&Token::Identifier(ref s), rest)) => Ok((rest, Factor::Id(Id(s.clone())))),
        Some((&Token::Keyword(Keyword::Not), rest)) => {
            let (tail, fact) = parse_factor(rest)?;
            Ok((tail, (Factor::NotFac(Box::new(fact)))))
        }
        Some((&Token::Punctuation(Punctuation::OpenParen), rest)) => {
            let (tail, expr) = parse_expr(rest)?;
            match tail.split_first() {
                Some((&Token::Punctuation(Punctuation::CloseParen), rest)) => Ok((
                    rest,
                    Factor::Expr(
                        Box::new(expr),
                    ),
                )),
                Some((token, _)) => return Err(format!("Missing delimeter ). Found {:?}", token)),
                _ => {
                    return Err("Unexpected eof".to_string())
                }

            }
        }
        Some((token, _)) => return Err(format!("Did not find a valid factor. Found {:?}", token)),
        _ => {
            return Err("Unexpected eof".to_string())
        }
    }
}

fn parse_term(mut input: &[Token]) -> Result<(&[Token], Term), String> {
    let (rest, fac) = parse_factor(input)?;
    input = rest;
    let mut fac_chain: Vec<(MulOp, Factor)> = Vec::new();
    loop {
        match input.split_first() {
            Some((&Token::Punctuation(Punctuation::Multiply), rest)) => {
                let (tail, factor) = parse_factor(rest)?;
                fac_chain.push((MulOp::Mul, factor));
                input = tail;
            }
            Some((&Token::Punctuation(Punctuation::Divide), rest)) => {
                let (tail, factor) = parse_factor(rest)?;
                fac_chain.push((MulOp::Div, factor));
                input = tail;
            }
            Some((&Token::Punctuation(Punctuation::Modulo), rest)) => {
                let (tail, factor) = parse_factor(rest)?;
                fac_chain.push((MulOp::Mod, factor));
                input = tail;
            }
            Some((&Token::Keyword(Keyword::And), rest)) => {
                let (tail, factor) = parse_factor(rest)?;
                fac_chain.push((MulOp::And, factor));
                input = tail;
            }
            _ => break,
        }
    }
    Ok((
        input,
        Term {
            factor: fac,
            fac_chain: fac_chain,
        },
    ))
}

fn parse_simple_expr(mut input: &[Token]) -> Result<(&[Token], SimpleExpr), String> {
    let term;
    let mut term_chain: Vec<(AddOp, Term)> = Vec::new();
    let mut sign: Option<SignOp> = None;
    match input.split_first() {
        Some((&Token::Punctuation(Punctuation::Plus), rest)) => {
            sign = Some(SignOp::Pos);
            let (tail, temp_term) = parse_term(rest)?;
            term = temp_term;
            input = tail;
        }
        Some((&Token::Punctuation(Punctuation::Minus), rest)) => {
            sign = Some(SignOp::Neg);
            let (tail, temp_term) = parse_term(rest)?;
            term = temp_term;
            input = tail;
        }
        Some((_, _)) => {
            let (tail, temp_term) = parse_term(input)?;
            term = temp_term;
            input = tail;
        } 
        _ => {
            return Err("Unexpected eof".to_string())
        }
    }
    loop {
        match input.split_first() {
            Some((&Token::Punctuation(Punctuation::Plus), rest)) => {
                let (tail, rel_term) = parse_term(rest)?;
                term_chain.push((AddOp::Add, rel_term));
                input = tail;
            }
            Some((&Token::Punctuation(Punctuation::Minus), rest)) => {
                let (tail, rel_term) = parse_term(rest)?;
                term_chain.push((AddOp::Sub, rel_term));
                input = tail;
            }
            Some((&Token::Keyword(Keyword::Or), rest)) => {
                let (tail, rel_term) = parse_term(rest)?;
                term_chain.push((AddOp::Or, rel_term));
                input = tail;
            }
            _ => break,
        }
    }
    Ok((
        input,
        SimpleExpr {
            sign: sign.unwrap_or(SignOp::Pos),
            term: term,
            term_chain: term_chain,
        },
    ))
}

fn parse_fun_call(mut input: &[Token]) -> Result<(&[Token], Id, Vec<Expr>), String> {
    match input.split_first() {
        Some((&Token::Keyword(Keyword::Call), rest)) => input = rest,
        Some((token, _)) => return Err(format!("Did not find call keyword. Found {:?}", token)),
        _ => {
            return Err("Unexpected eof".to_string())
        }
    }
    let (tail, fun) = parse_id(input)?;
    match tail.split_first() {
        Some((&Token::Punctuation(Punctuation::OpenParen), rest)) => input = rest,
        Some((token, _)) => return Err(format!("Did not find delimeter (. Found {:?}", token)),
        _ => {
            return Err("Unexpected eof".to_string())
        }
    }

    let (tok_tail, exprs) = parse_exprs(input)?;
    match tok_tail.split_first() {
        Some((&Token::Punctuation(Punctuation::CloseParen), rest)) => input = rest,
        Some((token, _)) => return Err(format!("Did not find delimeter ). Found {:?}", token)),
        _ => {
            return Err("Unexpected eof".to_string())
        }
    }
    match input.split_first() {
        Some((&Token::Punctuation(Punctuation::Semicolon), rest)) => Ok((rest, fun, exprs)),
        Some((token, _)) => return Err(format!("Missing token ;. Found {:?}", token)),
        _ => {
            return Err("Unexpected eof".to_string())
        }
    }

}

fn parse_id_cmd(mut input: &[Token]) -> Result<(&[Token], Cmd), String> {
    let (rest, var) = parse_id(input)?;
    let expr;
    match rest.split_first() {
        Some((&Token::Punctuation(Punctuation::ColonEquals), rest)) => input = rest,
        Some((token, _)) => return Err(format!("Did not find colon equals. Found {:?}", token)),
        _ => {
            return Err("Unexpected eof".to_string())
        }
    }
    match parse_expr(input) {
        Ok((rest, new_expr)) => {
            expr = new_expr;
            match rest.split_first() {
                Some((&Token::Punctuation(Punctuation::Semicolon), tail)) => Ok((tail, Cmd::Assignment { var, expr })),

                Some((token, _)) => Err(format!("Missing token ;. Found {:?}", token)),
                _ => Err("Unexpected eof".to_string())
            }
        }
        _ => {
            let (rest, fun, exprs) = parse_fun_call(input)?;
            Ok((rest, Cmd::FunCall{ var, fun, exprs}))
        }  
    }
}

fn parse_if(mut input: &[Token]) -> Result<(&[Token], Expr, Block), String> {
    match input.split_first() {
        Some((&Token::Keyword(Keyword::If), rest)) => input = rest,
        Some((token, _)) => return Err(format!("Did not find keyword if. Found {:?}", token)),
        _ => {
            return Err("Unexpected eof".to_string())
        }
    }
    match input.split_first() {
        Some((&Token::Punctuation(Punctuation::OpenParen), rest)) => input = rest,
        Some((token, _)) => return Err(format!("Did not find delimeter (. Found {:?}", token)),
        _ => {
            return Err("Unexpected eof".to_string())
        }
    }

    let (rest, expr) = parse_expr(input)?;

    match rest.split_first() {
        Some((&Token::Punctuation(Punctuation::CloseParen), rest)) => input = rest,
        Some((token, _)) => return Err(format!("Did not find delimeter ). Found {:?}", token)),
        _ => {
            return Err("Unexpected eof".to_string())
        }
    }

    let (tail, block) = parse_block(input)?;
    Ok((tail, expr, block))
}

fn parse_while(mut input: &[Token]) -> Result<(&[Token], Expr, Block), String> {
    let expr;
    match input.split_first() {
        Some((&Token::Keyword(Keyword::While), rest)) => input = rest,
        Some((token, _)) => return Err(format!("Did not find keyword while. Found {:?}", token)),
        _ => {
            return Err("Unexpected eof".to_string())
        }
    }
    match input.split_first() {
        Some((&Token::Punctuation(Punctuation::OpenParen), rest)) => {
            let (tail, new_expr) = parse_expr(rest)?;
            expr = new_expr;
            input = tail;
        }
        Some((token, _)) => return Err(format!("Did not find delimeter (. Found {:?}", token)),
        _ => {
            return Err("Unexpected eof".to_string())
        }
    }

    match input.split_first() {
        Some((&Token::Punctuation(Punctuation::CloseParen), rest)) => input = rest,
        Some((token, _)) => return Err(format!("Did not find delimeter ). Found {:?}", token)),
        _ => {
            return Err("Unexpected eof".to_string())
        }
    }

    let (tail, block) = parse_block(input)?;

    Ok((tail, expr, block))
}
