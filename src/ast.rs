use parser::MulOp;
use parser::AddOp;
use parser::RelOp;
use parser::SignOp;
use parser::Term;
use parser::Id;
use parser::Factor;
use parser::SimpleExpr;
use parser::Constant;
use evaluator::bool_to_int;
use parser;

pub enum Expr {
    BinOp(Box<Expr>, BinOp, Box<Expr>),
    Id(Id),
    Constant(Constant),
    NegId(Id),
}

enum BinOp {
    Add,
    Sub,
    Or,
    Mul,
    Div,
    Mod,
    And,
    Equal,
    NEqual,
    Greater,
    Lesser,
    GreaterEq,
    LesserEq,
}

impl From<parser::Expr> for Expr {
    fn from(parse_expr: parser::Expr) -> Expr {
    //    match parse_expr.relation {
    //        Some(rel) => 
    //        None => { if (parse_expr.simple_expr.term_chain.is_empty()) {
    //                    
    //                }
    //                else {

    //                }
    //        }
    //    }
    Expr::Constant(Constant::Bool(true))
    }
}

fn expr_from_parse_expr(parse_expr: parser::Expr) -> Expr {
    match parse_expr.relation {
        Some(rel) => match rel.0 {
            RelOp::Equal => Expr::BinOp(Box::new(expr_from_simple(parse_expr.simple_expr)), BinOp::Equal, Box::new(expr_from_simple(rel.1))),
            RelOp::NEqual => Expr::BinOp(Box::new(expr_from_simple(parse_expr.simple_expr)), BinOp::NEqual, Box::new(expr_from_simple(rel.1))),
            RelOp::Greater => Expr::BinOp(Box::new(expr_from_simple(parse_expr.simple_expr)), BinOp::Greater, Box::new(expr_from_simple(rel.1))),
            RelOp::Lesser => Expr::BinOp(Box::new(expr_from_simple(parse_expr.simple_expr)), BinOp::Lesser, Box::new(expr_from_simple(rel.1))),
            RelOp::GreaterEq => Expr::BinOp(Box::new(expr_from_simple(parse_expr.simple_expr)), BinOp::GreaterEq, Box::new(expr_from_simple(rel.1))),
            RelOp::LesserEq => Expr::BinOp(Box::new(expr_from_simple(parse_expr.simple_expr)), BinOp::LesserEq, Box::new(expr_from_simple(rel.1))),
        },
        None => expr_from_simple(parse_expr.simple_expr),
    }
}

fn expr_from_simple(mut simple: SimpleExpr) -> Expr {
    match simple.sign {
        SignOp::Pos => {
            match simple.term_chain.pop() {
                Some(term_c) => match term_c.0 {
                    AddOp::Add => Expr::BinOp(Box::new(expr_from_term(term_c.1)), BinOp::Add, Box::new(expr_from_simple(simple))),
                    AddOp::Sub => Expr::BinOp(Box::new(expr_from_term(term_c.1)), BinOp::Sub, Box::new(expr_from_simple(simple))),
                    AddOp::Or => Expr::BinOp(Box::new(expr_from_term(term_c.1)), BinOp::Or, Box::new(expr_from_simple(simple))),
                },
                None => expr_from_term(simple.term),
            }
        }
        SignOp::Neg => {
            match simple.term_chain.pop() {
                Some(term_c) => match term_c.0 {
                    AddOp::Add => Expr::BinOp(Box::new(expr_from_term(term_c.1)), BinOp::Add, Box::new(expr_from_simple(simple))),
                    AddOp::Sub => Expr::BinOp(Box::new(expr_from_term(term_c.1)), BinOp::Sub, Box::new(expr_from_simple(simple))),
                    AddOp::Or => Expr::BinOp(Box::new(expr_from_term(term_c.1)), BinOp::Or, Box::new(expr_from_simple(simple))),               
                },
                None => negative_from_expr(expr_from_term(simple.term)),
            }
        }
    }
}

fn negative_from_expr(mut expr: Expr) -> Expr {
    match expr {
        Expr::Constant(c) => match c {
            Constant::Bool(b) => Expr::Constant(Constant::Int(bool_to_int(b) * -1)),
            Constant::Real(r) => Expr::Constant(Constant::Real((r * -1.0))),
            Constant::Int(i) => Expr::Constant(Constant::Int(i.checked_neg().expect("Cannot make such a large number negative"))),
        },
        Expr::Id(id) => Expr::NegId(id),
        Expr::NegId(_) => panic!("This shouldn't be possible. Factor somehow created a negative id"),
        Expr::BinOp(l, c, tree) => Expr::BinOp(l, c, Box::new(negative_from_expr(*tree))),
    }
}

fn expr_from_term(mut term: Term) -> Expr {
    match term.fac_chain.pop() {
        Some(fac_ch) => match fac_ch.0 {
            MulOp::Mul => Expr::BinOp(Box::new(expr_from_factor(fac_ch.1)), BinOp::Mul, Box::new(expr_from_term(term))),
            MulOp::Div => Expr::BinOp(Box::new(expr_from_factor(fac_ch.1)), BinOp::Div, Box::new(expr_from_term(term))),
            MulOp::And => Expr::BinOp(Box::new(expr_from_factor(fac_ch.1)), BinOp::And, Box::new(expr_from_term(term))),
            MulOp::Mod => Expr::BinOp(Box::new(expr_from_factor(fac_ch.1)), BinOp::Mod, Box::new(expr_from_term(term))),
        },
        None => expr_from_factor(term.factor),
    }
}

fn expr_from_factor(factor: Factor) -> Expr {
    match factor {
        Factor::Constant(c) => Expr::Constant(c),
        Factor::Id(id) => Expr::Id(id),
        Factor::Expr(expr) => expr_from_parse_expr(*expr),
        _ => panic!("Not sure what to do with not fac"),
    }
}
