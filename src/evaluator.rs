// Maxim Veligan
// Programming Languages

use ast::Expr;
use ast::BinOp;
use parser::Block;
use parser::Function;
use parser::Program;
use parser::Cmd;
use parser::Constant;
use parser::Id;
use std::collections::HashMap;
use std::collections::HashSet;

#[derive(Debug)]
struct CallStack(Vec<Frame>);

#[derive(Debug)]
struct Frame(Vec<Scope>);

#[derive(Clone, Debug)]
struct Scope(HashMap<Id, Option<Constant>>);

enum Effect {
    Okay,
    Return(Expr),
    Error(String),
}


impl CallStack {
    pub fn new() -> CallStack {
        CallStack(Vec::new())
    }

    fn get_last_frame(&mut self) -> &mut Vec<Scope> {
        match self.0.last_mut() {
            Some(scopes) => &mut scopes.0,
            None => {
                panic!(
                    "Attempted to get last stack frame before building a stack. Did you forget to push a new frame?"
                )
            }
        }
    }

    fn get_global_scope(&mut self) -> &mut Scope {
        match self.0.first_mut() {
            Some(scopes) => {
                match scopes.0.first_mut() {
                    Some(gscope) => gscope,
                    None => panic!("Attempted to get globalscope before defining it"),
                }
            }
            None => panic!("Attempted to get globalscope without defining initial stack frame"),
        }
    }

    fn lookup(&mut self, id: &Id) -> Option<Option<Constant>> {
        for scope in self.get_last_frame().iter().rev() {
            if let Some(value) = scope.0.get(&id) {
                return Some(value.clone());
            }
        }
        if let Some(val) = self.get_global_scope().0.get(&id) {
            return Some(val.clone());
        }
        None
    }

    fn set_var(&mut self, id: &Id, constant: &Constant) -> Result<(), String> {

        for scope in self.get_last_frame().iter_mut().rev() {
            if let Some(val) = scope.clone().0.get(&id) {
                scope.0.insert(id.clone(), Some(constant.clone()));
                return Ok(())
            }
        }

        if let Some(old_val) = self.get_global_scope().clone().0.get(&id) {
            self.get_global_scope().0.insert(id.clone(), Some(constant.clone()));
            return Ok(())
        }
        Err("Bound a variable before initializing it".to_string())
    }

    fn push_new_frame(&mut self) {
        self.0.push(Frame::new());
    }

    fn push_new_scope(&mut self, scope: Scope) {
        self.get_last_frame().push(scope);
    }
}

impl Scope {
    pub fn new(vars: &HashSet<Id>) -> Scope {
        let mut hash_map: HashMap<Id, Option<Constant>> = HashMap::new();
        vars.iter().for_each(|var| match hash_map.insert(
            var.to_owned(),
            None,
        ) {
            None => (),
            Some(_) => panic!("Local scope somehow has elements in it before"),
        });
        Scope(hash_map)
    }
}

impl Frame {
    pub fn new() -> Frame {
        Frame(Vec::new())
    }
}

pub fn evaluate_program(program: Program) -> Result<(), String> {
    let mut funcs: HashMap<Id, (Vec<Id>, Block)> = HashMap::new();
    let mut callstack = CallStack::new();
    callstack.push_new_frame();

    for func in program.funcs.iter() {
        match funcs.insert(func.id.clone(), (func.parameters.clone(), func.block.clone()))
         {
            None => (),
            Some(_) => {
                return Err(
                    "Error: two or more functions have the same name".to_string(),
                )
            }
        }
    }

    match evaluate_block(
        &funcs,
        &Block {
            vars: program.vars,
            cmds: program.cmds,
        },
        &mut callstack,
    ) {
        Effect::Okay => Ok(()),
        Effect::Return(val) => Err(
            "This shouldn't be possible because the parser should catch it"
                .to_string(),
        ),
        Effect::Error(msg) => Err(msg),
    }
}

fn evaluate_cmds(
    funcs: &HashMap<Id, (Vec<Id>, Block)>,
    cmds: &Vec<Cmd>,
    callstack: &mut CallStack,
) -> Effect {
    for cmd in cmds.iter() {
        let effect = match *cmd {
            Cmd::Read(_) => {println!("Warning! Reading is not supported!"); Effect::Okay},
            Cmd::Write(ref id) => {match callstack.lookup(id) {
                Some(c)  => {println!("{:?}", c); Effect::Okay},
                None => Effect::Error(format!("Tried to print variable without defining it {}", id.string())),
            }}
            Cmd::Block(ref block) => evaluate_block(funcs, block, callstack),
            Cmd::Return(ref expr) => Effect::Return(Expr::from(expr.clone())),
            Cmd::While {
                ref expr,
                ref block,
            } => {
                evaluate_while(
                    funcs,
                    Expr::from(expr.clone()),
                    Block::from(block.clone()),
                    callstack,
                )
            }
            Cmd::If {
                ref expr,
                ref block,
            } => {
                evaluate_if(
                    funcs,
                    Expr::from(expr.clone()),
                    Block::from(block.clone()),
                    callstack,
                )
            }
            Cmd::Assignment { ref var, ref expr } => {
                evaluate_assign(var, Expr::from(expr.clone()), callstack)
            }
            Cmd::FunCall {
                ref var,
                ref fun,
                ref exprs,
            } => evaluate_fun_call(funcs, var, fun, exprs.clone().into_iter().map(Expr::from).collect(), callstack),
        };
        match effect {
            Effect::Okay => (),
            Effect::Error(msg) => return Effect::Error(msg),
            Effect::Return(val) => return Effect::Return(val),
        }
    }
    Effect::Okay
}

fn evaluate_block(
    funcs: &HashMap<Id, (Vec<Id>, Block)>,
    block: &Block,
    callstack: &mut CallStack,
) -> Effect {
    let local_scope = Scope::new(&block.vars.clone());
    callstack.push_new_scope(local_scope);
    match evaluate_cmds(funcs, &block.cmds, callstack) {
        Effect::Okay => {callstack.get_last_frame().pop(); Effect::Okay},
        Effect::Return(val) => Effect::Return(val),
        Effect::Error(msg) => Effect::Error(msg),
    }
}

fn evaluate_while(
    funcs: &HashMap<Id, (Vec<Id>, Block)>,
    expr: Expr,
    block: Block,
    callstack: &mut CallStack,
) -> Effect {
    loop {
        match evaluate_expr(expr.clone(), callstack) {
            Ok(c) => {
                match c {
                    Constant::Bool(b) => {
                        if (b) {
                            match evaluate_block(funcs, &block, callstack) {
                                Effect::Okay => (),
                                Effect::Return(val) => return Effect::Return(val),
                                Effect::Error(msg) =>return Effect::Error(msg),
                            }
                        }
                        else {
                            return Effect::Okay
                        }
                    }
                    Constant::Real(r) => {
                        if (real_to_bool(r)) {
                            match evaluate_block(funcs, &block, callstack) {
                                Effect::Okay => (),
                                Effect::Return(val) => return Effect::Return(val),
                                Effect::Error(msg) =>return Effect::Error(msg),
                            }
                        }
                        else {
                            return Effect::Okay
                        }
                    }
                    Constant::Int(i) => {
                        if (int_to_bool(i)) {
                            match evaluate_block(funcs, &block, callstack) {
                                Effect::Okay => (),
                                Effect::Return(val) => return Effect::Return(val),
                                Effect::Error(msg) =>return Effect::Error(msg),
                            }
                        }
                        else {
                            return Effect::Okay
                        }
                    }
                }
            }
            Err(msg) => return Effect::Error(msg),
        }
    }
}

fn evaluate_if(
    funcs: &HashMap<Id, (Vec<Id>, Block)>,
    expr: Expr,
    block: Block,
    callstack: &mut CallStack,
) -> Effect {
    match evaluate_expr(expr, callstack) {
        Ok(c) => {
            match c {
                Constant::Bool(b) => {
                    if (b) {
                        match evaluate_block(funcs, &block, callstack) {
                            Effect::Okay => return Effect::Okay,
                            Effect::Return(val) => return Effect::Return(val),
                            Effect::Error(msg) => return Effect::Error(msg),
                        }
                    } else {
                        return Effect::Okay;
                    }
                }
                Constant::Real(r) => {
                    if real_to_bool(r) {
                        match evaluate_block(funcs, &block, callstack) {
                            Effect::Okay => return Effect::Okay,
                            Effect::Return(val) => return Effect::Return(val),
                            Effect::Error(msg) => return Effect::Error(msg),
                        }
                    } else {
                        return Effect::Okay;
                    }
                }
                Constant::Int(i) => {
                    if int_to_bool(i) {
                        match evaluate_block(funcs, &block, callstack) {
                            Effect::Okay => return Effect::Okay,
                            Effect::Return(val) => return Effect::Return(val),
                            Effect::Error(msg) => return Effect::Error(msg),
                        }
                    } else {
                        return Effect::Okay;
                    }
                }
            }
        }
        Err(msg) => Effect::Error(msg),
    }
}

pub fn bool_to_int(b: bool) -> i64 {
    if b { 1 } else { 0 }
}

pub fn bool_to_real(b: bool) -> f64 {
    if b { 1.0 } else { 0.0 }
}

pub fn real_to_bool(real: f64) -> bool {
    real != 0.0
}

pub fn int_to_bool(int: i64) -> bool {
    int != 0
}

fn evaluate_assign(var: &Id, expr: Expr, callstack: &mut CallStack) -> Effect {
    match evaluate_expr(expr, callstack) {
        Ok(c) => {
            match callstack.set_var(&var, &c) {
                Ok(()) => Effect::Okay,
                Err(msg) => Effect::Error(msg.to_string()),
            }
        }    
        Err(msg) => Effect::Error(msg),
    }
}

fn evaluate_fun_call(
    funcs: &HashMap<Id, (Vec<Id>, Block)>,
    var: &Id,
    fun: &Id,
    param_vals: Vec<Expr>,
    callstack: &mut CallStack,
) -> Effect {
    if funcs.contains_key(fun) {
        match param_vals
            .into_iter()
            .map(|param| evaluate_expr(Expr::from(param), callstack))
            .collect::<Result<Vec<Constant>, String>>() {
            Ok(params) => {
                if ((params.len() == (funcs.get(fun).expect("Checked earlier for this error. Should've gotten a key")).0.len()))
                {
                    let zipped = funcs.get(fun).expect("Checked for this").0.iter().zip(params.iter());
                    let mut hash_map = HashMap::new();
                    for key_val in zipped {
                        hash_map.insert(key_val.0.clone(), Some(key_val.1.clone()));
                    }
                    callstack.push_new_frame();
                    callstack.push_new_scope(Scope(hash_map));
                } else {
                    return Effect::Error("The amount of parameters is not the same as the amount of expressions supplied.".to_string());
                }
            }
            Err(inv_expr) => return Effect::Error(
                format!("One or more parameters is invalid. {}", inv_expr),
            ),
        }
    } else {
        return Effect::Error("Tried to call a non existing function".to_string());
    }
    match evaluate_block(funcs, &funcs.get(fun).expect("Checked for this").1, callstack) {
        Effect::Return(val) => { match evaluate_expr(val, callstack) {
            Ok(c) => {callstack.0.pop(); match callstack.set_var(&var, &c) {
                Ok(()) => Effect::Okay,
                Err(msg) => Effect::Error(msg.to_string()),
            }},
            Err(msg) => return Effect::Error(msg),
        }},
        Effect::Error(msg) => Effect::Error(msg),
        _ => Effect::Error("Current function branch does not have a return".to_string()),
    }
}

fn evaluate_expr(expr: Expr, callstack: &mut CallStack) -> Result<Constant, String> {
    match expr {
        Expr::Constant(c) => match c {
            Constant::Bool(b) => Ok(Constant::Bool(b)),
            Constant::Int(i) => Ok(Constant::Int(i)),
            Constant::Real(r) => Ok(Constant::Real(r)),
        }
        Expr::Id(id) => match callstack.lookup(&id) {
            Some(oc) => match oc {
                Some(c) => match c {
                    Constant::Bool(b) => Ok(Constant::Bool(b)),
                    Constant::Int(i) => Ok(Constant::Int(i)),
                    Constant::Real(r) => Ok(Constant::Real(r)),
                }
                None => Err(format!("Null pointer error. No value found for variable {}.", id.string())),

            }

            None => Err(format!("No such variable exists {}", id.string())),
        }

        Expr::NegId(nid) => match callstack.lookup(&nid) {
            Some(oc) => match oc {
                Some(c) => match c {
                    Constant::Bool(b) => Ok(Constant::Int(-1 * bool_to_int(b))),
                    Constant::Int(i) => Ok(Constant::Int(i.checked_neg().expect("Cannot make such a large number negative"))),
                    Constant::Real(r) => Ok(Constant::Real(r * -0.1)),
                }
                None => Err(format!("No value found for variable {}. Did you to initialize it?", nid.string())),
            }
            None => Err(format!("No such variable exists {}", nid.string())),
        }
        Expr::NotId(id) => match callstack.lookup(&id) {
            Some(oc) => match oc {
                Some(c) => match c {
                    Constant::Bool(b) => Ok(Constant::Bool(!b)),
                    Constant::Int(i) => Ok(Constant::Bool((!int_to_bool(i)))),
                    Constant::Real(r) => Ok(Constant::Bool((!real_to_bool(r)))),
                }
                None => Err(format!("No value found for variable {}. Did not initialize it.", id.string())),
                }

            None => Err(format!("No such variable exists {}", id.string())),
        }

        Expr::NotNegId(nid) => match callstack.lookup(&nid) {
            Some(oc) => match oc {
                Some(c) => match c {
                    Constant::Bool(b) => Ok(Constant::Bool((!b))),
                    Constant::Int(i) => Ok(Constant::Bool((!int_to_bool(i.checked_neg().expect("Cannot make such a large number negative"))))),
                    Constant::Real(r) => Ok(Constant::Bool((!real_to_bool(r)))),
                }
                None => Err(format!("No value found for variable {}. Did not bind a value to it.", nid.string())),
            }
            None => Err(format!("No such variable exists {}", nid.string())),
        }
        Expr::NotExpr(expr) => match evaluate_expr(*expr, callstack) {
            Ok(c) => match c {
                Constant::Bool(b) => Ok(Constant::Bool((!b))),
                Constant::Int(i) => Ok(Constant::Bool((!int_to_bool(i)))),
                Constant::Real(r) => Ok(Constant::Bool((!real_to_bool(r)))),
            }
            Err(msg) => Err(msg),
        }

        Expr::NotNegExpr(expr) => match evaluate_expr(*expr, callstack) {
                    Ok(c) => match c {
                        Constant::Bool(b) => Ok(Constant::Int((bool_to_int(!b) * -1))),
                        Constant::Int(i) => Ok(Constant::Int((bool_to_int(!int_to_bool(i))))),
                        Constant::Real(r) => Ok(Constant::Real((bool_to_real(!real_to_bool(r))))),
                    }
                    Err(msg) => Err(msg),
                }
        Expr::BinOp(l, bo, r) => match bo {
            BinOp::Add => Ok(add_constants(match evaluate_expr(*l, callstack) {
               Ok(c) => c,
               Err(msg) => return Err(msg),
            }, match evaluate_expr(*r, callstack){ 
                Ok(c) => c,
                Err(msg) => return Err(msg)
            })),
            BinOp::Sub => Ok(sub_constants(match evaluate_expr(*l, callstack) {
                Ok(c) => c,
                Err(msg) => return Err(msg),
            }, match evaluate_expr(*r, callstack) {
                Ok(c) => c,
                Err(msg) => return Err(msg)
            })),
            BinOp::Or => Ok(or_constants(match evaluate_expr(*l, callstack) {
                Ok(c) => c,
                Err(msg) => return Err(msg),
            }, match evaluate_expr(*r, callstack) {
                Ok(c) => c,
                Err(msg) => return Err(msg),
            })),
            BinOp::Mul => Ok(mul_constants(match evaluate_expr(*l, callstack) {
                Ok(c) => c,
                Err(msg) => return Err(msg),
            }, match evaluate_expr(*r, callstack) {
                Ok(c) => c,
                Err(msg) => return Err(msg),
            })),
            BinOp::Div => Ok(div_constants(match evaluate_expr(*l, callstack) {
                Ok(c) => c,
                Err(msg) => return Err(msg),
            }, match evaluate_expr(*r, callstack) {
                Ok(c) => c,
                Err(msg) => return Err(msg),
            })),
            BinOp::Mod => Ok(mod_constants(match evaluate_expr(*l, callstack) {
                Ok(c) => c,
                Err(msg) => return Err(msg),
            }, match evaluate_expr(*r, callstack) {
                Ok(c) => c,
                Err(msg) => return Err(msg),
            })),
            BinOp::And => Ok(and_constants(match evaluate_expr(*l, callstack) {
                Ok(c) => c,
                Err(msg) => return Err(msg),
            }, match evaluate_expr(*r, callstack) {
                Ok(c) => c,
                Err(msg) => return Err(msg),
            })),
            BinOp::Equal => Ok(equal_constants(match evaluate_expr(*l, callstack) {
                Ok(c) => c,
                Err(msg) => return Err(msg),
            }, match evaluate_expr(*r, callstack) {
                Ok(c) => c,
                Err(msg) => return Err(msg),
            })),
            BinOp::NEqual => Ok(nEqual_constants(match evaluate_expr(*l, callstack) {
                Ok(c) => c,
                Err(msg) => return Err(msg),
            }, match evaluate_expr(*r, callstack) {
                Ok(c) => c,
                Err(msg) => return Err(msg),
            })),
            BinOp::Greater => Ok(greater_constants(match evaluate_expr(*l, callstack) {
                Ok(c) => c,
                Err(msg) => return Err(msg),
            }, match evaluate_expr(*r, callstack) {
                Ok(c) => c,
                Err(msg) => return Err(msg),
            })),
            BinOp::Lesser => Ok(lesser_constants(match evaluate_expr(*l, callstack) {
                Ok(c) => c,
                Err(msg) => return Err(msg),
            }, match evaluate_expr(*r, callstack) {
                Ok(c) => c,
                Err(msg) => return Err(msg),
            })),
            BinOp::GreaterEq => Ok(greaterEq_constants(match evaluate_expr(*l, callstack) {
                Ok(c) => c,
                Err(msg) => return Err(msg),
            }, match evaluate_expr(*r, callstack) {
                Ok(c) => c,
                Err(msg) => return Err(msg),
            })),
            BinOp::LesserEq => Ok(lessereq_constants(match evaluate_expr(*l, callstack) {
                Ok(c) => c,
                Err(msg) => return Err(msg),
            }, match evaluate_expr(*r, callstack) {
                Ok(c) => c,
                Err(msg) => return Err(msg),
            })),
        }
    }
}

fn add_constants(c1: Constant, c2: Constant) -> Constant {
    match c1 {
        Constant::Real(r) => match c2 {
            Constant::Real(r2) => Constant::Real((r + r2)),
            Constant::Int(i2) => Constant::Real((r + (i2 as f64))),
            Constant::Bool(b2) => {if b2 {return Constant::Real((1.0 + r))} else {return Constant::Real(r)}},
        }
        Constant::Int(i) => match c2 {
            Constant::Real(r2) => Constant::Real((r2 + (i as f64))),
            Constant::Int(i2) => Constant::Int((i2 + i)),
            Constant::Bool(b2) => {if b2 {return Constant::Int((1 + i))} else {return Constant::Int(i)}},
        }
        Constant::Bool(b) =>  match c2 {
            Constant::Real(r2) =>{if b {return Constant::Real((1.0 + r2))} else {return Constant::Real(r2)}},
            Constant::Int(i2) =>{if b {return Constant::Int((1 + i2))} else {return Constant::Int(i2)}},
            Constant::Bool(b2) =>{if b && b2 {return Constant::Int(2)} else if b || b2 {return Constant::Int(1)} else {return Constant::Int(0)}},
        }
    }
}

fn sub_constants(c2: Constant, c1: Constant) -> Constant {
    match c1 {
        Constant::Real(r) => match c2 {
            Constant::Real(r2) => Constant::Real((r - r2)),
            Constant::Int(i2) => Constant::Real((r - (i2 as f64))),
            Constant::Bool(b2) => {if b2 {return Constant::Real((r - 1.0))} else {return Constant::Real(r)}},
        }
        Constant::Int(i) => match c2 {
            Constant::Real(r2) => Constant::Real(((i as f64) - r2)),
            Constant::Int(i2) => Constant::Int((i - i2)),
            Constant::Bool(b2) => {if b2 {return Constant::Int((i - 1))} else {return Constant::Int(i)}},
        }
        Constant::Bool(b) =>  match c2 {
            Constant::Real(r2) =>{if b {return Constant::Real((1.0 - r2))} else {return Constant::Real((0.0 - r2))}},
            Constant::Int(i2) =>{if b {return Constant::Int((1 - i2))} else {return Constant::Int((0 - i2))}},
            Constant::Bool(b2) =>{if b && b2 {return Constant::Int(0)} else if (b && (!b2)) {return Constant::Int(0)} else {return Constant::Int(-1)}},
        }
    }
}

fn or_constants(c1: Constant, c2: Constant) -> Constant {
    match c1 {
        Constant::Real(r) => match c2 {
            Constant::Real(r2) => Constant::Bool((real_to_bool(r2) || real_to_bool(r))),
            Constant::Int(i2) => Constant::Bool((int_to_bool(i2) || real_to_bool(r))),
            Constant::Bool(b2) => Constant::Bool((b2 || real_to_bool(r))),
        }
        Constant::Int(i) => match c2 {
            Constant::Real(r2) => Constant::Bool((real_to_bool(r2) || int_to_bool(i))),
            Constant::Int(i2) => Constant::Bool((int_to_bool(i2) || int_to_bool(i))),
            Constant::Bool(b2) => Constant::Bool((b2 || int_to_bool(i))),
        }
        Constant::Bool(b) =>  match c2 {
            Constant::Real(r2) => Constant::Bool((real_to_bool(r2) || b)),
            Constant::Int(i2) => Constant::Bool((int_to_bool(i2) || b)),
            Constant::Bool(b2) => Constant::Bool((b2 || b)),
        }
    }
}

fn mul_constants(c1: Constant, c2: Constant) -> Constant {
    match c1 {
        Constant::Real(r) => match c2 {
            Constant::Real(r2) => Constant::Real((r * r2)), 
            Constant::Int(i2) => Constant::Real((r * (i2 as f64))),
            Constant::Bool(b2) => {if b2 {return Constant::Real(r)} else {return Constant::Real(0.0)}},
        }
        Constant::Int(i) => match c2 {
            Constant::Real(r2) => Constant::Real((r2 * (i as f64))),
            Constant::Int(i2) => Constant::Int((i2 * i)),
            Constant::Bool(b2) => {if b2 {return Constant::Int(i)} else {return Constant::Int(0)}},
        }
        Constant::Bool(b) =>  match c2 {
            Constant::Real(r2) =>{if b {return Constant::Real(r2)} else {return Constant::Real(0.0)}},
            Constant::Int(i2) =>{if b {return Constant::Int(i2)} else {return Constant::Int(0)}},
            Constant::Bool(b2) =>{if b && b2 {return Constant::Int(1)} else {return Constant::Int(0)}},
        }
    }
}

fn div_constants(c2: Constant, c1: Constant) -> Constant {
    match c1 {
        Constant::Real(r) => match c2 {
            Constant::Real(r2) => Constant::Real((r / r2)),
            Constant::Int(i2) => Constant::Real((r / (i2 as f64))),
            Constant::Bool(b2) => {if b2 {return Constant::Real((r / 1.0))} else {panic!("Tried to divide by 0")}},
        }
        Constant::Int(i) => match c2 {
            Constant::Real(r2) => Constant::Real(((i as f64) / r2)),
            Constant::Int(i2) => Constant::Int((i / i2)),
            Constant::Bool(b2) => {if b2 {return Constant::Int((i / 1))} else {panic!("Tried to divide by 0")}},
        }
        Constant::Bool(b) =>  match c2 {
            Constant::Real(r2) =>{if b {return Constant::Real((1.0 / r2))} else {panic!("Tried to divide by 0")}},
            Constant::Int(i2) =>{if b {return Constant::Int((1 / i2))} else {panic!("Tried to divide by 0")}},
            Constant::Bool(b2) =>{if b && b2 {return Constant::Int(1)} else if (b && (!b2)) {panic!("Tried to divide by 0")} else {return Constant::Int(0)}},
        }
    }
}

fn mod_constants(c2: Constant, c1: Constant) -> Constant {
    match c1 {
        Constant::Real(r) => match c2 {
            Constant::Real(r2) => Constant::Real((r % r2)),
            Constant::Int(i2) => Constant::Real((r % (i2 as f64))),
            Constant::Bool(b2) => {if b2 {return Constant::Real((r % 1.0))} else {panic!("Tried to mod by 0")}},
        }
        Constant::Int(i) => match c2 {
            Constant::Real(r2) => Constant::Real(((i as f64) % r2)),
            Constant::Int(i2) => Constant::Int((i % i2)),
            Constant::Bool(b2) => {if b2 {return Constant::Int((i % 1))} else {panic!("Tried to mod by 0")}},
        }
        Constant::Bool(b) =>  match c2 {
            Constant::Real(r2) =>{if b {return Constant::Real((1.0 % r2))} else {panic!("Tried to mod by 0")}},
            Constant::Int(i2) =>{if b {return Constant::Int((1 % i2))} else {panic!("Tried to mod by 0")}},
            Constant::Bool(b2) =>{if b && b2 {return Constant::Int((1))} else if (b && (!b2)) {panic!("Tried to mod by 0")} else {return Constant::Int(0)}},
        }
    }
}

fn and_constants(c1: Constant, c2: Constant) -> Constant {
    match c1 {
        Constant::Real(r) => match c2 {
            Constant::Real(r2) => Constant::Bool((real_to_bool(r2) && real_to_bool(r))),
            Constant::Int(i2) => Constant::Bool((int_to_bool(i2) && real_to_bool(r))),
            Constant::Bool(b2) => Constant::Bool((b2 && real_to_bool(r))),
        }
        Constant::Int(i) => match c2 {
            Constant::Real(r2) => Constant::Bool((real_to_bool(r2) && int_to_bool(i))),
            Constant::Int(i2) => Constant::Bool((int_to_bool(i2) && int_to_bool(i))),
            Constant::Bool(b2) => Constant::Bool((b2 && int_to_bool(i))),
        }
        Constant::Bool(b) =>  match c2 {
            Constant::Real(r2) => Constant::Bool((real_to_bool(r2) && b)),
            Constant::Int(i2) => Constant::Bool((int_to_bool(i2) && b)),
            Constant::Bool(b2) => Constant::Bool((b2 && b)),
        }
    }
}

fn equal_constants(c1: Constant, c2: Constant) -> Constant {
    match c1 {
        Constant::Real(r)  => match c2 {
            Constant::Real(r2) => Constant::Bool((r == r2)),
            Constant::Int(i2) => Constant::Bool((r == (i2 as f64))),
            Constant::Bool(b2) => Constant::Bool((r == bool_to_real(b2))),
        }
        Constant::Int(i) => match c2 {
            Constant::Real(r2) => Constant::Bool(((i as f64) == r2)),
            Constant::Int(i2) =>Constant::Bool((i == i2)),
            Constant::Bool(b2) =>Constant::Bool((bool_to_int(b2) == i)),
        }
        Constant::Bool(b) =>  match c2 {
            Constant::Real(r2) =>Constant::Bool((bool_to_real(b) == r2)),
            Constant::Int(i2) =>Constant::Bool((bool_to_int(b) == i2)),
            Constant::Bool(b2) =>Constant::Bool((b2 == b)),
        }
    }
}

fn nEqual_constants(c1: Constant, c2: Constant) -> Constant {
    match c1 {
        Constant::Real(r)  => match c2 {
            Constant::Real(r2) => Constant::Bool((r != r2)),
            Constant::Int(i2) => Constant::Bool((r != (i2 as f64))),
            Constant::Bool(b2) => Constant::Bool((r != bool_to_real(b2))),
        }
        Constant::Int(i) => match c2 {
            Constant::Real(r2) => Constant::Bool(((i as f64) != r2)),
            Constant::Int(i2) =>Constant::Bool((i != i2)),
            Constant::Bool(b2) =>Constant::Bool((bool_to_int(b2) != i)),
        }
        Constant::Bool(b) =>  match c2 {
            Constant::Real(r2) =>Constant::Bool((bool_to_real(b) != r2)),
            Constant::Int(i2) =>Constant::Bool((bool_to_int(b) != i2)),
            Constant::Bool(b2) =>Constant::Bool((b2 != b)),
        }
    }
}

fn greater_constants(c1: Constant, c2: Constant) -> Constant {
    match c1 {
        Constant::Real(r)  => match c2 {
            Constant::Real(r2) => Constant::Bool((r > r2)),
            Constant::Int(i2) => Constant::Bool((r > (i2 as f64))),
            Constant::Bool(b2) => Constant::Bool((r > bool_to_real(b2))),
        }
        Constant::Int(i) => match c2 {
            Constant::Real(r2) => Constant::Bool(((i as f64) > r2)),
            Constant::Int(i2) =>Constant::Bool((i > i2)),
            Constant::Bool(b2) =>Constant::Bool((i > bool_to_int(b2))),
        }
        Constant::Bool(b) =>  match c2 {
            Constant::Real(r2) =>Constant::Bool((bool_to_real(b) > r2)),
            Constant::Int(i2) =>Constant::Bool((bool_to_int(b) > i2)),
            Constant::Bool(b2) =>Constant::Bool((bool_to_int(b) > bool_to_int(b2))),
        }
    }
}

fn lesser_constants(c1: Constant, c2: Constant) -> Constant {
    match c1 {
        Constant::Real(r)  => match c2 {
            Constant::Real(r2) => Constant::Bool((r < r2)),
            Constant::Int(i2) => Constant::Bool((r < (i2 as f64))),
            Constant::Bool(b2) => Constant::Bool((r < bool_to_real(b2))),
        }
        Constant::Int(i) => match c2 {
            Constant::Real(r2) => Constant::Bool(((i as f64) < r2)),
            Constant::Int(i2) =>Constant::Bool((i < i2)),
            Constant::Bool(b2) =>Constant::Bool((i < bool_to_int(b2))),
        }
        Constant::Bool(b) =>  match c2 {
            Constant::Real(r2) =>Constant::Bool((bool_to_real(b) < r2)),
            Constant::Int(i2) =>Constant::Bool((bool_to_int(b) < i2)),
            Constant::Bool(b2) =>Constant::Bool((bool_to_int(b) < bool_to_int(b2))),
        }
    }
}

fn greaterEq_constants(c2: Constant, c1: Constant) -> Constant {
    match c1 {
        Constant::Real(r)  => match c2 {
            Constant::Real(r2) => Constant::Bool((r >= r2)),
            Constant::Int(i2) => Constant::Bool((r >= (i2 as f64))),
            Constant::Bool(b2) => Constant::Bool((r >= bool_to_real(b2))),
        }
        Constant::Int(i) => match c2 {
            Constant::Real(r2) => Constant::Bool(((i as f64) >= r2)),
            Constant::Int(i2) =>Constant::Bool((i >= i2)),
            Constant::Bool(b2) =>Constant::Bool((i >= bool_to_int(b2))),
        }
        Constant::Bool(b) =>  match c2 {
            Constant::Real(r2) =>Constant::Bool((bool_to_real(b) >= r2)),
            Constant::Int(i2) =>Constant::Bool((bool_to_int(b) >= i2)),
            Constant::Bool(b2) =>Constant::Bool((bool_to_int(b) >= bool_to_int(b2))),
        }
    }
}


fn lessereq_constants(c2: Constant, c1: Constant) -> Constant {
    match c1 {
        Constant::Real(r)  => match c2 { 
            Constant::Real(r2) => Constant::Bool(r <= r2),
            Constant::Int(i2) => Constant::Bool(r <= (i2 as f64)),
            Constant::Bool(b2) => Constant::Bool(r <= bool_to_real(b2)),
        }
        Constant::Int(i) => match c2 {
            Constant::Real(r2) => Constant::Bool((i as f64) <= r2),
            Constant::Int(i2) =>Constant::Bool(i <= i2),
            Constant::Bool(b2) =>Constant::Bool(i <= bool_to_int(b2)),
        }
        Constant::Bool(b) =>  match c2 {
            Constant::Real(r2) =>Constant::Bool(bool_to_real(b) <= r2),
            Constant::Int(i2) =>Constant::Bool(bool_to_int(b) <= i2),
            Constant::Bool(b2) =>Constant::Bool(bool_to_int(b) <= bool_to_int(b2)),
        }
    }
}
