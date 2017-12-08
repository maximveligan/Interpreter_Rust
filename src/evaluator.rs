use ast::Expr;
use parser::Block;
use parser::Function;
use parser::Program;
use parser::Cmd;
use parser::Constant;
use parser::Id;
use std::collections::HashMap;
use std::collections::HashSet;

struct CallStack(Vec<Frame>);

struct Frame(Vec<Scope>);

#[derive(Clone)]
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
            if let Some(_) = scope.0.insert(id.clone(), Some(constant.clone())) {
                return Ok(());
            }
        }
        if let Some(_) = self.get_global_scope().0.insert(
            id.clone(),
            Some(constant.clone()),
        )
        {
            return Ok(());
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

fn evaluate_program(program: Program) -> Result<(), String> {
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
    let mut effect: Option<Effect> = None;
    for cmd in cmds.iter() {
        match *cmd {
            Cmd::Read(_) => println!("Warning! Reading is not supported!"),
            Cmd::Write(ref id) => println!("{:?}", callstack.lookup(id)),
            Cmd::Block(ref block) => panic!("not done"),
            Cmd::Return(ref expr) => {
                callstack.get_last_frame().pop();
                return Effect::Return(Expr::from(expr.clone()));
            }
            Cmd::While {
                ref expr,
                ref block,
            } => {
                effect = Some(evaluate_while(
                    funcs,
                    Expr::from(expr.clone()),
                    Block::from(block.clone()),
                    callstack,
                ))
            }
            Cmd::If {
                ref expr,
                ref block,
            } => {
                effect = Some(evaluate_if(
                    funcs,
                    Expr::from(expr.clone()),
                    Block::from(block.clone()),
                    callstack,
                ))
            }
            Cmd::Assignment { ref var, ref expr } => {
                effect = Some(evaluate_assign(var, Expr::from(expr.clone()), callstack))
            }
            Cmd::FunCall {
                ref var,
                ref fun,
                ref exprs,
            } => panic!("not done"),
        }
        if let &Some(ref eff) = &effect {
            match *eff {
                Effect::Okay => (),
                Effect::Error(ref msg) => return Effect::Error(msg.clone()),
                Effect::Return(_) => {
                    panic!("Somehow got a return effect from a command that is not return")
                }
            }
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
    let effect = evaluate_cmds(funcs, &block.cmds, callstack);
    callstack.get_last_frame().pop();
    effect
}

fn evaluate_while(
    funcs: &HashMap<Id, (Vec<Id>, Block)>,
    expr: Expr,
    block: Block,
    callstack: &mut CallStack,
) -> Effect {
    match evaluate_expr(expr) {
        Ok(c) => {
            match c {
                Constant::Bool(b) => {
                    while (b) {
                        match evaluate_block(funcs, &block, callstack) {
                            Effect::Okay => (),
                            Effect::Return(val) => return Effect::Return(val),
                            Effect::Error(msg) => return Effect::Error(msg),
                        }
                    }
                    return Effect::Okay;
                }
                Constant::Real(r) => {
                    while real_to_bool(r) {
                        match evaluate_block(funcs, &block, callstack) {
                            Effect::Okay => (),
                            Effect::Return(val) => return Effect::Return(val),
                            Effect::Error(msg) => return Effect::Error(msg),
                        }
                    }
                    return Effect::Okay;
                }
                Constant::Int(i) => {
                    while int_to_bool(i) {
                        match evaluate_block(funcs, &block, callstack) {
                            Effect::Okay => (),
                            Effect::Return(val) => return Effect::Return(val),
                            Effect::Error(msg) => return Effect::Error(msg),
                        }
                    }
                    return Effect::Okay;
                }
            }
        }
        Err(msg) => Effect::Error(msg),
    }
}

fn evaluate_if(
    funcs: &HashMap<Id, (Vec<Id>, Block)>,
    expr: Expr,
    block: Block,
    callstack: &mut CallStack,
) -> Effect {
    match evaluate_expr(expr) {
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

fn real_to_bool(real: f64) -> bool {
    real != 0.0
}

fn int_to_bool(int: i64) -> bool {
    int != 0
}

fn evaluate_assign(var: &Id, expr: Expr, callstack: &mut CallStack) -> Effect {
    match evaluate_expr(expr) {
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
    callstack.push_new_frame();
    if funcs.contains_key(fun) {
        match param_vals
            .into_iter()
            .map(|param| evaluate_expr(Expr::from(param)))
            .collect::<Result<Vec<Constant>, String>>() {
            Ok(params) => {
                if ((params.len() == (funcs.get(fun).expect("Checked earlier for this error. Should've gotten a key")).0.len()))
                {
                    let zipped = funcs.get(fun).expect("Checked for this").0.iter().zip(params.iter());
                    let mut hash_map = HashMap::new();
                    for key_val in zipped {
                        hash_map.insert(key_val.0.clone(), Some(key_val.1.clone()));
                    }
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
        Effect::Return(val) => {callstack.0.pop(); evaluate_assign(var, val, callstack)},
        _ => Effect::Error("Current function branch does not have a return".to_string()),
    }
}

fn evaluate_expr(expr: Expr) -> Result<Constant, String> {
    Ok(Constant::Bool(true))
}
