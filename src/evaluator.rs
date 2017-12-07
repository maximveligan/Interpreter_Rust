use parser::Block;
use ast::Expr;
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
            None => panic!("Attempted to get last stack frame before building a stack. Did you forget to push a new frame?"),
        }
    }

    fn get_global_scope(&mut self) -> &mut Scope {
        match self.0.first_mut() {
            Some(scopes) => match scopes.0.first_mut() {
                Some(gscope) => gscope,
                None => panic!("Attempted to get globalscope before defining it"),
            }
            None => panic!("Attempted to get globalscope before defining it"),
        }
    }

    fn lookup(&mut self, id: &Id) -> Option<Option<Constant>> {
        for scope in self.get_last_frame().iter().rev() {
            if let Some(value) = scope.0.get(&id) {
                return Some(value.clone())
            }
        }
        if let Some(val) = self.get_global_scope().0.get(&id) {
            return Some(val.clone())
        }
        None
    }

    fn set_var(&mut self, id: &Id, constant: &Constant) -> Result<(), String> {
        for scope in self.get_last_frame().iter_mut().rev() {
            if let Some(_) =  scope.0.insert(id.clone(), Some(constant.clone())) {
                return Ok(())
            }
        }
        if let Some(_) = self.get_global_scope().0.insert(id.clone(), Some(constant.clone())) {
            return Ok(())
        }
        Err("Bound a variable before initializing it".to_string())
    }

}

impl Scope {
    pub fn new(vars: &HashSet<Id>) -> Scope {
        let mut hash_map: HashMap<Id, Option<Constant>> = HashMap::new();
        vars.iter().for_each(|var| match hash_map.insert(var.to_owned(), None) {
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

fn evaluate_program(program: Program) -> Result<(), String>{
    let mut funcs: HashMap<Id, (HashSet<Id>, Block)> = HashMap::new();
    let mut call_stack = CallStack::new();
    call_stack.0.push(Frame::new());
    for func in program.funcs.iter() {
        match funcs.insert(func.id.clone(), (func.parameters.clone(), func.block.clone())) {
            None => (),
            Some(_) => return Err("Error: two or more functions have the same name".to_string()),
        }
    }
    
    match evaluate_block(&funcs, &Block { vars: program.vars, cmds: program.cmds }, &mut call_stack) {
        Effect::Okay => Ok(()),
        Effect::Return(val) => Err("This shouldn't be possible because the parser should catch it".to_string()),
        Effect::Error(msg) => Err(msg),
    }
}

fn evaluate_cmds(funcs: &HashMap<Id, (HashSet<Id>, Block)>, cmds: &Vec<Cmd>, callstack: &mut CallStack) -> Effect {
    let mut effect: Option<Effect> = None;
    for cmd in cmds.iter() {
        match *cmd {
            Cmd::Read(_) => println!("Warning! Reading is not supported!"),
            Cmd::Write(ref id) => println!("{:?}", callstack.lookup(id)),
            Cmd::Block(ref block) => panic!("not done"),
            Cmd::Return(ref expr) => {callstack.get_last_frame().pop(); return Effect::Return(Expr::from(expr.clone()))},
            Cmd::While {ref expr,ref block} => effect = Some(evaluate_while(funcs, Expr::from(expr.clone()), block, callstack)),
            Cmd::If {ref expr,ref block} => effect = Some(evaluate_if(funcs, Expr::from(expr.clone()), block, callstack)),
            Cmd::Assignment {ref var,ref expr} => effect = Some(evaluate_assign(var, Expr::from(expr.clone()), callstack)),
            Cmd::FunCall {ref var,ref fun,ref exprs} => panic!("not done"),
        }
        if let &Some(ref eff) = &effect {
            match *eff {
                Effect::Okay => (),
                Effect::Error(ref msg) => return Effect::Error(msg.clone()),
                Effect::Return(_) => panic!("Somehow got a return effect from a command that is not return"),
            }
        }
            
    }
    Effect::Okay
}

fn evaluate_fun(fun: Function, callstack: &mut CallStack) {

}

fn evaluate_block(funcs: &HashMap<Id, (HashSet<Id>, Block)>, block: &Block, callstack: &mut CallStack) -> Effect {
    let mut local_scope = Scope::new(&block.vars);
    callstack.get_last_frame().push(local_scope);
    let effect = evaluate_cmds(funcs, &block.cmds, callstack);
    callstack.get_last_frame().pop();
    effect
}

fn evaluate_while(funcs: &HashMap<Id, (HashSet<Id>, Block)>, expr: Expr, block: &Block, callstack: &mut CallStack) -> Effect {
    match evaluate_expr(expr) {
        Ok(c) => match c {
            Constant::Bool(b) => {while (b) {
                match evaluate_block(funcs, block, callstack){
                    Effect::Okay => (),
                    Effect::Return(val) => return Effect::Return(val),
                    Effect::Error(msg) => return Effect::Error(msg),
                }
            }
            return Effect::Okay
            },
            Constant::Real(r) => {while real_to_bool(r) {
                match evaluate_block(funcs, block, callstack){
                    Effect::Okay => (),
                    Effect::Return(val) => return Effect::Return(val),
                    Effect::Error(msg) => return Effect::Error(msg),
                }
            }
            return Effect::Okay
            },
            Constant::Int(i) => {while int_to_bool(i) {
                match evaluate_block(funcs, block, callstack){
                    Effect::Okay => (),
                    Effect::Return(val) => return Effect::Return(val),
                    Effect::Error(msg) => return Effect::Error(msg),
                }
            }
            return Effect::Okay
            },
        }
        Err(msg) => Effect::Error(msg),
    }
}

fn evaluate_if(funcs: &HashMap<Id, (HashSet<Id>, Block)>, expr: Expr, block: &Block, callstack: &mut CallStack) -> Effect {
    match evaluate_expr(expr) {
        Ok(c) => match c {
            Constant::Bool(b) => {if (b) {
                match evaluate_block(funcs, block, callstack){
                    Effect::Okay => return Effect::Okay,
                    Effect::Return(val) => return Effect::Return(val),
                    Effect::Error(msg) => return Effect::Error(msg),
                }
            }
            else {
                return Effect::Okay
            }},
            Constant::Real(r) => {if real_to_bool(r) {
                match evaluate_block(funcs, block, callstack){
                    Effect::Okay => return Effect::Okay,
                    Effect::Return(val) => return Effect::Return(val),
                    Effect::Error(msg) => return Effect::Error(msg),
                }
            }
            else {
                return Effect::Okay
            }},
            Constant::Int(i) => {if int_to_bool(i) {
                match evaluate_block(funcs, block, callstack){
                    Effect::Okay => return Effect::Okay,
                    Effect::Return(val) => return Effect::Return(val),
                    Effect::Error(msg) => return Effect::Error(msg),
                }
            }
            else {
                return Effect::Okay
            }},
        }
        Err(msg) => Effect::Error(msg),
    }
}

pub fn bool_to_int(b: bool) -> i64{
    if b {
       1
   }
   else {
       0
   }
}

fn real_to_bool(real: f64) -> bool {
    real != 0.0
}

fn int_to_bool(int: i64) -> bool {
    int != 0
}

fn evaluate_assign(var: &Id, expr: Expr, callstack: &mut CallStack) -> Effect {
    match evaluate_expr(expr) {
        Ok(c) => match callstack.set_var(&var, &c) {
            Ok(()) => Effect::Okay,
            Err(msg) => Effect::Error(msg.to_string()),
        }    
        Err(msg) => Effect::Error(msg),
    }
}

fn evaluate_fun_call(funcs: Vec<Function>, var: Id, fun: Id, param_vals: Vec<Expr>, callstack: &mut CallStack) -> Effect {
    Effect::Okay
}

fn evaluate_expr(expr: Expr) -> Result<Constant, String> {
    Ok(Constant::Bool(true))
}
