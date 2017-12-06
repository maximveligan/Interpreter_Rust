use parser::Block;
use ast::Expr;

use parser::Program;
use parser::Cmd;
use parser::Constant;
use parser::Id;
use std::collections::HashMap;
use std::collections::HashSet;

struct CallStack(Vec<Frame>);
struct Frame(Vec<Scope>);
struct Scope(HashMap<Id, Option<Constant>>);
struct GlobalScope(Scope);

impl CallStack {
    pub fn new() -> CallStack {
        CallStack(Vec::new())
    }
    fn get_frames(&self) -> Vec<Frame> {
        self.0
    }
    fn get_last_scopes(&self) -> Option<Vec<Scope>> {
        match self.get_frames().last() {
            Some(scopes) => Some(scopes.0),
            None => None,
        }
    }
    fn lookup(self, id: Id) -> Option<Option<Constant>> {
        self.get_last_scopes().unwrap_or(Vec::new()).iter().rev().for_each(|scope| -> Option<Option<Constant>> {scope.0.get(&id)});
        None
    }
}

impl GlobalScope {
    pub fn new(vars: HashSet<Id>) -> GlobalScope {
        let mut hash_map: HashMap<Id, Option<Constant>> = HashMap::new();
        vars.iter().for_each(|var| hash_map.insert(*var, None));

        GlobalScope(Scope(hash_map))
    }
}

fn evaluate_program(program: Program) -> Result<(), String>{
    let call_stack = CallStack::new();
    let global_scope = GlobalScope::new(program.vars);
    program.cmds.iter().for_each(evaluate_cmd);
}

fn evaluate_cmd(cmd: Cmd) {
    match cmd {
        Cmd::Read(_) => println!("Warning! Reading is not supported!"),
        Cmd::Write(id) => println!("{:?}", id),
        Cmd::Block(Block) => panic!("not done"),
        Cmd::Return(Expr) => panic!("not done"),
        Cmd::While { expr, block} => panic!("not done"),
        Cmd::If { expr, block} => panic!("not done"),
        Cmd::Assignment { var, expr} => panic!("not done"),
        Cmd::FunCall { var, fun} => panic!("not done"),
    }
}

fn evaluate_block(block: Block) {

}

fn evaluate_return() {

}

fn evaluate_while() {

}

fn evaluate_if(expr: Expr, block: Block) {
    match evaluate_expression(expr) {
        Constant::Bool(b) => {if (b) {evaluate_block(block)}},
        Constant::Real(r) => {if real_to_bool(r) {evaluate_block(block)}},
        Constant::Int(i) => {if int_to_bool(i) {evaluate_block(block)}},
    }
}

fn bool_to_int() {

}

fn bool_to_real() {

}

fn real_to_bool() {

}

fn int_to_bool() {

}

fn evaluate_assign() {
    //TODO: Basically just find out which scope the variable lies under, and then assign it there.
    //if the variable doesn't lie under any scope, throw an error.
}

fn evaluate_expression(expr: Expr) -> Constant {

}
