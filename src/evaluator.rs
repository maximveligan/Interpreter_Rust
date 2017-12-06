use parser::Program;
use parser::Cmd;
use parser::Constant;

fn evaluate_program(program: Program) {
    program.cmds.iter().for_each(evaluate_cmd);
}

fn evaluate_cmd(cmd: Cmd) {
    match cmd {
        Cmd::Read(_) => println!("Warning! Reading is not supported!"),
        Cmd::Write(id) => println!("{:?}", id),
        Cmd::Block(Block),
        Cmd::Return(Expr),
        Cmd::While { expr: Expr, block: Block },
        Cmd::If { expr: Expr, block: Block },
        Cmd::Assignment { var: Id, expr: Expr },
        Cmd::FunCall { var: Id, fun: Id, exprs: Vec<Expr> },
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
