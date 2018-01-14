# Summary

This is a simple interpreted programming language based off of Ada syntax written in Rust.

# Details

This language is implemented using the nom crate to generate tokens. It then uses a recursive descent parser to generate a parse tree, and finally generates an abstract syntax tree based on the parse tree. It is then evaluated. The language does not have types, and converts data as needed in order to execute binary operations. Right now, the language supports real numbers, integers, and booleans.

# Installation

You can build this project from source simply by cloning this repository and running "cargo install". The program accepts standard input. As such, one can test their files using "./interpreter < program > output".

# Syntax

### Variable assigning

All variables must be initialized using the "var" keyword before attempting to use the variable.

Example: var test;

Variables are then bound using the ":=" token.

Example: test := 5;

### Function declarations and calls

All functions are declared using the "fun" keyword, followed by the function identifier, and then parameter identifiers (all functions must take in at least 1 parameter). Blocks are denoted using curly braces {}.

Example: fun test(x, y, z) {
write x;
return y;
}

Functions are called using the same syntax as a variable binding, followed by the "call" keyword. As such, it is invalid for a function to have no return statement.

Example: x := call test(1, 4.0, true);

### Supported commands

call: Check section "Function declarations and calls"

return: Check section "Function declarations and calls"

write: Prints out the expression to stdout.

block: Declares a new block and local scope. Every new block is considered a new scope and is denoted using curly braces {}.

if: If the condition following the keyword "if" evaluates to true, the following block is executed.

while: Executes the following block of code while the expression following the "while" keyword evaluates to true.

assignment: Check section "Variable assigning"

### Unsupported commands
read: Although this currently gets parsed as a keyword, any attempt to use it will result in an error stating that it is currently unsupported.
