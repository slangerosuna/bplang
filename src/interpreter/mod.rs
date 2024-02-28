use crate::{
    lexer::*,
    syntax_tree_gen::*,
};


struct Env {
    ast: AbstractSyntaxTree,

    vars: Vec<Variable>,
    funcs: Vec<Function>,
    types: Vec<Type>,
}

impl Env {
    fn new(
        ast: AbstractSyntaxTree,
    ) -> Self {
        Self {
            ast,
            vars: Vec::new(),
            funcs: Vec::new(),
            types: Vec::new(),
        }
    }
}

struct Variable {
    name: String,
    ty: Type,
    value: Box<dyn std::any::Any>,
}

struct Function {
    name: String,
    args: Vec<Type>,
    body: Node,
}

pub fn interpret(
    ast: AbstractSyntaxTree,
) {
    let mut env = Env::new(ast);
}
