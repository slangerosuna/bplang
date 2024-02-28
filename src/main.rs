use std::fs;

mod lexer;
mod syntax_tree_gen;
mod interpreter;

fn main() {
    let mut args: Vec<String> = std::env::args().collect();

    match args[1].as_str() { //can crash if no args, but that's fine because i'm lazy
        "b" => {
            let contents = fs::read_to_string(args[2].clone());
            let tokens = lexer::lex(contents.unwrap()).expect("failed to lex file");
            //println!("{:?}", tokens);

            let ast = syntax_tree_gen::gen_ast(tokens);

            println!("{:?}", ast);
        }
        "r" => {
            let contents = fs::read_to_string(args[2].clone());
            let tokens = lexer::lex(contents.unwrap()).expect("failed to lex file");
            //println!("{:?}", tokens);

            let ast = syntax_tree_gen::gen_ast(tokens);

            println!("{:?}", ast);

            interpreter::interpret(ast);
        }
        _ => {
            println!("Unknown command: {}", args[0]);
        }
    }
}
