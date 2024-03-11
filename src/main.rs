use std::fs;

mod lexer;
mod syntax_tree_gen;
mod interpreter;

fn main() -> Result<(), std::io::Error> {
    let mut args: Vec<String> = std::env::args().collect();
    if let Some(arg) = if args.len() > 1 { Some(args[1].as_str()) } else { None } {
        match arg {
            "b" => {
                let path =
                    if args.len() > 2
                      { args[2].clone() }
                    else {
                        return Err(std::io::Error::new(std::io::ErrorKind::InvalidInput, "No file path provided"));
                    };
                let contents = fs::read_to_string(path)?;
                let tokens = lexer::lex(contents)
                    .ok_or(std::io::Error::new(std::io::ErrorKind::InvalidInput, "Failed to lex file"))?;

                let ast = syntax_tree_gen::gen_ast(tokens);

                println!("{:?}", ast);
            }
            "r" => {
                let path =
                    if args.len() > 2
                      { args[2].clone() }
                    else {
                        return Err(std::io::Error::new(std::io::ErrorKind::InvalidInput, "No file path provided"));
                    };
                let contents = fs::read_to_string(path)?;
                let tokens = lexer::lex(contents)
                    .ok_or(std::io::Error::new(std::io::ErrorKind::InvalidInput, "Failed to lex file"))?;
                //println!("{:?}", tokens);

                let ast = syntax_tree_gen::gen_ast(tokens);

                println!("{:?}", ast);

                interpreter::interpret(ast);
            }
            _ => {
                return Err(std::io::Error::new(std::io::ErrorKind::InvalidInput, format!("Unknown command: {}", arg)));
            }
        }
    } else {
        return Err(std::io::Error::new(std::io::ErrorKind::InvalidInput, "No command provided"));
    }

    Ok(())
}
