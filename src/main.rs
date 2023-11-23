mod lexer;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        println!("Usage: {} <filename>", args[0]);
        return;
    }
    match args[1].as_str() {
        "b" => {
            let tokens = lexer::lex(args[1].clone());
            println!("{:?}", tokens);
        }
        _ => {
            println!("Unknown command: {}", args[0]);
        }
    }
}
