use std::collections::HashMap;

#[derive(PartialEq, Clone, Debug)]
pub enum Literal {
    Number(f64),
    TypedNumber(f64, Primitive),
    Str(String),
    Char(char),
    Bool(bool),
}

#[derive(PartialEq, Clone, Debug)]
pub enum Keyword {
    Auto, // auto (inferred type or part of algebraic data type)
    Const, // const (can be evaluated at compile time)
    Pure, // pure (all data is immutable)
    Async, // async (can be awaited)
    Await, // await (waits for async function to finish)
    Assert, // assert (panics if condition is false in debug mode; establishes an invariant in release mode)
    Struct, // struct
    Union, // union
    Enum, // enum
    Dyn, // dyn (dynamic type)
    Interface, // interface (trait)
    Impl, // impl (implement traits, methods, and associated functions)
    FN, // fn
    Let, // let (immutable)
    Var, // var (mutable)
    Extern, // extern (import from other languages)
    Import, // import (import from other files)
    Use, // use (exposes traits, methods, and associated functions)
    If, // if (conditional expression)
    Else, // else (conditional expression)
    Match, // match (pattern matching)
    For, // for (loop through an iterator, or c-style for loop)
    While, // while (loop through a condition)
    Break, // break
    Continue, // continue
    Loop, // loop (infinite loop, equivalent to while(true), exited with break or return)
    Return, // return
    Print, // print
    Println, // println
    PrintErr, // printerr
    PrintlnErr, // printlnerr
    Tabiffy, // tabiffy (increase the global indentation level on the next line)
    Error, // error
    Try, // try (try to execute a block of code, catch any errors with catch like C or CPP
         // or return an error/none similar like in Zig)
    Catch, // catch (catch any errors from a try block)
    Panic, // panic (unrecoverable error)
    Pub, // pub (public)
    Priv, // priv (private)
    Defer, // defer (execute a block of code after the current scope ends)
    Free, // free (free memory)
    Alloc, // alloc (allocate memory)
    As, // as (type cast)
    In, // in (used for iterating through a collection or for checking if a value is in a collection)
    Null, // null (null pointer)
    LDCompShader, // comp (creates a compute shader)
    LDGeomShader, // geom (creates a geometry shader)
    LDVertShader, // vert (creates a vertex shader)
    LDFragShader, // frag (creates a fragment shader)
    LDPostProcShader, // postproc (creates a post-processing shader)
    LDBuffer, // pushbuffer (pushes a buffer to the GPU)
    LDUniform, // setuniform (sets a uniform variable in the shader)
    CallCompShader, // callcomp (calls a compute shader)
    BindShader, // bind(binds a shader to the GPU)
    InitRenderPipeline, // pipeline (initializes a render pipeline)
    RenderFrame, // renderframe (renders a frame using the selected render pipeline)
}

#[derive(PartialEq, Clone, Debug)]
pub enum Primitive {
    // Integer primitives
    U8, U16, U32, U64, U128,
    I8, I16, I32, I64, I128,
    USIZE, ISIZE,

    // Float primitives
    F32, F64,

    // Other primitives
    Char, Bool, Str,
}

#[derive(PartialEq, Clone, Debug)]
pub enum Operator {
    Assignment, // =
    Equal, // ==
    FullEqual, // ===
    NotEqual, // ~=
    NotFullEqual, // ~==
    Plus, // +
    Minus, // -
    Asterisk, // *
    Divide, // /
    Modulo, // %
    LessThan, // <
    GreaterThan, // >
    LessThanOrEqual, // <=
    GreaterThanOrEqual, // >=
    Not, // ~
    And, // and
    Or, // or
    Xor, // xor
    Ampersand, // &
    Caret, // ^
    TypeConversion, // ->
}

#[derive(PartialEq, Clone, Debug)]
pub enum Delimiter {
    Bang, // !
    Semicolon, // ;
    OpenCurly, // {
    CloseCurly, // }
    OpenParen, // (
    CloseParen, // )
    OpenBracket, // [
    CloseBracket, // ]
    Pipe, // |
    Dot, // .
    Comma, // ,
    RowSeparator, // ,,
    Colon, // :
    QuestionMark, // ?
}

#[derive(PartialEq, Clone, Debug)]
pub enum Token {
    // Identifier
    Ident(String),

    Literal(Literal),

    Primitive(Primitive),

    Keyword(Keyword),

    Operator(Operator),

    Delimiter(Delimiter),

    EOF,
}

pub fn lex(input: String) -> Option<Vec<Token>> {
    let mut input = input.chars().rev().collect();
    let mut tokens = Vec::new();
    let mut map = HashMap::new();

    map.insert("auto", Token::Keyword(Keyword::Auto));
    map.insert("const", Token::Keyword(Keyword::Const));
    map.insert("pure", Token::Keyword(Keyword::Pure));
    map.insert("async", Token::Keyword(Keyword::Async));
    map.insert("await", Token::Keyword(Keyword::Await));
    map.insert("assert", Token::Keyword(Keyword::Assert));
    map.insert("fn", Token::Keyword(Keyword::FN));
    map.insert("let", Token::Keyword(Keyword::Let));
    map.insert("var", Token::Keyword(Keyword::Var));
    map.insert("extern", Token::Keyword(Keyword::Extern));
    map.insert("import", Token::Keyword(Keyword::Import));
    map.insert("use", Token::Keyword(Keyword::Use));
    map.insert("if", Token::Keyword(Keyword::If));
    map.insert("else", Token::Keyword(Keyword::Else));
    map.insert("for", Token::Keyword(Keyword::For));
    map.insert("while", Token::Keyword(Keyword::While));
    map.insert("match", Token::Keyword(Keyword::Match));
    map.insert("loop", Token::Keyword(Keyword::Loop));
    map.insert("break", Token::Keyword(Keyword::Break));
    map.insert("continue", Token::Keyword(Keyword::Continue));
    map.insert("return", Token::Keyword(Keyword::Return));
    map.insert("print", Token::Keyword(Keyword::Print));
    map.insert("println", Token::Keyword(Keyword::Println));
    map.insert("printerr", Token::Keyword(Keyword::PrintErr));
    map.insert("printlnerr", Token::Keyword(Keyword::PrintlnErr));
    map.insert("tabiffy", Token::Keyword(Keyword::Tabiffy));
    map.insert("error", Token::Keyword(Keyword::Error));
    map.insert("try", Token::Keyword(Keyword::Try));
    map.insert("catch", Token::Keyword(Keyword::Catch));
    map.insert("panic", Token::Keyword(Keyword::Panic));
    map.insert("pub", Token::Keyword(Keyword::Pub));
    map.insert("priv", Token::Keyword(Keyword::Priv));
    map.insert("true", Token::Literal(Literal::Bool(true)));
    map.insert("false", Token::Literal(Literal::Bool(false)));
    map.insert("dyn", Token::Keyword(Keyword::Dyn));
    map.insert("struct", Token::Keyword(Keyword::Struct));
    map.insert("union", Token::Keyword(Keyword::Union));
    map.insert("enum", Token::Keyword(Keyword::Enum));
    map.insert("interface", Token::Keyword(Keyword::Interface));
    map.insert("impl", Token::Keyword(Keyword::Impl));
    map.insert("defer", Token::Keyword(Keyword::Defer));
    map.insert("free", Token::Keyword(Keyword::Free));
    map.insert("alloc", Token::Keyword(Keyword::Alloc));
    map.insert("as", Token::Keyword(Keyword::As));
    map.insert("in", Token::Keyword(Keyword::In));
    map.insert("not", Token::Operator(Operator::Not));
    map.insert("and", Token::Operator(Operator::And));
    map.insert("or", Token::Operator(Operator::Or));
    map.insert("xor", Token::Operator(Operator::Xor));
    map.insert("comp", Token::Keyword(Keyword::LDCompShader));
    map.insert("geom", Token::Keyword(Keyword::LDGeomShader));
    map.insert("vert", Token::Keyword(Keyword::LDVertShader));
    map.insert("frag", Token::Keyword(Keyword::LDFragShader));
    map.insert("null", Token::Keyword(Keyword::Null));
    map.insert("postproc", Token::Keyword(Keyword::LDPostProcShader));
    map.insert("pushbuffer", Token::Keyword(Keyword::LDBuffer));
    map.insert("setuniform", Token::Keyword(Keyword::LDUniform));
    map.insert("callcomp", Token::Keyword(Keyword::CallCompShader));
    map.insert("bind", Token::Keyword(Keyword::BindShader));
    map.insert("pipeline", Token::Keyword(Keyword::InitRenderPipeline));
    map.insert("renderframe", Token::Keyword(Keyword::RenderFrame));

    map.insert("u8", Token::Primitive(Primitive::U8));
    map.insert("u16", Token::Primitive(Primitive::U16));
    map.insert("u32", Token::Primitive(Primitive::U32));
    map.insert("u64", Token::Primitive(Primitive::U64));
    map.insert("u128", Token::Primitive(Primitive::U128));
    map.insert("i8", Token::Primitive(Primitive::I8));
    map.insert("i16", Token::Primitive(Primitive::I16));
    map.insert("i32", Token::Primitive(Primitive::I32));
    map.insert("i64", Token::Primitive(Primitive::I64));
    map.insert("i128", Token::Primitive(Primitive::I128));
    map.insert("usize", Token::Primitive(Primitive::USIZE));
    map.insert("isize", Token::Primitive(Primitive::ISIZE));

    map.insert("f32", Token::Primitive(Primitive::F32));
    map.insert("f64", Token::Primitive(Primitive::F64));

    map.insert("char", Token::Primitive(Primitive::Char));
    map.insert("bool", Token::Primitive(Primitive::Bool));
    map.insert("str", Token::Primitive(Primitive::Str));

    loop {
        if let Some(token) = get_token(&mut input, &mut map) {
            if token == Token::EOF
              { break; }

            tokens.push(token);

            // Check for multi-token combinations
            if tokens.len() > 1 {
                match [tokens[tokens.len() - 2].clone(), tokens[tokens.len() - 1].clone()] {
                    [Token::Operator(Operator::Minus), Token::Operator(Operator::GreaterThan)] => {
                        tokens.pop(); tokens.pop();
                        tokens.push(Token::Operator(Operator::TypeConversion));
                    },
                    [Token::Operator(Operator::Assignment), Token::Operator(Operator::Assignment)] => {
                        tokens.pop(); tokens.pop();
                        tokens.push(Token::Operator(Operator::Equal));
                    },
                    [Token::Operator(Operator::LessThan), Token::Operator(Operator::Assignment)] => {
                        tokens.pop(); tokens.pop();
                        tokens.push(Token::Operator(Operator::LessThanOrEqual));
                    },
                    [Token::Operator(Operator::GreaterThan), Token::Operator(Operator::Assignment)] => {
                        tokens.pop(); tokens.pop();
                        tokens.push(Token::Operator(Operator::GreaterThanOrEqual));
                    },
                    [Token::Operator(Operator::Equal), Token::Operator(Operator::Assignment)] => {
                        tokens.pop(); tokens.pop();
                        tokens.push(Token::Operator(Operator::FullEqual));
                    },
                    [Token::Delimiter(Delimiter::Comma), Token::Delimiter(Delimiter::Comma)] => {
                        tokens.pop(); tokens.pop();
                        tokens.push(Token::Delimiter(Delimiter::RowSeparator));
                    },
                    [Token::Operator(Operator::Not), Token::Operator(Operator::Equal)] => {
                        tokens.pop(); tokens.pop();
                        tokens.push(Token::Operator(Operator::NotEqual));
                    },
                    [Token::Operator(Operator::NotEqual), Token::Operator(Operator::Equal)] => {
                        tokens.pop(); tokens.pop();
                        tokens.push(Token::Operator(Operator::NotFullEqual));
                    },
                    [Token::Literal(Literal::Number(n)), Token::Primitive(t)] => {
                        tokens.pop(); tokens.pop();
                        tokens.push(Token::Literal(Literal::TypedNumber(n, t)));
                    },
                    _ => {},
                }
            }

        } else {
            println!("Lexer error: invalid token");
            break;
        }
    }

    Some(tokens)
}

pub fn get_token(
    mut input: &mut String,
    mut map: &mut HashMap<&str, Token>,
) -> Option<Token> {
    let mut last_char = if let Some(l_char) = input.pop() { l_char }
        else { return Some(Token::EOF); };

    // Skip whitespace
    while last_char.is_whitespace() {
        if let Some(l_char) = input.pop() {
            last_char = l_char;
        } else {
            return Some(Token::EOF);
        }
    }

    // Check for single-character operators and delimiters
    if let Some(l_char) = match last_char {
        '=' => Some(Token::Operator(Operator::Assignment)),
        '+' => Some(Token::Operator(Operator::Plus)),
        '-' => Some(Token::Operator(Operator::Minus)),
        '*' => Some(Token::Operator(Operator::Asterisk)),
        '/' => Some(Token::Operator(Operator::Divide)),
        ';' => Some(Token::Delimiter(Delimiter::Semicolon)),
        '%' => Some(Token::Operator(Operator::Modulo)),
        '<' => Some(Token::Operator(Operator::LessThan)),
        '>' => Some(Token::Operator(Operator::GreaterThan)),
        '~' => Some(Token::Operator(Operator::Not)),
        '!' => Some(Token::Delimiter(Delimiter::Bang)),
        '{' => Some(Token::Delimiter(Delimiter::OpenCurly)),
        '}' => Some(Token::Delimiter(Delimiter::CloseCurly)),
        '(' => Some(Token::Delimiter(Delimiter::OpenParen)),
        ')' => Some(Token::Delimiter(Delimiter::CloseParen)),
        '[' => Some(Token::Delimiter(Delimiter::OpenBracket)),
        ']' => Some(Token::Delimiter(Delimiter::CloseBracket)),
        '|' => Some(Token::Delimiter(Delimiter::Pipe)),
        '&' => Some(Token::Operator(Operator::Ampersand)),
        '^' => Some(Token::Operator(Operator::Caret)),
        ',' => Some(Token::Delimiter(Delimiter::Comma)),
        ':' => Some(Token::Delimiter(Delimiter::Colon)),
        '?' => Some(Token::Delimiter(Delimiter::QuestionMark)),
        '.' => Some(Token::Delimiter(Delimiter::Dot)),
        _ => None,
    } {
        return Some(l_char);
    }

    if last_char == '#' {
        loop {
            if let Some(l_char) = input.pop() {
                last_char = l_char;
            } else {
                return Some(Token::EOF);
            }
            if last_char == '\n' || last_char == '\r'
              { break; }
        }

        return get_token(&mut input, &mut map);
    }

    if last_char == '"' {
        let mut string = String::new();

        loop {
            if let Some(l_char) = input.pop() {
                last_char = l_char;
            } else {
                panic!("Lexer error: unterminated string literal");
            }
            if last_char == '"'
              { break; }
            string.push(last_char);
        }

        return Some(Token::Literal(Literal::Str(string)));
    }

    if last_char == '\'' {
        if let Some(l_char) = input.pop() {
            last_char = l_char;
        } else {
            panic!("Lexer error: unterminated char literal");
        }
        let l_char = last_char;

        if input.pop() != Some('\'')
          { panic!("Lexer error: unterminated char literal"); }

        return Some(Token::Literal(Literal::Char(l_char)));
    }

    if last_char.is_alphabetic() {
        let mut ident = String::new();

        loop {
            ident.push(last_char);
            if let Some(l_char) = input.pop() {
                last_char = l_char;
            } else {
                return Some(Token::EOF);
            }
            if !last_char.is_alphanumeric() && !(last_char == '_') {
                input.push(last_char);
                break;
            }
        }

        if let Some(token) = map.get_mut(&ident[..])
          { return Some(token.clone()); }

        return Some(Token::Ident(ident));
    }

    if last_char.is_numeric() {
        let mut num = String::new();

        let mut period_count = 0;
        loop {
            num.push(last_char);
            if let Some(l_char) = input.pop() {
                last_char = l_char;
            } else {
                return Some(Token::EOF);
            }

            //to make sure that the numbers on both sides of range initializations are parsed correctly
            if last_char == '.' {
                period_count += 1;
                if period_count > 1 {
                    input.push(last_char);
                    input.push(last_char);

                    break;
                }
            }

            //if the last character is not a number or a period, then we have reached the end of the number
            if !last_char.is_numeric() && !(last_char == '.') {
                input.push(last_char);
                break;
            }
        }

        return Some(Token::Literal(Literal::Number(num.parse::<f64>()
                    .expect("Lexer error: invalid number literal"))));
    }

    println!("{}", last_char);
    panic!("Lexer error: invalid token");
}
