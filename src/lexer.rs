use std::collections::HashMap;

#[derive(PartialEq, Clone, Debug)]
pub enum Literal {
    Number(f64),
    Str(String),
    Char(char),
    Bool(bool),
}

#[derive(PartialEq, Clone, Debug)]
pub enum Keyword {
    Assert,
    Struct,
    Union,
    Dyn,
    Interface,
    Impl,
    FN,
    Let,
    Var,
    Extern,
    Import,
    If,
    Else,
    Match,
    For,
    While,
    Break,
    Continue,
    Loop,
    Return,
    Print,
    Println,
    PrintErr,
    PrintlnErr,
    Tabiffy,
    Error,
    Try,
    Catch,
    Panic,
    Pub,
    Priv,
    Defer,
    Free,
    Alloc,
    As,
    In,
    Null,
    LDCompShader,
    LDGeomShader,
    LDVertShader,
    LDFragShader,
    LDPostProcShader,
    LDBuffer,
    LDUniform,
    CallCompShader,
    RenderFrame,
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
    Equal,
    Plus,
    Minus,
    Asterisk,
    Divide,
    Modulo,
    LessThan,
    GreaterThan,
    Not,
    And,
    Or,
    Xor,
    Ampersand,
    Caret,
}

#[derive(PartialEq, Clone, Debug)]
pub enum Delimiter {
    Bang,
    Semicolon,
    OpenCurly,
    CloseCurly,
    OpenParen,
    CloseParen,
    OpenBracket,
    CloseBracket,
    Pipe,
    Dot,
    Comma,
    Colon,
    QuestionMark,
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

    //TODO make the map not be created at runtime
    map.insert("assert", Token::Keyword(Keyword::Assert));
    map.insert("fn", Token::Keyword(Keyword::FN));
    map.insert("let", Token::Keyword(Keyword::Let));
    map.insert("var", Token::Keyword(Keyword::Var));
    map.insert("extern", Token::Keyword(Keyword::Extern));
    map.insert("import", Token::Keyword(Keyword::Import));
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
    let mut last_char;

    if let Some(l_char) = input.pop() {
        last_char = l_char;
    } else {
        return Some(Token::EOF);
    }

    // Skip whitespace
    while last_char.is_whitespace() {
        if let Some(l_char) = input.pop() {
            last_char = l_char;
        } else {
            return Some(Token::EOF);
        }
    }

    if let Some(l_char) = match last_char {
        '=' => Some(Token::Operator(Operator::Equal)),
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
            if !last_char.is_numeric() && !(last_char == '.') {
                input.push(last_char);
                break;
            }
        }

        return Some(Token::Literal(Literal::Number(num.parse::<f64>().ok()?)));
    }

    println!("{}", last_char);
    panic!("Lexer error: invalid token");
}
