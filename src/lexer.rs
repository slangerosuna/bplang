use std::collections::HashMap;

#[derive(PartialEq, Clone, Debug)]
pub struct VecLiteral {
    pub data: Vec<Number>,
    pub ty: VecType,
}

#[derive(PartialEq, Clone, Debug)]
pub struct MatLiteral {
    pub data: Vec<Number>,
    pub ty: MatType,
}

#[derive(PartialEq, Clone, Debug)]
pub enum Number {
    Float(f64),
    Int(i128),
    Uint(u128),
    Ident(String),
}

#[derive(PartialEq, Clone, Debug)]
pub enum VecType {
    Fx2, Fx3, Fx4,
    Ix2, Ix3, Ix4,
    Ux2, Ux3, Ux4,
}

#[derive(PartialEq, Clone, Debug)]
pub enum MatType {
    Fx2x2, Fx2x3, Fx2x4,
    Fx3x2, Fx3x3, Fx3x4,
    Fx4x2, Fx4x3, Fx4x4,
}

#[derive(PartialEq, Clone, Debug)]
pub enum Token {
    // Identifier
    Ident(String),
    
    // Literals
    Number(f64),
    StringLiteral(String),
    CharLiteral(char),
    BoolLiteral(bool),
    VectorLiteral(VecLiteral),
    MatrixLiteral(MatLiteral),

    // Integer primitives
    U8, U16, U32, U64, U128,
    I8, I16, I32, I64, I128,
    USIZE, ISIZE,

    // Float primitives
    F32, F64,

    // Floating point Vector primitives
    F32x2, F32x3, F32x4,
    F64x2, F64x3, F64x4,

    // Integer Vector primitives
    U8x2, U8x3, U8x4,
    U16x2, U16x3, U16x4,
    U32x2, U32x3, U32x4,
    U64x2, U64x3, U64x4,
    U128x2, U128x3, U128x4,
    I8x2, I8x3, I8x4,
    I16x2, I16x3, I16x4,
    I32x2, I32x3, I32x4,
    I64x2, I64x3, I64x4,
    I128x2, I128x3, I128x4,

    // Matrix primitives
    F32x2x2, F32x2x3, F32x2x4,
    F32x3x2, F32x3x3, F32x3x4,
    F32x4x2, F32x4x3, F32x4x4,

    F64x2x2, F64x2x3, F64x2x4,
    F64x3x2, F64x3x3, F64x3x4,
    F64x4x2, F64x4x3, F64x4x4,

    // Other primitives
    Char, Bool, Str,

    // Keywords
    Struct,
    FN,
    Let,
    Var,
    Extern,
    If,
    Else,
    Match,
    Goto,
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
    Crash,
    Restart,
    Pub,
    Priv,
    Defer,
    Free,
    Alloc,
    As,
    LDCompShader,
    LDGeomShader,
    LDVertShader,
    LDFragShader,
    LDPostProcShader,
    LDBuffer,
    LDUniform,
    CallCompShader,
    RenderFrame,

    // Operators
    Equal,
    Plus,
    Minus,
    Multiply,
    Divide,
    Modulo,

    // Delimiters
    EOS,
    OpenCurly,
    CloseCurly,
    OpenParen,
    CloseParen,
    OpenBracket,
    CloseBracket,
    
    // Punctuation
    Comma,
    Colon,
    QuestionMark,

    // End of file
    EOF,
}

pub fn lex(input: String) -> Option<Vec<Token>>{
    let mut input = input.chars();
    let mut tokens = Vec::new();
    let mut map = HashMap::new();

    map.insert("fn", Token::FN);
    map.insert("let", Token::Let);
    map.insert("var", Token::Var);
    map.insert("extern", Token::Extern);
    map.insert("if", Token::If);
    map.insert("else", Token::Else);
    map.insert("for", Token::For);
    map.insert("while", Token::While);
    map.insert("goto", Token::Goto);
    map.insert("match", Token::Match);  
    map.insert("loop", Token::Loop);
    map.insert("break", Token::Break);
    map.insert("continue", Token::Continue);
    map.insert("return", Token::Return);
    map.insert("print", Token::Print); 
    map.insert("println", Token::Println); 
    map.insert("printerr", Token::PrintErr); 
    map.insert("printlnerr", Token::PrintlnErr);
    map.insert("tabiffy", Token::Tabiffy);
    map.insert("crash", Token::Crash);
    map.insert("restart", Token::Restart);
    map.insert("pub", Token::Pub);
    map.insert("priv", Token::Priv);
    map.insert("true", Token::BoolLiteral(true)); 
    map.insert("false", Token::BoolLiteral(false));
    map.insert("struct", Token::Struct);
    map.insert("defer", Token::Defer);
    map.insert("free", Token::Free);
    map.insert("alloc", Token::Alloc);
    map.insert("as", Token::As);
    map.insert("comp", Token::LDCompShader);
    map.insert("geom", Token::LDGeomShader);
    map.insert("vert", Token::LDVertShader);
    map.insert("frag", Token::LDFragShader);
    map.insert("postproc", Token::LDPostProcShader);
    map.insert("pushbuffer", Token::LDBuffer);
    map.insert("setuniform", Token::LDUniform);
    map.insert("callcomp", Token::CallCompShader);
    map.insert("renderframe", Token::RenderFrame);

    map.insert("u8", Token::U8); 
    map.insert("u16", Token::U16); 
    map.insert("u32", Token::U32); 
    map.insert("u64", Token::U64); 
    map.insert("u128", Token::U128);
    map.insert("i8", Token::I8); 
    map.insert("i16", Token::I16); 
    map.insert("i32", Token::I32); 
    map.insert("i64", Token::I64); 
    map.insert("i128", Token::I128);
    map.insert("usize", Token::USIZE);
    map.insert("isize", Token::ISIZE);

    map.insert("f32", Token::F32); 
    map.insert("f64", Token::F64); 

    map.insert("char", Token::Char); 
    map.insert("bool", Token::Bool); 
    map.insert("str", Token::Str);

    map.insert("f32x2", Token::F32x2);
    map.insert("f32x3", Token::F32x3);
    map.insert("f32x4", Token::F32x4);
    map.insert("f64x2", Token::F64x2);
    map.insert("f64x3", Token::F64x3);
    map.insert("f64x4", Token::F64x4);
    
    map.insert("u8x2", Token::U8x2);
    map.insert("u8x3", Token::U8x3);
    map.insert("u8x4", Token::U8x4);
    map.insert("u16x2", Token::U16x2);
    map.insert("u16x3", Token::U16x3);
    map.insert("u16x4", Token::U16x4);
    map.insert("u32x2", Token::U32x2);
    map.insert("u32x3", Token::U32x3);
    map.insert("u32x4", Token::U32x4);
    map.insert("u64x2", Token::U64x2);
    map.insert("u64x3", Token::U64x3);
    map.insert("u64x4", Token::U64x4);
    map.insert("u128x2", Token::U128x2);
    map.insert("u128x3", Token::U128x3);
    map.insert("u128x4", Token::U128x4);
    
    map.insert("i8x2", Token::I8x2);
    map.insert("i8x3", Token::I8x3);
    map.insert("i8x4", Token::I8x4);
    map.insert("i16x2", Token::I16x2);
    map.insert("i16x3", Token::I16x3);
    map.insert("i16x4", Token::I16x4);
    map.insert("i32x2", Token::I32x2);
    map.insert("i32x3", Token::I32x3);
    map.insert("i32x4", Token::I32x4);
    map.insert("i64x2", Token::I64x2);
    map.insert("i64x3", Token::I64x3);
    map.insert("i64x4", Token::I64x4);
    map.insert("i128x2", Token::I128x2);
    map.insert("i128x3", Token::I128x3);
    map.insert("i128x4", Token::I128x4);

    map.insert("f32x2x2", Token::F32x2x2);
    map.insert("f32x2x3", Token::F32x2x3);
    map.insert("f32x2x4", Token::F32x2x4);
    map.insert("f32x3x2", Token::F32x3x2);
    map.insert("f32x3x3", Token::F32x3x3);
    map.insert("f32x3x4", Token::F32x3x4);
    map.insert("f32x4x2", Token::F32x4x2);
    map.insert("f32x4x3", Token::F32x4x3);
    map.insert("f32x4x4", Token::F32x4x4);
    map.insert("f64x2x2", Token::F64x2x2);
    map.insert("f64x2x3", Token::F64x2x3);
    map.insert("f64x2x4", Token::F64x2x4);
    map.insert("f64x3x2", Token::F64x3x2);
    map.insert("f64x3x3", Token::F64x3x3);
    map.insert("f64x3x4", Token::F64x3x4);
    map.insert("f64x4x2", Token::F64x4x2);
    map.insert("f64x4x3", Token::F64x4x3);
    map.insert("f64x4x4", Token::F64x4x4);

    let mut i = 0;
    loop {
        if i > 1000 { 
            println!("Lexer error: too many tokens");
            break; 
        }
        if let Some(token) = get_token(&mut input, &mut map) {
            if token == Token::EOF 
              { break; }

            tokens.push(token);
        } else {
            println!("Lexer error: invalid token");
            break;
        }

        i += 1;
    }

    Some(tokens)
}

pub fn get_token(
    mut input: &mut std::str::Chars<'_>,
    mut map: &mut HashMap<&str, Token>,
) -> Option<Token> {
    let mut last_char;

    if let Some(l_char) = input.next() { 
        last_char = l_char; 
    } else { 
        return Some(Token::EOF); 
    }

    // Skip whitespace
    while last_char.is_whitespace() { 
        if let Some(l_char) = input.next() { 
            last_char = l_char; 
        } else { 
            return Some(Token::EOF); 
        }
    }
    
    if let Some(l_char) = match last_char {
        '=' => Some(Token::Equal),
        '+' => Some(Token::Plus),
        '-' => Some(Token::Minus),
        '*' => Some(Token::Multiply),
        '/' => Some(Token::Divide),
        ';' => Some(Token::EOS),
        '%' => Some(Token::Modulo),
        '{' => Some(Token::OpenCurly),
        '}' => Some(Token::CloseCurly),
        '(' => Some(Token::OpenParen),
        ')' => Some(Token::CloseParen),
        '[' => Some(Token::OpenBracket),
        ']' => Some(Token::CloseBracket),
        ',' => Some(Token::Comma),
        ':' => Some(Token::Colon),
        '?' => Some(Token::QuestionMark),
        _ => None,
    } {
        return Some(l_char);
    }

    if last_char == '#' {
        loop {
            if let Some(l_char) = input.next() { 
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
            if let Some(l_char) = input.next() { 
                last_char = l_char; 
            } else { 
                panic!("Lexer error: unterminated string literal");
            }
            if last_char == '"' 
              { break; }
            string.push(last_char);
        }

        return Some(Token::StringLiteral(string));
    }

    if last_char == '\'' {
        if let Some(l_char) = input.next() { 
            last_char = l_char; 
        } else { 
            panic!("Lexer error: unterminated char literal");
        }
        let l_char = last_char;
        
        if input.next() != Some('\'') 
          { panic!("Lexer error: unterminated char literal"); }

        return Some(Token::CharLiteral(l_char));
    }

    if last_char.is_alphabetic() {
        let mut ident = String::new();

        loop {
            ident.push(last_char);
            if let Some(l_char) = input.next() { 
                last_char = l_char; 
            } else { 
                return Some(Token::EOF); 
            }
            if !last_char.is_alphanumeric() && !(last_char == '_') 
              { break; }
        }

        if let Some(token) = map.get_mut(&ident[..]) 
          { return Some(token.clone()); }

        return Some(Token::Ident(ident));
    }

    if last_char.is_numeric() {
        let mut num = String::new();

        loop {
            num.push(last_char);
            if let Some(l_char) = input.next() { 
                last_char = l_char; 
            } else { 
                return Some(Token::EOF); 
            }
            if !last_char.is_numeric() && !(last_char == '.') 
              { break; }
        }

        return Some(Token::Number(num.parse::<f64>().ok()?));
    }

    if last_char == '#' {
        loop {
            if let Some(l_char) = input.next() { 
                last_char = l_char; 
            } else { 
                return Some(Token::EOF); 
            }
            if last_char == '\n' || last_char == '\r' 
              { break; }
        }

        return get_token(&mut input, &mut map);
    }
    
    panic!("Lexer error: invalid token");
}   
