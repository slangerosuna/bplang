use std::collections::HashMap;
use std::str::Chars;

mod typedef;
pub use typedef::*;

struct CharIter<'a> {
    input: Chars<'a>,
    pushed: Vec<char>,
}

impl CharIter<'_> {
    fn new(input: &str) -> CharIter {
        CharIter {
            input: input.chars(),
            pushed: Vec::with_capacity(2),
        }
    }

    #[inline]
    fn push(&mut self, c: char) { self.pushed.push(c); }

    #[inline]
    fn pop(&mut self) -> Option<char> {
        if let Some(c) = self.pushed.pop() { return Some(c); } else { self.input.next_back() }
    }
}

pub fn lex(input: String) -> Option<Vec<Token>> {
    let mut input = CharIter::new(&input);
    let mut tokens = Vec::new();
    let mut map = get_map();

    while let Some(token) = get_token(&mut input, &mut map) {
        tokens.push(token);
        if tokens.len() > 1 { concat_tokens(&mut tokens) }
    }

    Some(tokens)
}

macro_rules! replace { ($new:expr, $tokens:expr) => {{ $tokens.pop(); $tokens.pop(); $tokens.push($new); }}; }
fn concat_tokens(tokens: &mut Vec<Token>) {
    match [tokens[tokens.len() - 2].clone(), tokens[tokens.len() - 1].clone()] {
        [Token::Operator(Operator::Minus), Token::Operator(Operator::GreaterThan)] => replace!(Token::Operator(Operator::ThinArrow), tokens),
        [Token::Operator(Operator::Equal), Token::Operator(Operator::GreaterThan)] => replace!(Token::Operator(Operator::FatArrow), tokens),
        [Token::Operator(Operator::Assignment), Token::Operator(Operator::Assignment)] => replace!(Token::Operator(Operator::Equal), tokens),
        [Token::Operator(Operator::LessThan), Token::Operator(Operator::Assignment)] => replace!(Token::Operator(Operator::LessThanOrEqual), tokens),
        [Token::Operator(Operator::GreaterThan), Token::Operator(Operator::Assignment)] => replace!(Token::Operator(Operator::GreaterThanOrEqual), tokens),
        [Token::Operator(Operator::Equal), Token::Operator(Operator::Assignment)] => replace!(Token::Operator(Operator::FullEqual), tokens),
        [Token::Delimiter(Delimiter::Comma), Token::Delimiter(Delimiter::Comma)] => replace!(Token::Delimiter(Delimiter::RowSeparator), tokens),
        [Token::Operator(Operator::Not), Token::Operator(Operator::Equal)] => replace!(Token::Operator(Operator::NotEqual), tokens),
        [Token::Operator(Operator::NotEqual), Token::Operator(Operator::Equal)] => replace!(Token::Operator(Operator::NotFullEqual), tokens),
        [Token::Literal(Literal::Number(n)), Token::Primitive(t)] => replace!(Token::Literal(Literal::TypedNumber(n, t)), tokens),
        [Token::Delimiter(Delimiter::Colon), Token::Delimiter(Delimiter::Colon)] => replace!(Token::Delimiter(Delimiter::DoubleColon), tokens),
        [Token::Delimiter(Delimiter::Colon), Token::Operator(Operator::Assignment)] => replace!(Token::Operator(Operator::Declaration), tokens),
        [Token::Operator(Operator::LessThan), Token::Operator(Operator::LessThan)] => replace!(Token::Operator(Operator::ShiftLeft), tokens),
        [Token::Operator(Operator::GreaterThan), Token::Operator(Operator::GreaterThan)] => replace!(Token::Operator(Operator::ShiftRight), tokens),
        _ => (),
    }
}


fn get_token(
    mut input: &mut CharIter,
    mut map: &mut HashMap<&str, Token>,
) -> Option<Token> {
    skip_whitespace(&mut input);
    let last_char = input.pop()?;

    if let Some(l_char) = get_symbol_token(last_char) { return Some(l_char); }

    match last_char {
        '#' => { skip_line(&mut input); return get_token(&mut input, &mut map); },
        '"' => return get_string_literal(&mut input) ,
        '\'' => return get_char_literal(&mut input),
        _ => {},
    }
    if last_char.is_alphabetic() || last_char == '_' { return get_ident(&mut input, map, last_char); }
    if last_char.is_numeric() { return get_number_literal(&mut input, last_char); }

    None
}

#[inline]
fn skip_whitespace(input: &mut CharIter) {
    while let Some(c) = input.pop() {
        if !c.is_whitespace() {
            input.push(c);
            break;
        }
    }
}

#[inline]
fn skip_line(input: &mut CharIter) { while let Some(c) = input.pop() { if c == '\n' || c == '\r' { break; } } }

#[inline]
fn get_symbol_token(last_char: char) -> Option<Token> {
    match last_char {
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
    }
}

fn get_string_literal(input: &mut CharIter) -> Option<Token> {
    let mut string = String::new();

    while let Some(last_char) = input.pop() {
        if last_char == '"' { break; }
        string.push(last_char);
    }

    Some(Token::Literal(Literal::Str(string)))
}

fn get_char_literal(input: &mut CharIter) -> Option<Token> {
    let last_char = input.pop()?;

    if input.pop() != Some('\'')
      { panic!("Lexer error: unterminated char literal"); }

    Some(Token::Literal(Literal::Char(last_char)))
}

fn get_ident(
    input: &mut CharIter,
    map: &mut HashMap<&str, Token>,
    last_char: char,
) -> Option<Token> {
    let mut ident = last_char.to_string();

    loop {
        let last_char = input.pop()?;
        if !last_char.is_alphanumeric() && !(last_char == '_') {
            input.push(last_char);
            break;
        }
        ident.push(last_char);
    }

    if let Some(token) = map.get_mut(&ident[..])
      { return Some(token.clone()); }

    Some(Token::Ident(ident))
}

fn get_number_literal(
    input: &mut CharIter,
    last_char: char,
) -> Option<Token> {
    let mut num = String::new();

    let mut period_count = 0;
    loop {
        num.push(last_char);
        let last_char = input.pop()?;

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

    Some(Token::Literal(Literal::Number(num.parse::<f64>()
         .expect("Lexer error: invalid number literal"))))
}

//TODO: use phf_map for compile time hash map generation
fn get_map() -> HashMap<&'static str, Token> {
    let mut map = HashMap::new();

    map.insert("auto", Token::Keyword(Keyword::Auto));
    map.insert("const", Token::Keyword(Keyword::Const));
    map.insert("pure", Token::Keyword(Keyword::Pure));
    map.insert("async", Token::Keyword(Keyword::Async));
    map.insert("await", Token::Keyword(Keyword::Await));
    map.insert("assert", Token::Keyword(Keyword::Assert));
    map.insert("fn", Token::Keyword(Keyword::FN));
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
    map.insert("eprint", Token::Keyword(Keyword::PrintErr));
    map.insert("eprintln", Token::Keyword(Keyword::PrintlnErr));
    map.insert("try", Token::Keyword(Keyword::Try));
    map.insert("catch", Token::Keyword(Keyword::Catch));
    map.insert("panic", Token::Keyword(Keyword::Panic));
    map.insert("unreachable", Token::Keyword(Keyword::Unreachable));
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
    map.insert("task", Token::Keyword(Keyword::LDTaskShader));
    map.insert("mesh", Token::Keyword(Keyword::LDMeshShader));
    map.insert("cull", Token::Keyword(Keyword::LDCullShader));
    map.insert("comp", Token::Keyword(Keyword::LDCompShader));
    map.insert("geom", Token::Keyword(Keyword::LDGeomShader));
    map.insert("vert", Token::Keyword(Keyword::LDVertShader));
    map.insert("frag", Token::Keyword(Keyword::LDFragShader));
    map.insert("tesc", Token::Keyword(Keyword::LDTescShader));
    map.insert("tese", Token::Keyword(Keyword::LDTeseShader));
    map.insert("rayg", Token::Keyword(Keyword::LDRayGenShader));
    map.insert("inter", Token::Keyword(Keyword::LDIRShader));
    map.insert("anyhit", Token::Keyword(Keyword::LDAnyHitShader));
    map.insert("closesthit", Token::Keyword(Keyword::LDClosestHitShader));
    map.insert("miss", Token::Keyword(Keyword::LDMissShader));
    map.insert("hitgroup", Token::Keyword(Keyword::LDHitGroupShader));
    map.insert("null", Token::Keyword(Keyword::Null));
    map.insert("postproc", Token::Keyword(Keyword::LDPostProcShader));
    map.insert("pushbuffer", Token::Keyword(Keyword::LDBuffer));
    map.insert("setuniform", Token::Keyword(Keyword::LDUniform));
    map.insert("bind", Token::Keyword(Keyword::BindShader));
    map.insert("unbind", Token::Keyword(Keyword::UnbindShader));
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

    map.insert("f16", Token::Primitive(Primitive::F16));
    map.insert("f32", Token::Primitive(Primitive::F32));
    map.insert("f64", Token::Primitive(Primitive::F64));
    map.insert("f128", Token::Primitive(Primitive::F128));

    map.insert("char", Token::Primitive(Primitive::Char));
    map.insert("bool", Token::Primitive(Primitive::Bool));
    map.insert("str", Token::Primitive(Primitive::Str));

    map
}
