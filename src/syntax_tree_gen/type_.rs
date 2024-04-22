use crate::syntax_tree_gen::*;

fn handle_ident_token(toks: &Vec<Token>, i: &mut usize) -> Type {
    match &toks[*i] {
        Token::Ident(ident) => Type::Ident(ident.clone()),
        _ => unreachable!(),
    }
}

fn handle_primitive_token(toks: &Vec<Token>, i: &mut usize) -> Type {
    match &toks[*i] {
        Token::Primitive(type_) => Type::Primitive(type_.clone()),
        _ => unreachable!(),
    }
}

fn handle_delimiter_token(toks: &Vec<Token>, i: &mut usize) -> Type {
    match &toks[*i] {
        Token::Delimiter(delim) => match delim {
            Delimiter::OpenParen => get_tuple_type(toks, i),
            Delimiter::Bang => Type::Result{
                err: None,
                ok: { *i += 1; Box::new(get_type(toks, i)) },
            },
            Delimiter::QuestionMark => Type::Optional {
                type_: Box::new(get_type(toks, i)),
            },
            _ => panic!("Expected type, found delimiter: {:?}", delim),
        },
        _ => unreachable!(),
    }
}

fn handle_keyword_token(toks: &Vec<Token>, i: &mut usize) -> Type {
    match &toks[*i] {
        Token::Keyword(kw) => match kw {
            Keyword::FN => get_fn_type(toks, i),
            Keyword::Struct => Type::Struct {
                fields: get_names_and_known_types(toks, i)
            },
            Keyword::Union => Type::Union {
                variants: get_names_and_known_types(toks, i)
            },
            Keyword::Enum => Type::Enum {
                variants: get_names_and_types(toks, i),
            },
            _ => panic!("Expected type, found keyword: {:?}", kw),
        },
        _ => unreachable!(),
    }
}

pub fn get_names_and_known_types(
    toks: &Vec<Token>,
    i: &mut usize,
) -> Vec<(String, Type)> {
    get_names_and_types(toks, i)
        .into_iter()
        .map(|(name, type_)| (name, type_.unwrap_or(Type::Infer)))
        .collect()
}

fn handle_operator_token(toks: &Vec<Token>, i: &mut usize) -> Type {
    match &toks[*i] {
        Token::Operator(op) => match op {
            Operator::Asterisk => Type::Pointer {
                type_: Box::new(get_type(toks, i)),
            },
            Operator::Ampersand => Type::Reference {
                type_: Box::new(get_type(toks, i)),
            },
            _ => panic!("Expected type, found operator: {:?}", op),
        },
        _ => unreachable!(),
    }
}

fn get_names_and_types(
    toks: &Vec<Token>,
    i: &mut usize,
) -> Vec<(String, Option<Type>)> {
    let mut v = Vec::new();
    *i += 1;

    assert_eq!(toks[*i], Token::Delimiter(Delimiter::OpenCurly),
               "Expected open curly after struct/union/enum keyword");

    loop {
        *i += 1;
        match &toks[*i] {
            Token::Delimiter(Delimiter::CloseCurly) => break,
            Token::Delimiter(Delimiter::Comma) => continue,
            Token::Ident(name) => {
                *i += 1;
                match &toks[*i] {
                    Token::Delimiter(Delimiter::Colon) => {
                        *i += 1;
                        v.push((name.clone(), Some(get_type(toks, i))));
                    },
                    _ => v.push((name.clone(), None)),
                }
            },
            _ => panic!("Expected identifier"),
        }
    }

    v
}

fn get_tuple_type(
    toks: &Vec<Token>,
    i: &mut usize,
) -> Type {
    let mut v = Vec::new();

    loop {
        *i += 1;
        match &toks[*i] {
            Token::Delimiter(Delimiter::CloseParen) => break,
            Token::Delimiter(Delimiter::Comma) => continue,
            _ => v.push(get_type(toks, i)),
        }
    }

    if v.is_empty() { panic!("Expected type"); }
    //if v.len() == 1 { return v[0].clone(); }
    //doesn't do the above so that single element tuples can be used for precedence
    Type::Tuple{
        types: v,
    }
}

fn get_fn_type(
    toks: &Vec<Token>,
    i: &mut usize,
) -> Type {
    *i += 1;

    let args = get_fn_args(toks, i);
    let args = args.into_iter().map(|(_, type_)| type_).collect();
    *i += 1;

    let return_type = match &toks[*i] {
        Token::Operator(Operator::ThinArrow) => { *i += 1; get_type(toks, i) },
        _ => Type::None,
    };
    let return_type = Box::new(return_type);

    Type::FunctionPointer {
        args,
        return_type,
    }
}

/*
 * sum types:
 * A | B
 * A or B
 * A + B
 * enum { A: A, B: B }
 *
 * product types:
 * A & B
 * A and B
 * A * B
 * (A, B)
 * struct { A: A, B: B }
 *
 * type arithmetic:
 * (A | B) - B = A
 * (A & B) + C = (A & B) | C
 * (A & B) / B = A
 * (A | B) * C = (A | B) & C = (A & C) | (B & C)
 * (A | B) & (C | D) = (A & C) | (A & D) | (B & C) | (B & D)a
 * A / B = A & ~B # ~B is the complement of B, i.e. all types that satisfy A but not B
 */

pub fn get_type(toks: &Vec<Token>, i: &mut usize) -> Type {
    let type_ = match &toks[*i] {
        Token::Ident(_) => handle_ident_token(toks, i),
        Token::Primitive(_) => handle_primitive_token(toks, i),
        Token::Delimiter(_) => handle_delimiter_token(toks, i),
        Token::Keyword(_) => handle_keyword_token(toks, i),
        Token::Operator(_) => handle_operator_token(toks, i),
        Token::Literal(_) => panic!("Expected type, found literal: {:?}", toks[*i]),
    };

    *i += 1;

    let mut type_ = match &toks[*i] {
        Token::Delimiter(Delimiter::Pipe) => handle_anon_sum(toks, i, type_),
        Token::Delimiter(Delimiter::Bang) => Type::Result {
            err: Some(Box::new(type_)),
            ok: Box::new(get_type(toks, i)),
        },
        _ => type_,
    };

    unwrap_single_element_tuples(&mut type_);

    type_
}

fn unwrap_single_element_tuples(type_: &mut Type) {
    if let Type::Tuple { types } = type_
      { if types.len() == 1 { *type_ = types[0].clone(); } }

    for child in type_.children() { unwrap_single_element_tuples(child); }
}

fn handle_anon_sum(
    toks: &Vec<Token>,
    i: &mut usize,
    type_: Type,
) -> Type {
    *i += 1;
    let other = get_type(toks, i);

    //TODO handle precedence
    match other {
        Type
            ::AnonSum { mut variants } => {
            variants.insert(0, type_);
            Type::AnonSum { variants }
        },
        _ => Type::AnonSum { variants: vec![type_, other] },
    }
}

//TODO add lifetimes and mutability as part of types
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    None, // () type
    Infer, //The type needs to be inferred by the semantic analyzer
    Ident(String),
    Primitive(Primitive),
    Array {
        type_: Box<Type>,
        size: Option<Box<Literal>>,
    },
    Tuple {
        types: Vec<Type>,
    },
    Struct {
        fields: Vec<(String, Type)>,
    },
    Union {
        variants: Vec<(String, Type)>,
    },
    Enum {
        variants: Vec<(String, Option<Type>)>,
    },
    AnonSum {
        variants: Vec<Type>,
    },
    Vector {
        type_: Box<Type>,
        size: Box<Literal>,
    },
    Matrix {
        type_: Box<Type>,
        size: Box<Literal>,
    },
    Slice {
        type_: Box<Type>,
    },
    FunctionPointer {
        args: Vec<Type>,
        return_type: Box<Type>,
    },
    Pointer {
        type_: Box<Type>,
    },
    Reference {
        type_: Box<Type>,
    },
    Type,
    Result {
        err: Option<Box<Type>>,
        ok: Box<Type>,
    },
    Optional {
        type_: Box<Type>,
    },
}

impl Type {
    fn children<'a>(&'a mut self) -> Vec<&'a mut Type> {
        match self {
            Type::Array { type_, ..} => vec![type_],
            Type::Tuple { types } => types.iter_mut().collect(),
            Type::Struct { fields } => fields.iter_mut().map(|(_, type_)| type_).collect(),
            Type::Union { variants } => variants.iter_mut().map(|(_, type_)| type_).collect(),
            Type::Enum { variants } => variants.iter_mut().filter_map(|(_, type_)| type_.as_mut()).collect(),
            Type::AnonSum { variants } => variants.iter_mut().collect(),
            Type::Vector { type_, ..} => vec![type_],
            Type::Matrix { type_, ..} => vec![type_],
            Type::Slice { type_ } => vec![type_],
            Type::FunctionPointer { args, return_type } => {
                let mut v: Vec<_> = args.iter_mut().collect();
                v.push(return_type);
                v
            },
            Type::Pointer { type_ } => vec![type_],
            Type::Reference { type_ } => vec![type_],
            Type::Result { err, ok } => {
                let mut v: Vec<_> = vec![ok.as_mut()];
                if let Some(err) = err {
                    v.push(err.as_mut());
                }
                v
            },
            Type::Optional { type_ } => vec![type_],
            _ => Vec::new(),
        }
    }
}
