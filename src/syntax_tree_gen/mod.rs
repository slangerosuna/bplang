pub use crate::lexer::*;

pub fn gen_ast(
    toks: Vec<Token>,
) -> AbstractSyntaxTree {
    AbstractSyntaxTree { nodes: Generator { toks, i: 0 }.collect() }
}

struct Generator {
    toks: Vec<Token>,
    i: usize,
}

impl Iterator for Generator {
    type Item = Node;

    fn next(&mut self) -> Option<Self::Item> {
        next_node(&self.toks, &mut self.i)
    }
}

fn next_node(
    toks: &Vec<Token>,
    i: &mut usize,
) -> Option<Node> {
    if *i >= toks.len() { return None; }

    let tok = &toks[*i];

    match tok {
        Token::Ident(ident) => node_from_ident(toks, i, ident),
        Token::Keyword(kw) => node_from_keyword(toks, i, kw),
        Token::Operator(op) => node_from_operator(toks, i, op),
        Token::Delimiter(del) => node_from_delimiter(toks, i, del),
        Token::Literal(lit) => node_from_literal(toks, i, lit),

        _ => panic!("Unexpected token: {:?}", tok),
    }
}

fn node_from_ident(
    toks: &Vec<Token>,
    i: &mut usize,
    ident: &String,
) -> Option<Node> {
    panic!("Not implemented");
}

fn node_from_keyword (
    toks: &Vec<Token>,
    i: &mut usize,
    kw: &Keyword,
) -> Option<Node> {
    match kw {
        Keyword::FN => get_fn_node(toks, i),
        Keyword::Return => {
            *i += 1;
            Some(Node::Statement(Statement::Return {
                return_val: Box::new(match next_node(toks, i).unwrap() { Node::Expr(expr) => expr, _ => panic!("expected expression")}),
            }))
        },
        //TODO: handle other keywords

        _ => panic!("Unexpected keyword: {:?}", kw),
    }
}

fn get_fn_node(
    toks: &Vec<Token>,
    i: &mut usize,
) -> Option<Node> {
    *i += 1;
    let ident = match &toks[*i] {
        Token::Ident(ident) => ident,
        _ => panic!("Expected identifier after fn keyword"),
    };

    *i += 1;
    let args = get_fn_args(toks, i);
    *i += 1;

    let return_type = match &toks[*i] {
        Token::Delimiter(del) => {
            match del {
                Delimiter::Colon => {
                    *i += 1;
                    get_type(toks, i)
                },
                _ => Type::Infer,
            }
        },
        _ => Type::Infer,
    };

    *i += 1;

    let scope = Box::new(get_scope(toks, i));

    Some(Node::Statement(Statement::Function {
        ident: ident.clone(),
        args,
        return_type,
        scope,
    }))
}

fn get_fn_args(
    toks: &Vec<Token>,
    i: &mut usize,
) -> Vec<(String, Type)> {
    assert_eq!(toks[*i], Token::Delimiter(Delimiter::OpenParen),
               "Expected open paren after fn identifier");

    let mut v = Vec::new();

    loop {
        match get_next_arg(toks, i) {
            Ok((ident, type_)) => {
                v.push((ident, type_));
            },
            Err(is_comma) => {
                if is_comma { continue; } //this was a comma, so there's another arg
                else { break; } //this was a close paren, so we're done
            },
        }
    }

    v
}

fn get_next_arg(
    toks: &Vec<Token>,
    i: &mut usize,
) -> Result<(String, Type), bool> {
    *i += 1;
    match &toks[*i] {
        Token::Delimiter(del) => {
            match del {
                Delimiter::CloseParen => Err(false),
                Delimiter::Comma => Err(true),

                _ => panic!("Expected parentheses or comma"),
            }
        },
        Token::Ident(ident) => {
            *i += 1;
            match &toks[*i] {
                Token::Delimiter(del) => {
                    match del {
                        Delimiter::Colon => {
                            *i += 1;
                            Ok((ident.clone(), get_type(toks, i)))
                        },
                        Delimiter::Comma => {
                            Ok((ident.clone(), Type::Infer))
                        },
                        _ => panic!("Expected parentheses or comma"),
                    }
                },
                _ => panic!("Expected identifier"),
            }

        },
        _ => panic!("Expected identifier"),
    }
}

fn get_type(
    toks: &Vec<Token>,
    i: &mut usize,
) -> Type {
    let res = match &toks[*i] {
        Token::Ident(ident) => Type::Ident(ident.clone()),
        Token::Primitive(type_) => Type::Primitive(type_.clone()),
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
        Token::Keyword(kw) => match kw {
            Keyword::FN => get_fn_type(toks, i),
            Keyword::Struct => Type::Struct {
                fields: get_names_and_types(toks, i)
                        .into_iter().map(|(name, type_)|
                            (name, type_.unwrap_or(Type::Infer)))
                        .collect()
            },
            Keyword::Union => Type::Union {
                variants: get_names_and_types(toks, i)
                        .into_iter().map(|(name, type_)|
                            (name, type_.unwrap_or(Type::Infer)))
                        .collect()
            },
            Keyword::Enum => Type::Enum {
                variants: get_names_and_types(toks, i),
            },
            _ => panic!("Expected type, found keyword: {:?}", kw),
        },

        _ => panic!("Expected type, found: {:?}", toks[*i]),
    };

    match &toks[*i + 1] {
        Token::Delimiter(Delimiter::Bang) => {
            *i += 1;
            Type::Result{
                err: Some(Box::new(res)),
                ok: Box::new(get_type(toks, i)),
            }
        },
        Token::Delimiter(Delimiter::Pipe) => {
            *i += 2;

            let next_type = get_type(toks, i);
            match next_type {
                Type::AnonSum { variants, .. } => Type::AnonSum {
                    variants: { let mut v = vec![res]; v.extend(variants); v }
                },
                _ => Type::AnonSum {
                    variants: vec![res, next_type],
                },
            }
        },
        Token::Operator(Operator::Ampersand) => {
            *i += 2;

            let next_type = get_type(toks, i);
            match next_type {
                Type::AnonProduct { fields, .. } => Type::AnonProduct {
                    fields: { let mut v = vec![res]; v.extend(fields); v }
                },
                _ => Type::AnonProduct {
                    fields: vec![res, next_type],
                },
            }
        },
        _ => res,
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
    if v.len() == 1 { return v[0].clone(); }

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
        Token::Delimiter(del) => {
            match del {
                Delimiter::Colon => {
                    get_type(toks, i)
                },
                _ => Type::Infer,
            }
        },
        _ => Type::Infer,
    };
    let return_type = Box::new(return_type);

    Type::FunctionPointer {
        args,
        return_type,
    }
}

fn get_statement(
    toks: &Vec<Token>,
    i: &mut usize,
) -> Option<Statement> {
    let node = next_node(toks, i)?;
    match node {
        Node::Statement(stmnt) => Some(stmnt),
        _ => None,
    }
}

fn get_scope(
    toks: &Vec<Token>,
    i: &mut usize,
) -> Scope {
    match &toks[*i] {
        Token::Delimiter(Delimiter::OpenCurly) => {
            *i += 1;
            let mut nodes = Vec::new();
            let mut defered_statements = Vec::new();

            loop {
                match &toks[*i] {
                    Token::Delimiter(Delimiter::CloseCurly) => { *i += 1; break; },
                    _ => {
                        match &toks[*i] {
                            Token::Keyword(kw) => {
                                match kw {
                                    Keyword::Defer => { *i += 1; defered_statements.push((nodes.len() - 1,get_statement(toks, i).unwrap()));},
                                    _ => nodes.push(node_from_keyword(toks, i, kw).unwrap()),
                                }
                            },
                            Token::Ident(ident) => {
                                if let Some(node) = node_from_ident(toks, i, ident) {
                                    nodes.push(node);
                                }
                            },
                            Token::Operator(op) => {
                                if let Some(node) = node_from_operator(toks, i, op) {
                                    nodes.push(node);
                                }
                            },
                            Token::Delimiter(del) => {
                                if let Some(node) = node_from_delimiter(toks, i, del) {
                                    nodes.push(node);
                                }
                            },
                            Token::Literal(lit) => {
                                if let Some(node) = node_from_literal(toks, i, lit) {
                                    nodes.push(node);
                                }
                            },
                            _ => panic!("Unexpected token: {:?}", toks[*i]),
                        }
                    },
                }
            }


            let type_ = if let Some(type_) = nodes[nodes.len() - 1].type_() { type_.clone() } else { Type::Infer };
            Scope {
                nodes,
                defered_statements,
                type_
            }
        },
        _ => panic!("Expected open curly"),
    }
}

fn node_from_operator(
    toks: &Vec<Token>,
    i: &mut usize,
    op: &Operator,
) -> Option<Node> {
    *i += 1;

    match op {
        Operator::Asterisk => { //pointer dereference
            panic!("Not implemented"); //TODO
        },
        Operator::Ampersand => { //address-of
            panic!("Not implemented"); //TODO
        },
        Operator::Not => { //not
            panic!("Not implemented"); //TODO
        }, //TODO: Verify that there are no other unary operators
        _ => panic!("Unexpected operator: {:?}", op),
    }
}

fn node_from_delimiter(
    toks: &Vec<Token>,
    i: &mut usize,
    del: &Delimiter,
) -> Option<Node> {
    match del {
        Delimiter::OpenCurly => Some(Node::Scope(get_scope(toks, i))),
        Delimiter::Semicolon => { *i += 1; None },
        _ => panic!("Unexpected delimiter: {:?}", del),
    }
}

#[inline]
fn node_from_literal(
    toks: &Vec<Token>,
    i: &mut usize,
    lit: &Literal,
) -> Option<Node> {
    *i += 1;
    Some(Node::Expr(Expr::Literal(lit.clone())))
}

#[derive(Debug, Clone)]
pub struct AbstractSyntaxTree {
    pub nodes: Vec<Node>,
}

//TODO add lifetimes and mutability as part of types
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Infer, //The type needs to be inferred by the semantic analyzer
    Ident(String),
    Primitive(Primitive),
    Array {
        type_: Box<Type>,
        size: Box<Expr>,
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
    AnonProduct {
        fields: Vec<Type>,
    },
    Vector {
        type_: Box<Type>,
        size: Box<Expr>,
    },
    Matrix {
        type_: Box<Type>,
        size: Box<Expr>,
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
    Type {
        type_: Box<Type>,
    },
    Result {
        err: Option<Box<Type>>,
        ok: Box<Type>,
    },
    Optional {
        type_: Box<Type>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Ident(String),
    Literal(Literal),
    Operation {
        op: Operation,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    Call {
        ident: String,
        args: Vec<Expr>,
    },
    Index {
        ident: String,
        index: Box<Expr>,
    },
    Member {
        ident: String,
        member: Box<Expr>,
    },
    SizeOf {
        expr: Box<Expr>,
    },
    Cast {
        expr: Box<Expr>,
        type_: Type,
    },
    Type {
        type_: Type,
    },
    Array {
        type_: Type,
        size: Box<Expr>,
    },
    Tuple {
        exprs: Vec<Expr>,
    },
    Struct {
        //TODO
    },
    Union {
        //TODO
    },
    VectorLiteral { //formatted: |x, y, z|
        exprs: Vec<Expr>,
    },
    MatrixLiteral { //formatted: |00, 01, 02,, //type annotation is optional
                    //            10, 11, 12,,
                    //            20, 21, 22|
        exprs: Vec<Expr>,
    },
    SliceLiteral { //formatted: [0, 1, 2, 3, 4, 5]
        exprs: Vec<Expr>,
    },
    TupleLiteral { //formatted: (0, 1: u32, 2, 3, 4, 5: i32) type annotation is optional
        exprs: Vec<Expr>,
    },
    StructLiteral { //formatted: {x = 0, y: i32 = 1, z: i32 = 2} type annotation is optional
        //TODO
    },
    UnionLiteral { //formatted: union {x: u32 = 0, y: f32, z: i32} type annotation is required
        //TODO
    },
    Match {
        expr: Box<Expr>,
        arms: Vec<(Expr /*what value to go down the arm*/, Scope)>,
    },
}

impl Literal {
    fn type_(&self) -> Type {
        match self {
            Literal::Number(_) => Type::Infer,
            Literal::TypedNumber(_, t) => Type::Primitive(t.clone()),
            Literal::Str(_) => Type::Primitive(Primitive::Str),
            Literal::Char(_) => Type::Primitive(Primitive::Char),
            Literal::Bool(_) => Type::Primitive(Primitive::Bool),
        }
    }
}

impl Expr {
    fn type_(&self) -> Type {
        match self {
            Expr::Ident(_) => Type::Infer,
            Expr::Literal(lit) => lit.type_(),
            Expr::Operation { op, lhs, rhs } => {
                let lhs_type = lhs.type_();
                let rhs_type = rhs.type_();

                match op {
                    Operation::Add | Operation::Sub | Operation::Mul | Operation::Div | Operation::Mod => {
                        if lhs_type == Type::Primitive(Primitive::Str) {
                            if rhs_type == Type::Primitive(Primitive::Str) {
                                return Type::Primitive(Primitive::Str);
                            }
                        }
                    },
                    Operation::BitAnd | Operation::BitOr | Operation::BitXor | Operation::BitNot | Operation::BitShiftLeft | Operation::BitShiftRight => {
                        if lhs_type == Type::Primitive(Primitive::Bool) {
                            if rhs_type == Type::Primitive(Primitive::Bool) {
                                return Type::Primitive(Primitive::Bool);
                            }
                        }
                    },
                    Operation::And | Operation::Or | Operation::Not => {
                        if lhs_type == Type::Primitive(Primitive::Bool) {
                            if rhs_type == Type::Primitive(Primitive::Bool) {
                                return Type::Primitive(Primitive::Bool);
                            }
                        }
                    },
                    Operation::Equal | Operation::NotEqual | Operation::LessThan | Operation::GreaterThan | Operation::LessThanOrEqual | Operation::GreaterThanOrEqual => {
                        if lhs_type == rhs_type {
                            return Type::Primitive(Primitive::Bool);
                        }
                    },
                }

                Type::Infer
            },
            Expr::Call { ident, args } => {
                //TODO get function's return type
                Type::Infer
            },
            Expr::Index { ident, index } => {
                //TODO get ident's type
                Type::Infer
            },
            Expr::Member { ident, member } => {
                //TODO get ident's type
                Type::Infer
            },
            Expr::SizeOf { expr } => {
                Type::Primitive(Primitive::USIZE)
            },
            Expr::Cast { expr, type_ } => {
                type_.clone()
            },
            Expr::Type { type_ } => {
                Type::Type { type_: Box::new(type_.clone()) }
            },
            Expr::Array { type_, size } => {
                Type::Array {
                    type_: Box::new(type_.clone()),
                    size: size.clone(),
                }
            },
            Expr::Tuple { exprs } => {
                Type::Tuple {
                    types: exprs.iter().map(|expr| expr.type_()).collect(),
                }
            },
            Expr::Struct { .. } => {
                //TODO
                Type::Infer
            },
            Expr::Union { .. } => {
                //TODO
                Type::Infer
            },
            Expr::VectorLiteral { exprs } => {
                Type::Vector {
                    type_: Box::new(exprs[0].type_()),
                    size: Box::new(Expr::Literal(Literal::Number(exprs.len() as f64))),
                }
            },
            Expr::MatrixLiteral { exprs } => {
                let size = (exprs.len() as f64).sqrt() as i64;
                Type::Matrix {
                    type_: Box::new(exprs[0].type_()),
                    size: Box::new(Expr::Literal(Literal::Number(size as f64))),
                }
            },
            Expr::SliceLiteral { exprs } => {
                Type::Slice {
                    type_: Box::new(exprs[0].type_()),
                }
            },
            Expr::TupleLiteral { exprs } => {
                Type::Tuple {
                    types: exprs.iter().map(|expr| expr.type_()).collect(),
                }
            },
            Expr::StructLiteral { .. } => {
                //TODO
                Type::Infer
            },
            Expr::UnionLiteral { .. } => {
                //TODO
                Type::Infer
            },
            Expr::Match { expr, arms } => {
                Type::AnonSum {
                    variants: arms.iter().map(|(expr, _)| expr.type_()).collect(),
                }
            },
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Operation { //integer values are the precedence (last digit is ignored, used for discrimination)
    Add = 200,
    Sub = 201,
    Mul = 400,
    Div = 401,
    Mod = 402,
    BitAnd = 100,
    BitOr = 50,
    BitXor = 51,
    BitNot = 1000,
    BitShiftLeft = 800,
    BitShiftRight = 801,
    And = 101,
    Or = 52,
    Not = 1001,
    Equal = 500,
    NotEqual = 501,
    LessThan = 502,
    GreaterThan = 503,
    LessThanOrEqual = 504,
    GreaterThanOrEqual = 505,
}

impl Operation {
    #[inline]
    fn get_precedence(self) -> isize { (self as isize) / 10 }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Assert {
        expr: Box<Expr>,
    },

    //Assignments
    Assign {
        ident: String,
        expr: Box<Expr>,
    },
    AssignWithOp {
        ident: String,
        op: Operation,
        expr: Box<Expr>,
    },

    //Declarations
    Var {
        ident: String,
        expr: Box<Expr>,
    },
    Const {
        ident: String,
        expr: Box<Expr>,
    },
    Function {
        ident: String,
        args: Vec<(String, Type)>,
        return_type: Type,
        scope: Box<Scope>,
    },
    ExternFunction {
        ident: String,
        args: Vec<Type>,
        return_type: Type,
    },
    ExternVar {
        ident: String,
        expr: Box<Expr>,
    },
    ExternConst {
        ident: String,
        expr: Box<Expr>,
    },
    ExternType {
        ident: String,
    },
    Struct {
        //TODO
    },
    Union {
        //TODO
    },

    //Control Flow
    Loop {
        //looks like:
        //loop { break A; break B; };
        //evaluates to sum type of all break statements, ie. A | B
        scope: Box<Scope>,
    },
    While {
        //looks like:
        //while condition {...};
        //evaluates to ()
        scope: Box<Scope>,
        condition: Box<Expr>,
    },
    If {
        //looks like:
        //if condition {A} else {B};
        //evaluates to A | B, and if A and B are the same type, A | B is simplified to A
        //or
        //if condition {A};
        //evaluates to ?A
        scope: Box<Scope>,
        condition: Box<Expr>,
        else_scope: Option<Scope>,
    },
    CStyleFor {
        //TODO: add support for multiple initializers and multiple iterators
        //
        //looks like:
        //for var i = 0; i < 10; i += 1 {...}
        //evaluates to ()
        scope: Box<Scope>,
        declarer: Box<Statement>,
        condition: Box<Expr>,
        iterator: Box<Statement>,
    },
    IteratorFor {
        //looks like:
        //for i in 0..10 {A}
        //evaluates to Iter<A, 10> (works like a map)
        scope: Box<Scope>,
        iterator: Box<Expr>,
        varident: String,
    },
    Panic {
        // looks like:
        // panic "message";
        message: Box<Expr>,
    },
    Break {
        // looks like: break; or break A;
        label: Option<String>,
        return_val: Option<Box<Expr>>,
    },
    Continue, // looks like: continue;
    Return {
        return_val: Box<Expr>,
    },
    UnevaluatedExpr {
        expr: Box<Expr>,
    },
}

impl Statement {
    fn type_(&self) -> Option<Type> {
        match self {
            Statement::Assert { .. } => None,
            Statement::Assign { .. } => None, //TODO: return assigned variable's type
            Statement::AssignWithOp { .. } => None, //TODO: return assigned variable's type
            Statement::Var { .. } => None,
            Statement::Const { .. } => None,
            Statement::Function { .. } => None, //TODO: return function's pointer type
            Statement::ExternFunction { .. } => None, //TODO: return function pointer type
            Statement::ExternVar { .. } => None,
            Statement::ExternConst { .. } => None,
            Statement::ExternType { .. } => None, //TODO: return type's type
            Statement::Struct { .. } => None, //TODO: return struct's type
            Statement::Union { .. } => None, //TODO: return union's type
            Statement::Loop { .. } => None, //TODO: return sum type of all break statements
            Statement::While { .. } => None,
            Statement::If { .. } => None, //TODO: return sum type of both scopes or optional of only scope
            Statement::CStyleFor { .. } => None,
            Statement::IteratorFor { .. } => None, //TODO: return Iter type
            Statement::Panic { .. } => None,
            Statement::Break { .. } => None,
            Statement::Continue => None,
            Statement::Return { .. } => None,
            Statement::UnevaluatedExpr { .. } => None, //TODO: return expr's type
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Scope {
    nodes: Vec<Node>,
    defered_statements: Vec<(usize /*index of node where it's defered*/, Statement)>,
    type_: Type, // the type the scope evaluates to
}

#[derive(Debug, Clone, PartialEq)]
pub enum Node {
    Statement(Statement),
    Expr(Expr),
    Scope(Scope),
}

impl Node {
    fn type_(&self) -> Option<Type> {
        match self {
            Node::Statement(stmnt) => stmnt.type_(),
            Node::Expr(expr) => Some(expr.type_()),
            Node::Scope(scope) => Some(scope.type_.clone()),
        }
    }
}
