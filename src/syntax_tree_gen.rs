use crate::lexer::*;

pub fn gen_ast(
    toks: Vec<Token>,
) -> AbstractSyntaxTree {
    let mut nodes = Vec::new();
    let mut i = 0;

    loop {
        if let Some(node) = next_node(&toks, &mut i) {
            nodes.push(node);
        } else {
            break;
        }
    }

    AbstractSyntaxTree {
        nodes: nodes,
    }
}

fn next_node(
    toks: &Vec<Token>,
    i: &mut usize,
) -> Option<Node> {
    if *i >= toks.len() {
        return None;
    }

    let tok = &toks[*i];

    match tok {
        Token::Ident(ident) => node_from_ident(toks, i, ident),
        Token::Keyword(kw) => node_from_keyword(toks, i, kw),
        Token::Operator(op) => node_from_operator(toks, i, op),
        Token::Delimiter(del) => node_from_delimiter(toks, i, del),
        
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
                _ => panic!("Expected colon after fn arguments"),
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
    panic!("Not implemented"); //TODO
}

fn get_scope(
    toks: &Vec<Token>,
    i: &mut usize,
) -> Scope {
    panic!("Not implemented"); //TODO
}

fn node_from_operator(
    toks: &Vec<Token>,
    i: &mut usize,
    op: &Operator,
) -> Option<Node> {
    match op {

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
        _ => panic!("Unexpected delimiter: {:?}", del),
    }
}

#[derive(Debug)]
pub struct AbstractSyntaxTree {
    nodes: Vec<Node>,
}

#[derive(Debug)]
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
        //TODO
    },
    Union {
        //TODO
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
    FunctionPointers {
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
}

#[derive(Debug)]
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

#[derive(Debug)]
pub enum Operation { //integer values are the precedence (last digit is ignored, used for discrination)
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

#[derive(Debug)]
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
        scope: Box<Scope>,
    },
    While {
        scope: Box<Scope>,
        condition: Box<Expr>,
    },
    If {
        scope: Box<Scope>,
        condition: Box<Expr>,
        else_scope: Option<Scope>,
    },
    CStyleFor {
        scope: Box<Scope>,
        declarer: Box<Statement>,
        condition: Box<Expr>,
        iterator: Box<Statement>,
    },
    IteratorFor {
        scope: Box<Scope>,
        iterator: Box<Expr>,
        varident: String,
    },
    Panic {
        message: Box<Expr>,
    },
    Break,
    Continue,
    Return {
        return_val: Box<Expr>,
    },
    UnevaluatedExpr {
        expr: Box<Expr>,
    },
}

#[derive(Debug)]
pub struct Scope {
    nodes: Vec<Node>,
    defered_statements: Vec<(usize /*index of node where it's defered*/, Statement)>,
}

#[derive(Debug)]
pub enum Node {
    Statement(Statement),
    Expr(Expr),
    Scope(Scope),
}
