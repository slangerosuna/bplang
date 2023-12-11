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
        Keyword::FN => {
            *i += 1;
            let ident = match &toks[*i] {
                Token::Ident(ident) => ident,
                _ => panic!("Expected identifier after fn keyword"),
            };
            
            *i += 1;
            let args = match &toks[*i] {
                Token::Delimiter(del) => {
                    match del {
                        Delimiter::OpenParen => {
                            let mut v = Vec::new();

                            loop {
                                *i += 1;
                                match &toks[*i] {
                                    Token::Delimiter(del) => {
                                        match del {
                                            Delimiter::CloseParen => break,
                                            Delimiter::Comma => continue,

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

                                                        v.push((ident.clone(), get_type(toks, i)));
                                                    },
                                                    Delimiter::Comma => {
                                                        v.push((ident.clone(), Type::Infer));
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

                            v
                        },
                        _ => panic!("Expected parentheses after fn identifier"),
                    }
                },
                _ => panic!("Expected parentheses after fn identifier"),
            };

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
                _ => panic!("Expected colon after fn arguments"),
            };

            *i += 1;

            let scope = get_scope(toks, i);

            Some(Node::Statement(Statement::Function {
                ident: ident.clone(),
                args,
                return_type,
                scope,
            }))
        },
        _ => panic!("Unexpected keyword: {:?}", kw),
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

        _ => panic!("Unexpected delimiter: {:?}", del),
    }
}

pub struct AbstractSyntaxTree {
    nodes: Vec<Node>,
}

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
    VectorLiteral {
        exprs: Vec<Expr>,
    },
    MatrixLiteral {
        exprs: Vec<Expr>,
    },
    SliceLiteral {
        exprs: Vec<Expr>,
    },
    TupleLiteral {
        exprs: Vec<Expr>,
    },
    StructLiteral {
        //TODO
    },
    UnionLiteral {
        //TODO
    },
}

pub enum Operation {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    BitAnd,
    BitOr,
    BitXor,
    BitNot,
    BitShiftLeft,
    BitShiftRight,
    And,
    Or,
    Not,
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
    LessThanOrEqual,
    GreaterThanOrEqual,
}

pub enum Statement {
    Assert {
        expr: Expr,
    },

    //Assignments
    Assign {
        ident: String,
        expr: Expr,
    },
    AssignWithOp {
        ident: String,
        op: Operation,
        expr: Expr,
    },

    //Declarations
    Var {
        ident: String,
        expr: Expr,
    },
    Const {
        ident: String,
        expr: Expr,
    },
    Function {
        ident: String,
        args: Vec<(String, Type)>,
        return_type: Type,
        scope: Scope,
    },
    ExternFunction {
        ident: String,
        args: Vec<Type>,
        return_type: Type,
    },
    ExternVar {
        ident: String,
        expr: Expr,
    },
    ExternConst {
        ident: String,
        expr: Expr,
    },
    ExternType {
        ident: String,
    },


    //Memory structures
    Struct {
        //TODO
    },
    Union {
        //TODO
    },
    
    //Control Flow
    Loop {
        scope: Scope,
    },
    While {
        scope: Scope,
        condition: Expr,
    },
    If {
        scope: Scope,
        condition: Expr,
        else_scope: Option<Scope>,
    },
    CStyleFor {
        scope: Scope,
        declarer: Box<Statement>,
        condition: Expr,
        iterator: Box<Statement>,
    },
    IteratorFor {
        scope: Scope,
        iterator: Expr,
        varident: String,
    },
    Panic {
        message: Expr,
    },
    Break,
    Continue,
    Return,
}

pub struct Scope {
    nodes: Vec<Node>,
    defered_statements: Vec<(usize /*index of node where it's defered*/, Statement)>,
}

pub enum Node {
    Statement(Statement),
    Expr(Expr),
    Scope(Scope),
}
