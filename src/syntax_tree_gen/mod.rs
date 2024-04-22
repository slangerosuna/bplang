// does many things to satisfy the borrow checker that are not efficient
// in the future, this will be implemented in bplang
// in said implementation, the code will be refactored to not require these inefficient workarounds
use std::collections::HashMap;

pub use crate::lexer::*;

mod type_;
mod declaration;
mod expr;
mod statement;

pub use self::{
    type_::*,
    declaration::*,
    expr::*,
    statement::*,
};


struct Queue<T> {
    queue: Vec<T>,
    pointer: usize,
}

impl<T> Queue<T> {
    fn new() -> Self {
        Self {
            queue: Vec::new(),
            pointer: 0,
        }
    }

    fn queue(&mut self, item: T) {
        self.queue.push(item);
    }

    fn dequeue<'a>(&'a mut self) -> Option<&'a T> {
        if self.pointer < self.queue.len() {
            let item = &self.queue[self.pointer];
            self.pointer += 1;
            Some(item)
        } else {
            None
        }
    }

    fn map_some_remaining(
        &mut self,
        f: impl Fn(&T) -> Option<T>,
    ) {
        for i in self.pointer..self.queue.len() {
            if let Some(new_item) = f(&self.queue[i])
              { self.queue[i] = new_item; }
        }
    }

    fn new_from_map_remaining(
        &self,
        f: impl Fn(&T) -> T,
    ) -> Self {
        Queue {
            queue: self.queue[self.pointer..].iter().map(f).collect(),
            pointer: 0,
        }
    }
}

#[derive(Debug, Clone)]
pub struct AbstractSyntaxTree {
    pub declr: HashMap<String, Declaration>,
    pub nodes: Vec<Node>,
}
pub fn gen_ast(
    toks: Vec<Token>,
) -> AbstractSyntaxTree {
    let mut declr = HashMap::new();
    AbstractSyntaxTree { nodes: Generator { declr: &mut declr, toks, i: 0 }.collect(), declr }
}

struct Generator<'a> {
    declr: &'a mut HashMap<String, Declaration>,
    toks: Vec<Token>,
    i: usize,
}

impl Iterator for Generator<'_> {
    type Item = Node;

    fn next(&mut self) -> Option<Self::Item> {
        next_node(&self.toks, &mut self.i, self.declr)
    }
}

fn next_node(
    toks: &Vec<Token>,
    i: &mut usize,
    declr: &mut HashMap<String, Declaration>,
) -> Option<Node> {
    if *i >= toks.len() { return None; }

    let tok = &toks[*i];

    match tok {
        Token::Ident(ident) => node_from_ident(toks, i, declr, ident),
        Token::Keyword(kw) => node_from_keyword(toks, i, declr, kw),
        Token::Operator(op) => node_from_operator(toks, i, op),
        Token::Delimiter(del) => node_from_delimiter(toks, i, del),
        Token::Literal(lit) => node_from_literal(toks, i, lit),

        _ => panic!("Unexpected token: {:?}", tok),
    }
}

fn node_from_ident(
    toks: &Vec<Token>,
    i: &mut usize,
    declr: &mut HashMap<String, Declaration>,
    ident: &String,
) -> Option<Node> {
    *i += 1;
    match toks[*i] {
        Token::Delimiter(Delimiter::DoubleColon) => {
            *i += 1;
            match toks[*i] {
                Token::Keyword(Keyword::Struct) => todo!(),
                Token::Keyword(Keyword::Union) => todo!(),
                Token::Keyword(Keyword::Enum) => todo!(),
                Token::Keyword(Keyword::Interface) => todo!(),
                Token::Keyword(Keyword::LDCompShader) => todo!(),
                Token::Keyword(Keyword::LDGeomShader) => todo!(),
                Token::Keyword(Keyword::LDVertShader) => todo!(),
                Token::Keyword(Keyword::LDFragShader) => todo!(),
                Token::Keyword(Keyword::LDTescShader) => todo!(),
                Token::Keyword(Keyword::LDTeseShader) => todo!(),
                Token::Keyword(Keyword::LDPostProcShader) => todo!(),
                //TODO: handle compile-time args when there's `(args) =>` rather than `(args) -> return_type`
                Token::Delimiter(Delimiter::OpenParen) => get_fn_node(toks, i, declr, ident.to_owned()),

                _ => panic!("Expected identifier after double colon"),
            }
        },
        Token::Operator(Operator::Assignment) => todo!(),
        Token::Operator(Operator::Equal) => todo!(),
        Token::Operator(Operator::FullEqual) => todo!(),
        Token::Operator(Operator::NotEqual) => todo!(),
        Token::Operator(Operator::NotFullEqual) => todo!(),
        Token::Operator(Operator::LessThan) => todo!(),
        Token::Operator(Operator::GreaterThan) => todo!(),
        Token::Operator(Operator::LessThanOrEqual) => todo!(),
        Token::Operator(Operator::GreaterThanOrEqual) => todo!(),
        Token::Operator(Operator::Plus) => todo!(),
        Token::Operator(Operator::Minus) => todo!(),
        Token::Operator(Operator::Asterisk) => todo!(),
        Token::Operator(Operator::Divide) => todo!(),
        Token::Operator(Operator::Modulo) => todo!(),
        Token::Operator(Operator::And) => todo!(),
        Token::Operator(Operator::Or) => todo!(),
        Token::Operator(Operator::Xor) => todo!(),
        Token::Operator(Operator::Caret) => todo!(),
        Token::Operator(Operator::ThinArrow) => todo!(),
        _ => Some(Node::Expr(Expr::Ident(ident.clone()))),
    }
}

fn handle_return_keyword(
    toks: &Vec<Token>,
    i: &mut usize,
    declr: &mut HashMap<String, Declaration>,
) -> Option<Node> {
    *i += 1;
    Some(Node::Statement(Statement::Return {
        return_val: Some(Box::new(match next_node(toks, i, declr).unwrap() { Node::Expr(expr) => expr, _ => panic!("expected expression after `return`")})),
    }))
}

fn node_from_keyword(toks: &Vec<Token>, i: &mut usize, declr: &mut HashMap<String, Declaration>, kw: &Keyword) -> Option<Node> {
    match kw {
        Keyword::Return => handle_return_keyword(toks, i, declr),
        //TODO: handle other keywords
        _ => panic!("Unexpected keyword: {:?}", kw),
    }
}

fn get_fn_node_args(toks: &Vec<Token>, i: &mut usize) -> Vec<(String, Type)> {
    *i += 1;
    get_fn_args(toks, i)
}

fn get_fn_node_return_type(toks: &Vec<Token>, i: &mut usize) -> Type {
    *i += 1;
    match &toks[*i] {
        Token::Operator(Operator::ThinArrow) => { *i += 1; get_type(toks, i) },
        _ => Type::Infer,
    }
}

fn get_fn_node_scope(toks: &Vec<Token>, i: &mut usize) -> Box<Scope> {
    *i += 1;
    Box::new(get_scope(toks, i))
}

fn get_fn_node(
    toks: &Vec<Token>,
    i: &mut usize,
    declr: &mut HashMap<String, Declaration>,
    ident: String
) -> Option<Node> {
    let args = get_fn_node_args(toks, i);
    let return_type = get_fn_node_return_type(toks, i);
    let scope = get_fn_node_scope(toks, i);

    let function_declaration = Declaration::Function(FunctionDeclaration {
        ident: ident.clone(),
        args,
        return_type,
        scope,
    });

    declr.insert(ident, function_declaration.clone());

    Some(Node::Statement(Statement::Declaration(function_declaration)))
}

pub(self) fn get_fn_args(
    toks: &Vec<Token>,
    i: &mut usize,
) -> Vec<(String, Type)> {
    assert_eq!(toks[*i], Token::Delimiter(Delimiter::OpenParen),
               "Expected open paren after fn identifier");

    let mut v = Vec::new();

    loop {
        match get_next_arg(toks, i) {
            GNARes::Arg(ident, type_) => v.push((ident, type_)),
            GNARes::Comma => continue,
            GNARes::CloseParen => break,
        }
    }

    v
}

enum GNARes {
    Arg(String, Type),
    Comma,
    CloseParen,
}

fn get_next_arg(
    toks: &Vec<Token>,
    i: &mut usize,
) -> GNARes {
    *i += 1;
    match &toks[*i] {
        Token::Delimiter(Delimiter::CloseParen) => GNARes::CloseParen,
        Token::Delimiter(Delimiter::Comma) => GNARes::Comma,
        Token::Ident(ident) => {
            *i += 1;
            match &toks[*i] {
                Token::Delimiter(Delimiter::Colon) => {
                    *i += 1;
                    GNARes::Arg(ident.clone(), get_type(toks, i))
                },
                Token::Delimiter(Delimiter::Comma) => GNARes::Arg(ident.clone(), Type::Infer),
            _ => panic!("Expected colon or comma following identifier"),
            }

        },
        _ => panic!("Expected identifier, comma, or close paren, found: {:?}", toks[*i]),
    }
}

fn get_statement(
    toks: &Vec<Token>,
    i: &mut usize,
    declr: &mut HashMap<String, Declaration>,
) -> Option<Statement> {
    let node = next_node(toks, i, declr)?;
    match node {
        Node::Statement(stmnt) => Some(stmnt),
        _ => None,
    }
}

fn get_scope(
    toks: &Vec<Token>,
    i: &mut usize,
) -> Scope {
    assert_eq!(toks[*i], Token::Delimiter(Delimiter::OpenCurly),
               "Expected open curly after scope keyword");
    let mut declr = HashMap::new();

    *i += 1;

    let nodes = get_scope_nodes(toks, i, &mut declr);
    let type_ = nodes[nodes.len() - 1].type_().clone();

    Scope {
        nodes,
        declr,
        type_,
    }
}

fn get_scope_nodes(
    toks: &Vec<Token>,
    i: &mut usize,
    declr: &mut HashMap<String, Declaration>,
) -> Vec<Node> {
    let mut nodes = Vec::new();
    let mut defered_statements = Queue::new();

    loop {
        match &toks[*i] {
            Token::Delimiter(Delimiter::CloseCurly) => { *i += 1; break; },
            Token::Keyword(Keyword::Defer) => { *i += 1; defered_statements.queue((nodes.len() - 1,get_statement(toks, i, declr).unwrap()));},
            _ => { nodes.push(node_from_keyword(toks, i, declr, &Keyword::Return).unwrap()); },
        }
    }

    apply_defered_statements(&mut nodes, defered_statements);

    nodes
}

fn apply_defered_statements(
    nodes: &mut Vec<Node>,
    mut defered_statements: Queue<(usize, Statement)>,
) {
    while let Some((index, statement)) = defered_statements.dequeue() {
        let mut i = *index;
        // allows for the mutable borrow of defered_statements to be dropped
        let statement = statement.clone();
        while i < nodes.len() {
            match &nodes[i] {
                Node::Statement(Statement::Return { .. }) => nodes.insert(i, Node::Statement(statement.clone() )),
                _ => {
                    for scope in nodes[i].get_mut_scopes() {
                        let defered_statements =
                            // currently cloning the entire remaining queue, this will be fixed when implementing in bplang
                            defered_statements.new_from_map_remaining(|(_, stmnt)| (0, stmnt.clone()));
                        apply_defered_statements(&mut scope.nodes, defered_statements);
                    }
                    i += 1;
                },
            }
        }
    }
}

fn node_from_operator(
    _toks: &Vec<Token>,
    i: &mut usize,
    op: &Operator,
) -> Option<Node> {
    *i += 1;

    match op {
        Operator::Asterisk => todo!(), //pointer dereference
        Operator::Ampersand => todo!(), //address-of
        Operator::Not => todo!(), //not (logical or bitwise depending on type)
        Operator::Minus => todo!(), // negation (ie -1)

        // TODO: add support for all other unary operators
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

fn node_from_literal(
    _toks: &Vec<Token>,
    i: &mut usize,
    lit: &Literal,
) -> Option<Node> {
    *i += 1;
    Some(Node::Expr(Expr::Literal(lit.clone())))
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


#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Operation { //integer values are the precedence (last digit is ignored, used for discrimination)
    Add = 0x200,
    Sub = 0x201,
    Mul = 0x400,
    Div = 0x401,
    Mod = 0x402,
    BitAnd = 0x100,
    BitOr = 0x50,
    BitXor = 0x51,
    BitNot = 0x1000,
    BitShiftLeft = 0x800,
    BitShiftRight = 0x801,
    And = 0x101,
    Or = 0x52,
    Not = 0x1001,
    Equal = 0x500,
    NotEqual = 0x501,
    LessThan = 0x502,
    GreaterThan = 0x503,
    LessThanOrEqual = 0x504,
    GreaterThanOrEqual = 0x505,
}

impl Operation {
    #[inline]
    fn get_precedence(self) -> isize { (self as isize) & 0xFFF0 }
}

#[derive(Debug, Clone)]
pub struct Scope {
    pub nodes: Vec<Node>,
    pub declr: HashMap<String, Declaration>,
    pub type_: Type, // the type the scope evaluates to
}

#[derive(Debug, Clone)]
pub enum Node {
    Statement(Statement),
    Expr(Expr),
    Scope(Scope),
}

impl Node {
    fn type_(&self) -> Type {
        match self {
            Node::Statement(_stmnt) => Type::None,
            Node::Expr(expr) => expr.type_(),
            Node::Scope(scope) => scope.type_.clone(),
        }
    }

    fn get_mut_scopes(&mut self) -> Vec<&mut Scope> {
        match self {
            Node::Statement(stmnt) => stmnt.get_mut_scopes(),
            Node::Expr(expr) => expr.get_mut_scopes(),
            Node::Scope(scope) => vec![scope],
        }
    }
}
