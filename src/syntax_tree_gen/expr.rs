pub use super::*;

#[derive(Debug, Clone)]
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
        size: Option<Box<Literal>>,
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
    IteratorFor {
        //looks like:
        //for i in -1..10 {A}
        //evaluates to Iter<A, 9> (works like a map)
        scope: Box<Scope>,
        iterator: Box<Expr>,
        varident: String,
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
    Loop {
        //looks like:
        //loop { break A; break B; };
        //evaluates to sum type of all break statements, ie. A | B
        scope: Box<Scope>,
    },
}

impl Expr {
    pub fn type_(&self) -> Type {
        match self {
            Expr::Ident(_) => Type::Infer,
            Expr::Literal(lit) => lit.type_(),
            Expr::Operation { op, lhs, rhs } => {
                let lhs_type = lhs.type_();
                let rhs_type = rhs.type_();

                //TODO

                Type::Infer
            },
            Expr::Call { ident: _, args: _ } => todo!(),
            Expr::Index { ident: _, index: _ } => todo!(),
            Expr::Member { ident: _, member: _ } => todo!(),
            Expr::SizeOf { expr: _ } => Type::Primitive(Primitive::USIZE),
            Expr::Cast { expr: _, type_ } => type_.clone(),
            Expr::Type {..} => Type::Type,
            Expr::Array { type_, size } => Type::Array {
                type_: Box::new(type_.clone()),
                size: size.clone(),
            },
            Expr::Tuple { exprs } => Type::Tuple { types: exprs.iter().map(|expr| expr.type_()).collect(), },
            Expr::Struct { .. } => todo!(),
            Expr::Union { .. } => todo!(),
            Expr::VectorLiteral { exprs } => Type::Vector {
                type_: Box::new(exprs[0].type_()),
                size: Box::new(Literal::Number(exprs.len() as f64)),
            },
            Expr::MatrixLiteral { exprs } => {
                let size = (exprs.len() as f64).sqrt() as i64;
                Type::Matrix {
                    type_: Box::new(exprs[0].type_()),
                    size: Box::new(Literal::Number(size as f64)),
                }
            },
            Expr::SliceLiteral { exprs } => Type::Slice { type_: Box::new(exprs[0].type_()), },
            Expr::TupleLiteral { exprs } => Type::Tuple { types: exprs.iter().map(|expr| expr.type_()).collect(), },
            Expr::StructLiteral { .. } => todo!(),
            Expr::UnionLiteral { .. } => todo!(),
            Expr::Match { expr: _, arms } => Type::AnonSum { variants: arms.iter().map(|(expr, _)| expr.type_()).collect(), },
            Expr::IteratorFor { scope, ..} => scope.type_.clone(),
            Expr::If { scope, else_scope, .. } => {
                if let Some(else_scope) = else_scope {
                    if scope.type_ == else_scope.type_ { scope.type_.clone() }
                    else {Type::AnonSum {
                        variants: vec![scope.type_.clone(), else_scope.type_.clone()],
                    }}
                } else { return Type::Optional { type_: Box::new(scope.type_.clone()) }; }
            },
            Expr::Loop { scope: _ } => todo!(),
        }
    }

    pub fn get_mut_scopes<'a>(&'a mut self) -> Vec<&'a mut Scope> {
        match self {
            Expr::If { scope, else_scope, .. } => {
                if let Some(else_scope) = else_scope {
                    vec![scope, else_scope]
                } else {
                    vec![scope]
                }
            },
            Expr::Loop { scope } => vec![scope],
            Expr::Match { arms, .. } => arms.iter_mut().map(|(_, scope)| scope).collect(),
            Expr::IteratorFor { scope, .. } => vec![scope],

            _ => vec![],
        }
    }
}
