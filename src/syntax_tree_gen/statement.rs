use super::*;
#[derive(Debug, Clone)]
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

    Declaration(Declaration),

    //Control Flow
    While {
        //looks like:
        //while condition {...};
        scope: Box<Scope>,
        condition: Box<Expr>,
    },
    CStyleFor {
        //TODO: add support for multiple initializers and multiple iterators
        //
        //looks like:
        //for var i = 0; i < 10; i += 1 {...}
        scope: Box<Scope>,
        declarer: Box<Statement>,
        condition: Box<Expr>,
        iterator: Box<Statement>,
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
    Return { // looks like: return; or return 0;
        return_val: Option<Box<Expr>>,
    },
    UnevaluatedExpr {
        expr: Box<Expr>,
    },
}

impl Statement {
    pub fn get_mut_scopes(&mut self) -> Vec<&mut Scope> {
        match self {
            Statement::While { scope, .. } => vec![scope],
            Statement::CStyleFor { scope, .. } => vec![scope],
            Statement::UnevaluatedExpr { expr } => expr.get_mut_scopes(),
            _ => vec![],
        }
    }
}
