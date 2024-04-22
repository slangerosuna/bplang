pub use crate::syntax_tree_gen::*;
pub use crate::lexer::*;

#[derive(Debug, Clone)]
pub enum Declaration {
    Variable(VariableDeclaration),
    Function(FunctionDeclaration),
    Type(TypeDeclaration),
    ExternVariable(ExternVariableDeclaration),
    ExternFunction(ExternFunctionDeclaration),
    ExternType(ExternTypeDeclaration),
}

#[derive(Debug, Clone)]
pub struct VariableDeclaration {
    pub name: String,
    pub type_: Type,
    pub value: Option<Expr>,
}

#[derive(Debug, Clone)]
pub struct FunctionDeclaration {
    pub ident: String,
    pub args: Vec<(String, Type)>,
    pub return_type: Type,
    pub scope: Box<Scope>,
}

#[derive(Debug, Clone)]
pub struct TypeDeclaration {
    pub name: String,
    pub type_: Type,
}

#[derive(Debug, Clone)]
pub struct ExternVariableDeclaration {
    pub name: String,
    pub type_: Type,
}

#[derive(Debug, Clone)]
pub struct ExternFunctionDeclaration {
    pub ident: String,
    pub args: Vec<(String, Type)>,
    pub return_type: Type,
}

#[derive(Debug, Clone)]
pub struct ExternTypeDeclaration {
    pub name: String,
    pub type_: Type,
}
