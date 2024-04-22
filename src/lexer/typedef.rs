#[derive(PartialEq, Clone, Debug)]
pub enum Literal {
    Number(f64),
    TypedNumber(f64, Primitive),
    Str(String),
    Char(char),
    Bool(bool),
}

#[derive(PartialEq, Clone, Debug)]
pub enum Keyword {
    Auto, // auto (inferred type or part of algebraic data type)
    Const, // const (can be evaluated at compile time)
    Pure, // pure (all data is immutable)
    Async, // async (can be awaited)
    Await, // await (waits for async function to finish)
    Assert, // assert (panics if condition is false in debug mode; establishes an invariant in release mode)
    Struct, // struct
    Union, // union
    Enum, // enum
    Dyn, // dyn (dynamic type)
    Interface, // interface (trait)
    Impl, // impl (implement traits, methods, and associated functions)
    FN, // fn
    Extern, // extern (import from other languages)
    Import, // import (import from other files)
    Use, // use (exposes traits, methods, and associated functions)
    If, // if (conditional expression)
    Else, // else (conditional expression)
    Match, // match (pattern matching)
    For, // for (loop through an iterator, or c-style for loop)
    While, // while (loop through a condition)
    Break, // break
    Continue, // continue
    Loop, // loop (infinite loop, equivalent to while(true), exited with break or return)
    Return, // return
    Print, // print
    Println, // println
    PrintErr, // eprint
    PrintlnErr, // eprintln
    Try, // try (try to execute a block of code, catch any errors with catch like C or CPP
         // or return an error/none similar like in Zig)
    Catch, // catch (catch any errors from a try block)
    Panic, // panic (unrecoverable error)
    Unreachable, // unreachable (unreachable code)
    Pub, // pub (public)
    Priv, // priv (private)
    Defer, // defer (execute a block of code after the current scope ends)
    Free, // free (free memory)
    Alloc, // alloc (allocate memory)
    As, // as (type cast)
    In, // in (used for iterating through a collection or for checking if a value is in a collection)
    Null, // null (null pointer)
    LDTaskShader, // task (creates a task shader)
    LDMeshShader, // mesh (creates a mesh shader)
    LDCullShader, // cull (creates a culling shader)
    LDCompShader, // comp (creates a compute shader)
    LDGeomShader, // geom (creates a geometry shader)
    LDVertShader, // vert (creates a vertex shader)
    LDFragShader, // frag (creates a fragment shader)
    LDTescShader, // tesc (creates a tessellation control shader)
    LDTeseShader, // tese (creates a tessellation evaluation shader)
    LDRayGenShader, // rayg (creates a ray generation shader)
    LDIRShader, // inter (creates an intersection shader)
    LDAnyHitShader, // anyhit (creates an any-hit shader)
    LDClosestHitShader, // closesthit (creates a closest-hit shader)
    LDMissShader, // miss (creates a miss shader)
    LDHitGroupShader, // hitgroup (creates a hit group shader)
    LDPostProcShader, // postproc (creates a post-processing shader)
    LDBuffer, // pushbuffer (pushes a buffer to the GPU)
    LDUniform, // setuniform (sets a uniform variable in the shader)
    BindShader, // bind(binds a shader to the GPU)
    UnbindShader, // unbind(unbinds a shader from the GPU)
    InitRenderPipeline, // pipeline (initializes a render pipeline)
    RenderFrame, // renderframe (renders a frame using the selected render pipeline)
}

#[derive(PartialEq, Clone, Debug)]
pub enum Primitive {
    // Integer primitives
    U8, U16, U32, U64, U128,
    I8, I16, I32, I64, I128,
    USIZE, ISIZE,

    // Float primitives
    F16, F32, F64, F128,

    // Other primitives
    Char, Bool, Str,
}

#[derive(PartialEq, Clone, Debug)]
pub enum Operator {
    Assignment, // =
    Declaration, // :=
    Equal, // ==
    FullEqual, // ===
    NotEqual, // ~=
    NotFullEqual, // ~==
    LessThan, // <
    GreaterThan, // >
    LessThanOrEqual, // <=
    GreaterThanOrEqual, // >=
    Plus, // +
    Minus, // -
    Asterisk, // *
    Divide, // /
    Modulo, // %
    Not, // ~ or not
    And, // and
    Or, // or
    Xor, // xor
    Ampersand, // &
    Caret, // ^
    ShiftLeft, // <<
    ShiftRight, // >>
    ThinArrow, // ->
    FatArrow, // =>
}

#[derive(PartialEq, Clone, Debug)]
pub enum Delimiter {
    Bang, // !
    Semicolon, // ;
    OpenCurly, // {
    CloseCurly, // }
    OpenParen, // (
    CloseParen, // )
    OpenBracket, // [
    CloseBracket, // ]
    Pipe, // |
    Dot, // .
    Comma, // ,
    RowSeparator, // ,,
    Colon, // :
    DoubleColon, // ::
    QuestionMark, // ?
}

#[derive(PartialEq, Clone, Debug)]
pub enum Token {
    Ident(String),
    Literal(Literal),
    Primitive(Primitive),
    Keyword(Keyword),
    Operator(Operator),
    Delimiter(Delimiter),
}
