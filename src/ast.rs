//! trashcan's internal representation of abstract syntax trees

use parser::SrcLoc;

/// A trashcan "project" is of course referred to as a dumpster
#[derive(Clone, Debug)]
pub struct Dumpster {
    pub modules: Vec<Module>,
}

/// Modules may be ordinary or class modules
#[derive(Clone, Debug)]
pub enum ModuleKind {
    Normal(Vec<NormalItem>),
    // Class(Vec<ClassItem>),
}

/// Modules are the basic unit of code organization, and make up a dumpster
#[derive(Clone, Debug)]
pub struct Module {
    pub name: Ident,
    pub data: ModuleKind,
    pub loc: SrcLoc,
}

/// Items define functions or types, and make up modules
#[derive(Clone, Debug)]
pub enum NormalItem {
    Function(FunDef),
    // Struct(StructDef)
    // Enm(EnmDef)
    // Constant(...)
}

/// A function (or "sub") definition
#[derive(Clone, Debug)]
pub struct FunDef {
    pub name: Ident,
    pub access: Access,
    pub params: Vec<FunParam>,
    pub ret: Option<Type>,
    pub body: Vec<Stmt>,
    pub loc: SrcLoc,
}

/// An individual function parameter
#[derive(Clone, Debug)]
pub struct FunParam {
    pub name: Ident,
    pub typ: Type,
    pub mode: ParamMode,
    pub loc: SrcLoc,
}

/// Statements
#[derive(Clone, Debug)]
pub struct Stmt {
    pub data: StmtKind,
    pub loc: SrcLoc,
}

/// Statements are ...
#[derive(Clone, Debug)]
pub enum StmtKind {
    /// expression-as-statement
    ExprStmt(Expr),

    /// variable declaration(s) with optional initializer(s)
    VarDecl(Vec<(Ident, Type, Option<Expr>)>),

    /// assignment statement (including += et. al)
    Assign(Expr, AssignOp, Expr),

    /// return statement
    Return(Option<Expr>),

    /// conditional statement (if, else if, else)
    IfStmt {
        cond: Expr,
        body: Vec<Stmt>,
        elsifs: Vec<(Expr, Vec<Stmt>)>,
        els: Option<Vec<Stmt>>,
    },

    /// while loop
    WhileLoop {
        cond: Expr,
        body: Vec<Stmt>,
    },

    /// while loop
    ForLoop {
        var: (Ident, Type),
        spec: ForSpec,
        body: Vec<Stmt>,
    },

    /// `print` statement (i.e. Debug.Print)
    Print(Expr),
}

/// For loop specs: range (from, to, step) or each (expr)
#[derive(Clone, Debug)]
pub enum ForSpec {
    Range(Expr, Expr, Option<Expr>),
    Each(Expr),
}

/// Expressions
#[derive(Clone, Debug)]
pub struct Expr {
    pub data: ExprKind,
    pub loc: SrcLoc,
}

/// Expressions are...
#[derive(Clone, Debug)]
pub enum ExprKind {
    /// A literal
    Lit(Literal),

    /// A "named thing"; e.g. `x` or `mod1.y`
    Name(Path),

    /// An indexing expression `e1[e2]`
    Index(Box<Expr>, Box<Expr>),

    /// A function call `f(a1, a2, ...)`
    Call(Path, Vec<Expr>),

    /// A member invoke (expr).m
    Member(Box<Expr>, Ident),

    /// A member function invoke (expr).f(args, ...)
    MemberInvoke(Box<Expr>, Ident, Vec<Expr>),

    /// A unary application e.g. `-x`
    UnOpApp(Box<Expr>, UnOp),

    /// A binary application e.g. `x + y`
    BinOpApp(Box<Expr>, Box<Expr>, BinOp),

    /// a conditional expression e.g. `x == 2 ? y : z`
    CondExpr {
        cond: Box<Expr>,
        if_expr: Box<Expr>,
        else_expr: Box<Expr>,
    },

    /// pass-through literal VB expression (raw bytes)
    VbExpr(Vec<u8>),
}

// TODO: maybe fix order of operations with multiple
//   productions here?

/// Module, item, variable, or type identifiers
#[derive(Clone, Debug)]
pub struct Ident(pub String);

/// A "name path" e.g. a.b.c.d
#[derive(Clone, Debug)]
pub struct Path(pub Vec<Ident>);

/// Item access specifiers (private by default)
#[derive(Copy, Clone, Debug)]
pub enum Access {
    /// (Module- or class-) private (default)
    Private,
    /// Public (requires `pub` keyword)
    Public,
}

/// Parameter passing modes
#[derive(Clone, Copy, Debug)]
pub enum ParamMode {
    /// Pass by value (default)
    ByVal,
    /// Pass by reference (requires `&`)
    ByRef,
}

/// Primitive types of trashcan
#[derive(Clone, Debug)]
pub enum Type {
    /// bool
    Bool,
    /// i8
    UInt8,
    /// i16
    Int16,
    /// i32
    Int32,
    /// isize
    IntPtr,
    /// f32
    Float32,
    /// f64
    Float64,
    /// str
    String,
    /// currency
    Currency,
    /// date
    Date,
    /// var
    Variant,
    /// obj (unspecified object type)
    Obj,
    /// T[] (possibly multidimensional)
    Array(Box<Type>, Vec<(i64, i64)>),
    /// named object type
    Object(Ident),
    /// named structure type
    Struct(Ident),
    /// named enum type
    Enum(Ident),
    /// identifier-as-typename; unknown until symbol table construction
    Deferred(Ident),
}

#[derive(Clone, Copy, Debug)]
/// Assignment operators
pub enum AssignOp {
    /// `x = y`
    Assign,
    /// `x += y`
    AddAssign,
    /// `x -= y`
    SubAssign,
    /// `x *= y`
    MulAssign,
    /// `x /= y`
    DivAssign,
    /// `x %= y`
    ModAssign,
    /// `x ^= y`
    PowAssign,
    // for now: later this is just AddAssign on string expressions
    /// `x @= y`
    StrCatAssign,
    /// `x &= y`
    BitAndAssign,
    /// `x |= y`
    BitOrAssign,
    // nah
    // /// `x = y`
    // BitXorAssign,
    /// `x &&= y`
    LogAndAssign,
    /// `x ||= y`
    LogOrAssign,
}

#[derive(Clone, Copy, Debug)]
/// Unary operators
pub enum UnOp {
    Negate,
    BitNot,
    LogNot,
    AddressOf, // -> VarPtr, ObjPtr, AddressOf
}

/// Binary operators
#[derive(Clone, Copy, Debug)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
    StrCat,
    Eq,
    NotEq,
    Lt,
    Gt,
    LtEq,
    GtEq,
    BitAnd,
    BitOr,
    // nah
    // BitXor,
    LogAnd,
    LogOr,
}

/// Literals are...
#[derive(Clone, Debug)]
pub enum Literal {
    /// bool
    Bool(bool),
    /// u8
    UInt8(u8),
    /// i16
    Int16(i16),
    /// i32
    Int32(i32),
    /// isize
    IntPtr(i64),
    /// f32
    Float32(f32),
    /// f64
    Float64(f64),
    /// str
    String(String),
    /// currency
    Currency(i64),
    /// date
    Date(f64),
    // later: array literals & struct literals & ...
}
