//! trashcan's internal representation of abstract syntax trees

use parser::SrcLoc;

#[derive(Clone)]
pub struct Dumpster {
    pub modules: Vec<Module>,
}

#[derive(Clone)]
pub enum ModuleKind {
    Normal(Vec<NormalItem>),
    // Class(Vec<ClassItem>),
}

#[derive(Clone)]
pub struct Module {
    pub name: Ident,
    pub data: ModuleKind,
    pub loc: SrcLoc,
}

#[derive(Clone)]
pub enum NormalItem {
    Function(FunDef),
}

/// Function definitions
#[derive(Clone)]
pub struct FunDef {
    pub name: Ident,
    pub access: Access,
    pub params: Vec<FunParam>,
    pub ret: Option<Type>,
    pub body: Vec<Stmt>,
    pub loc: SrcLoc,
}

#[derive(Clone)]
pub struct FunParam {
    pub name: Ident,
    pub typ: Type,
    pub mode: ParamMode,
    pub loc: SrcLoc,
}

/// Statements
#[derive(Clone)]
pub struct Stmt {
    pub data: StmtKind,
    pub loc: SrcLoc,
}

#[derive(Clone)]
pub enum StmtKind {
    /// expression-as-statement
    ExprStmt(Expr),

    /// variable declaration(s) with optional initializer(s)
    VarDecl(Vec<(Ident, Type, Option<Expr>)>),

    /// assignment statement (including += et. al)
    Assign(Expr, AssignOp, Expr),

    /// return statement
    Return(Expr),

    /// conditional statement (if, else if, else)
    IfStmt {
        cond: Expr,
        body: Vec<Stmt>,
        elsifs: Vec<(Expr, Vec<Stmt>)>,
        els: Vec<Stmt>,
    },

    /// while loop
    WhileLoop {
        cond: Expr,
        body: Vec<Stmt>,
    },

    /// `print` statement (i.e. Debug.Print)
    Print(Expr),

    /// pass-through literal VB statement
    VbStmt(String),
}

/// Expressions
#[derive(Clone)]
pub struct Expr {
    pub data: ExprKind,
    pub loc: SrcLoc,
}

#[derive(Clone)]
pub enum ExprKind {
    /// A literal
    Literal,

    /// A "named thing"; e.g. `x` or `mod1.y`
    Name(Path),

    /// An indexing expression `e1[e2]`
    Index(Box<Expr>, Box<Expr>),

    /// A function call `f(a1, a2, ...)`
    Call(Box<Path>, Vec<Expr>),

    /// A unary application e.g. `-x`
    UnOpApp(Box<Expr>, UnOp),

    /// A binary application e.g. `x + y`
    BinOpApp(Box<Expr>, Box<Expr>, BinOp),

    // a conditional expression e.g. `x == 2 ? y : z`
    /* for later...
    CondExpr {
        cond: Box<Expr>,
        if_expr: Box<Expr>,
        else_expr: Box<Expr>,
    },
    */
}

/// Module, item, variable, or type identifiers
#[derive(Clone)]
pub struct Ident(pub String);

// TODO: what about x.f().g()? (we need to distinguish path and method-call)
/// A "name path" e.g. a.b.c.d
#[derive(Clone)]
pub struct Path(pub Vec<Ident>);

/// Item access specifiers (private by default)
#[derive(Copy, Clone)]
pub enum Access {
    /// (Module- or class-) private (default)
    Private,
    /// Public (requires `pub` keyword)
    Public,
}

/// Parameter passing modes
#[derive(Clone, Copy)]
pub enum ParamMode {
    /// Pass by value (default)
    ByVal,
    /// Pass by reference (requires `&`)
    ByRef,
}

/// Primitive types of trashcan
#[derive(Clone)]
pub enum Type {
    /// bool
    Bool,
    /// i8
    Int8,
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
    /// named object type
    Object(Ident),
    /// named structure type
    Struct(Ident),
    /// named enum type
    Enum(Ident),
    /// T[] (multidimensional?)
    Array(Box<Type>, Option<u32>),
    // eventually... FnPtr ( args, ret )
}

#[derive(Clone, Copy)]
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

#[derive(Clone, Copy)]
/// Unary operators
pub enum UnOp {
    Negate,
    BitNot,
    LogNot,
}

/// Binary operators
#[derive(Clone, Copy)]
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
