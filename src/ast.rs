//! trashcan's internal representation of abstract syntax trees

use std::fmt;

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

impl Module {
    pub fn filename(&self) -> String {
        let mut name = self.name.0.clone();
        match self.data {
            ModuleKind::Normal(_) => name.push_str(".bas"),
        }
        name
    }
}

/// Items define functions or types, and make up modules
#[derive(Clone, Debug)]
pub enum NormalItem {
    Function(FunDef),
    Struct(StructDef),
    // Enm(EnmDef),
    // Constant(...),
}

/// A function (or "sub") definition
#[derive(Clone, Debug)]
pub struct FunDef {
    pub name: Ident,
    pub access: Access,
    pub params: Vec<FunParam>,
    pub ret: Type,
    pub body: Vec<Stmt>,
    pub loc: SrcLoc,
}

/// An individual function parameter
#[derive(Clone, Debug)]
pub struct FunParam {
    pub name: Ident,
    pub ty: Type,
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

/* TODO: maybe use for-each by-ref to signify local
 *   lvalue rebinding? e.g.
 *   for x: &i32 in xs {
 *       x *= 3;
 *   }
 *   =>
 *   Dim i As Long
 *   For i = LBound(xs) To UBound(xs)
 *       xs(i) = xs(i) * 3
 *   Next i
 */
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

    /// An indexing expression `e1[e2]` or e1[e2;e3;...]
    Index(Box<Expr>, Vec<Expr>),

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

impl Expr {
    pub fn is_lvalue(&self) -> bool {
        match self.data {
            ExprKind::Name(_)
          | ExprKind::Index(_, _)
          | ExprKind::Member(_, _)
          | ExprKind::VbExpr(_) => true,
            _ => false,
        }
    }

    /*
    pub fn might_need_parens(&self) -> {

    }
    */
}

/// A struct type definition
#[derive(Clone, Debug)]
pub struct StructDef {
    pub name: Ident,
    pub access: Access,
    pub members: Vec<StructMem>,
    pub loc: SrcLoc,
}

/// A struct member definition
#[derive(Clone, Debug)]
pub struct StructMem {
    pub name: Ident,
    pub ty: Type,
    pub loc: SrcLoc,
}

/// Module, item, variable, or type identifiers: (name, renamed-from)
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Ident(pub String, pub Option<String>);

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.1.as_ref().unwrap_or(&self.0))?;
        if self.1.is_some() {
            write!(f, " (but it had a past life)")?;
        }
        Ok(())
    }
}

/// A "name path" e.g. module::item or item
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Path(pub Option<Ident>, pub Ident);

impl fmt::Display for Path {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.0 {
            Some(ref module) => write!(f, "{}::{}", module, self.1),
            None => write!(f, "{}", self.1),
        }
    }
}

/// Item access specifiers (private by default)
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
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
#[derive(Clone, Debug, Eq, PartialEq)]
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
    Array(Box<Type>, Vec<(i32, i32)>),
    /// named object type
    Object(Path),
    /// named structure type
    Struct(Path),
    /// named enum type
    Enum(Path),
    /// identifier-as-typename; unknown until symbol table construction
    Deferred(Path),
    /// unit type (only used in function returns)
    Void,
}

impl Type {
    /// does this Type describe an object type?
    /// i.e. do we need to Set assignments with lvalues of this
    /// type; if "maybe" at runtime we return None here
    pub fn is_object(&self) -> Option<bool> {
        match *self {
            Type::Obj | Type::Object(_) => Some(true),
            Type::Variant | Type::Deferred(_) => None,
            _ => Some(false),
        }
    }

    /// is this type a scalar type?
    /// i.e. can we check equality "directly"?
    pub fn is_scalar(&self) -> bool {
        match *self {
            Type::Array(_, _) | Type::Struct(_) | Type::Void => false,
            _ => true,
        }
    }

    /// does this Type describe a numeric type?
    pub fn might_be_numeric(&self) -> bool {
        match *self {
            Type::UInt8
          | Type::Int16
          | Type::Int32
          | Type::IntPtr
          | Type::Float32
          | Type::Float64
          | Type::Variant => true,
            _ => false,
        }
    }

    /// does this Type describe a bitwise type?
    pub fn might_be_bitwise(&self) -> bool {
        match *self {
            Type::UInt8
          | Type::Int16
          | Type::Int32
          | Type::IntPtr
          | Type::Variant => true,
            _ => false,
        }
    }

    /// does this Type describe a string type?
    pub fn might_be_string(&self) -> bool {
        match *self {
            Type::String
          | Type::Variant => true,
            _ => false,
        }
    }

    /// what does this type decay to when passed as a function
    /// argument; we only use this for array types so far
    pub fn decay(&self) -> Type {
        match *self {
            Type::Array(ref base, _) => Type::Array(base.clone(), vec![]),
            ref ty => ty.clone(),
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Type::Bool => write!(f, "bool"),
            Type::UInt8 => write!(f, "u8"),
            Type::Int16 => write!(f, "i16"),
            Type::Int32 => write!(f, "i32"),
            Type::IntPtr => write!(f, "isize"),
            Type::Float32 => write!(f, "f32"),
            Type::Float64 => write!(f, "f64"),
            Type::String => write!(f, "str"),
            Type::Currency => write!(f, "currency"),
            Type::Date => write!(f, "date"),
            Type::Variant => write!(f, "var"),
            Type::Obj => write!(f, "obj"),
            Type::Object(ref path) => write!(f, "{}", path),
            Type::Enum(ref path) => write!(f, "{}", path),
            Type::Struct(ref path) => write!(f, "{}", path),
            Type::Deferred(ref path) => write!(f, "{}", path),
            Type::Array(ref base, ref bounds) => {
                write!(f, "{}[", base)?;
                for (i, &(lb, ub)) in bounds.iter().enumerate() {
                    if i != 0 {
                        write!(f, "; ")?;
                    }
                    write!(f, "{}:{}", lb, ub)?;
                }
                write!(f, "]")?;
                Ok(())
            },
            Type::Void => write!(f, "void"),
        }
    }
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
    IdentEq,
    NotIdentEq,
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
    /// null pointer literal (= VB6 Nothing)
    NullPtr,
    /// null variant literal (= VB6 Null)
    NullVar,
    /// empty variant literal (= VB6 Empty)
    EmptyVar,
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
