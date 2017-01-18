//! trashcan's internal representation of abstract syntax trees

/// A trashcan "project" is of course referred to as a dumpster
pub struct Dumpster<'a>(&'a [Module<'a>]);

/// Modules may be ordinary or class modules, and make up a dumpster
pub enum Module<'a> {
    Normal(Ident<'a>, &'a [Item<'a>]),
    Class(Ident<'a>, &'a [Item<'a>]),
}

/// Items may be functions, globals, or type definitions
pub enum Item<'a> {
    Function(AccessMode, &'a Function<'a>),
    StructDef(AccessMode, &'a StructDef<'a>),
    EnumDef(AccessMode, &'a EnumDef<'a>),
}

/// A function (or "sub") definition
pub struct Function<'a> {
    pub name: Ident<'a>,
    pub params: &'a [FunctionParameter<'a>],
    pub ret: Option<Type<'a>>,
    pub body: &'a [Statement<'a>],
}

/// A custom structure type definition
pub struct StructDef<'a> {
    pub name: Ident<'a>,
    pub members: &'a [VariableDeclaration<'a>],
}

// TODO: allow specified values for members
/// A custom enum type definition
pub struct EnumDef<'a> {
    pub name: Ident<'a>,
    pub members: &'a [Ident<'a>],
}

/// A individual function parameter
pub struct FunctionParameter<'a> {
    pub name: Ident<'a>,
    pub typ: &'a Type<'a>,
    pub mode: ParamMode,
}

/// A variable declaration binding an identifier with a type
pub struct VariableDeclaration<'a> {
    pub name: Ident<'a>,
    pub typ: &'a Type<'a>,
}

/// Statements are either assignments or...
pub enum Statement<'a> {
    Declaration(&'a VariableDeclaration<'a>, Option<&'a Expression<'a>>),
    Assignment(Ident<'a>, &'a Expression<'a>),
}

/// Expressions are...
pub enum Expression<'a> {
    Literal(Literal),
    Ident(Ident<'a>),
    AddressOf(Ident<'a>),
    Index(Ident<'a>, &'a Expression<'a>),
    UnOpApply(UnOp, &'a Expression<'a>),
    BinOpApply(BinOp, &'a Expression<'a>, &'a Expression<'a>),
    Grouped(&'a Expression<'a>),
}

/// Literals are...
pub enum Literal {
    Bool(bool),
    Int(i32),
    Float(f64),
    Str(String),
    // TODO maybe currency and date?
    Struct(Vec<Literal>),
    Array(Vec<Literal>),
}

#[derive(Clone, Copy)]
/// Built-in unary operators
pub enum UnOp {
    Minus,
    LogNot,
    BitNot,
}

/// Built-in binary operators
#[derive(Clone, Copy)]
pub enum BinOp {
    Dot,
    Add,
    Sub,
    Mul,
    Div,
    Pow,
    StrConcat,
    LogAnd,
    LogOr,
    // these may emit the same code, but I'm not using the same syntax for it
    BitAnd,
    BitOr,
    BitXor,
}

/// Access specifiers
#[derive(Clone, Copy)]
pub enum AccessMode {
    /// (Module- or class-) private
    Private,
    /// Public
    Public,
}

/// Parameter passing modes
#[derive(Clone, Copy)]
pub enum ParamMode {
    /// Pass by value
    ByVal,
    /// Pass by reference
    ByRef,
}

/// Valid types (some placeholder () members for now)
pub enum Type<'a> {
    Boolean,
    Byte,
    Integer,
    Long,
    Single,
    Double,
    String,
    Currency,
    Date,
    Variant,
    Object(Ident<'a>),
    Struct(Ident<'a>),
    Enum(Ident<'a>),
    Array(&'a Type<'a>, Option<u32>),
}

#[derive(Clone, Copy)]
/// Identifiers
pub struct Ident<'a>(pub &'a str);

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn array_type() {
        let _ = Type::Array(&Type::Array(&Type::Long, None), None);
    }

    #[test]
    fn struct_type() {
        let _ = Type::Struct(Ident("my_struct"));
    }

    #[test]
    fn struct_item() {
        let _ = Item::StructDef(
            AccessMode::Public,
            &StructDef {
                name: Ident("my_struct"),
                members: &[
                    VariableDeclaration {
                        name: Ident("my_arr"),
                        typ: &Type::Array(&Type::Double, Some(10)),
                    },
                    VariableDeclaration {
                        name: Ident("my_dbl"),
                        typ: &Type::Double,
                    },
                ],
            },
        );
    }
}
