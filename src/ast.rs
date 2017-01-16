//! trashcan's internal representation of abstract syntax trees

/// A trashcan "project" is of course referred to as a dumpster
pub struct Dumpster<'a>(&'a [Module<'a>]);

/// Modules may be ordinary or class modules, and make up a dumpster
pub enum Module<'a> {
    Normal(&'a [Item<'a>]),
    Class(&'a [Item<'a>]),
}

/// Items may be functions, globals, or type definitions
pub enum Item<'a> {
    Function {
        name: Ident<'a>,
        params: &'a [(Ident<'a>, Type<'a>)],
        ret: Option<Type<'a>>,
        body: &'a [Statement<'a>],
    },

    StructDef {
        name: Ident<'a>,
        members: &'a [(Ident<'a>, Type<'a>)],
    },

    EnumDef {
        name: Ident<'a>,
        members: &'a [Ident<'a>],
    },
}

/// Statements are either assignments or...
pub enum Statement<'a> {
    Declaration(Ident<'a>, Type<'a>, Option<Expression<'a>>),
    Assignment(Ident<'a>, Expression<'a>),
}

/// Expressions are...
pub enum Expression<'a> {
    Literal(()),
    Ident(Ident<'a>),
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
    Enum(&'a [Ident<'a>]),
    Array(&'a Type<'a>, ()),
}

/// Identifiers
pub struct Ident<'a>(pub &'a str);

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn array_type() {
        let _ = Type::Array(&Type::Array(&Type::Long, ()), ());
    }

    #[test]
    fn struct_type() {
        let _ = Type::Struct(Ident("my_struct"));
    }

    #[test]
    fn struct_item() {
        let _ = Item::StructDef {
            name: Ident("my_struct"),
            members: &[
                (Ident("my_arr"), Type::Array(&Type::Double, ()))
            ],
        };
    }
}
