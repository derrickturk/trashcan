//! trashcan's internal representation of abstract syntax trees

/// A trashcan "project" is of course referred to as a dumpster
struct Dumpster<'a>(&'a [Module]);

/// Modules may be ordinary or class modules, and make up a dumpster
enum Module {
    Normal,
    Class,
}

/// Valid types (some placeholder () members for now)
enum Type<'a> {
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
    Object(()),
    Struct(&'a [(Ident<'a>, Type<'a>)]),
    Enum(()),
    Array(&'a Type<'a>, ()),
}

/// Identifiers
struct Ident<'a>(&'a str);

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn array_type() {
        let _ = Type::Array(&Type::Array(&Type::Long, ()), ());
    }

    #[test]
    fn struct_type() {
        let _ = Type::Struct(&[
            (Ident("my_arr"), Type::Array(&Type::Array(&Type::Long, ()), ())),
            (Ident("some_double"), Type::Double),
        ]);
    }
}
