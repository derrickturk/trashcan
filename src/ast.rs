//! trashcan's internal representation of abstract syntax trees

/// A trashcan "project" is of course referred to as a dumpster
struct Dumpster<'a>(&'a [Module]);

/// Modules may be ordinary or class modules, and make up a dumpster
enum Module {
    Normal,
    Class,
}
