//! trashcan's AST rewrite rules, used to implement various language features

use std::sync::atomic::{AtomicUsize, Ordering, ATOMIC_USIZE_INIT};

use super::*;
use ast::*;

static mut GENSYM_ID: AtomicUsize = ATOMIC_USIZE_INIT;

pub fn gensym() -> Ident {
    let num = unsafe { GENSYM_ID.fetch_add(1, Ordering::Relaxed) };
    Ident(format!("ø{}", num))
}

pub trait Substitute {
    fn substitute(self, orig: &Ident, replace: Ident) -> Self;
}

// pub fn substitute(orig: &Ident, replace: &Ident)
