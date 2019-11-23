use std::convert::AsRef;
use std::ops::{Deref, DerefMut};
use std::hash::{Hash, Hasher};
use std::fmt;
use source_span::Span;

pub struct Located<T> {
    span: Span,
    value: T
}

impl<T> Located<T> {
    pub fn new(t: T, span: Span) -> Located<T> {
        Located {
            span: span,
            value: t
        }
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn into_inner(self) -> T {
        self.value
    }
}

impl<T: Clone> Clone for Located<T> {
    fn clone(&self) -> Located<T> {
        Located {
            span: self.span,
            value: self.value.clone()
        }
    }
}

impl<T> AsRef<T> for Located<T> {
    fn as_ref(&self) -> &T {
        &self.value
    }
}


impl<T: fmt::Display> fmt::Display for Located<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.value.fmt(f)
    }
}

impl<T: fmt::Debug> fmt::Debug for Located<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.value.fmt(f)
    }
}

impl<T: Hash> Hash for Located<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.span.hash(state);
        self.value.hash(state);
    }
}

impl<T: PartialEq> PartialEq for Located<T> {
    fn eq(&self, other: &Located<T>) -> bool {
        self.span == other.span && self.value == other.value
    }
}

impl<T: Eq> Eq for Located<T> {}

impl<T> Deref for Located<T> {
    type Target = T;

    fn deref(&self) -> &T {
        &self.value
    }
}

impl<T> DerefMut for Located<T> {
    fn deref_mut(&mut self) -> &mut T {
        &mut self.value
    }
}
