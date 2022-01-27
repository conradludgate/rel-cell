#![cfg_attr(not(test), no_std)]
#![feature(ptr_metadata)]

use core::{marker::PhantomData, ops::Deref, ptr};

type InvariantLifetime<'token> = PhantomData<fn(&'token ()) -> &'token ()>;

pub struct Token<'token> {
    _token: InvariantLifetime<'token>,
}

impl<'token> Token<'token> {
    pub fn with<R, F>(fun: F) -> R
    where
        for<'new_id> F: FnOnce(Token<'new_id>) -> R,
    {
        let token = Self {
            _token: PhantomData,
        };
        fun(token)
    }
}

pub struct OwnedCell<'token, T> {
    inner: T,
    _token: Token<'token>,
}

impl<'token, T> OwnedCell<'token, T> {
    pub fn new(token: Token<'token>, inner: T) -> Self {
        Self {
            inner,
            _token: token,
        }
    }

    pub fn into_inner(self) -> T {
        self.inner
    }

    pub fn new_ref<'a, R: 'a>(&'a self, f: impl FnOnce(&'a R) -> &'a R) -> Relative<'token, R>
    where
        T: AsRef<R>,
    {
        Self::new_rel(self.inner.as_ref(), f)
    }

    pub fn new_deref<'a>(
        &'a self,
        f: impl FnOnce(&'a T::Target) -> &'a T::Target,
    ) -> Relative<'token, T::Target>
    where
        T: Deref,
    {
        Self::new_rel(&*self.inner, f)
    }

    fn new_rel<'a, R: 'a + ?Sized>(
        ref_: &'a R,
        f: impl FnOnce(&'a R) -> &'a R,
    ) -> Relative<'token, R> {
        let (ptr, metadata) = (f(ref_) as *const R).to_raw_parts();
        let (base, _) = (ref_ as *const R).to_raw_parts();
        let rel = ptr as usize - base as usize;
        Relative {
            inner: ptr::from_raw_parts(rel as *const (), metadata),
            _token: PhantomData,
        }
    }

    pub fn as_ref<'a, R>(&'a self, rel: Relative<'token, R>) -> &'a R
    where
        T: AsRef<R>,
    {
        Self::as_rel(self.inner.as_ref(), rel)
    }

    pub fn as_deref<'a>(&'a self, rel: Relative<'token, T::Target>) -> &'a T::Target
    where
        T: Deref,
    {
        Self::as_rel(&*self.inner, rel)
    }

    fn as_rel<'a, R: 'a + ?Sized>(ref_: &'a R, rel: Relative<'token, R>) -> &'a R {
        let (base, _) = (ref_ as *const R).to_raw_parts();
        let (ptr, metadata) = rel.inner.to_raw_parts();
        let out = ptr::from_raw_parts((base as usize + ptr as usize) as *const (), metadata);
        unsafe { &*out }
    }
}

#[derive(Clone, Copy)]
pub struct Relative<'token, T: ?Sized> {
    /// this is not going to be a valid pointer. Do not assume it can be dereferenced.
    /// The pointer part should be an offset from a valid pointer, and this will be valid relative to that
    inner: *const T,
    _token: InvariantLifetime<'token>,
}

#[cfg(test)]
mod tests {
    use std::mem::{size_of, size_of_val};

    use crate::{OwnedCell, Relative, Token};

    fn string(token: Token<'_>) -> (OwnedCell<'_, String>, Relative<'_, str>, Relative<'_, str>) {
        let owned = OwnedCell::new(token, "foo bar".to_owned());
        let foo = owned.new_deref(|foobar| &foobar[..3]);
        let bar = owned.new_deref(|foobar| &foobar[4..]);

        (owned, foo, bar)
    }

    #[test]
    fn borrow() {
        Token::with(|token| {
            let (foo_bar, foo, bar) = string(token);

            assert_eq!(size_of_val(&foo_bar), size_of::<String>());
            assert_eq!(size_of_val(&foo), size_of::<&str>());
            assert_eq!(size_of_val(&bar), size_of::<&str>());

            assert_eq!(foo_bar.as_deref(foo), "foo");
            assert_eq!(foo_bar.as_deref(bar), "bar");
            assert_eq!(foo_bar.into_inner(), "foo bar");
        })
    }
}
