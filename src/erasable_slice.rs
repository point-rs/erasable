// NB: Last updated for Rust 1.60 parity. All impls are in rustdoc gutter order.
// Forwarding impls are provided for impls provided on primitive slices/arrays.

#[cfg(feature = "alloc")]
use alloc::{borrow::Cow, boxed::Box, rc::Rc, sync::Arc, vec::Vec};
use {
    crate::{Erasable, ErasedPtr},
    core::{
        borrow::{Borrow, BorrowMut},
        cmp::Ordering,
        fmt::Debug,
        hash::{Hash, Hasher},
        ops::{Deref, DerefMut},
        ptr::{self, NonNull},
    },
};

/// A wrapper around array/slice types which stores the slice length alongside
/// the slice, allowing pointers to the slice to be type-erased (made thin).
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ErasableSlice<S: ?Sized> {
    len: usize,
    slice: S,
}

impl<T, const N: usize> ErasableSlice<[T; N]> {
    /// Create a new erasable slice from an array.
    pub const fn new(array: [T; N]) -> Self {
        Self {
            len: N,
            slice: array,
        }
    }
}

unsafe impl<T> Erasable for ErasableSlice<[T]> {
    unsafe fn unerase(this: ErasedPtr) -> NonNull<Self> {
        let ptr = this.as_ptr();
        let len = ptr.cast::<usize>().read();
        let ptr = ptr::slice_from_raw_parts_mut(ptr.cast::<T>(), len);
        NonNull::new_unchecked(ptr as *mut Self)
    }
}

impl<T> Deref for ErasableSlice<[T]> {
    type Target = [T];

    fn deref(&self) -> &[T] {
        &self.slice
    }
}

impl<T> DerefMut for ErasableSlice<[T]> {
    fn deref_mut(&mut self) -> &mut [T] {
        &mut self.slice
    }
}

impl<T, const N: usize> Deref for ErasableSlice<[T; N]> {
    type Target = [T; N];

    fn deref(&self) -> &[T; N] {
        &self.slice
    }
}

impl<T, const N: usize> DerefMut for ErasableSlice<[T; N]> {
    fn deref_mut(&mut self) -> &mut [T; N] {
        &mut self.slice
    }
}

// ~~~ [T; N] like impls ~~~ //

impl<T, const N: usize> AsMut<[T]> for ErasableSlice<[T; N]> {
    fn as_mut(&mut self) -> &mut [T] {
        &mut **self
    }
}

impl<T, const N: usize> AsMut<ErasableSlice<[T]>> for ErasableSlice<[T; N]> {
    fn as_mut(&mut self) -> &mut ErasableSlice<[T]> {
        self
    }
}

impl<T, const N: usize> AsRef<[T]> for ErasableSlice<[T; N]> {
    fn as_ref(&self) -> &[T] {
        &**self
    }
}

impl<T, const N: usize> AsRef<ErasableSlice<[T]>> for ErasableSlice<[T; N]> {
    fn as_ref(&self) -> &ErasableSlice<[T]> {
        self
    }
}

impl<T, const N: usize> Borrow<[T]> for ErasableSlice<[T; N]> {
    fn borrow(&self) -> &[T] {
        &**self
    }
}

impl<T, const N: usize> Borrow<ErasableSlice<[T]>> for ErasableSlice<[T; N]> {
    fn borrow(&self) -> &ErasableSlice<[T]> {
        self
    }
}

impl<T, const N: usize> BorrowMut<[T]> for ErasableSlice<[T; N]> {
    fn borrow_mut(&mut self) -> &mut [T] {
        &mut **self
    }
}

impl<T, const N: usize> BorrowMut<ErasableSlice<[T]>> for ErasableSlice<[T; N]> {
    fn borrow_mut(&mut self) -> &mut ErasableSlice<[T]> {
        self
    }
}

impl<T, const N: usize> Default for ErasableSlice<[T; N]>
where
    [T; N]: Default,
{
    fn default() -> Self {
        Self {
            len: N,
            slice: Default::default(),
        }
    }
}

impl<T, const N: usize> Hash for ErasableSlice<[T; N]>
where
    T: Hash,
{
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.slice.hash(state);
    }
}

// nonlocal: Index, IndexMut

impl<T, const N: usize> IntoIterator for ErasableSlice<[T; N]> {
    type Item = T;
    type IntoIter = <[T; N] as IntoIterator>::IntoIter;
    fn into_iter(self) -> Self::IntoIter {
        self.slice.into_iter()
    }
}

impl<'a, T, const N: usize> IntoIterator for &'a ErasableSlice<[T; N]> {
    type Item = &'a T;
    type IntoIter = <&'a [T; N] as IntoIterator>::IntoIter;
    fn into_iter(self) -> Self::IntoIter {
        self.slice.iter()
    }
}

impl<'a, T, const N: usize> IntoIterator for &'a mut ErasableSlice<[T; N]> {
    type Item = &'a mut T;
    type IntoIter = <&'a mut [T; N] as IntoIterator>::IntoIter;
    fn into_iter(self) -> Self::IntoIter {
        self.slice.iter_mut()
    }
}

impl<T, const N: usize> Ord for ErasableSlice<[T; N]>
where
    T: Ord,
{
    fn cmp(&self, other: &Self) -> Ordering {
        self.slice.cmp(&other.slice)
    }
}

// later: PartialEq

impl<T, const N: usize> PartialOrd for ErasableSlice<[T; N]>
where
    T: PartialOrd,
{
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.slice.partial_cmp(other)
    }

    fn lt(&self, other: &Self) -> bool {
        self.slice.lt(other)
    }

    fn le(&self, other: &Self) -> bool {
        self.slice.le(other)
    }

    fn gt(&self, other: &Self) -> bool {
        self.slice.gt(other)
    }

    fn ge(&self, other: &Self) -> bool {
        self.slice.ge(other)
    }
}

// unstable: Pattern, SlicePattern

impl<'a, T, const N: usize> TryFrom<&'a [T]> for ErasableSlice<[T; N]>
where
    T: Copy,
{
    type Error = <[T; N] as TryFrom<&'a [T]>>::Error;
    fn try_from(value: &'_ [T]) -> Result<Self, Self::Error> {
        value.try_into().map(Self::from)
    }
}

impl<'a, T, const N: usize> TryFrom<&'a mut [T]> for ErasableSlice<[T; N]>
where
    T: Copy,
{
    type Error = <[T; N] as TryFrom<&'a mut [T]>>::Error;
    fn try_from(value: &'_ mut [T]) -> Result<Self, Self::Error> {
        value.try_into().map(Self::from)
    }
}

impl<'a, T, const N: usize> TryFrom<&'a ErasableSlice<[T]>> for &'a ErasableSlice<[T; N]> {
    type Error = <&'a [T; N] as TryFrom<&'a [T]>>::Error;
    fn try_from(value: &'_ ErasableSlice<[T]>) -> Result<Self, Self::Error> {
        <&[T; N] as TryFrom<&[T]>>::try_from(&**value)?;
        Ok(unsafe { &*(value as *const ErasableSlice<[T]> as *const ErasableSlice<[T; N]>) })
    }
}

impl<'a, T, const N: usize> TryFrom<&'a mut ErasableSlice<[T]>> for &'a mut ErasableSlice<[T; N]> {
    type Error = <&'a mut [T; N] as TryFrom<&'a mut [T]>>::Error;
    fn try_from(value: &'_ mut ErasableSlice<[T]>) -> Result<Self, Self::Error> {
        <&mut [T; N] as TryFrom<&mut [T]>>::try_from(&mut **value)?;
        Ok(unsafe { &mut *(value as *mut ErasableSlice<[T]> as *mut ErasableSlice<[T; N]>) })
    }
}

// ~~~ [T] like impls ~~~ //

impl<T> AsMut<[T]> for ErasableSlice<[T]> {
    fn as_mut(&mut self) -> &mut [T] {
        &mut **self
    }
}

impl<T> AsRef<[T]> for ErasableSlice<[T]> {
    fn as_ref(&self) -> &[T] {
        &**self
    }
}

// deprecated: AsciiExt
// impossible: BufRead
// unstable: Concat

impl<T> Default for &'_ ErasableSlice<[T]> {
    fn default() -> Self {
        &ErasableSlice { len: 0, slice: [] }
    }
}

// impossible: Default for &mut ErasableSlice<[T]>

#[cfg(feature = "alloc")]
macro_rules! alloc_from_slice_impls {
    (for<$T:ident> $($ty:ty),* $(,)?) => {$(
        impl<'a, T> From<&'a ErasableSlice<[T]>> for $ty
        where
            $ty: From<&'a [T]>,
        {
            fn from(slice: &'a ErasableSlice<[T]>) -> Self {
                Self::from(&**slice)
            }
        }
        impl<'a, T> From<&'a mut ErasableSlice<[T]>> for $ty
        where
            $ty: From<&'a mut [T]>,
        {
            fn from(slice: &'a mut ErasableSlice<[T]>) -> Self {
                Self::from(&mut **slice)
            }
        }
    )*}
}

#[cfg(feature = "alloc")]
alloc_from_slice_impls! { for<T>
    Box<[T]>,
    Arc<[T]>,
    Rc<[T]>,
    Vec<T>,
}

#[cfg(feature = "alloc")]
impl<'a, T> From<&'a ErasableSlice<[T]>> for Cow<'a, [T]>
where
    T: Clone,
{
    fn from(slice: &'a ErasableSlice<[T]>) -> Self {
        Self::from(&**slice)
    }
}

impl<T> Hash for ErasableSlice<[T]>
where
    T: Hash,
{
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.slice.hash(state)
    }
}

// nonlocal: Index, IndexMut

impl<'a, T> IntoIterator for &'a ErasableSlice<[T]> {
    type Item = &'a T;
    type IntoIter = <&'a [T] as IntoIterator>::IntoIter;
    fn into_iter(self) -> Self::IntoIter {
        self.slice.iter()
    }
}

impl<'a, T> IntoIterator for &'a mut ErasableSlice<[T]> {
    type Item = &'a mut T;
    type IntoIter = <&'a mut [T] as IntoIterator>::IntoIter;
    fn into_iter(self) -> Self::IntoIter {
        self.slice.iter_mut()
    }
}

// unstable: Join
// later: Ord, PartialEq, PartialOrd
// unstable: Pattern
// impossible: Read
// unstable: SliceIndex, SlicePattern
// impossible: ToOwned
// later: ToSocketAddrs
// impossible: Write
