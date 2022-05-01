// NB: Last updated for Rust 1.60 parity. All impls are in rustdoc gutter
// (alphabetic) order. Forwarding impls are provided for impls provided on Box.

#[cfg(feature = "alloc")]
use alloc::{string::String, vec::Vec};
#[cfg(feature = "std")]
use std::io::{self, BufRead, Read, Seek, Write};
use {
    crate::{ErasablePtr, ErasedPtr},
    core::{
        cmp::Ordering,
        fmt::{self, Debug, Display, Formatter, Pointer},
        future::Future,
        hash::{Hash, Hasher},
        iter::{FromIterator, FusedIterator},
        marker::PhantomData,
        mem::ManuallyDrop,
        ops::{Deref, DerefMut},
        panic::{RefUnwindSafe, UnwindSafe},
        pin::Pin,
        ptr,
        task::{Context, Poll},
    },
};

/// Wrapper struct to create thin pointer types.
///
/// This type is guaranteed to have the same repr as [`ErasedNonNull`].
///
/// # Examples
///
/// ```rust
/// use erasable::*;
///
/// let array = [0; 10];
/// let boxed = Box::new(array);
/// let thin_box: Thin<Box<_>> = boxed.into();
/// dbg!(thin_box);
/// ```
///
/// Note that this uses a `Sized` type (`[i32; 10]`) which has a thin pointer
/// normally. This library does not provide any actual erasable unsized types.
#[repr(transparent)]
pub struct Thin<P: ErasablePtr> {
    pub(crate) ptr: ErasedPtr,
    pub(crate) marker: PhantomData<P>,
}

// oibits
unsafe impl<P: ErasablePtr> Send for Thin<P> where P: Send {}
unsafe impl<P: ErasablePtr> Sync for Thin<P> where P: Sync {}
impl<P: ErasablePtr> Unpin for Thin<P> where P: Unpin {}
impl<P: ErasablePtr> UnwindSafe for Thin<P> where P: UnwindSafe {}
impl<P: ErasablePtr> RefUnwindSafe for Thin<P> where P: RefUnwindSafe {}

impl<P: ErasablePtr> From<P> for Thin<P> {
    #[inline(always)]
    fn from(this: P) -> Self {
        Self {
            ptr: P::erase(this),
            marker: PhantomData,
        }
    }
}

impl<P: ErasablePtr> Thin<P> {
    fn inner(this: &Self) -> ManuallyDrop<P> {
        unsafe { ManuallyDrop::new(P::unerase(this.ptr)) }
    }

    /// Extract the wrapped pointer.
    pub fn into_inner(this: Self) -> P {
        unsafe { P::unerase(ManuallyDrop::new(this).ptr) }
    }

    /// Run a closure with a borrow of the real pointer.
    pub fn with<F, T>(this: &Self, f: F) -> T
    where
        F: FnOnce(&P) -> T,
    {
        f(&Thin::inner(this))
    }

    /// Run a closure with a mutable borrow of the real pointer.
    pub fn with_mut<F, T>(this: &mut Self, f: F) -> T
    where
        F: FnOnce(&mut P) -> T,
    {
        // SAFETY: guard is required to write potentially changed pointer value
        let mut this = unsafe {
            scopeguard::guard(P::unerase(this.ptr), |unerased| {
                ptr::write(this, Thin::from(unerased))
            })
        };
        f(&mut this)
    }

    /// Check two thin pointers for pointer equivalence.
    pub fn ptr_eq<Q: ErasablePtr>(this: &Self, that: &Thin<Q>) -> bool {
        this.ptr == that.ptr
    }
}

impl<P: ErasablePtr> Drop for Thin<P> {
    fn drop(&mut self) {
        unsafe { P::unerase(self.ptr) };
    }
}

// ~~~ Box<T> like impls ~~~ //

impl<P: ErasablePtr, T: ?Sized> AsMut<T> for Thin<P>
where
    P: AsMut<T>,
{
    fn as_mut(&mut self) -> &mut T {
        unsafe { Thin::with_mut(self, |p| erase_lt_mut(p.as_mut())) }
    }
}

impl<P: ErasablePtr, T: ?Sized> AsRef<T> for Thin<P>
where
    P: AsRef<T>,
{
    fn as_ref(&self) -> &T {
        unsafe { Thin::with(self, |p| erase_lt(p.as_ref())) }
    }
}

// unstable: AsyncIterator
// conflict: Borrow, BorrowMut

#[cfg(feature = "std")]
impl<P: ErasablePtr> BufRead for Thin<P>
where
    P: BufRead,
{
    fn fill_buf(&mut self) -> io::Result<&[u8]> {
        Thin::with_mut(self, |p| p.fill_buf().map(|x| unsafe { erase_lt(x) }))
    }

    fn consume(&mut self, amt: usize) {
        Thin::with_mut(self, |p| p.consume(amt))
    }

    fn read_until(&mut self, byte: u8, buf: &mut Vec<u8>) -> io::Result<usize> {
        Thin::with_mut(self, |p| p.read_until(byte, buf))
    }

    fn read_line(&mut self, buf: &mut String) -> io::Result<usize> {
        Thin::with_mut(self, |p| p.read_line(buf))
    }
}

impl<P: ErasablePtr> Clone for Thin<P>
where
    P: Clone,
{
    fn clone(&self) -> Self {
        Thin::with(self, |p| p.clone()).into()
    }
}

// unstable: CoerceUnsized

impl<P: ErasablePtr> Debug for Thin<P>
where
    P: Debug,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Thin::with(self, |p| p.fmt(f))
    }
}

impl<P: ErasablePtr> Default for Thin<P>
where
    P: Default,
{
    fn default() -> Self {
        Thin::from(P::default())
    }
}

impl<P: ErasablePtr> Deref for Thin<P>
where
    P: Deref,
{
    type Target = P::Target;
    fn deref(&self) -> &P::Target {
        // SAFETY: This is safe because we are promoting the lifetime of
        // &P::Target from borrowing from the transient &P to borrowing from our
        // &Thin<P>. The Thin<P> is equivalent to the P for the purposes of
        // owning derived pointers, and ErasablePtr guarantees that Deref goes
        // to an independent location.
        unsafe { Thin::with(self, |p| erase_lt(P::deref(p))) }
    }
}

impl<P: ErasablePtr> DerefMut for Thin<P>
where
    P: DerefMut,
{
    fn deref_mut(&mut self) -> &mut P::Target {
        // SAFETY: This is safe because we are promoting the lifetime of
        // &mut P::Target from borrowing from the transient &mut P to borrowing
        // from our &mut Thin<P>. The Thin<P> is equivalent to the P for the
        // purposes of owning derived pointers, and ErasablePtr guarantees that
        // DerefMut goes to an independent location.
        unsafe { Thin::with_mut(self, |p| erase_lt_mut(P::deref_mut(p))) }
    }
}

// unstable: DispatchFromDyn

impl<P: ErasablePtr> Display for Thin<P>
where
    P: Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Thin::with(self, |p| p.fmt(f))
    }
}

impl<P: ErasablePtr> DoubleEndedIterator for Thin<P>
where
    P: DoubleEndedIterator,
{
    fn next_back(&mut self) -> Option<Self::Item> {
        Thin::with_mut(self, |p| p.next_back())
    }

    fn nth_back(&mut self, n: usize) -> Option<Self::Item> {
        Thin::with_mut(self, |p| p.nth_back(n))
    }
}

impl<P: ErasablePtr> Eq for Thin<P> where P: Eq {}

impl<P: ErasablePtr> ExactSizeIterator for Thin<P>
where
    P: ExactSizeIterator,
{
    fn len(&self) -> usize {
        Thin::with(self, |p| p.len())
    }
}

// unstable: Fn, FnMut, FnOnce

impl<P: ErasablePtr, A> FromIterator<A> for Thin<P>
where
    P: FromIterator<A>,
{
    fn from_iter<T: IntoIterator<Item = A>>(iter: T) -> Self {
        P::from_iter(iter).into()
    }
}

impl<P: ErasablePtr> FusedIterator for Thin<P> where P: FusedIterator {}

impl<P: ErasablePtr> Future for Thin<P>
where
    P: Future,
{
    type Output = P::Output;
    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        unsafe { Thin::with_mut(self.get_unchecked_mut(), |p| Pin::new_unchecked(p).poll(cx)) }
    }
}

// unstable: Generator

impl<P: ErasablePtr> Hash for Thin<P>
where
    P: Hash,
{
    fn hash<H: Hasher>(&self, state: &mut H) {
        Thin::with(self, |p| p.hash(state))
    }
}

impl<P: ErasablePtr> Hasher for Thin<P>
where
    P: Hasher,
{
    fn finish(&self) -> u64 {
        Thin::with(self, |p| p.finish())
    }

    fn write(&mut self, bytes: &[u8]) {
        Thin::with_mut(self, |p| p.write(bytes))
    }

    fn write_u8(&mut self, i: u8) {
        Thin::with_mut(self, |p| p.write_u8(i))
    }

    fn write_u16(&mut self, i: u16) {
        Thin::with_mut(self, |p| p.write_u16(i))
    }

    fn write_u32(&mut self, i: u32) {
        Thin::with_mut(self, |p| p.write_u32(i))
    }

    fn write_u64(&mut self, i: u64) {
        Thin::with_mut(self, |p| p.write_u64(i))
    }

    fn write_u128(&mut self, i: u128) {
        Thin::with_mut(self, |p| p.write_u128(i))
    }

    fn write_usize(&mut self, i: usize) {
        Thin::with_mut(self, |p| p.write_usize(i))
    }

    fn write_i8(&mut self, i: i8) {
        Thin::with_mut(self, |p| p.write_i8(i))
    }

    fn write_i16(&mut self, i: i16) {
        Thin::with_mut(self, |p| p.write_i16(i))
    }

    fn write_i32(&mut self, i: i32) {
        Thin::with_mut(self, |p| p.write_i32(i))
    }

    fn write_i64(&mut self, i: i64) {
        Thin::with_mut(self, |p| p.write_i64(i))
    }

    fn write_i128(&mut self, i: i128) {
        Thin::with_mut(self, |p| p.write_i128(i))
    }

    fn write_isize(&mut self, i: isize) {
        Thin::with_mut(self, |p| p.write_isize(i))
    }
}

impl<P: ErasablePtr> Iterator for Thin<P>
where
    P: Iterator,
{
    type Item = P::Item;

    fn next(&mut self) -> Option<Self::Item> {
        Thin::with_mut(self, |p| p.next())
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        Thin::with(self, |p| p.size_hint())
    }

    fn nth(&mut self, n: usize) -> Option<Self::Item> {
        Thin::with_mut(self, |p| p.nth(n))
    }

    fn last(self) -> Option<Self::Item> {
        Thin::into_inner(self).last()
    }
}

impl<P: ErasablePtr> Ord for Thin<P>
where
    P: Ord,
{
    fn cmp(&self, other: &Thin<P>) -> Ordering {
        Thin::with(self, |p| Thin::with(other, |other| p.cmp(other)))
    }
}

impl<P: ErasablePtr> PartialEq for Thin<P>
where
    P: PartialEq,
{
    fn eq(&self, other: &Thin<P>) -> bool {
        Thin::with(self, |p| Thin::with(other, |other| p.eq(other)))
    }

    #[allow(clippy::partialeq_ne_impl)] // imitating Box
    fn ne(&self, other: &Self) -> bool {
        Thin::with(self, |p| Thin::with(other, |other| p.ne(other)))
    }
}

impl<P: ErasablePtr> PartialOrd for Thin<P>
where
    P: PartialOrd,
{
    fn partial_cmp(&self, other: &Thin<P>) -> Option<Ordering> {
        Thin::with(self, |p| Thin::with(other, |other| p.partial_cmp(other)))
    }

    fn lt(&self, other: &Self) -> bool {
        Thin::with(self, |p| Thin::with(other, |other| p.lt(other)))
    }

    fn le(&self, other: &Self) -> bool {
        Thin::with(self, |p| Thin::with(other, |other| p.le(other)))
    }

    fn gt(&self, other: &Self) -> bool {
        Thin::with(self, |p| Thin::with(other, |other| p.gt(other)))
    }

    fn ge(&self, other: &Self) -> bool {
        Thin::with(self, |p| Thin::with(other, |other| p.ge(other)))
    }
}

impl<P: ErasablePtr> Pointer for Thin<P>
where
    P: Pointer,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Thin::with(self, |p| p.fmt(f))
    }
}

#[cfg(feature = "std")]
impl<P: ErasablePtr> Read for Thin<P>
where
    P: Read,
{
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        Thin::with_mut(self, |p| p.read(buf))
    }

    fn read_vectored(&mut self, bufs: &mut [io::IoSliceMut<'_>]) -> io::Result<usize> {
        Thin::with_mut(self, |p| p.read_vectored(bufs))
    }

    fn read_to_end(&mut self, buf: &mut Vec<u8>) -> io::Result<usize> {
        Thin::with_mut(self, |p| p.read_to_end(buf))
    }

    fn read_to_string(&mut self, buf: &mut String) -> io::Result<usize> {
        Thin::with_mut(self, |p| p.read_to_string(buf))
    }

    fn read_exact(&mut self, buf: &mut [u8]) -> io::Result<()> {
        Thin::with_mut(self, |p| p.read_exact(buf))
    }
}

#[cfg(feature = "std")]
impl<P: ErasablePtr> Seek for Thin<P>
where
    P: Seek,
{
    fn seek(&mut self, pos: io::SeekFrom) -> io::Result<u64> {
        Thin::with_mut(self, |p| p.seek(pos))
    }

    fn stream_position(&mut self) -> io::Result<u64> {
        Thin::with_mut(self, |p| p.stream_position())
    }
}

#[cfg(feature = "std")]
impl<P: ErasablePtr> Write for Thin<P>
where
    P: Write,
{
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        Thin::with_mut(self, |p| p.write(buf))
    }

    fn write_vectored(&mut self, bufs: &[io::IoSlice<'_>]) -> io::Result<usize> {
        Thin::with_mut(self, |p| p.write_vectored(bufs))
    }

    fn flush(&mut self) -> io::Result<()> {
        Thin::with_mut(self, |p| p.flush())
    }

    fn write_all(&mut self, buf: &[u8]) -> io::Result<()> {
        Thin::with_mut(self, |p| p.write_all(buf))
    }

    fn write_fmt(&mut self, fmt: fmt::Arguments<'_>) -> io::Result<()> {
        Thin::with_mut(self, |p| p.write_fmt(fmt))
    }
}

// ~~~ Helpers ~~~ //

#[inline(always)]
unsafe fn erase_lt<'a, 'b, T: ?Sized>(this: &'a T) -> &'b T {
    &*(this as *const T)
}

#[inline(always)]
unsafe fn erase_lt_mut<'a, 'b, T: ?Sized>(this: &'a mut T) -> &'b mut T {
    &mut *(this as *mut T)
}
