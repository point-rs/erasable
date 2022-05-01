//! Erase pointers of their concrete type and store type-erased pointers.
//!
//! This is roughly equivalent to C's `void*`.
//!
//! There are two main useful reasons to type erase pointers in Rust:
//!
//! - Removing viral generics from internal implementation details. If the
//!   internals truly don't care about the stored type, treating it opaquely
//!   reduces monomorphization cost both to the author and the compiler.
//! - Thin pointers to `?Sized` types. If an unsized type stores its metadata
//!   inline, then it can implement [`Erasable`] and be used behind type-erased
//!   pointers. The type erased pointer does not have to carry the metadata, and
//!   the fat pointer can be recovered from the inline metadata. We provide the
//!   [`Thin`] wrapper type to provide thin pointer types and [`ErasableSlice`]
//!   for thin pointer capable slices.

#![warn(missing_docs, missing_debug_implementations)]
#![no_std]

#[cfg(feature = "alloc")]
extern crate alloc;

#[cfg(feature = "std")]
extern crate std;

pub use self::{erasable_slice::ErasableSlice, thin::Thin};
#[cfg(feature = "alloc")]
use alloc::{boxed::Box, rc, sync};
use core::{
    fmt,
    marker::PhantomData,
    mem::ManuallyDrop,
    ops::Deref,
    pin::Pin,
    ptr::{self, NonNull},
};
#[cfg(test)]
use {
    core::panic::{RefUnwindSafe, UnwindSafe},
    static_assertions::*,
};

mod erasable_slice;
mod thin;

/// A thin, type-erased pointer.
///
/// The `Erased` type is private, and should be treated as an opaque type.
/// If/when `extern type` is ever stabilized, `Erased` will be defined as one,
/// and this will not be considered a breaking change. If you want to offset the
/// pointer, make sure to cast to a `u8` or other known type pointer first.
///
/// To ensure minimal breaking changes when replacing `Erased` with an `extern`
/// definition, `Erased` is currently defined with current stable-best-practice
/// for opaque FFI types, meaning that it is temporarily considered `Sized` but
/// it is not `Send`, `Sync`, `Unpin`, `UnwindSafe`, nor `RefUnwindSafe`.
///
/// As a result, we get the desired results that `ErasedPtr` is a thin pointer
/// but not `Send`, `Sync`, `UnwindSafe`, nor `RefUnwindSafe` by default. If you
/// know your usage fulfils these autotraits, you should `impl` them yourself.
pub type ErasedPtr = NonNull<Erased>;

#[cfg(test)]
assert_not_impl_any!(ErasedPtr: Send, Sync, UnwindSafe, RefUnwindSafe);

#[cfg(test)]
assert_eq_size!(ErasedPtr, Option<ErasedPtr>, *mut ());

pub(crate) use priv_in_pub::Erased;
mod priv_in_pub {
    #[cfg(test)]
    use super::*;

    /// An erased type.
    #[repr(C)]
    pub struct Erased {
        _oibit: core::marker::PhantomData<core::cell::Cell<&'static mut *mut u8>>,
        _pin: core::marker::PhantomPinned,
    }

    #[cfg(test)]
    assert_not_impl_any!(Erased: Send, Sync, Unpin, UnwindSafe, RefUnwindSafe);
}

impl fmt::Debug for Erased {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("{erased}").finish()
    }
}

/// A (smart) pointer type that can be type-erased (making a thin pointer).
///
/// When implementing this trait, you should implement it for all `Erasable`
/// pointee types. Implementing this trait allows use of the pointer in erased
/// contexts, such as [`Thin`].
///
/// # Safety
///
/// A pointer type which is erasable must not include shared mutability before
/// indirection. Equivalently, the erased pointer produced by calling `erase` on
/// some `P` must be the same both before and after performing any set of
/// operations on `&P`. `&mut P` methods (notably `DerefMut`) are still allowed
/// to mutate the pointer value, if necessary.
///
/// Additionally, the address of the deref target must be independent of the
/// address of the pointer. For example, `Box` implements `ErasablePtr` because
/// it's a pointer to a managed heap allocation. [`Lazy`](std::lazy::Lazy) and
/// [`ManuallyDrop`], however, `Deref`s into their own location, and as such,
/// can not implement `ErasablePtr`.
///
/// This is similar to (but distinct from!) the guarantees required by [`Pin`].
/// `Pin` requires no access to `&mut P`/`&mut P::target`, but these remain safe
/// even when using [`Thin`] through [`Thin::with_mut`] and `DerefMut for Thin`.
///
/// # Examples
///
/// ```rust
/// use erasable::*;
///
/// #[derive(Debug)]
/// struct MyBox<T: ?Sized>(Box<T>);
///
/// unsafe impl<T: ?Sized> ErasablePtr for MyBox<T>
/// where
///     T: Erasable
/// {
///     fn erase(this: Self) -> ErasedPtr {
///         ErasablePtr::erase(this.0)
///     }
///
///     unsafe fn unerase(this: ErasedPtr) -> Self {
///         Self(ErasablePtr::unerase(this))
///     }
/// }
///
/// let array = [0; 10];
/// let boxed = MyBox(Box::new(array));
/// let thin_box: Thin<MyBox<_>> = boxed.into();
/// dbg!(thin_box);
/// ```
///
/// # Counterexamples
///
//  These are available to be run under miri to observe UB in tests/this_is_ub_examples.rs!
//  The proper command is `cargo miri test -- --ignored {test-name}`.
//  Keep these tests in sync! Individual doctests can't be filtered for.
///
/// This implementation of `ErasablePtr` is unsound
/// because it features shared mutability before indirection:
///
/// ```rust,no_run (example of unsound code)
/// # use {erasable::*, std::cell::Cell};
/// struct Pls {
///     inner: Cell<Box<u8>>,
/// }
///
/// unsafe impl ErasablePtr for Pls {
///     fn erase(this: Self) -> ErasedPtr { ErasablePtr::erase(this.inner.into_inner()) }
///     unsafe fn unerase(this: ErasedPtr) -> Self {
///         Pls { inner: Cell::new(ErasablePtr::unerase(this)) }
///     }
/// }
///
/// impl Pls {
///     fn mutate(&self, to: Box<u8>) { self.inner.set(to); }
/// }
///
/// let thin = Thin::from(Pls { inner: Cell::new(Box::new(0)) });
/// Thin::with(&thin, |pls| pls.mutate(Box::new(1))); // drops box(0), leaks box(1)
/// drop(thin); // `thin` is still Pls(Box(0)); use-after-free
/// ```
///
/// This implementation of `ErasablePtr` is unsound
/// because it dereferences to the interior of the type:
///
/// ```rust,no_run (example of unsound code)
/// # use {erasable::*, std::ops::Deref};
/// struct Why {
///     inner: Box<u8>,
/// }
///
/// unsafe impl ErasablePtr for Why {
///     fn erase(this: Self) -> ErasedPtr { ErasablePtr::erase(this.inner) }
///     unsafe fn unerase(this: ErasedPtr) -> Self { Why { inner: ErasablePtr::unerase(this) } }
/// }
///
/// impl Deref for Why {
///     type Target = Box<u8>;
///     fn deref(&self) -> &Box<u8> { &self.inner }
/// }
///
/// let thin = Thin::from(Why { inner: Box::new(0) });
/// let _: &Box<u8> = thin.deref(); // use-after-free; cannot deref to value that does not exist
/// ```
pub unsafe trait ErasablePtr {
    /// Turn this erasable pointer into an erased pointer with the same
    /// provenance but potentially different address.
    ///
    /// To retrieve the original pointer, use `unerase`.
    fn erase(this: Self) -> ErasedPtr;

    /// Recover the typed pointer from an erased pointer.
    ///
    /// # Safety
    ///
    /// The erased pointer must have been created by `erase`.
    ///
    /// Unlike [`Erasable::unerase`], the pointer is *not* required to be valid;
    /// it is *only* required to have come from [`ErasablePtr::erase`]. The impl
    /// for `ErasablePtr` must be restricted to pointers which can soundly be
    /// unerased using `Erasable::unerase` (i.e. known valid pointers).
    unsafe fn unerase(this: ErasedPtr) -> Self;
}

/// A pointee type that supports type-erased pointers (thin pointers).
///
/// This trait is automatically implemented for all sized types, and can be
/// manually implemented for unsized types that know their own metadata.
///
/// # Safety
///
/// Must be implemented as described and may be relied upon by generic code.
pub unsafe trait Erasable {
    /// Turn this erasable pointer into an erased pointer with the same address
    /// and provenance.
    ///
    /// To retrieve the original pointer, use `unerase`.
    ///
    /// # Implementing this method
    ///
    /// There is only one reasonable implementation, and that implementation
    /// (simply casting the pointer to a thin `ErasedPtr` pointer) is provided.
    #[inline(always)]
    fn erase(this: NonNull<Self>) -> ErasedPtr {
        unsafe { NonNull::new_unchecked(this.as_ptr() as *mut Erased) }
    }

    /// Recover the typed pointer from a valid erased pointer.
    ///
    /// # Safety
    ///
    /// This method must only be called with pointers previously erased via a
    /// call to [`erase`](Self::erase). Additionally, the erased pointer must
    /// be valid for reads and point to a valid instance of the pointee type.
    ///
    /// This method may perform some number of *reads* from the pointer. This
    /// invalidates any `&mut` derived from this raw pointer (or its siblings).
    ///
    /// Note that the exact rules for pointer invalidation are still under flux,
    /// and so the *exact* rules for this method cannot be specified precisely.
    ///
    /// # Implementing this method
    ///
    /// The implementation *must* correctly roundtrip pointers with *any*
    /// provenance, whether that be unique (`&mut _`), shared (`&_`), read-only
    /// raw (`*const _`, typically), read-write raw (`*mut _`, typically), or
    /// any other set of permissions.
    ///
    /// As much as is possible, the implementation should not invalidate any
    /// outstanding borrows/pointers to the pointee location. To accomplish
    /// this, the implementation must not create any references, must not write
    /// to the pointee, and should read the minimal amount of pointee state
    /// required to recover the pointee metadata.
    ///
    /// The intent is that `unerase(erase(ptr))` should minimally impact the
    /// abstract machine state by recovering the pointee metadata from the
    /// pointee representation, and any other safe usage of the pointee should
    /// not conflict with this operation.
    unsafe fn unerase(this: ErasedPtr) -> NonNull<Self>;
}

// ~~~ impl Eraseable ~~~ //

/// In addition to the trait requirements, this implementation guarantees that
/// `unerase` for sized `T` is safe to call for invalid pointers.
unsafe impl<T: Sized> Erasable for T {
    unsafe fn unerase(this: ErasedPtr) -> NonNull<T> {
        this.cast()
    }
}

// ~~~ impl ErasablePtr ~~~ //

// SAFETY: this impl is sound for sized types because of the guarantee of the
// Erasable impl above that <for<T: Sized> T as Erasable>::unerase is valid for
// invalid pointers, strengthened from the Erasable::unerase guarantees. This
// impl would be unsound if extended to cover ?Sized pointee types.
unsafe impl<T: Sized> ErasablePtr for ptr::NonNull<T>
where
    T: Erasable,
{
    fn erase(this: Self) -> ErasedPtr {
        T::erase(this)
    }

    unsafe fn unerase(this: ErasedPtr) -> Self {
        T::unerase(this)
    }
}

unsafe impl<P: ErasablePtr> ErasablePtr for Thin<P> {
    fn erase(this: Self) -> ErasedPtr {
        ManuallyDrop::new(this).ptr
    }

    unsafe fn unerase(this: ErasedPtr) -> Self {
        Thin {
            ptr: this,
            marker: PhantomData,
        }
    }
}

unsafe impl<T: ?Sized> ErasablePtr for &'_ T
where
    T: Erasable,
{
    fn erase(this: Self) -> ErasedPtr {
        T::erase(this.into())
    }

    unsafe fn unerase(this: ErasedPtr) -> Self {
        T::unerase(this).as_ref()
    }
}

unsafe impl<T: ?Sized> ErasablePtr for &'_ mut T
where
    T: Erasable,
{
    fn erase(this: Self) -> ErasedPtr {
        T::erase(this.into())
    }

    unsafe fn unerase(this: ErasedPtr) -> Self {
        T::unerase(this).as_mut()
    }
}

unsafe impl<P> ErasablePtr for Pin<P>
where
    P: ErasablePtr + Deref,
{
    fn erase(this: Self) -> ErasedPtr {
        unsafe { P::erase(Pin::into_inner_unchecked(this)) }
    }

    unsafe fn unerase(this: ErasedPtr) -> Self {
        Pin::new_unchecked(P::unerase(this))
    }
}

#[cfg(feature = "alloc")]
macro_rules! impl_erasable {
    (for<$T:ident> $($ty:ty),* $(,)?) => {$(
        unsafe impl<$T: ?Sized> ErasablePtr for $ty
        where
            $T: Erasable,
        {
            fn erase(this: Self) -> ErasedPtr {
                unsafe {
                    $T::erase(NonNull::new_unchecked(<$ty>::into_raw(this) as *mut $T))
                }
            }

            unsafe fn unerase(this: ErasedPtr) -> Self {
                Self::from_raw($T::unerase(this).as_ptr())
            }
        }
    )*}
}

#[cfg(feature = "alloc")]
impl_erasable! { for<T>
    Box<T>,
    sync::Arc<T>,
    sync::Weak<T>,
    rc::Rc<T>,
    rc::Weak<T>,
}
