#![cfg_attr(feature = "unstable", feature(core_intrinsics, const_fn))]
#![no_std]

use core::ops::{
    Add, Sub, Mul, Div, Rem, Shl, Shr,
    BitXor, BitOr, BitAnd,
    Deref, DerefMut
};

#[cfg(feature = "unstable")]
use core::intrinsics;

pub trait UncheckedAdd: Copy + Sized + Add<Self, Output = Self> {
    unsafe fn unchecked_add(self, rhs: Self) -> Self;
}

pub trait UncheckedSub: Copy + Sized + Sub<Self, Output = Self> {
    unsafe fn unchecked_sub(self, rhs: Self) -> Self;
}

pub trait UncheckedMul: Copy + Sized + Mul<Self, Output = Self> {
    unsafe fn unchecked_mul(self, rhs: Self) -> Self;
}

pub trait UncheckedDiv: Copy + Sized + Div<Self, Output = Self> {
    unsafe fn unchecked_div(self, rhs: Self) -> Self;
}

pub trait UncheckedRem: Copy + Sized + Rem<Self, Output = Self> {
    unsafe fn unchecked_rem(self, rhs: Self) -> Self;
}

pub trait UncheckedShl: Copy + Sized + Shl<u32, Output = Self> {
    unsafe fn unchecked_shl(self, rhs: u32) -> Self;
}

pub trait UncheckedShr: Copy + Sized + Shr<u32, Output = Self> {
    unsafe fn unchecked_shr(self, rhs: u32) -> Self;
}

macro_rules! impl_traits {
    (@i $i:ident $t:ident $f:ident $ff:ident) => {
        impl $t for $i {
            #[inline]
            #[cfg(feature = "unstable")]
            unsafe fn $f(self, rhs: Self) -> Self {
                intrinsics::$f(self, rhs)
            }

            #[inline]
            #[cfg(not(feature = "unstable"))]
            unsafe fn $f(self, rhs: Self) -> Self {
                <$i>::$ff(self, rhs)
            }
        }
    };
    ($($i:ident,)*) => {
        $(
            impl_traits! { @i $i UncheckedAdd unchecked_add wrapping_add }
            impl_traits! { @i $i UncheckedSub unchecked_sub wrapping_sub }
            impl_traits! { @i $i UncheckedMul unchecked_mul wrapping_mul }
            impl_traits! { @i $i UncheckedDiv unchecked_div wrapping_div }
            impl_traits! { @i $i UncheckedRem unchecked_rem wrapping_rem }

            impl UncheckedShl for $i {
                #[inline]
                #[cfg(feature = "unstable")]
                unsafe fn unchecked_shl(self, rhs: u32) -> Self {
                    intrinsics::unchecked_shl(self, rhs as _)
                }

                #[inline]
                #[cfg(not(feature = "unstable"))]
                unsafe fn unchecked_shl(self, rhs: u32) -> Self {
                    <$i>::wrapping_shl(self, rhs)
                }
            }

            impl UncheckedShr for $i {
                #[inline]
                #[cfg(feature = "unstable")]
                unsafe fn unchecked_shr(self, rhs: u32) -> Self {
                    intrinsics::unchecked_shr(self, rhs as _)
                }

                #[inline]
                #[cfg(not(feature = "unstable"))]
                unsafe fn unchecked_shr(self, rhs: u32) -> Self {
                    <$i>::wrapping_shr(self, rhs)
                }
            }
        )*
    };
}

impl_traits! {
    usize, u8, u16, u32, u64,
    isize, i8, i16, i32, i64,
}

#[derive(Debug, Copy, Clone)]
#[repr(transparent)]
pub struct Unchecked<T>(T);

impl<T> Unchecked<T> {
    #[inline]
    pub const unsafe fn new(v: T) -> Self {
        Self(v)
    }

    #[inline]
    pub fn into_inner(self) -> T {
        self.0
    }

    #[inline]
    pub const fn as_ref(&self) -> &T {
        &self.0
    }

    #[inline]
    pub fn as_mut(&mut self) -> &mut T {
        &mut self.0
    }

    #[inline]
    pub fn map<F: FnOnce(T) -> T>(self, f: F) -> Self {
        Self(f(self.0))
    }
}

impl<T: Copy> Unchecked<T> {
    #[cfg(feature = "unstable")]
    #[inline]
    pub const fn value(self) -> T {
        self.0
    }

    #[cfg(not(feature = "unstable"))]
    #[inline]
    pub fn value(self) -> T {
        self.0
    }
}

impl<T> Deref for Unchecked<T> {
    type Target = T;

    #[inline]
    fn deref(&self) -> &Self::Target {
        self.as_ref()
    }
}

impl<T> DerefMut for Unchecked<T> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.as_mut()
    }
}

impl<T: UncheckedAdd> Add for Unchecked<T> {
    type Output = Self;

    #[inline]
    fn add(self, rhs: Self) -> Self {
        self.map(|s| unsafe {
            UncheckedAdd::unchecked_add(s, rhs.value())
        })
    }
}

impl<T: UncheckedAdd> Add<T> for Unchecked<T> {
    type Output = Self;

    #[inline]
    fn add(self, rhs: T) -> Self {
        self.map(|s| unsafe {
            UncheckedAdd::unchecked_add(s, rhs)
        })
    }
}

impl<T: UncheckedSub> Sub for Unchecked<T> {
    type Output = Self;

    #[inline]
    fn sub(self, rhs: Self) -> Self {
        self.map(|s| unsafe {
            UncheckedSub::unchecked_sub(s, rhs.value())
        })
    }
}

impl<T: UncheckedSub> Sub<T> for Unchecked<T> {
    type Output = Self;

    #[inline]
    fn sub(self, rhs: T) -> Self {
        self.map(|s| unsafe {
            UncheckedSub::unchecked_sub(s, rhs)
        })
    }
}

impl<T: UncheckedMul> Mul for Unchecked<T> {
    type Output = Self;

    #[inline]
    fn mul(self, rhs: Self) -> Self {
        self.map(|s| unsafe {
            UncheckedMul::unchecked_mul(s, rhs.value())
        })
    }
}

impl<T: UncheckedMul> Mul<T> for Unchecked<T> {
    type Output = Self;

    #[inline]
    fn mul(self, rhs: T) -> Self {
        self.map(|s| unsafe {
            UncheckedMul::unchecked_mul(s, rhs)
        })
    }
}

impl<T: UncheckedDiv> Div for Unchecked<T> {
    type Output = Self;

    #[inline]
    fn div(self, rhs: Self) -> Self {
        self.map(|s| unsafe {
            UncheckedDiv::unchecked_div(s, rhs.value())
        })
    }
}

impl<T: UncheckedDiv> Div<T> for Unchecked<T> {
    type Output = Self;

    #[inline]
    fn div(self, rhs: T) -> Self {
        self.map(|s| unsafe {
            UncheckedDiv::unchecked_div(s, rhs)
        })
    }
}

impl<T: UncheckedRem> Rem for Unchecked<T> {
    type Output = Self;

    #[inline]
    fn rem(self, rhs: Self) -> Self {
        self.map(|s| unsafe {
            UncheckedRem::unchecked_rem(s, rhs.value())
        })
    }
}

impl<T: UncheckedRem> Rem<T> for Unchecked<T> {
    type Output = Self;

    #[inline]
    fn rem(self, rhs: T) -> Self {
        self.map(|s| unsafe {
            UncheckedRem::unchecked_rem(s, rhs)
        })
    }
}

impl<T: UncheckedShl> Shl<u32> for Unchecked<T> {
    type Output = Self;

    #[inline]
    fn shl(self, rhs: u32) -> Self {
        self.map(|s| unsafe {
            UncheckedShl::unchecked_shl(s, rhs)
        })
    }
}

impl<T: UncheckedShr> Shr<u32> for Unchecked<T> {
    type Output = Self;

    #[inline]
    fn shr(self, rhs: u32) -> Self {
        self.map(|s| unsafe {
            UncheckedShr::unchecked_shr(s, rhs)
        })
    }
}

impl<T: Copy + BitXor> BitXor for Unchecked<T> {
    type Output = Unchecked<T::Output>;

    #[inline]
    fn bitxor(self, rhs: Self) -> Self::Output {
        Self(self.value() ^ rhs.value())
    }
}

impl<T: Copy + BitXor> BitXor<T> for Unchecked<T> {
    type Output = Unchecked<T::Output>;

    #[inline]
    fn bitxor(self, rhs: T) -> Self::Output {
        Self(self.value() ^ rhs)
    }
}

impl<T: Copy + BitOr> BitOr for Unchecked<T> {
    type Output = Unchecked<T::Output>;

    #[inline]
    fn bitor(self, rhs: Self) -> Self::Output {
        Self(self.value() | rhs.value())
    }
}

impl<T: Copy + BitOr> BitOr<T> for Unchecked<T> {
    type Output = Unchecked<T::Output>;

    #[inline]
    fn bitor(self, rhs: T) -> Self::Output {
        Self(self.value() | rhs)
    }
}

impl<T: Copy + BitAnd> BitAnd for Unchecked<T> {
    type Output = Unchecked<T::Output>;

    #[inline]
    fn bitand(self, rhs: Self) -> Self::Output {
        Self(self.value() & rhs.value())
    }
}

impl<T: Copy + BitAnd> BitAnd<T> for Unchecked<T> {
    type Output = Unchecked<T::Output>;

    #[inline]
    fn bitand(self, rhs: T) -> Self::Output {
        Self(self.value() & rhs)
    }
}

// TODO assign variants
