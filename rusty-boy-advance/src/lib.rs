#![feature(test)]
#![feature(iterator_try_collect)]
#![feature(core_intrinsics)]
#![no_std]
//#![warn(clippy::pedantic, clippy::nursery, clippy::cargo)]

extern crate alloc;
#[cfg(feature = "std")]
extern crate std;

#[cfg(feature = "std")]
pub mod disasm;
mod gba;
pub use gba::*;
