#![feature(test)]
#![no_std]
//#![warn(clippy::pedantic, clippy::nursery, clippy::cargo)]

extern crate alloc;
#[cfg(feature = "std")]
extern crate std;

mod gba;
pub use gba::*;
