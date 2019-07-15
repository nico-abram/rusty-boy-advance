#![feature(box_syntax)]
#![feature(test)]
#![no_std]

extern crate alloc;
#[cfg(feature = "std")]
extern crate std;

mod gba;
pub use gba::*;
