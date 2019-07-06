#![feature(box_syntax)]
#![feature(test)]
#![feature(slice_concat_ext)]
#![no_std]

extern crate alloc;
#[cfg(feature = "std")]
extern crate std;

mod gba;
pub use gba::*;
