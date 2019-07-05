#![feature(box_syntax)]
#![feature(test)]
#![feature(slice_concat_ext)]
#![no_std]

#[cfg(feature = "std")]
extern crate std;
extern crate alloc;

mod gba;
pub use gba::*;
