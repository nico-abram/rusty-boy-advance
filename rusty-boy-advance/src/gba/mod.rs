mod cpsr;
mod cpu_mode;
mod gba;
mod instructions;
mod rom;
pub mod utils;

use alloc::boxed::Box;

/// Wrapper around the GBA emulator struct to prevent dereferencing the Box
/// Since it's too big for the stack
pub struct GBA {
  internal_gba: Box<gba::GBA>,
}
impl GBA {
  pub fn new(log_level: LogLevel, bios_file: core::option::Option<&[u8]>, print_fn:Option<fn(&str) -> ()>) -> Self {
    GBA { internal_gba: gba::GBA::new(log_level, bios_file, print_fn) }
  }
}
impl core::ops::Deref for GBA {
  type Target = gba::GBA;

  fn deref(&self) -> &Self::Target {
    &*self.internal_gba
  }
}
impl core::ops::DerefMut for GBA {
  fn deref_mut(&mut self) -> &mut Self::Target {
    &mut *self.internal_gba
  }
}

pub use gba::LogLevel;
