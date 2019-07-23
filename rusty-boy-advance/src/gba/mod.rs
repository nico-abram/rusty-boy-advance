mod cpsr;
mod cpu_mode;
mod gba;
mod instructions;
mod rom;
mod utils;

pub use cpsr::CPSR;
pub use cpu_mode::CpuMode;
pub use gba::{GBAButton, GBAError, LogLevel, GBA};
pub use rom::Rom;

use alloc::boxed::Box;

/// Wrapper around the GBA emulator struct to prevent dereferencing the Box
/// Since it's too big for the stack
pub struct GBABox {
  internal_gba: Box<gba::GBA>,
}
impl GBABox {
  pub fn new(
    log_level: LogLevel,
    bios_file: core::option::Option<&[u8]>,
    print_fn: Option<fn(&str) -> ()>,
  ) -> Self {
    GBABox { internal_gba: gba::GBA::new(log_level, bios_file, print_fn) }
  }
  pub fn update_video_output(&mut self) {
    self.internal_gba.update_video_output()
  }
  pub fn video_output(&self) -> &[u8] {
    &self.internal_gba.output_texture[..]
  }
  pub fn io_memory(&self) -> &[u8] {
    &self.internal_gba.io_mem[..]
  }
  pub fn input(&mut self, button: GBAButton) {
    GBA::input(self, button);
  }
  pub fn loaded_rom(&self) -> Option<&Rom> {
    self.internal_gba.loaded_rom.as_ref()
  }
  pub fn registers(&self) -> [u32; 16] {
    self.regs
  }
  pub fn cpsr(&self) -> CPSR {
    self.cpsr
  }
  pub fn vram_bytes(&self) -> &[u8] {
    &self.vram[..]
  }
  pub fn chip_wram_bytes(&self) -> &[u8] {
    &self.internal_gba.wram_chip[..]
  }
  pub fn board_wram_bytes(&self) -> &[u8] {
    &self.internal_gba.wram_board[..]
  }
  pub fn palette_bytes(&self) -> &[u8] {
    &self.internal_gba.palette_ram[..]
  }
  pub fn oam_bytes(&self) -> &[u8] {
    &self.internal_gba.oam[..]
  }
  pub fn bios_bytes(&self) -> &[u8] {
    &self.internal_gba.bios_rom[..]
  }
  pub fn spsr(&self) -> Option<CPSR> {
    let mode = self.cpsr.mode();
    if mode == CpuMode::Privileged || mode == CpuMode::User {
      None
    } else {
      Some(self.spsrs[self.cpsr.mode().as_usize() - 1])
    }
  }
}
impl core::ops::Deref for GBABox {
  type Target = gba::GBA;

  fn deref(&self) -> &Self::Target {
    &*self.internal_gba
  }
}

impl core::ops::DerefMut for GBABox {
  fn deref_mut(&mut self) -> &mut Self::Target {
    &mut *self.internal_gba
  }
}
