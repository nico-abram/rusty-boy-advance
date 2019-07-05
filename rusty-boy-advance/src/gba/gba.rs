#![allow(non_snake_case)]
#![allow(dead_code)]

use alloc::{boxed::Box, format, slice::SliceConcatExt, string::String, vec::Vec};

// Hack so I can "easily" disable a bunch of prints throught the program
macro_rules! println_maybe {
  ($($arg:tt)*) => {{
    let x = format!($($arg)*);
    //dbg!(x);
    //println!($($arg)*);
  }};
}

use super::{cpsr, cpu_mode, instructions, rom};
pub use cpsr::CPSR;
pub use cpu_mode::CpuMode;
use instructions::{arm, thumb};
pub use rom::Rom;
const KB: usize = 1024;
const MB: usize = KB * KB;

const RESET_HANDLER: u32 = 0x0000_0000;
const RESET_UNDEF_HANDLER: u32 = 0x0000_0004;
pub(crate) const SWI_HANDLER: u32 = 0x0000_0008;
const PREFETCH_ABORT_HANDLER: u32 = 0x0000_000C;
const DATA_ABORT_HANDLER: u32 = 0x0000_0010;
const ADDRESS_TOO_BIG_HANDLER: u32 = 0x0000_0014;
const NORMAL_INTERRUPT_HANDLER: u32 = 0x0000_0018;
const FAST_INTERRUPT_HANDLER: u32 = 0x0000_001C;

pub(crate) static mut COUNT: u32 = 0;

pub enum LogLevel {
  None,
  NormalizedEveryInstruction,
  Debug,
}
#[derive(Debug)]
pub enum GBAError {
  ARM(arm::ARMError),
  Thumb(thumb::ThumbError),
  InvalidRom,
}
impl GBAError {
  fn as_string(&self) -> &str {
    match self {
      GBAError::ARM(err) => err,
      GBAError::Thumb(err) => err,
      GBAError::InvalidRom => "Invalid ROM",
    }
  }
}
impl core::fmt::Display for GBAError {
  fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
    write!(f, "{}", self.as_string())
  }
}
#[cfg(feature = "std")]
impl std::error::Error for GBAError {
  fn description(&self) -> &str {
    self.as_string()
  }
}
pub struct GBA {
  //// Output image
  output_texture: [u32; 240 * 160],
  /// Current registers. Swapped on mode change acordingly.
  pub(crate) regs: [u32; 16],
  /// FIQ-only banked registers.
  ///
  /// Swapped on mode change acordingly.
  pub(crate) fiq_only_banks: [[u32; 5]; 2],
  /// Mode specific register banks.
  ///
  /// Swapped on mode change acordingly.
  pub(crate) all_modes_banks: [[u32; 2]; 6],
  /// Current cpsr
  ///
  /// Swapped on mode change acordingly.
  pub(crate) cpsr: CPSR,
  /// Mode specific spsrs
  pub(crate) spsrs: [CPSR; 5],
  /// ROM contents
  game_pak: Box<[u8; 32 * MB]>,
  game_pak_sram: [u8; 64 * KB],
  bios_rom: [u8; 16 * KB],
  wram_board: [u8; 256 * KB],
  wram_chip: [u8; 32 * KB],
  palette_ram: [u8; KB],
  vram: [u8; 96 * KB],
  oam: [u8; KB],
  io_mem: [u8; 1022],
  pub(crate) clocks: u32,
  instruction_hook: fn(&mut GBA),
  pub(crate) instruction_hook_with_opcode: fn(&mut GBA, u32),
  loaded_rom: Option<Rom>,
  print_fn: Option<fn(&str) -> ()>,
}
impl GBA {
  pub fn new(
    log_level: LogLevel,
    bios_file: core::option::Option<&[u8]>,
    print_fn: Option<fn(&str) -> ()>,
  ) -> Box<Self> {
    fn nothing(_: &mut GBA) {}
    fn nothing_with_opcode(_: &mut GBA, _: u32) {}
    fn print_opcode(gba: &mut GBA, opcode: u32) {
      gba.print_fn.map(|f| f(format!("opcode {}:{:x}", unsafe { COUNT }, opcode).as_str()));
    }
    fn print_state_normalized(gba: &mut GBA) {
      gba.print_fn.map(|f| f(format!("{}", gba.state_as_string_without_pc()).as_str()));
    }
    fn print_state(gba: &mut GBA) {
      gba.print_fn.map(|f| f(format!("{}", gba.state_as_string()).as_str()));
    }
    // Was still getting stack overflows without Box X
    let mut gba = box GBA {
      output_texture: [0xFF00_0000u32; 240 * 160],
      regs: [0u32; 16],
      fiq_only_banks: [[0u32; 5]; 2],
      all_modes_banks: [[0u32; 2]; 6],
      spsrs: [CPSR(0u32); 5],
      cpsr: CPSR(0u32),
      bios_rom: [0u8; 16 * KB],
      wram_board: [0u8; 256 * KB],
      wram_chip: [0u8; 32 * KB],
      palette_ram: [0u8; KB],
      vram: [0u8; 96 * KB],
      oam: [0u8; KB],
      io_mem: [0u8; 1022],
      game_pak: box [0u8; 32 * MB],
      game_pak_sram: [0u8; 64 * KB],
      clocks: 0u32,
      loaded_rom: None,
      instruction_hook: match log_level {
        LogLevel::Debug => print_state,
        LogLevel::NormalizedEveryInstruction => print_state_normalized,
        LogLevel::None => nothing,
      },
      instruction_hook_with_opcode: match log_level {
        LogLevel::Debug => print_opcode,
        LogLevel::NormalizedEveryInstruction => print_opcode,
        LogLevel::None => nothing_with_opcode,
      },
      print_fn: print_fn,
    };
    gba.reset();
    let bios_file = bios_file.unwrap_or(include_bytes!("gba_bios.bin"));
    gba.bios_rom.clone_from_slice(bios_file);
    gba
  }
  pub(crate) fn get_spsr_mut(&mut self) -> Option<&mut CPSR> {
    let mode = self.cpsr.mode();
    if mode == CpuMode::Privileged || mode == CpuMode::User {
      None
    } else {
      Some(&mut self.spsrs[self.cpsr.mode().as_usize() - 1])
    }
  }
  pub(crate) fn set_mode(&mut self, new_mode: CpuMode) {
    let old_mode = self.cpsr.mode();
    if new_mode == old_mode {
      return;
    }
    self.cpsr.set_mode(new_mode);
    // FIQ mode banks some extra registers
    if new_mode == CpuMode::FIQ {
      let mut set_fiq = |i| {
        self.fiq_only_banks[0][i] = self.regs[8 + i];
        self.regs[8 + i] = self.fiq_only_banks[1][i];
      };
      for i in 0..5 {
        set_fiq(i);
      }
    } else if old_mode == CpuMode::FIQ {
      let mut unset_fiq = |i| {
        self.fiq_only_banks[1][i] = self.regs[8 + i];
        self.regs[8 + i] = self.fiq_only_banks[0][i];
      };
      for i in 0..5 {
        unset_fiq(i);
      }
    }
    let new_idx = new_mode.as_usize();
    let old_idx = old_mode.as_usize();
    // TODO:
    // core::mem::swap(x: &mut T, y: &mut T); // Can I use mem swap here?
    self.all_modes_banks[old_idx][0] = self.regs[13];
    self.all_modes_banks[old_idx][1] = self.regs[14];
    self.regs[13] = self.all_modes_banks[new_idx][0];
    self.regs[14] = self.all_modes_banks[new_idx][1];
  }
  pub(crate) fn pc(&mut self) -> &mut u32 {
    self.reg_mut(15)
  }
  pub(crate) fn reg_mut(&mut self, reg: usize) -> &mut u32 {
    match reg {
      0..=16 => &mut self.regs[reg],
      _ => unimplemented!("Unaccesible register {}", reg),
    }
  }
  // TODO: Make this not take a &mut
  pub(crate) fn fetch_reg(&mut self, reg: usize) -> u32 {
    *self.reg_mut(reg)
  }
  /// Max valid addressable value is 224Mb
  pub(crate) fn fetch_byte(&self, addr: u32) -> u8 {
    let addr = addr as usize;
    match addr {
      //General Internal Memory
      //BIOS - System ROM (16 KBytes) E3A02004
      0x0000_0000..=0x0000_3FFF => self.bios_rom[addr],
      //WRAM - On-board Work RAM  (256 KBytes) 2 Wait
      0x0200_0000..=0x0203_FFFF => self.wram_board[addr - 0x0200_0000],
      //WRAM - On-chip Work RAM   (32 KBytes) (Mirrored until 0x0400_0000)
      0x0300_0000..=0x0300_7FFF => self.wram_chip[addr - 0x0300_0000],
      0x0300_8000..=0x03FF_FFFF => self.wram_chip[(addr - 0x0300_8000) % 0x0000_7FFF],
      //I/O Registers             (1022 Bytes)
      0x0400_0000..=0x0400_03FE => self.io_mem[addr - 0x0400_0000], //TODO:IO
      0x0400_4000..=0x04FF_FFFF => {
        self.print_fn.map(|f| f(format!("Bad io read at {:x}", addr).as_str()));
        0
      }
      //Internal Display Memory
      0x0500_0000..=0x0500_03FF => self.palette_ram[addr - 0x0500_0000], /* BG/OBJ Palette RAM        (1 Kbyte) */
      0x0600_0000..=0x0601_7FFF => self.vram[addr - 0x0600_0000], /* VRAM - Video RAM          (96 KBytes) */
      0x0700_0000..=0x0700_03FF => self.oam[addr - 0x0700_0000], /* OAM - OBJ Attributes      (1 Kbyte) */
      //External Memory (Game Pak)
      // TODO: Wait states
      //0x0200 0000
      0x0800_0000..=0x09FF_FFFF => self.game_pak[addr - 0x0800_0000], /* Game Pak ROM/FlashROM (max 32MB) - Wait State 0 */
      0x0A00_0000..=0x0BFF_FFFF => self.game_pak[addr - 0x0A00_0000], /* Game Pak ROM/FlashROM (max 32MB) - Wait State 1 */
      0x0C00_0000..=0x0DFF_FFFF => self.game_pak[addr - 0x0C00_0000], /* Game Pak ROM/FlashROM (max 32MB) - Wait State 2 */
      0x0E00_0000..=0x0E00_FFFF => self.game_pak[addr - 0x0E00_0000], /* Game Pak SRAM    (max 64 KBytes) - 8bit Bus width */
      //_ => unimplemented!("Invalid address: {:x}", addr),
      _ => 0u8,
    }
  }
  /// The GBA GBA always runs in LE mode
  pub(crate) fn write_u8(&mut self, addr: u32, byte: u8) {
    let addr = addr as usize;
    match addr {
      //General Internal Memory
      //BIOS - System ROM (16 KBytes) E3A02004
      0x0000_0000..=0x0000_3FFF => {
        self.bios_rom[addr] = byte;
      }
      //WRAM - On-board Work RAM  (256 KBytes) 2 Wait
      0x0200_0000..=0x0203_FFFF => {
        self.wram_board[addr & 0x0003_FFFF] = byte;
      }
      //WRAM - On-chip Work RAM   (32 KBytes) (Mirrored until 0x0400_0000)
      0x0300_0000..=0x03FF_FFFF => {
        self.wram_chip[addr & 0x0000_7FFF] = byte;
      }
      //I/O Registers             (1022 Bytes)0xFFFF
      0x0400_0000..=0x0400_03FE => {
        self.io_mem[addr - 0x0400_0000] = byte;
      }
      0x0400_0400..=0x04FF_FFFF => {
        let addr = addr & 0xFFFF;
        self.io_mem[0] = byte;
      }
      //Internal Display Memory
      0x0500_0000..=0x0500_03FF => {
        self.palette_ram[addr - 0x0500_0000] = byte;
      } /* BG/OBJ Palette RAM        (1 Kbyte) */
      0x0600_0000..=0x0601_7FFF => {
        self.vram[addr - 0x0600_0000] = byte;
      } /* VRAM - Video RAM          (96 KBytes) */
      0x0700_0000..=0x0700_03FF => {
        self.oam[addr - 0x0700_0000] = byte;
      } /* OAM - OBJ Attributes      (1 Kbyte) */
      //External Memory (Game Pak)
      // TODO: Wait states
      0x0800_0000..=0x09FF_FFFF => {
        self.game_pak[addr - 0x0800_0000] = byte;
      } /* Game Pak ROM/FlashROM (max 32MB) - Wait State 0 */
      0x0A00_0000..=0x0BFF_FFFF => {
        self.game_pak[addr - 0x0A00_0000] = byte;
      } /* Game Pak ROM/FlashROM (max 32MB) - Wait State 1 */
      0x0C00_0000..=0x0DFF_FFFF => {
        self.game_pak[addr - 0x0C00_0000] = byte;
      } /* Game Pak ROM/FlashROM (max 32MB) - Wait State 2 */
      0x0E00_0000..=0x0E00_FFFF => {
        self.game_pak[addr - 0x0E00_0000] = byte;
      } /* Game Pak SRAM    (max 64 KBytes) - 8bit Bus width */
      _ => unimplemented!("Invalid address: {:x}", addr),
    }
  }
  pub(crate) fn write_u16(&mut self, addr: u32, value: u16) {
    self.write_u8(addr + 1, (value >> 8) as u8);
    self.write_u8(addr, value as u8);
  }
  pub(crate) fn LR(&mut self) -> &mut u32 {
    self.reg_mut(14)
  }
  pub(crate) fn write_u32(&mut self, addr: u32, value: u32) {
    self.write_u16(addr, value as u16);
    self.write_u16(addr + 2, (value >> 16) as u16);
  }
  pub(crate) fn fetch_u16(&mut self, addr: u32) -> u16 {
    u16::from(self.fetch_byte(addr)) + (u16::from(self.fetch_byte(addr + 1)) << 8)
  }
  pub(crate) fn fetch_u32(&mut self, addr: u32) -> u32 {
    u32::from(self.fetch_u16(addr)) + (u32::from(self.fetch_u16(addr + 2)) << 16)
  }
  pub fn reset(&mut self) {
    self.regs = [0; 16];
    *self.pc() = RESET_HANDLER;
    self.output_texture = [0xFF00_0000u32; 240 * 160];
    self.fiq_only_banks = [[0u32; 5]; 2];
    self.all_modes_banks = [[0u32; 2]; 6];
    self.io_mem = [0u8; 1022];
    self.write_u32(0x0400_0088, 0x200);
    self.spsrs = [CPSR(0x0000_0000F); 5];
    self.all_modes_banks[CpuMode::IRQ.as_usize()][0] = 0x0300_7FA0;
    self.all_modes_banks[CpuMode::Supervisor.as_usize()][0] = 0x0300_7FE0;
    self.cpsr = CPSR(0x0000_001F);
    self.set_mode(CpuMode::Privileged);
    self.wram_board = [0u8; 256 * KB];
    self.wram_chip = [0u8; 32 * KB];
    self.palette_ram = [0u8; KB];
    self.vram = [0u8; 96 * KB];
    self.oam = [0u8; KB];
    self.game_pak = box [0u8; 32 * MB];
    self.game_pak_sram = [0u8; 64 * KB];
    self.clocks = 0u32;
    self.regs[13] = 0x0300_7F00; // Taken from mrgba
  }
  /// Load a ROM from a reader
  pub fn load(&mut self, rom_bytes: &[u8]) -> Result<(), Box<GBAError>> {
    self.reset();
    //TODO: Verify nintendo logo?
    let title = &rom_bytes[0x00A0..0x00AC];
    let title =
      if let Some(idx) = title.iter().position(|x| *x == 0) { &title[0..idx] } else { title };
    let title = String::from_utf8(title.to_vec()).map_err(|_| GBAError::InvalidRom)?;
    let code = &rom_bytes[0x00AC..0x00B0];
    let code = String::from_utf8(code.to_vec()).map_err(|_| GBAError::InvalidRom)?;
    for (input, out) in rom_bytes.iter().zip(self.game_pak.iter_mut()) {
      *out = *input;
    }
    self.loaded_rom = Some(Rom::new(Vec::from(rom_bytes), title, code));
    Ok(())
  }
  pub(crate) fn thumb(&mut self) -> bool {
    self.cpsr.thumb_state_flag()
  }
  pub fn state_as_string_without_pc(&mut self) -> String {
    format!(
      "{}\n{}",
      self
        .regs
        .iter()
        .enumerate()
        .take(15)
        .map(|(idx, val)| format!("r{}:{:x}", idx, val))
        .collect::<Vec<_>>()
        .join(" "),
      self.cpsr
    )
  }
  pub fn state_as_string(&mut self) -> String {
    format!(
      "{}\n{}",
      self
        .regs
        .iter()
        .enumerate()
        .map(|(idx, val)| format!("r{}:{:x}", idx, val))
        .collect::<Vec<_>>()
        .join(" "),
      self.cpsr
    )
  }
  pub fn run_one_instruction(&mut self) -> Result<(), GBAError> {
    (self.instruction_hook)(self);
    if self.thumb() {
      thumb::execute_one_instruction(self).map_err(GBAError::Thumb)?;
    } else {
      arm::execute_one_instruction(self).map_err(GBAError::ARM)?;
    }
    unsafe {
      COUNT += 1;
    }
    Ok(())
  }
  pub fn run_one_frame(&mut self) -> Result<(), GBAError> {
    // TODO: However this is meant to be done
    for _ in 0..100 {
      self.run_one_instruction()?;
    }
    Ok(())
  }
  pub fn run_forever(&mut self) -> Result<(), GBAError> {
    loop {
      self.run_one_frame()?;
    }
  }
  pub(crate) fn vblank(&mut self) -> u32 {
    self.fetch_u32(0x0300_7FFC)
  }
  pub fn video_output(&self) -> &[u32] {
    &self.output_texture[..]
  }
  pub fn loaded_rom(&self) -> Option<&Rom> {
    self.loaded_rom.as_ref()
  }
  pub fn registers(&self) -> [u32; 16] {
    self.regs
  }
  pub fn cpsr(&self) -> CPSR {
    self.cpsr
  }
  pub fn bios_bytes(&self) -> &[u8] {
    &self.bios_rom[..]
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
