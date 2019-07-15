#![allow(dead_code)]

use alloc::{boxed::Box, format, string::String, vec::Vec};

use super::{cpsr, cpu_mode, instructions, rom};
use instructions::{arm, thumb};

use cpsr::CPSR;
use cpu_mode::CpuMode;
use rom::Rom;

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

const VIDEO_HEIGHT: usize = 160;
const VIDEO_WIDTH: usize = 240;

// Numbers taken from https://rust-console.github.io/gba/io-registers.html#vcount-vertical-display-counter
const NUM_VIRTUAL_HORIZONTAL_PIXELS: u16 = 68;
const NUM_REAL_HORIZONTAL_PIXELS: u16 = VIDEO_WIDTH as u16;
const NUM_HORIZONTAL_PIXELS: u16 = NUM_VIRTUAL_HORIZONTAL_PIXELS + NUM_REAL_HORIZONTAL_PIXELS;
const CLOCKS_PER_PIXEL: u32 = 4;
const CLOCKS_PER_SCANLINE: u32 = (NUM_HORIZONTAL_PIXELS as u32) * CLOCKS_PER_PIXEL;
const NUM_VIRTUAL_SCANLINES: u16 = 68;
const NUM_REAL_SCANLINES: u16 = VIDEO_HEIGHT as u16;
const NUM_SCANLINES: u16 = NUM_REAL_SCANLINES + NUM_VIRTUAL_SCANLINES;
const CLOCKS_PER_FRAME: u32 = (NUM_SCANLINES as u32) * CLOCKS_PER_SCANLINE;

#[derive(PartialEq)]
pub enum LogLevel {
  None,
  EveryInstruction,
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
  pub(crate) output_texture: [u8; VIDEO_WIDTH * VIDEO_HEIGHT * 3],
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
  pub(crate) game_pak: Box<[u8; 32 * MB]>,
  pub(crate) game_pak_sram: [u8; 64 * KB],
  pub(crate) bios_rom: [u8; 16 * KB],
  pub(crate) wram_board: [u8; 256 * KB],
  pub(crate) wram_chip: [u8; 32 * KB],
  pub(crate) palette_ram: [u8; KB],
  pub(crate) vram: [u8; 96 * KB],
  pub(crate) oam: [u8; KB],
  pub(crate) io_mem: [u8; 1022],
  pub(crate) clocks: u32,
  instruction_hook: fn(&mut GBA),
  pub(crate) instruction_hook_with_opcode: fn(&mut GBA, u32),
  pub(crate) loaded_rom: Option<Rom>,
  pub(crate) print_fn: Option<fn(&str) -> ()>,
  pub(crate) debug_print_fn: Option<fn(&str) -> ()>,
  pub(crate) executed_instructions_count: u64,
}
impl GBA {
  pub(crate) fn new(
    log_level: LogLevel,
    bios_file: core::option::Option<&[u8]>,
    print_fn: Option<fn(&str) -> ()>,
  ) -> Box<Self> {
    let nothing = |_: &mut GBA| {};
    let nothing_with_opcode = |_: &mut GBA, _: u32| {};
    let print_opcode = |gba: &mut GBA, opcode: u32| {
      gba
        .print_fn
        .map(|f| f(format!("opcode {}:{:x}\n", gba.executed_instructions_count, opcode).as_str()));
    };
    let print_state = |gba: &mut GBA| {
      gba.print_fn.map(|f| f(format!("{}\n", gba.state_as_string()).as_str()));
    };
    // Was still getting stack overflows without Box X
    let mut gba = box GBA {
      output_texture: [0x00u8; VIDEO_WIDTH * VIDEO_HEIGHT * 3],
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
        LogLevel::Debug | LogLevel::EveryInstruction => print_state,
        LogLevel::None => nothing,
      },
      instruction_hook_with_opcode: match log_level {
        LogLevel::Debug => print_opcode,
        LogLevel::EveryInstruction => print_opcode,
        LogLevel::None => nothing_with_opcode,
      },
      print_fn: print_fn,
      debug_print_fn: if log_level == LogLevel::Debug { print_fn } else { None },
      executed_instructions_count: 0,
    };
    gba.reset();
    //let bios_file = bios_file.unwrap_or(include_bytes!("gba_bios.bin"));
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
  /// Max valid addressable value is 224Mb
  pub(crate) fn fetch_byte(&self, addr: u32) -> u8 {
    let addr = addr as usize;
    match addr {
      // General Internal Memory
      // BIOS - System ROM (16 KBytes) E3A02004
      0x0000_0000..=0x0000_3FFF => self.bios_rom[addr],
      // WRAM - On-board Work RAM  (256 KBytes) 2 Wait
      0x0200_0000..=0x0203_FFFF => self.wram_board[addr - 0x0200_0000],
      // WRAM - On-chip Work RAM   (32 KBytes) (Mirrored until 0x0400_0000)
      0x0300_0000..=0x03FF_FFFF => self.wram_chip[addr & 0x0000_7FFF],
      // I/O Registers             (1022 Bytes)
      0x0400_0000..=0x0400_03FD => self.io_mem[addr - 0x0400_0000], // TODO:IO
      0x0400_03FE..=0x04FF_FFFF => {
        self.debug_print_fn.map(|f| f(format!("Bad I/O read at {:x}", addr).as_str()));
        0
      }
      // Internal Display Memory
      // BG/OBJ Palette RAM        (1 Kbyte)
      0x0500_0000..=0x0500_03FF => self.palette_ram[addr - 0x0500_0000],
      // VRAM - Video RAM          (96 KBytes)
      0x0600_0000..=0x0601_7FFF => self.vram[addr - 0x0600_0000],
      // OAM - OBJ Attributes      (1 Kbyte)
      0x0700_0000..=0x0700_03FF => self.oam[addr - 0x0700_0000],
      // External Memory (Game Pak)
      // TODO: Wait states
      // Game Pak ROM/FlashROM (max 32MB) - Wait State 0
      0x0800_0000..=0x09FF_FFFF => {
        let addr = addr - 0x0800_0000;
        if addr <= 0x00FF_FFFF {
          self.game_pak[addr]
        } else {
          // Out of bounds ROM access (Behaviour taken from MGBA)
          ((addr >> 1) & 0x0000_00FF) as u8
        }
      }
      // Game Pak ROM/FlashROM (max 32MB) - Wait State 1
      0x0A00_0000..=0x0BFF_FFFF => {
        let addr = addr - 0x0A00_0000;
        if addr <= 0x00FF_FFFF { self.game_pak[addr] } else { ((addr >> 1) & 0x0000_00FF) as u8 }
      }
      // Game Pak ROM/FlashROM (max 32MB) - Wait State 2
      0x0C00_0000..=0x0DFF_FFFF => {
        let addr = addr - 0x0C00_0000;
        if addr <= 0x00FF_FFFF { self.game_pak[addr] } else { ((addr >> 1) & 0x0000_00FF) as u8 }
      }
      0x0E00_0000..=0x0E00_FFFF => self.game_pak[addr & 0x01FF_FFFF],
      // Game Pak SRAM    (max 64 KBytes) - 8bit Bus width
      _ => 0u8, //  unimplemented!("Invalid address: {:x}", addr)
    }
  }
  pub(crate) fn write_u16(&mut self, addr: u32, value: u16) {
    self.write_u8(addr, value as u8);
    self.write_u8(addr + 1, (value >> 8) as u8);
  }
  pub(crate) fn write_u32(&mut self, addr: u32, value: u32) {
    self.write_u16(addr, value as u16);
    self.write_u16(addr + 2, (value >> 16) as u16);
  }
  /// The GBA GBA always runs in LE mode
  pub(crate) fn write_u8(&mut self, addr: u32, byte: u8) {
    self
      .debug_print_fn
      .map(|f| f(format!("\twriting to addr {:x} value {:x}\n", addr, byte).as_str()));
    let addr = addr as usize;
    match addr {
      // General Internal Memory
      // BIOS - System ROM (16 KBytes) E3A02004
      0x0000_0000..=0x0000_3FFF => {
        self.bios_rom[addr] = byte;
      }
      // WRAM - On-board Work RAM  (256 KBytes) 2 Wait
      0x0200_0000..=0x0203_FFFF => {
        self.wram_board[addr & 0x0003_FFFF] = byte;
      }
      // WRAM - On-chip Work RAM   (32 KBytes) (Mirrored until 0x0400_0000)
      0x0300_0000..=0x03FF_FFFF => {
        self.wram_chip[addr & 0x0000_7FFF] = byte;
      }
      // I/O Registers             (1022 Bytes)0xFFFF
      0x0400_0000..=0x0400_03FE => {
        self.io_mem[addr - 0x0400_0000] = byte;
      }
      0x0400_0400..=0x04FF_FFFF => {
        let _addr = addr & 0xFFFF;
        self.io_mem[0] = byte;
      }
      // Internal Display Memory
      // BG/OBJ Palette RAM        (1 Kbyte)
      0x0500_0000..=0x0500_03FF => {
        self.palette_ram[addr - 0x0500_0000] = byte;
      }
      // VRAM - Video RAM          (96 KBytes)
      0x0600_0000..=0x0601_7FFF => {
        self.vram[addr - 0x0600_0000] = byte;
      }
      // OAM - OBJ Attributes      (1 Kbyte)
      0x0700_0000..=0x0700_03FF => {
        self.oam[addr - 0x0700_0000] = byte;
      }
      // External Memory (Game Pak)
      // TODO: Wait states
      0x0800_0000..=0x09FF_FFFF => {
        self.game_pak[addr - 0x0800_0000] = byte;
      }
      // Game Pak ROM/FlashROM (max 32MB) - Wait State 0
      0x0A00_0000..=0x0BFF_FFFF => {
        self.game_pak[addr & 0x01FF_FFFF] = byte;
      }
      // Game Pak ROM/FlashROM (max 32MB) - Wait State 1
      0x0C00_0000..=0x0DFF_FFFF => {
        self.game_pak[addr & 0x01FF_FFFF] = byte;
      }
      // Game Pak ROM/FlashROM (mx 32MB) - Wait State 2
      0x0E00_0000..=0x0E00_FFFF => {
        self.game_pak[addr & 0x01FF_FFFF] = byte;
      }
      // Game Pak SRAM    (max 64 KBytes) - 8bit Bus width
      _ => (), //unimplemented!("Invalid address: {:x}", addr),
    }
  }
  pub(crate) fn fetch_u16(&mut self, addr: u32) -> u16 {
    match addr {
      // ROM out of bounds access (Behaviour taken from MGBA)
      0x08FF_FFFF..=0x09FF_FFFF => ((addr >> 1) & 0x0000_FFFF) as u16,
      0x0AFF_FFFF..=0x0BFF_FFFF => ((addr >> 1) & 0x0000_FFFF) as u16,
      0x0CFF_FFFF..=0x0DFF_FFFF => ((addr >> 1) & 0x0000_FFFF) as u16,
      _ => u16::from(self.fetch_byte(addr)) + (u16::from(self.fetch_byte(addr + 1)) << 8),
    }
  }
  pub(crate) fn fetch_u32(&mut self, addr: u32) -> u32 {
    match addr {
      // ROM out of bounds access (Behaviour taken from MGBA)
      0x08FF_FFFF..=0x09FF_FFFF => ((addr >> 1) & 0x0000_FFFF) as u32,
      0x0AFF_FFFF..=0x0BFF_FFFF => ((addr >> 1) & 0x0000_FFFF) as u32,
      0x0CFF_FFFF..=0x0DFF_FFFF => ((addr >> 1) & 0x0000_FFFF) as u32,
      _ => u32::from(self.fetch_u16(addr)) + (u32::from(self.fetch_u16(addr + 2)) << 16),
    }
  }
  pub fn reset(&mut self) {
    self.executed_instructions_count = 0;
    self.regs = [0; 16];
    self.output_texture = [0x00u8; VIDEO_WIDTH * VIDEO_HEIGHT * 3];
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
    self.write_u16(0x400_0130, 0b0000_0011_1111_1111); // KEYINPUT register. 1's denote "not pressed"
    self.regs[13] = 0x0300_7F00; // Taken from mgba
    self.regs[15] = RESET_HANDLER;
    self.regs[15] = 0x0800_0000; //  To skip BIOS boot code
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
  pub fn state_as_string(&mut self) -> String {
    format!(
      "{}\n{}",
      self
        .regs
        .iter()
        .enumerate()
        .map(|(idx, val)| format!(
          "r{}:{:x}",
          idx,
          if idx == 15 { val + if self.cpsr.thumb_state_flag() { 2 } else { 4 } } else { *val }
        ))
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
    self.executed_instructions_count += 1;
    self.update_hvblank();
    Ok(())
  }
  fn update_hvblank(&mut self) {
    let column_offset = ((self.clocks / CLOCKS_PER_PIXEL) as u16) % NUM_HORIZONTAL_PIXELS;
    let row_offset = ((self.clocks / CLOCKS_PER_SCANLINE) as u16) % NUM_SCANLINES;

    let hblank = column_offset >= NUM_REAL_HORIZONTAL_PIXELS;
    let vblank = row_offset >= NUM_REAL_SCANLINES;

    let prev_status = self.fetch_u16(0x0400_0004);
    //let prev_vcount = self.fetch_u16(0x0400_0006);

    let vcount_setting = (prev_status & 0xFF00) >> 8;
    let vcounter_flag = vcount_setting == row_offset;

    let new_status = (prev_status & 0b1111_1111_1111_1000)
      | if vcounter_flag { 1 << 2 } else { 0 }
      | if hblank { 1 << 1 } else { 0 }
      | if vblank { 1 } else { 0 };

    self.write_u16(0x0400_0006, row_offset & 0xFF);
    self.write_u16(0x0400_0004, new_status);
  }
  pub fn run_one_frame(&mut self) -> Result<(), GBAError> {
    while self.clocks < CLOCKS_PER_FRAME {
      //for _ in 1..1000 {
      self.run_one_instruction()?;
    }
    self.clocks -= CLOCKS_PER_FRAME;
    self.update_video_output();
    Ok(())
  }
  /// Fills three bytes starting at the given index in the given slice with the RGB values of the given
  /// 16 bit GBA color. See http://problemkaputt.de/gbatek.htm#lcdcolorpalettes for details.
  /// (the intensities 0-14 are practically all black, and only intensities 15-31 are resulting in visible medium.)
  #[inline]
  fn fill_output_color(output_texture: &mut [u8], idx: usize, color: u16) {
    let r = ((color & 0x001F) >> 0) as u8;
    let g = ((color & 0x03E0) >> 5) as u8;
    let b = ((color & 0xFC00) >> 10) as u8;
    output_texture[(idx * 3) + 0] = (std::cmp::max(r, 14) - 14).overflowing_mul(15).0;
    output_texture[(idx * 3) + 1] = (std::cmp::max(g, 14) - 14).overflowing_mul(15).0;
    output_texture[(idx * 3) + 2] = (std::cmp::max(b, 14) - 14).overflowing_mul(15).0;
  }
  /// Fill the field output_texture with RGB values. See http://problemkaputt.de/gbatek.htm#gbalcdvideocontroller
  /// for details.
  pub(crate) fn update_video_output(&mut self) {
    let bg_mode = self.io_mem[0] & 0x0000_0007;
    match bg_mode {
      0 => (),
      1 => (),
      2 => (),
      3 => {
        // 16 bit color bitmap. One frame buffer (240x160 pixels, 32768 colors)
        for (idx, slice) in self.vram.chunks_exact(2).take(VIDEO_WIDTH * VIDEO_HEIGHT).enumerate() {
          if let &[high_byte, low_byte] = slice {
            let color = (low_byte as u16) + ((high_byte as u16) << 8);
            Self::fill_output_color(&mut self.output_texture[..], idx, color);
          }
        }
      }
      4 => {
        // 8 bit palette indexed bitmap. Two frame buffers (240x160 pixels, 256 colors)
        let second_frame = (self.io_mem[0] & 0x0000_0008) != 0;
        for (idx, palette_idx) in self
          .vram
          .iter()
          .skip(if second_frame { VIDEO_WIDTH * VIDEO_HEIGHT } else { 0 })
          .take(VIDEO_WIDTH * VIDEO_HEIGHT)
          .enumerate()
        {
          let palette_idx = (*palette_idx as usize) * 2;
          let color = (self.palette_ram[palette_idx] as u16)
            + ((self.palette_ram[palette_idx + 1] as u16) << 8);
          Self::fill_output_color(&mut self.output_texture[..], idx, color);
        }
      }
      5 => {
        // 16 bit color bitmap. Two frame buffers (160x128 pixels, 32768 colors)
        ()
      }
      _ => (),
    }
  }
  pub fn run_forever(&mut self) -> Result<(), GBAError> {
    loop {
      self.run_one_frame()?;
    }
  }
  #[inline]
  pub(crate) fn sequential_cycle(&self) -> u32 {
    2
  }
  #[inline]
  pub(crate) fn nonsequential_cycle(&self) -> u32 {
    2
  }
  #[inline]
  pub(crate) fn internal_cycle(&self) -> u32 {
    1
  }
}
