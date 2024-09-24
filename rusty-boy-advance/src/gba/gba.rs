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

/// See [`GBAButton`].bit for button->bit mappings
const KEY_STATUS_REG: u32 = 0x0400_0130;
/// Display control (http://problemkaputt.de/gbatek.htm#lcdiodisplaycontrol)
const DISPCNT_ADDR: u32 = 0x0400_0000;
/// Display status (http://problemkaputt.de/gbatek.htm#lcdiointerruptsandstatus)
const DISPSTAT_ADDR: u32 = 0x0400_0004;
/// Scanline(Vertical) counter (http://problemkaputt.de/gbatek.htm#lcdiointerruptsandstatus)
const VCOUNT_ADDR: u32 = 0x0400_0006;

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

const VBLANK_IE_BIT: u32 = 0;
const HBLANK_IE_BIT: u32 = 1;
const VCOUNTER_IE_BIT: u32 = 2;
const TIMER0_IE_BIT: u32 = 3;
const TIMER1_IE_BIT: u32 = 4;
const TIMER2_IE_BIT: u32 = 5;
const TIMER3_IE_BIT: u32 = 6;
const SERIAL_IE_BIT: u32 = 7;
const DMA0_IE_BIT: u32 = 8;
const DMA1_IE_BIT: u32 = 9;
const DMA2_IE_BIT: u32 = 10;
const DMA3_IE_BIT: u32 = 11;
const KEYPAD_IE_BIT: u32 = 12;
const GAMEPAK_IE_BIT: u32 = 13;

#[derive(Debug, Copy, Clone)]
pub enum GBAButton {
  ButtonA,
  ButtonB,
  Select,
  Start,
  Right,
  Left,
  Up,
  Down,
  ButtonR,
  ButtonL,
}

impl GBAButton {
  fn bit(self) -> u16 {
    match self {
      GBAButton::ButtonA => 1,
      GBAButton::ButtonB => 1 << 1,
      GBAButton::Select => 1 << 2,
      GBAButton::Start => 1 << 3,
      GBAButton::Right => 1 << 4,
      GBAButton::Left => 1 << 5,
      GBAButton::Up => 1 << 6,
      GBAButton::Down => 1 << 7,
      GBAButton::ButtonR => 1 << 8,
      GBAButton::ButtonL => 1 << 9,
    }
  }
}

#[derive(PartialEq, Debug, Copy, Clone)]
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
  instruction_hook: Option<fn(&mut GBA)>,
  pub(crate) instruction_hook_with_opcode: Option<fn(&mut GBA, u32)>,
  pub(crate) loaded_rom: Option<Rom>,
  pub(crate) print_fn: Option<fn(&str) -> ()>,
  pub(crate) debug_print_fn: Option<fn(&str) -> ()>,
  pub(crate) executed_instructions_count: u64,
  persistent_input_bitmask: u16,
}

impl GBA {
  pub(crate) fn new(
    log_level: LogLevel,
    bios_file: core::option::Option<&[u8]>,
    print_fn: Option<fn(&str) -> ()>,
  ) -> Box<Self> {
    // Was still getting stack overflows without Box X
    let mut gba = Box::new(Self {
      output_texture: [0x00_u8; VIDEO_WIDTH * VIDEO_HEIGHT * 3],
      regs: [0_u32; 16],
      fiq_only_banks: [[0_u32; 5]; 2],
      all_modes_banks: [[0_u32; 2]; 6],
      spsrs: [CPSR(0_u32); 5],
      cpsr: CPSR(0_u32),
      bios_rom: [0_u8; 16 * KB],
      wram_board: [0_u8; 256 * KB],
      wram_chip: [0_u8; 32 * KB],
      palette_ram: [0_u8; KB],
      vram: [0_u8; 96 * KB],
      oam: [0_u8; KB],
      io_mem: [0_u8; 1022],
      game_pak: Box::new([0_u8; 32 * MB]),
      game_pak_sram: [0_u8; 64 * KB],
      clocks: 0_u32,
      loaded_rom: None,
      instruction_hook: None,
      instruction_hook_with_opcode: None,
      debug_print_fn: None,
      print_fn,
      executed_instructions_count: 0,
      persistent_input_bitmask: 0x000,
    });

    gba.reset();

    //let bios_file = bios_file.unwrap_or(include_bytes!("gba_bios.bin"));
    let bios_file = bios_file.unwrap_or(include_bytes!("gba_bios.bin"));
    gba.bios_rom.clone_from_slice(bios_file);

    gba.set_log_level(log_level);

    gba
  }

  pub(crate) fn set_log_level(&mut self, log_level: LogLevel) {
    const PRINT_OPCODE: fn(&mut GBA, u32) = |gba: &mut GBA, opcode: u32| {
      if let Some(f) = gba.print_fn {
        f(format!("opcode {}:{:x}\n", gba.executed_instructions_count, opcode).as_str());
      }
    };
    const PRINT_STATE: fn(&mut GBA) = |gba: &mut GBA| {
      if let Some(f) = gba.print_fn {
        f(format!("{}\n", gba.state_as_string()).as_str());
      }
    };

    self.instruction_hook = match log_level {
      LogLevel::Debug | LogLevel::EveryInstruction => Some(PRINT_STATE),
      LogLevel::None => None,
    };
    self.instruction_hook_with_opcode = match log_level {
      LogLevel::Debug | LogLevel::EveryInstruction => Some(PRINT_OPCODE),
      LogLevel::None => None,
    };
    self.debug_print_fn = if log_level == LogLevel::Debug { self.print_fn } else { None };
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
  #[inline]
  fn fetch_byte_inline(&self, addr: u32) -> u8 {
    match addr >> 24 {
      // General Internal Memory
      // BIOS - System ROM (16 KBytes) E3A02004
      0x00 => self.bios_rom[(addr & 0x0000_3FFF) as usize],
      // WRAM - On-board Work RAM  (256 KBytes) 2 Wait
      0x02 => self.wram_board[(addr & 0x00FF_FFFF) as usize],
      // WRAM - On-chip Work RAM   (32 KBytes) (Mirrored until 0x0400_0000)
      0x03 => self.wram_chip[(addr & 0x0000_7FFF) as usize],
      // I/O Registers             (1022 Bytes)
      0x04 => match addr {
        0x0400_0000..=0x0400_03FD => self.io_mem[(addr & 0x00FF_FFFF) as usize],
        _ => {
          if let Some(f) = self.debug_print_fn {
            f(format!("Bad I/O read at {:x}", addr).as_str());
          }
          0
        }
      },
      // Internal Display Memory
      // BG/OBJ Palette RAM        (1 Kbyte)
      0x05 => self.palette_ram[(addr & 0x0000_03FF) as usize],
      // VRAM - Video RAM          (96 KBytes)
      //0x0600_0000..=0x0601_7FFF => self.vram[addr - 0x0600_0000],
      0x06 => self.vram[(addr & 0x00FF_FFFF) as usize],
      // OAM - OBJ Attributes      (1 Kbyte)
      0x07 => self.oam[(addr & 0x0000_03FF) as usize],
      // External Memory (Game Pak)
      // TODO: Wait states
      // Game Pak ROM/FlashROM (max 32MB) - Wait State 0
      0x08 => self.game_pak[(addr & 0x00FF_FFFF) as usize],
      // Game Pak ROM/FlashROM (max 32MB) - Wait State 1
      0x0A => self.game_pak[(addr & 0x00FF_FFFF) as usize],
      // Game Pak ROM/FlashROM (max 32MB) - Wait State 2
      0x0C => self.game_pak[(addr & 0x00FF_FFFF) as usize],
      // Out of bounds ROM access (Behaviour taken from MGBA)
      0x0B | 0x0D | 0x09 => self.game_pak[(addr & 0x01FF_FFFF) as usize],
      // Game Pak SRAM    (max 64 KBytes) - 8bit Bus width (TODO)
      _ => 0_u8, //  unimplemented!("Invalid address: {:x}", addr)
    }
  }

  pub(crate) fn fetch_byte(&self, addr: u32) -> u8 {
    self.fetch_byte_inline(addr)
  }

  #[inline]
  pub(crate) fn write_u16_inline(&mut self, addr: u32, value: u16) {
    self.write_u8_inline(addr, value as u8);
    self.write_u8_inline(addr + 1, (value >> 8) as u8);
  }

  pub(crate) fn write_u16(&mut self, addr: u32, value: u16) {
    self.write_u16_inline(addr, value);
    if addr & 0xFFFF_FF00 == 0x0400_0000 && (value & 0x0000_8000) != 0 {
      match addr {
        0x0400_00BA => self.do_dma(0x0400_00B0), // DMA 0
        0x0400_00C6 => self.do_dma(0x0400_00BC), // DMA 1
        0x0400_00D2 => self.do_dma(0x0400_00C8), // DMA 2
        0x0400_00DE => self.do_dma(0x0400_00D4), // DMA 3
        _ => (),
      }
    }
  }

  pub(crate) fn write_u32(&mut self, addr: u32, value: u32) {
    self.write_u16_inline(addr, value as u16);
    self.write_u16_inline(addr + 2, (value >> 16) as u16);
    if addr & 0xFFFF_FF00 == 0x0400_0000 && (value & 0x0000_8000) != 0 {
      match addr {
        0x0400_00BA => self.do_dma(0x0400_00B0), // DMA 0
        0x0400_00C6 => self.do_dma(0x0400_00BC), // DMA 1
        0x0400_00D2 => self.do_dma(0x0400_00C8), // DMA 2
        0x0400_00DE => self.do_dma(0x0400_00D4), // DMA 3
        _ => (),
      }
    }
  }

  /// The GBA GBA always runs in LE mode
  #[inline]
  fn write_u8_inline(&mut self, addr: u32, byte: u8) {
    match addr >> 24 {
      // General Internal Memory
      // BIOS - System ROM (16 KBytes) E3A02004
      0x00 => {
        self.bios_rom[(addr & 0x0000_3FFF) as usize] = byte;
      }
      // WRAM - On-board Work RAM  (256 KBytes) 2 Wait
      0x02 => {
        self.wram_board[(addr & 0x00FF_FFFF) as usize] = byte;
      }
      // WRAM - On-chip Work RAM   (32 KBytes) (Mirrored until 0x0400_0000)
      0x03 => {
        self.wram_chip[(addr & 0x0000_7FFF) as usize] = byte;
      }
      // I/O Registers             (1022 Bytes)0xFFFF
      0x04 => {
        if addr < (0x0400_0000 + 1022) {
          self.io_mem[(addr & 0x00FF_FFFF) as usize] = byte;
        } else {
          self.io_mem[0] = byte;
        }
      }
      // Internal Display Memory
      // BG/OBJ Palette RAM        (1 Kbyte)
      0x05 => {
        if addr < 0x0500_03FF {
          self.palette_ram[(addr & 0x00FF_FFFF) as usize] = byte;
        }
      }
      // VRAM - Video RAM          (96 KBytes)
      0x06 => {
        self.vram[(addr & 0x00FF_FFFF) as usize] = byte;
      }
      // OAM - OBJ Attributes      (1 Kbyte)
      0x07 => {
        self.oam[(addr & 0x00FF_FFFF) as usize] = byte;
      }
      // External Memory (Game Pak)
      // TODO: Wait states
      // Game Pak ROM/FlashROM (mx 32MB)
      0x0E | 0x0D | 0x0C | 0x0B | 0x0A => {
        self.game_pak[(addr & 0x01FF_FFFF) as usize] = byte;
      }
      // Game Pak SRAM    (max 64 KBytes) - 8bit Bus width (TODO)
      _ => (), //unimplemented!("Invalid address: {:x}", addr),
    }
  }

  pub(crate) fn write_u8(&mut self, addr: u32, byte: u8) {
    self.write_u8_inline(addr, byte);
  }

  #[inline]
  pub(crate) fn fetch_u16_inline(&mut self, addr: u32) -> u16 {
    match addr >> 24 {
      // ROM out of bounds access (Behaviour taken from MGBA)
      0x0B | 0x09 | 0x0D => ((addr >> 1) & 0x0000_FFFF) as u16,
      _ => {
        u16::from(self.fetch_byte_inline(addr)) + (u16::from(self.fetch_byte_inline(addr + 1)) << 8)
      }
    }
  }

  pub(crate) fn fetch_u16(&mut self, addr: u32) -> u16 {
    self.fetch_u16_inline(addr)
  }

  #[inline]
  pub(crate) fn fetch_u32_inline(&mut self, addr: u32) -> u32 {
    match addr {
      // ROM out of bounds access (Behaviour taken from MGBA)
      0x09 | 0x0B | 0x0D => ((addr >> 1) & 0x0000_FFFF) as u32,
      _ => {
        u32::from(self.fetch_u16_inline(addr)) + (u32::from(self.fetch_u16_inline(addr + 2)) << 16)
      }
    }
  }
  pub(crate) fn fetch_u32(&mut self, addr: u32) -> u32 {
    self.fetch_u32_inline(addr)
  }

  pub fn reset(&mut self) {
    self.executed_instructions_count = 0;
    self.regs = [0; 16];
    self.output_texture = [0x00_u8; VIDEO_WIDTH * VIDEO_HEIGHT * 3];
    self.fiq_only_banks = [[0_u32; 5]; 2];
    self.all_modes_banks = [[0_u32; 2]; 6];
    self.io_mem = [0_u8; 1022];
    self.write_u32(0x0400_0088, 0x200);
    self.spsrs = [CPSR(0x0000_001F); 5];
    self.all_modes_banks[CpuMode::IRQ.as_usize()][0] = 0x0300_7FA0;
    self.all_modes_banks[CpuMode::Supervisor.as_usize()][0] = 0x0300_7FE0;
    self.cpsr = CPSR(0x0000_001F);
    self.set_mode(CpuMode::Privileged);
    self.wram_board = [0_u8; 256 * KB];
    self.wram_chip = [0_u8; 32 * KB];
    self.palette_ram = [0_u8; KB];
    self.vram = [0_u8; 96 * KB];
    self.oam = [0_u8; KB];
    self.game_pak = Box::new([0_u8; 32 * MB]);
    self.game_pak_sram = [0_u8; 64 * KB];
    self.clocks = 0_u32;
    self.write_u16(0x400_0130, 0b0000_0011_1111_1111); // KEYINPUT register. 1's denote "not pressed"
    self.regs[13] = 0x0300_7F00; // Taken from mgba
    self.regs[15] = RESET_HANDLER;
    self.regs[15] = 0x0800_0000; // Skip BIOS
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
    if let Some(f) = self.instruction_hook {
      f(self);
    }

    if self.thumb() {
      thumb::execute_one_instruction(self).map_err(GBAError::Thumb)?;
    } else {
      arm::execute_one_instruction(self).map_err(GBAError::ARM)?;
    }
    self.executed_instructions_count += 1;

    self.update_hvblank();

    Ok(())
  }

  #[inline]
  pub fn set_input(&mut self, button: GBAButton, set_else_unset: bool) {
    let mut val = self.fetch_u16(KEY_STATUS_REG);
    let bit = button.bit();

    if set_else_unset {
      val &= !bit;
    } else {
      val |= bit;
    }

    self.write_u16(KEY_STATUS_REG, val);
  }

  pub fn input(&mut self, button: GBAButton) {
    self.set_input(button, true);
  }

  pub fn persistent_input_pressed(&mut self, button: GBAButton) {
    self.input(button);
    self.persistent_input_bitmask |= button.bit();
  }

  pub fn persistent_input_released(&mut self, button: GBAButton) {
    self.set_input(button, false);
    self.persistent_input_bitmask &= !button.bit();
  }

  fn update_hvblank(&mut self) {
    let column_offset = ((self.clocks / CLOCKS_PER_PIXEL) as u16) % NUM_HORIZONTAL_PIXELS;
    let row_offset = ((self.clocks / CLOCKS_PER_SCANLINE) as u16) % NUM_SCANLINES;

    let hblank = column_offset >= NUM_REAL_HORIZONTAL_PIXELS;
    let vblank = row_offset >= NUM_REAL_SCANLINES;

    if column_offset == NUM_HORIZONTAL_PIXELS {
      self.trigger_interrupt_if_enabled(HBLANK_IE_BIT);
    }
    if row_offset == NUM_REAL_SCANLINES {
      self.trigger_interrupt_if_enabled(VBLANK_IE_BIT);
    }

    let prev_status = self.fetch_u16(DISPSTAT_ADDR); // DIPSTAT in gbatek
    let _prev_vcount = self.fetch_u16(VCOUNT_ADDR);

    let vcount_setting = (prev_status & 0xFF00) >> 8;
    let vcounter_flag = vcount_setting == row_offset;

    if vcounter_flag {
      self.trigger_interrupt_if_enabled(VCOUNTER_IE_BIT);
    }

    let new_status = (prev_status & 0b1111_1111_1111_1000)
      | if vcounter_flag { 1 << 2 } else { 0 }
      | if hblank { 1 << 1 } else { 0 }
      | if vblank { 1 } else { 0 };

    self.write_u16(VCOUNT_ADDR, row_offset & 0xFF); // Write VCOUNT
    self.write_u16(DISPSTAT_ADDR, new_status); // Write DIPSTAT
  }

  pub fn run_one_frame(&mut self) -> Result<(), GBAError> {
    while self.clocks < CLOCKS_PER_FRAME {
      //for _ in 1..1000 {
      self.run_one_instruction()?;
    }
    self.clocks -= CLOCKS_PER_FRAME;

    self.update_video_output();
    self.write_u16(KEY_STATUS_REG, !self.persistent_input_bitmask);

    Ok(())
  }

  /// Fills three bytes starting at the given index in the given slice with the RGB values of the given
  /// 16 bit GBA color. See http://problemkaputt.de/gbatek.htm#lcdcolorpalettes for details.
  /// (the intensities 0-14 are practically all black, and only intensities 15-31 are resulting in visible medium.)
  #[allow(clippy::identity_op)]
  #[allow(clippy::redundant_closure_call)]
  #[inline]
  fn fill_output_color(output_texture: &mut [u8], idx: usize, color: u16) {
    let r = ((color >> 0) & 0x001F) as u8;
    let g = ((color >> 5) & 0x001F) as u8;
    let b = ((color >> 10) & 0x001F) as u8;

    let col5_to_col8 = |x| (x << 3) | (x >> 2);
    output_texture[(idx * 3) + 0] = col5_to_col8(r);
    output_texture[(idx * 3) + 1] = col5_to_col8(g);
    output_texture[(idx * 3) + 2] = col5_to_col8(b);
  }

  /// Fill the field output_texture with RGB values. See http://problemkaputt.de/gbatek.htm#gbalcdvideocontroller
  /// for details.
  #[allow(clippy::unused_unit)]
  pub(crate) fn update_video_output(&mut self) {
    let display_control = self.fetch_u16(DISPCNT_ADDR);
    let bg_mode = display_control & 0x0000_0007;
    match bg_mode {
      0 => {
        const TILE_WIDTH: u32 = 8;
        const TILE_HEIGHT: u32 = 8;

        let (scroll_x, scroll_y) = (
          (self.fetch_u16(0x0400_0010) & 0x1FF) as u32,
          (self.fetch_u16(0x0400_0012) & 0x1FF) as u32,
        );
        let (bg_first_tile_x, bg_first_tile_y) = (scroll_x / 8, scroll_y / 8);
        let (leftover_x_pxs_from_scroll, leftover_y_pxs_from_scroll) =
          (scroll_x % 8, (scroll_y % 8));

        let bg_0_control_flags = self.fetch_u16(0x0400_0008); // BG0CNT in gbatek

        // The tile data contains colours, format depends on full palette
        // (1 byte per colour) or not (4 bits a colour)
        const TILE_DATA_PAGE_SIZE: u32 = 16 * 1024;
        let tile_data_page = u32::from((bg_0_control_flags >> 2) & 0x3);
        let tile_data_base_addr = 0x0600_0000 + tile_data_page * TILE_DATA_PAGE_SIZE;

        // The tilemap contains the attributes for the tile(Like being flipped)
        // and the tile index into the tile data
        const TILE_MAP_PAGE_SIZE: u32 = 2 * 1024;
        let tile_map_page = u32::from((bg_0_control_flags >> 8) & 0x1F);
        let tile_map_base_addr = 0x0600_0000 + tile_map_page * TILE_MAP_PAGE_SIZE;

        let full_palette = (bg_0_control_flags & 0x0000_0080) != 0;
        let screen_size_flag = ((bg_0_control_flags >> 14) & 0x3) as usize;
        let (bg_x_tile_count, bg_y_tile_count) = match screen_size_flag {
          0 => (32, 32),
          1 => (64, 32),
          2 => (32, 64),
          3 => (64, 64),
          _ => std::unreachable!(),
        };

        let bytes_per_tile =
          if full_palette { TILE_WIDTH * TILE_HEIGHT } else { TILE_WIDTH * (TILE_HEIGHT / 2) };
        let draw_tile = |gba: &mut GBA, bg_tile_x, bg_tile_y, screen_tile_x, screen_tile_y| {
          // The math here is pretty weird. The way the tiles are laid out in memory seems to be
          // the first (top-left) 32x32 tiles, then the bottom-left 32x32, then top-right
          // then bottom-right
          let tile_map_element_addr = tile_map_base_addr
            + (((bg_tile_x & 0x1F) + bg_tile_y * bg_y_tile_count) * 2)
            + if bg_tile_x >= 32 { 32 * bg_y_tile_count * 2 } else { 0 };

          // See http://problemkaputt.de/gbatek.htm#lcdvrambgscreendataformatbgmap
          let tile_map_element = u32::from(gba.fetch_u16(tile_map_element_addr));

          let tile_number = tile_map_element & (if full_palette { 0x3FF } else { 0x1FF });
          let tile_addr = 0x0600_0000 + tile_number * bytes_per_tile;

          let h_flip = (tile_map_element & 0x0000_0400) != 0;
          let v_flip = (tile_map_element & 0x0000_0800) != 0;

          if full_palette {
            // 1 byte per pixel
            for i in 0..64 {
              let palette_idx = gba.fetch_byte(tile_data_base_addr + tile_number + i) as usize;
              let color = u16::from(gba.palette_ram[palette_idx])
                + (u16::from(gba.palette_ram[palette_idx + 1]) << 8);
              let px_idx = (screen_tile_x * 8
                + (screen_tile_y * bg_x_tile_count * 8)
                + (i % 8)
                + (i / 8) * bg_x_tile_count * 8) as usize;
              if px_idx < (&gba.output_texture[..]).len() {
                Self::fill_output_color(&mut gba.output_texture[..], px_idx, color);
              }
            }
          } else {
            // 4 bits per pixel
            let palette_base = {
              let palette_number = (tile_map_element >> 12) & 0xF;
              // Each sub palette has 16 colors of 2 bytes each
              (palette_number * 16 * 2) as usize
            };
            for i in 0..32u32 {
              let (mut px_x_within_tile, mut px_y_within_tile) = ((i & 0x3) * 2, (i >> 2));

              // TODO: Things seem to be wrong when moving along both ways mid-tile when flipping?
              if h_flip {
                px_x_within_tile = 7 - px_x_within_tile;
              };
              if v_flip {
                px_y_within_tile = 7 - px_y_within_tile;
              };

              let (px_x_idx, overflows_x) = ((px_x_within_tile + screen_tile_x * 8) as usize)
                .overflowing_sub(leftover_x_pxs_from_scroll as usize);
              let (px_y_idx, overflows_y) = ((px_y_within_tile + screen_tile_y * 8) as usize)
                .overflowing_sub(leftover_y_pxs_from_scroll as usize);
              if overflows_x || overflows_y {
                continue;
              }

              if px_y_idx >= VIDEO_HEIGHT {
                continue;
              }

              // First 4 bits are the first px's palette idx, next 4 are the next color's
              let (palette_idx1, palette_idx2) = {
                let palette_idxs = gba.fetch_byte(tile_addr + i) as usize;
                #[allow(clippy::identity_op)]
                ((palette_idxs >> 0) & 0xF, (palette_idxs >> 4) & 0xF)
              };

              if px_x_idx < VIDEO_WIDTH {
                let px_idx1 = px_x_idx.wrapping_add(px_y_idx * VIDEO_WIDTH);
                let palette_idx1 = palette_base + (palette_idx1 * 2);

                let color = u16::from(gba.palette_ram[palette_idx1])
                  + (u16::from(gba.palette_ram[palette_idx1 + 1]) << 8);
                Self::fill_output_color(&mut gba.output_texture[..], px_idx1, color);
              }
              if if h_flip { px_x_idx - 1 } else { px_x_idx.saturating_add(1) } < VIDEO_WIDTH {
                let px_idx2 = (if h_flip { px_x_idx - 1 } else { px_x_idx.saturating_add(1) })
                  .wrapping_add(px_y_idx * VIDEO_WIDTH);
                let palette_idx2 = palette_base + (palette_idx2 * 2);
                let color = u16::from(gba.palette_ram[palette_idx2])
                  + (u16::from(gba.palette_ram[palette_idx2 + 1]) << 8);
                Self::fill_output_color(&mut gba.output_texture[..], px_idx2, color);
              }
            }
          }
        };
        const SCREEN_Y_TILE_COUNT: u32 = (VIDEO_HEIGHT as u32) / TILE_HEIGHT;
        const SCREEN_X_TILE_COUNT: u32 = (VIDEO_WIDTH as u32) / TILE_WIDTH;
        // We use an extra one for the case were we are mid-scroll and are displaying N+1 tiles
        for y in 0..=SCREEN_Y_TILE_COUNT {
          for x in 0..=SCREEN_X_TILE_COUNT {
            draw_tile(
              self,
              (bg_first_tile_x + x) % bg_x_tile_count,
              (bg_first_tile_y + y) % bg_y_tile_count,
              x,
              y,
            );
          }
        }
      }
      1 => (),
      2 => (),
      3 => {
        // 16 bit color bitmap. One frame buffer (240x160 pixels, 32768 colors)
        #[allow(clippy::match_ref_pats)]
        for (idx, slice) in self.vram.chunks_exact(2).take(VIDEO_WIDTH * VIDEO_HEIGHT).enumerate() {
          if let &[high_byte, low_byte] = slice {
            let color = u16::from(low_byte) + (u16::from(high_byte) << 8);
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
          let color = u16::from(self.palette_ram[palette_idx])
            + (u16::from(self.palette_ram[palette_idx + 1]) << 8);
          Self::fill_output_color(&mut self.output_texture[..], idx, color);
        }
      }
      5 => {
        // 16 bit color bitmap. Two frame buffers (160x128 pixels, 32768 colors)
        const BGMODE5_WIDTH: usize = 160;
        const BGMODE5_HEIGHT: usize = 128;
        const BGMODE5_FRAMEBUFFER_PX_COUNT: usize = BGMODE5_WIDTH * BGMODE5_HEIGHT;
        const BGMODE5_FIRST_X_PX: usize = (VIDEO_WIDTH - BGMODE5_WIDTH) / 2;
        const BGMODE5_FIRST_Y_PX: usize = (VIDEO_HEIGHT - BGMODE5_HEIGHT) / 2;
        let second_frame = (self.io_mem[0] & 0x0000_0008) != 0; // TODO: Is this right?
        let mut iter = self
          .vram
          .chunks_exact(2)
          .skip(if second_frame { BGMODE5_FRAMEBUFFER_PX_COUNT } else { 0 })
          .take(BGMODE5_FRAMEBUFFER_PX_COUNT);
        for x in 0..VIDEO_WIDTH {
          for y in 0..VIDEO_HEIGHT {
            let idx = x + y * VIDEO_WIDTH;
            if x < BGMODE5_FIRST_X_PX
              || x > BGMODE5_FIRST_X_PX + BGMODE5_WIDTH
              || y < BGMODE5_FIRST_Y_PX
              || y > BGMODE5_FIRST_Y_PX + BGMODE5_HEIGHT
            {
              // TODO: Is this right?
              Self::fill_output_color(&mut self.output_texture[..], idx, u16::max_value());
            } else if let &[high_byte, low_byte] = iter.next().unwrap() {
              // TODO: Is this right?
              let color = u16::from(low_byte) + (u16::from(high_byte) << 8);
              Self::fill_output_color(&mut self.output_texture[..], idx, color);
            }
          }
        }
      }
      _ => (),
    }
  }

  #[inline]
  fn trigger_interrupt_if_enabled(&mut self, bit: u32) {
    const INTERRUPT_ENABLE_REG_ADDR: u32 = 0x0400_0200;
    const INTERRUPT_ENABLE_MASTER_REG_ADDR: u32 = 0x0400_0208;
    const INTERRUPT_REQUEST_ACKNOWLEDGE_REG_ADDR: u32 = 0x0400_0202;
    const BIOS_INTERRUPT_HANDLER: u32 = 0x0000_0018;
    let master_interrupt_enable =
      self.fetch_u32(INTERRUPT_ENABLE_MASTER_REG_ADDR) & 0x0000_0001 != 0;
    let cpsr_interrupt_enable = !self.cpsr.irq_disabled_flag();
    let bit = 1 << bit;
    let this_interrupt_enable = self.fetch_u32(INTERRUPT_ENABLE_REG_ADDR) & bit != 0;
    if master_interrupt_enable && cpsr_interrupt_enable && this_interrupt_enable {
      self.write_u32(INTERRUPT_REQUEST_ACKNOWLEDGE_REG_ADDR, bit);
      self.set_mode(CpuMode::IRQ);
      self.regs[15] = BIOS_INTERRUPT_HANDLER;
    }
  }

  pub(crate) fn do_dma(&mut self, base_addr: u32) {
    let dma_num = (base_addr - 0x0400_00B0) / 0xA;

    let source_addr_addr = base_addr;
    let dest_addr_addr = base_addr + 4;
    let count_addr = base_addr + 8;
    let control_addr = base_addr + 10;

    let mut source_addr =
      self.fetch_u32(source_addr_addr) & if dma_num == 0 { 0x07FF_FFFF } else { 0x0FFF_FFFF };
    let mut dest_addr =
      self.fetch_u32(dest_addr_addr) & if dma_num == 3 { 0x0FFF_FFFF } else { 0x07FF_FFFF };

    let control_flags = self.fetch_u16(control_addr);
    let size = if (control_flags & 0x0000_0400) != 0 { 4 } else { 2 };

    let count = {
      let count = self.fetch_u16(count_addr);
      let max_count = if dma_num == 3 { 0xFFFF } else { 0x3FFF };
      if count == 0 {
        max_count
      } else {
        count & max_count
      }
    };

    let modifier_source: fn(&mut u32, u32) = match (control_flags >> 7) & 3 {
      0 => |source, size| *source += size,
      1 => |source, size| *source -= size,
      2 => |_, _| (),
      3 => |source, size| *source += size,
      _ => std::panic!("Impossible"),
    };
    let modifier_dest: fn(&mut u32, u32) = match (control_flags >> 5) & 3 {
      0 => |source, size| *source += size,
      1 => |source, size| *source -= size,
      2 => |_, _| (),
      _ => std::panic!("Impossible"),
    };

    if let Some(f) = self.debug_print_fn {
      f(format!(
        "DMA source:{:x} dest:{:x} size:{} count:{} modifier_source:{} modifier_dest:{} flags:{:x}",
        source_addr,
        dest_addr,
        size,
        count,
        (control_flags >> 7) & 3,
        (control_flags >> 5) & 3,
        control_flags
      )
      .as_str());
    }

    if size == 2 {
      for _ in 0..count {
        let byte = self.fetch_u16(source_addr);
        self.write_u16(dest_addr, byte);
        modifier_source(&mut source_addr, size);
        modifier_dest(&mut dest_addr, size);
      }
    } else {
      for _ in 0..count {
        let byte = self.fetch_u32(source_addr);
        self.write_u32(dest_addr, byte);
        modifier_source(&mut source_addr, size);
        modifier_dest(&mut dest_addr, size);
      }
    }

    let repeat = (control_flags & 0x0200) != 0;
    if !repeat {
      self.write_u16(control_addr, control_flags & (!0x8000));
    }

    let irq = (control_flags & 0x4000) != 0;
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
