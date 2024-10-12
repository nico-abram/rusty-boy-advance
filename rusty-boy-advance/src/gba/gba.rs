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

const INTERRUPT_ENABLE_REG_ADDR: u32 = 0x0400_0200;
const INTERRUPT_ENABLE_MASTER_REG_ADDR: u32 = 0x0400_0208;
const INTERRUPT_REQUEST_ACKNOWLEDGE_REG_ADDR: u32 = 0x0400_0202;
const HALTCNT_REG_ADDR: u32 = 0x0400_0301;

const INTERRUPT_REQUEST_ACKNOWLEDGE_REG_IOMEMIDX: usize =
  (INTERRUPT_REQUEST_ACKNOWLEDGE_REG_ADDR - 0x0400_0000) as usize;
const INTERRUPT_ENABLE_REG_IOMEMIDX: usize = (INTERRUPT_ENABLE_REG_ADDR - 0x0400_0000) as usize;

pub const VIDEO_HEIGHT: usize = 160;
pub const VIDEO_WIDTH: usize = 240;

/// See [`GBAButton`].bit for button->bit mappings
pub(crate) const KEY_STATUS_REG: u32 = 0x0400_0130;
/// Display control (http://problemkaputt.de/gbatek.htm#lcdiodisplaycontrol)
pub(crate) const DISPCNT_ADDR: u32 = 0x0400_0000;
/// Display status (http://problemkaputt.de/gbatek.htm#lcdiointerruptsandstatus)
const DISPSTAT_ADDR: u32 = 0x0400_0004;
/// Scanline(Vertical) counter (http://problemkaputt.de/gbatek.htm#lcdiointerruptsandstatus)
const VCOUNT_ADDR: u32 = 0x0400_0006;

// Numbers taken from https://rust-console.github.io/gba/io-registers.html#vcount-vertical-display-counter
const NUM_VIRTUAL_HORIZONTAL_PIXELS: u16 = 68;
const NUM_REAL_HORIZONTAL_PIXELS: u16 = VIDEO_WIDTH as u16;
const NUM_HORIZONTAL_PIXELS: u16 = NUM_VIRTUAL_HORIZONTAL_PIXELS + NUM_REAL_HORIZONTAL_PIXELS;
const HBLANK_CYCLES: u32 = (NUM_REAL_HORIZONTAL_PIXELS as u32) * CLOCKS_PER_PIXEL + 46;
const DRAW_SCANLINE_CYCLES: u32 = (NUM_REAL_HORIZONTAL_PIXELS as u32) * CLOCKS_PER_PIXEL;
pub const CLOCKS_PER_PIXEL: u32 = 4;
pub const CLOCKS_PER_SCANLINE: u32 = (NUM_HORIZONTAL_PIXELS as u32) * CLOCKS_PER_PIXEL;
const NUM_VIRTUAL_SCANLINES: u16 = 68;
pub(crate) const NUM_REAL_SCANLINES: u16 = VIDEO_HEIGHT as u16;
const NUM_SCANLINES: u16 = NUM_REAL_SCANLINES + NUM_VIRTUAL_SCANLINES;
pub const CLOCKS_PER_FRAME: u32 = (NUM_SCANLINES as u32) * CLOCKS_PER_SCANLINE;

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
  ControlFlow,
  EveryInstruction,
  Debug,
}

#[derive(Debug, PartialEq)]
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
  pub(crate) output_texture: [u16; VIDEO_WIDTH * VIDEO_HEIGHT],
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
  pub(crate) bios_copy: [u8; 16 * KB],
  pub(crate) bios_rom: [u8; 16 * KB],
  pub(crate) wram_board: [u8; 256 * KB],
  pub(crate) wram_chip: [u8; 32 * KB],
  pub(crate) palette_ram: [u8; KB],
  pub(crate) vram: [u8; 96 * KB],
  pub(crate) oam: [u8; KB],
  pub(crate) io_mem: [u8; 1022],
  pub(crate) clocks: u32,
  pub(crate) clocks_at_last_instruction: u32,
  instruction_hook: Option<fn(&mut GBA)>,
  pub(crate) instruction_hook_with_opcode: Option<fn(&mut GBA, u32)>,
  pub(crate) loaded_rom: Option<Rom>,
  pub(crate) rom_size: u32,
  pub(crate) print_fn: Option<fn(&str) -> ()>,
  pub(crate) debug_print_fn: Option<fn(&str) -> ()>,
  pub(crate) branch_print_fn: Option<fn(&str) -> ()>,
  pub(crate) executed_instructions_count: u64,
  pub(crate) persistent_input_bitmask: u16,
  halted: bool,
}

impl GBA {
  pub(crate) fn new(
    log_level: LogLevel,
    bios_file: core::option::Option<&[u8]>,
    print_fn: Option<fn(&str) -> ()>,
  ) -> Box<Self> {
    // Was still getting stack overflows without Box X
    let mut gba = Box::new(Self {
      output_texture: [0x00_u16; VIDEO_WIDTH * VIDEO_HEIGHT],
      regs: [0_u32; 16],
      fiq_only_banks: [[0_u32; 5]; 2],
      all_modes_banks: [[0_u32; 2]; 6],
      spsrs: [CPSR(0_u32); 5],
      cpsr: CPSR(0_u32),
      bios_rom: [0_u8; 16 * KB],
      bios_copy: [0_u8; 16 * KB],
      wram_board: [0_u8; 256 * KB],
      wram_chip: [0_u8; 32 * KB],
      palette_ram: [0_u8; KB],
      vram: [0_u8; 96 * KB],
      oam: [0_u8; KB],
      io_mem: [0_u8; 1022],
      game_pak: Box::new([0_u8; 32 * MB]),
      game_pak_sram: [0x00_u8; 64 * KB], // mesen fills it with 0xFF
      clocks: 0_u32,
      clocks_at_last_instruction: 0_u32,
      loaded_rom: None,
      rom_size: 0,
      instruction_hook: None,
      instruction_hook_with_opcode: None,
      debug_print_fn: None,
      branch_print_fn: None,
      print_fn,
      executed_instructions_count: 0,
      persistent_input_bitmask: 0x000,
      halted: false,
    });

    let bios_file = bios_file.unwrap_or(include_bytes!("gba_bios.bin"));
    gba.bios_copy.clone_from_slice(bios_file);

    gba.reset();
    gba.set_log_level(log_level);

    gba
  }

  pub(crate) fn reload_bios(&mut self) {
    self.bios_rom.clone_from_slice(&self.bios_copy[..]);
  }

  pub fn enable_control_flow_logs(&mut self, enable_interrupt_logs: bool) {
    self.branch_print_fn = if enable_interrupt_logs { self.print_fn } else { None };
  }

  pub(crate) fn set_log_level(&mut self, log_level: LogLevel) {
    const PRINT_OPCODE: fn(&mut GBA, u32) = |gba: &mut GBA, opcode: u32| {
      if let Some(f) = gba.print_fn {
        f(format!(
          "instcount:{:10} opcode:{:08X} pc:${:08X}  {}\n",
          gba.executed_instructions_count,
          opcode,
          gba.regs[15],
          if gba.cpsr.thumb_state_flag() {
            crate::disasm::disasm_thumb(opcode as u16)
          } else {
            crate::disasm::disasm_arm(opcode)
          }
        )
        .as_str());
      }
    };
    const PRINT_STATE: fn(&mut GBA) = |gba: &mut GBA| {
      if let Some(f) = gba.print_fn {
        f(format!("{}\n", gba.state_as_string()).as_str());
      }
    };

    self.instruction_hook = match log_level {
      LogLevel::Debug | LogLevel::EveryInstruction => Some(PRINT_STATE),
      LogLevel::None | LogLevel::ControlFlow => None,
    };
    self.instruction_hook_with_opcode = match log_level {
      LogLevel::Debug | LogLevel::EveryInstruction => Some(PRINT_OPCODE),
      LogLevel::None | LogLevel::ControlFlow => None,
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
      0x02 => self.wram_board[(addr & 0x0003_FFFF) as usize],
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
      // if top bit is set: 0x0001_FFFF otherwise: 0x0001_7FFF
      // (addr&0x0001_0000>>1) xor 0x0001_FFFF
      //0x06 => self.vram[(addr & 0x0001_FFFF) as usize],
      0x06 => self.vram[(addr & (((addr & 0x0001_0000) >> 1) ^ 0x0001_FFFF)) as usize],
      // OAM - OBJ Attributes      (1 Kbyte)
      0x07 => self.oam[(addr & 0x0000_03FF) as usize],
      // External Memory (Game Pak)
      // TODO: Wait states
      // Game Pak ROM/FlashROM (max 32MB) - Wait State 0
      0x08 | 0x09 => self.game_pak[(addr & 0x01FF_FFFF) as usize],
      // Game Pak ROM/FlashROM (max 32MB) - Wait State 1
      0x0A | 0x0B => self.game_pak[(addr & 0x01FF_FFFF) as usize],
      // Game Pak ROM/FlashROM (max 32MB) - Wait State 2
      0x0C | 0x0D => self.game_pak[(addr & 0x01FF_FFFF) as usize],
      // Out of bounds ROM access (Behaviour taken from MGBA)
      //0x0B | 0x0D | 0x09 => self.game_pak[(addr & 0x01FF_FFFF) as usize],
      // Game Pak SRAM    (max 64 KBytes) - 8bit Bus width (TODO)
      0x0E => self.game_pak_sram[(addr & 0x0000_FFFF) as usize],
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
        0x0400_00BA => self.do_dma(0), // DMA 0
        0x0400_00C6 => self.do_dma(1), // DMA 1
        0x0400_00D2 => self.do_dma(2), // DMA 2
        0x0400_00DE => self.do_dma(3), // DMA 3
        _ => (),
      }
    }
  }

  pub(crate) fn write_u32(&mut self, addr: u32, value: u32) {
    self.write_u16_inline(addr, value as u16);
    self.write_u16_inline(addr + 2, (value >> 16) as u16);
    if addr & 0xFFFF_FF00 == 0x0400_0000 {
      if (value & 0x0000_8000) != 0 {
        match addr {
          0x0400_00BA => self.do_dma(0), // DMA 0
          0x0400_00C6 => self.do_dma(1), // DMA 1
          0x0400_00D2 => self.do_dma(2), // DMA 2
          0x0400_00DE => self.do_dma(3), // DMA 3
          _ => (),
        }
      } else if (value & 0x8000_0000) != 0 {
        match addr {
          0x0400_00B8 => self.do_dma(0), // DMA 0
          0x0400_00C4 => self.do_dma(1), // DMA 1
          0x0400_00D0 => self.do_dma(2), // DMA 2
          0x0400_00DC => self.do_dma(3), // DMA 3
          _ => (),
        }
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
        //self.bios_rom[(addr & 0x0000_3FFF) as usize] = byte;
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
          if addr & 0x04FF_FFFE == INTERRUPT_REQUEST_ACKNOWLEDGE_REG_ADDR {
            self.io_mem[(addr & 0x00FF_FFFF) as usize] &= !byte;
          } else if addr == HALTCNT_REG_ADDR {
            // HALTCNT
            let is_stop = byte & 0x80 != 0;
            if is_stop {
              unimplemented!("STOP written to HALTCNT");
            } else {
              if let Some(branch_print_fn) = self.branch_print_fn {
                branch_print_fn(&format!(
                  "HALT instcount:{} pc:{:08X}",
                  self.executed_instructions_count, self.regs[15]
                ));
              }
              self.halted = true;
            }
          } else {
            self.io_mem[(addr & 0x00FF_FFFF) as usize] = byte;
          }
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
      //0x0E |
      0x0D | 0x0C | 0x0B | 0x0A => {
        //self.game_pak[(addr & 0x01FF_FFFF) as usize] = byte;
      }
      // Game Pak SRAM (TODO)
      0x0E => {
        self.game_pak_sram[(addr & 0x0000_FFFF) as usize] = byte;
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
      0x08 | 0x09 | 0x0A | 0x0B | 0x0C | 0x0D => {
        if addr & 0x01_FFFF >= self.rom_size {
          ((addr >> 1) & 0x0000_FFFF) as u16
        } else {
          u16::from(self.fetch_byte_inline(addr))
            + (u16::from(self.fetch_byte_inline(addr + 1)) << 8)
        }
      }
      _ => {
        u16::from(self.fetch_byte_inline(addr)) + (u16::from(self.fetch_byte_inline(addr + 1)) << 8)
      }
    }
  }

  pub(crate) fn fetch_u16(&mut self, addr: u32) -> u16 {
    self.fetch_u16_inline(addr)
  }

  pub(crate) fn fetch_u16_vram(&mut self, addr: u32) -> u16 {
    self.vram[addr as usize] as u16 + (self.vram[addr as usize + 1] as u16) << 8
  }

  #[inline]
  pub(crate) fn fetch_u32_inline(&mut self, addr: u32) -> u32 {
    match addr {
      // ROM out of bounds access (Behaviour taken from MGBA)
      0x08 | 0x09 | 0x0A | 0x0B | 0x0C | 0x0D => {
        if addr & 0x01_FFFF >= self.rom_size {
          ((addr >> 1) & 0x0000_FFFF) as u32
        } else {
          u32::from(self.fetch_u16_inline(addr))
            + (u32::from(self.fetch_u16_inline(addr + 2)) << 16)
        }
      }
      _ => {
        u32::from(self.fetch_u16_inline(addr)) + (u32::from(self.fetch_u16_inline(addr + 2)) << 16)
      }
    }
  }
  pub(crate) fn fetch_u32(&mut self, addr: u32) -> u32 {
    self.fetch_u32_inline(addr)
  }

  pub fn reset(&mut self) {
    self.reload_bios();

    self.halted = false;
    self.executed_instructions_count = 0;
    self.regs = [0; 16];
    self.output_texture = [0x00_u16; VIDEO_WIDTH * VIDEO_HEIGHT];
    self.fiq_only_banks = [[0_u32; 5]; 2];
    self.all_modes_banks = [[0_u32; 2]; 6];
    self.io_mem = [0_u8; 1022];
    self.write_u32(0x0400_0088, 0x200);
    self.write_u32(DISPCNT_ADDR, 0x0080);
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
    self.game_pak_sram = [0x00_u8; 64 * KB]; // mesen fills it with 0xFF
    self.game_pak_sram[0] = 0xC2;
    self.game_pak_sram[1] = 0x09;
    self.clocks = 0_u32;
    self.clocks_at_last_instruction = 0u32;
    self.write_u16(0x400_0130, 0b0000_0011_1111_1111); // KEYINPUT register. 1's denote "not pressed"
    self.regs[13] = 0x0300_7F00; // Taken from mgba
    self.regs[15] = RESET_HANDLER;
    self.regs[15] = 0x0800_0000; // Skip BIOS
    self.regs[14] = self.regs[15];
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
    self.rom_size = rom_bytes.len() as u32;

    Ok(())
  }

  pub(crate) fn thumb(&mut self) -> bool {
    self.cpsr.thumb_state_flag()
  }

  pub fn state_as_string(&mut self) -> String {
    format!(
      "{} CPSR:{:08X}\n{} halted:{}",
      self
        .regs
        .iter()
        .enumerate()
        .map(|(idx, val)| format!(
          "R{}:{:08X}",
          idx,
          if idx == 15 { val + if self.cpsr.thumb_state_flag() { 4 } else { 8 } } else { *val }
        ))
        .collect::<Vec<_>>()
        .join(" "),
      self.cpsr.0,
      self.cpsr,
      self.halted
    )
  }

  pub(crate) fn run_one_instruction(&mut self) -> Result<(), GBAError> {
    if self.halted {
      if self.io_mem[INTERRUPT_ENABLE_REG_IOMEMIDX]
        & self.io_mem[INTERRUPT_REQUEST_ACKNOWLEDGE_REG_IOMEMIDX]
        != 0
      {
        self.halted = false;
      } else {
        if self.clocks % CLOCKS_PER_SCANLINE == HBLANK_CYCLES {
          self.clocks += CLOCKS_PER_SCANLINE - HBLANK_CYCLES + 1;
        } else {
          self.clocks += HBLANK_CYCLES - self.clocks % CLOCKS_PER_SCANLINE;
        }
        self.update_hvblank();
        self.clocks_at_last_instruction = self.clocks;
        return Ok(());
      }
    }
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
    self.clocks_at_last_instruction = self.clocks;

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
  pub fn set_clocks(&mut self, clocks: u32) {
    self.clocks = clocks;
  }
  pub fn clocks(&self) -> u32 {
    self.clocks
  }
  pub fn halted(&self) -> bool {
    self.halted
  }

  fn update_hvblank(&mut self) {
    let clocks_pre_exec = self.clocks_at_last_instruction;
    //let column_offset = ((self.clocks / CLOCKS_PER_PIXEL) as u16) % NUM_HORIZONTAL_PIXELS;

    let row_offset = (self.clocks / CLOCKS_PER_SCANLINE) as u16;
    let row_offset_pre_exec = (clocks_pre_exec / CLOCKS_PER_SCANLINE) as u16;

    let clocks_within_scanline = self.clocks % CLOCKS_PER_SCANLINE;
    let clocks_within_scanline_pre_exec = clocks_pre_exec % CLOCKS_PER_SCANLINE;

    let draw_scanline = clocks_within_scanline >= DRAW_SCANLINE_CYCLES;
    let hblank = clocks_within_scanline >= HBLANK_CYCLES;
    let vblank = row_offset >= NUM_REAL_SCANLINES;

    let prev_status = self.fetch_u16(DISPSTAT_ADDR); // DIPSTAT in gbatek
    let _prev_vcount = self.fetch_u16(VCOUNT_ADDR);

    let vcount_setting = (prev_status & 0xFF00) >> 8;
    let vcounter_flag = row_offset >= vcount_setting;
    let vcounter_flag_pre = row_offset_pre_exec >= vcount_setting;

    if draw_scanline && clocks_within_scanline_pre_exec < DRAW_SCANLINE_CYCLES {
      if row_offset < NUM_REAL_SCANLINES {
        super::draw::draw_scanline(self, row_offset as u32);
      }
    }

    if hblank && clocks_within_scanline_pre_exec < HBLANK_CYCLES {
      self.trigger_interrupt_if_enabled(HBLANK_IE_BIT);
    }
    if vblank && row_offset_pre_exec < NUM_REAL_SCANLINES {
      self.trigger_interrupt_if_enabled(VBLANK_IE_BIT);
    }

    if vcounter_flag && !vcounter_flag_pre {
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
      self.run_one_instruction()?;
    }
    self.clocks -= CLOCKS_PER_FRAME;

    //self.update_video_output();
    self.write_u16(KEY_STATUS_REG, !self.persistent_input_bitmask);

    Ok(())
  }

  /// Fill the field output_texture with RGB values. See http://problemkaputt.de/gbatek.htm#gbalcdvideocontroller
  /// for details.
  #[allow(clippy::unused_unit)]
  pub(crate) fn update_video_output(&mut self) {
    for i in 0..NUM_REAL_SCANLINES {
      super::draw::draw_scanline(self, i as u32);
    }
  }

  #[inline]
  fn trigger_interrupt_if_enabled(&mut self, bit: u32) {
    const BIOS_INTERRUPT_HANDLER: u32 = 0x0000_0018;
    let master_interrupt_enable =
      self.fetch_u32(INTERRUPT_ENABLE_MASTER_REG_ADDR) & 0x0000_0001 != 0;
    let cpsr_interrupt_enable = !self.cpsr.irq_disabled_flag();
    let bit = 1 << bit;
    let this_interrupt_enable = self.fetch_u16(INTERRUPT_ENABLE_REG_ADDR) & bit != 0;

    let reg_if = (self.io_mem[INTERRUPT_REQUEST_ACKNOWLEDGE_REG_IOMEMIDX] as u16)
      | ((self.io_mem[INTERRUPT_REQUEST_ACKNOWLEDGE_REG_IOMEMIDX + 1] as u16) << 8);
    let new_if = reg_if | bit as u16;

    if master_interrupt_enable && this_interrupt_enable && cpsr_interrupt_enable {
      self.io_mem[INTERRUPT_REQUEST_ACKNOWLEDGE_REG_IOMEMIDX] = (new_if & 0xFF) as u8;
      self.io_mem[INTERRUPT_REQUEST_ACKNOWLEDGE_REG_IOMEMIDX + 1] = ((new_if >> 8) & 0xFF) as u8;

      self.spsrs[CpuMode::IRQ.as_usize() - 1] = self.cpsr;
      let old_pc = self.regs[15];
      //let was_thumb = self.cpsr.thumb_state_flag();
      self.set_mode(CpuMode::IRQ);
      self.cpsr.set_irq_disabled_flag(true);
      self.cpsr.set_thumb_state_flag(false);
      self.regs[14] = old_pc + 4;
      self.regs[15] = BIOS_INTERRUPT_HANDLER;

      if let Some(branch_print_fn) = self.branch_print_fn {
        branch_print_fn(&format!(
          "interrupt requested serviced type:{bit:04X} old_pc:{old_pc:08X} old_if:{reg_if:08X} new_if:{new_if:08X}"
        ));
      }
    } else {
      if let Some(branch_print_fn) = self.branch_print_fn {
        branch_print_fn(&format!("interrupt requested, not serviced type:{bit:04X}"));
      }
    }
  }

  pub(crate) fn do_dma(&mut self, dma_num: u32) {
    let base_addr = 0x0400_00B0 + dma_num * 12;
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
      if count == 0 { max_count } else { count & max_count }
    };

    // Indicates if, on repeat, the destination address should be reloaded
    // into the internal register (the source is untouched)
    let mut reload = false;

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
      3 => {
        reload = true;
        |source, size| *source += size
      }
      _ => std::panic!("Impossible"),
    };
    let timing = (control_flags >> 12) & 0x3;

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

    // TODO: VBlank/HBlank/Sound DMA
    if timing != 3 {
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
    }

    let repeat = (control_flags & 0x0200) != 0;
    if !repeat {
      self.write_u16(control_addr, control_flags & (!0x8000));
    }

    let irq = (control_flags & 0x4000) != 0;
    if irq {
      self.trigger_interrupt_if_enabled(DMA0_IE_BIT + dma_num);
    }
  }

  pub fn run_forever(&mut self) -> Result<(), GBAError> {
    loop {
      self.run_one_frame()?;
    }
  }

  #[inline]
  pub(crate) fn sequential_cycle(&self) -> u32 {
    1
  }

  #[inline]
  pub(crate) fn nonsequential_cycle(&self) -> u32 {
    2
  }

  #[inline]
  pub(crate) fn internal_cycle(&self) -> u32 {
    1
  }

  pub fn get_executed_instruction_count(&self) -> u64 {
    self.executed_instructions_count
  }
}
