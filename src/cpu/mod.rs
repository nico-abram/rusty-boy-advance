#![allow(non_snake_case)]
#![allow(dead_code)]
#![allow(unused_variables)]

mod arm;
mod cpsr;
mod cpu_mode;
mod thumb;
pub mod utils;

use cpsr::CPSR;
use cpu_mode::CpuMode;

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

pub struct Cpu {
    //// Output image
    pub output_texture: [u32; 240 * 160],
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
    game_pak: [u8; 64 * KB],
    bios_rom: [u8; 16 * KB],
    wram_board: [u8; 256 * KB],
    wram_chip: [u8; 32 * KB],
    palette_ram: [u8; KB],
    vram: [u8; 96 * KB],
    oam: [u8; KB],
    pub(crate) clocks: u32,
}
impl Cpu {
    /// Too big for the stack so we return a Box
    pub fn new() -> Box<Self> {
        // Was still getting stack overflows without box X
        let mut cpu = box Cpu {
            output_texture: [0u32; 240 * 160],
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
            game_pak: [0u8; 64 * KB],
            clocks: 0u32,
        };
        cpu.reset();
        cpu.bios_rom.clone_from_slice(include_bytes!("gba_bios.bin")); // Bios also taken from mrgba
        cpu
    }
    pub(crate) fn get_spsr_mut(&mut self) -> Option<&mut CPSR> {
        let mode = self.cpsr.mode();
        if mode == CpuMode::Privileged || mode == CpuMode::User {
            None
        } else {
            dbg!(self.cpsr.mode().as_usize() - 1);
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
        // std::mem::swap(x: &mut T, y: &mut T); // Can I use mem swap here?
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
            //WRAM - On-chip Work RAM   (32 KBytes)
            0x0300_0000..=0x0300_7FFF => self.wram_chip[addr - 0x0300_0000],
            //I/O Registers             (1022 Bytes)
            0x0400_0000..=0x0400_03FE => self.wram_chip[000],
            //Internal Display Memory
            0x0500_0000..=0x0500_03FF => self.palette_ram[addr - 0x0500_0000], /* BG/OBJ Palette RAM        (1 Kbyte) */
            0x0600_0000..=0x0601_7FFF => self.vram[addr - 0x0600_0000], /* VRAM - Video RAM          (96 KBytes) */
            0x0700_0000..=0x0700_03FF => self.oam[addr - 0x0700_0000], /* OAM - OBJ Attributes      (1 Kbyte) */
            //External Memory (Game Pak)
            // TODO: Wait states
            0x0800_0000..=0x09FF_FFFF => self.game_pak[addr - 0x0800_0000], /* Game Pak ROM/FlashROM (max 32MB) - Wait State 0 */
            0x0A00_0000..=0x0BFF_FFFF => self.game_pak[addr - 0x0A00_0000], /* Game Pak ROM/FlashROM (max 32MB) - Wait State 1 */
            0x0C00_0000..=0x0DFF_FFFF => self.game_pak[addr - 0x0C00_0000], /* Game Pak ROM/FlashROM (max 32MB) - Wait State 2 */
            0x0E00_0000..=0x0E00_FFFF => self.game_pak[addr - 0x0E00_0000], /* Game Pak SRAM    (max 64 KBytes) - 8bit Bus width */
            _ => {println!("Invalid address: {}", addr); 0u8},
        }
    }
    /// The GBA cpu always runs in LE mode
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
                self.wram_board[addr - 0x0200_0000] = byte;
            }
            //WRAM - On-chip Work RAM   (32 KBytes)
            0x0300_0000..=0x0300_7FFF => {
                self.wram_chip[addr - 0x0300_0000] = byte;
            }
            //I/O Registers             (1022 Bytes)
            0x0400_0000..=0x0400_03FE => {
                self.wram_chip[000] = byte;
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
            _ => println!("Invalid address: {}", addr),
        }
    }
    pub(crate) fn write_u16(&mut self, addr: u32, value: u16) {
        self.write_u8(addr, (value >> 8) as u8);
        self.write_u8(addr + 1, value as u8);
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
        self.fetch_u16(addr) as u32 + ((self.fetch_u16(addr + 2) as u32) << 16)
    }
    pub fn reset(&mut self) {
        self.regs = [0; 16];
        *self.pc() = RESET_HANDLER;
        self.output_texture = [0u32; 240 * 160];
        self.fiq_only_banks = [[0u32; 5]; 2];
        self.all_modes_banks = [[0u32; 2]; 6];
        self.spsrs = [CPSR(0x0000_0000F); 5];
        self.all_modes_banks[CpuMode::IRQ.as_usize()][0] = 0x3007fa0;
        self.all_modes_banks[CpuMode::Supervisor.as_usize()][0] = 0x3007fe0;
        self.cpsr = CPSR(0x0000_001F);
        self.set_mode(CpuMode::Privileged);
        self.wram_board = [0u8; 256 * KB];
        self.wram_chip = [0u8; 32 * KB];
        self.palette_ram = [0u8; KB];
        self.vram = [0u8; 96 * KB];
        self.oam = [0u8; KB];
        self.game_pak = [0u8; 64 * KB];
        self.clocks = 0u32;
        self.regs[13] = 50364160; // Taken from mrgba
    }
    /// Load a ROM from a reader
    pub fn load<T: std::io::Read>(&mut self, mut reader: T) {
        self.reset();
        let mut buf = Vec::with_capacity(16 * MB);
        reader.read_to_end(&mut buf).unwrap();
        //TODO: Verify nintendo logo?
        let title = &buf[0x00A0..0x00AC];
        let title =
            if let Some(idx) = title.iter().position(|x| *x == 0) { &title[0..idx] } else { title };
        let title = String::from_utf8(title.to_vec()).unwrap();
        let code = &buf[0x00AC..0x00B0];
        //dbg!(std::str::from_utf8(code).unwrap());
        for (input, out) in buf.iter().zip(self.game_pak.iter_mut()) {
            *out = *input;
        }
        println!(
            "Running rom '{}' (Code: {})",
            title,
            String::from_utf8(code.to_vec()).unwrap_or_else(|_| String::from("Invalid"))
        );
    }
    pub(crate) fn thumb(&mut self) -> bool {
        self.cpsr.T()
    }
    pub fn state_as_string(&mut self) -> String {
        let pc = *self.pc();
        format!("Registers: {:x?}\n PC:{:x} {}", self.regs, pc, self.cpsr.to_string())
    }
    pub fn run_once(&mut self) {
        println!("{}", self.state_as_string());
        if self.thumb() {
            thumb::execute_one_instruction(self);
        } else {
            arm::execute_one_instruction(self);
        }
    }
    pub fn run_forever(&mut self) {
        loop {
            self.run_once();
        }
    }
    pub(crate) fn vblank(&mut self) -> u32 {
        self.fetch_u32(0x0300_7FFC)
    }
}
