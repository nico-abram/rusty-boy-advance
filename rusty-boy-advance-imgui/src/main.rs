use capstone::prelude::*;
use glium::{
  backend::Facade,
  texture::{ClientFormat, RawImage2d},
  uniforms::SamplerBehavior,
  Texture2d,
};
use imgui::{Condition, ImString, SelectableFlags};
use imgui_winit_support::winit::event::ElementState;
use rusty_boy_advance::{GBABox, GBAButton, LogLevel};

use std::{borrow::Cow, collections::VecDeque, io::Read};

mod support;

#[derive(Copy, Clone, PartialEq, Debug)]
enum BrowsableMemory {
  BIOS,
  ChipWRAM,
  BoardWRAM,
  ROM,
  VRAM,
  Palette,
  OAM,
  IO,
}
fn offset(mem: BrowsableMemory) -> u32 {
  match mem {
    BrowsableMemory::BIOS => 0,
    BrowsableMemory::BoardWRAM => 0x0200_0000,
    BrowsableMemory::ChipWRAM => 0x0300_0000,
    BrowsableMemory::IO => 0x0400_0000,
    BrowsableMemory::Palette => 0x0500_0000,
    BrowsableMemory::VRAM => 0x0600_0000,
    BrowsableMemory::OAM => 0x0700_0000,
    BrowsableMemory::ROM => 0x0800_0000,
  }
}
fn get_memory(gba: &GBABox, mem: BrowsableMemory) -> &[u8] {
  match mem {
    BrowsableMemory::BIOS => gba.bios_bytes(),
    BrowsableMemory::BoardWRAM => gba.board_wram_bytes(),
    BrowsableMemory::ChipWRAM => gba.chip_wram_bytes(),
    BrowsableMemory::IO => gba.io_memory(),
    BrowsableMemory::Palette => gba.palette_bytes(),
    BrowsableMemory::VRAM => gba.vram_bytes(),
    BrowsableMemory::ROM => gba.loaded_rom().unwrap().game_pak(),
    BrowsableMemory::OAM => gba.oam_bytes(),
  }
}
struct BrowsedMemory {
  mem: BrowsableMemory,
  chunk_size: usize,
  auto_update: bool,
  contents: Vec<(ImString, String, String, u32, bool)>,
  breakpoints: Vec<u32>,
  is_little_endian: bool,
  last_update: std::time::Instant,
}
fn update_currently_browsed_memory_string(gba: &GBABox, mem: &mut BrowsedMemory) {
  mem.contents.clear();
  let disassembler = Capstone::new()
    .arm()
    .mode(if mem.chunk_size == 2 { arch::arm::ArchMode::Thumb } else { arch::arm::ArchMode::Arm })
    .build()
    .unwrap();
  for (chunk_idx, chunks) in get_memory(gba, mem.mem).chunks(mem.chunk_size).enumerate() {
    let addr = ((chunk_idx * mem.chunk_size) as u32) + offset(mem.mem);
    let asm = disassembler.disasm_all(chunks, 0).unwrap();
    mem.contents.push((
      ImString::new(format!("{:08x}:", addr)),
      (if !mem.is_little_endian {
        chunks.iter().rev().map(|x| format!("{:02x}", x)).collect::<Vec<_>>()
      } else {
        chunks.iter().map(|x| format!("{:02x}", x)).collect::<Vec<_>>()
      })
      .join(""),
      asm
        .iter()
        .next()
        .map(|x| format!("{} {}", x.mnemonic().unwrap_or("err"), x.op_str().unwrap_or("err")))
        .unwrap_or_else(|| String::from("Error")),
      addr,
      mem.breakpoints.iter().any(|x| *x == addr),
    ));
  }
}
#[repr(transparent)]
pub struct RacyUnsafeCell<T>(std::cell::UnsafeCell<T>);

unsafe impl<T: Sync> Sync for RacyUnsafeCell<T> {}

impl<T> RacyUnsafeCell<T> {
  pub const fn new(x: T) -> Self {
    RacyUnsafeCell(std::cell::UnsafeCell::new(x))
  }
  pub fn get(&self) -> *mut T {
    self.0.get()
  }
}
const LOGS_SIZE: usize = 10_000;
static mut GBA_LOGS: Option<RacyUnsafeCell<VecDeque<String>>> = None;

fn push_log(s: &str) {
  let logs = unsafe { &mut *GBA_LOGS.as_ref().unwrap().get() };
  logs.push_front(String::from(s));
  logs.truncate(LOGS_SIZE);
}
fn main() -> std::result::Result<(), Box<dyn std::error::Error>> {
  unsafe { GBA_LOGS = Some(RacyUnsafeCell::new(VecDeque::with_capacity(LOGS_SIZE))) };
  const WIDTH: u32 = 240;
  const HEIGHT: u32 = 160;
  let mut log_level = LogLevel::None;
  let mut gba = GBABox::new(log_level, None, Some(push_log));
  let mut system = support::init("Rusty Boy Advance ImGui");
  let gl_texture = {
    let raw = RawImage2d {
      data: Cow::Owned(gba.video_output().into()),
      width: WIDTH as u32,
      height: HEIGHT as u32,
      format: ClientFormat::U8U8U8,
    };
    let tex2d = Texture2d::new(system.display.get_context(), raw).unwrap();
    imgui_glium_renderer::Texture { texture: tex2d.into(), sampler: SamplerBehavior::default() }
  };
  let video_output_texture_id = system.renderer.textures().insert(gl_texture);
  let mut current_rom_file = String::with_capacity(128);
  let mut running = false;
  let mut just_clicked_continue = false;
  let mut rom_open_msg = None;
  let mut variable_step = 0;
  let mut browsed_memory = BrowsedMemory {
    mem: BrowsableMemory::BIOS,
    chunk_size: 4,
    auto_update: true,
    contents: Vec::with_capacity(1024 * 1024 * 16),
    breakpoints: Vec::new(),
    is_little_endian: false,
    last_update: std::time::Instant::now(),
  };

  update_currently_browsed_memory_string(&gba, &mut browsed_memory);
  system.main_loop(|_opened, ui, renderer, _display, framerate, key_events| {
    let mut scroll_memory_to_pc = false;
    if running {
      if !ui.io().want_capture_keyboard {
        for (key, state) in key_events {
          let input_f = if *state == ElementState::Pressed {
            GBABox::persistent_input_pressed
          } else {
            GBABox::persistent_input_released
          };
          use imgui_winit_support::winit::keyboard::{KeyCode, PhysicalKey};
          match key {
            PhysicalKey::Code(KeyCode::Enter) => input_f(&mut gba, GBAButton::Start),
            PhysicalKey::Code(KeyCode::ArrowLeft) => input_f(&mut gba, GBAButton::Left),
            PhysicalKey::Code(KeyCode::ArrowUp) => input_f(&mut gba, GBAButton::Up),
            PhysicalKey::Code(KeyCode::ArrowDown) => input_f(&mut gba, GBAButton::Down),
            PhysicalKey::Code(KeyCode::ArrowRight) => input_f(&mut gba, GBAButton::Right),
            _ => (),
          };
        }
      }
      if browsed_memory.breakpoints.is_empty() {
        gba.run_one_frame().unwrap();
      } else {
        for _ in 0..5000 {
          if !just_clicked_continue
            && browsed_memory.breakpoints.iter().any(|x| *x == gba.registers()[15])
          {
            running = false;
            break;
          }
          just_clicked_continue = false;
          gba.run_one_instruction().unwrap();
        }
        gba.update_video_output();
      }
      just_clicked_continue = false;
    }
    let x = ui.window_size();
    ui.window(unsafe {
      imgui::ImStr::from_utf8_with_nul_unchecked(
        format!(
          "{} ({:03.0} FPS)###main\0",
          gba
            .loaded_rom()
            .map(|rom| rom.title())
            .unwrap_or(&(x[0].to_string() + "," + &x[1].to_string() + "No ROM loaded")),
          framerate
        )
        .as_str()
        .as_bytes(),
      )
    })
    .position([210.0, 110.0], Condition::Appearing)
    .always_auto_resize(true)
    .build(|| {
      let texture = renderer.textures().get(video_output_texture_id).unwrap();
      let raw = RawImage2d {
        data: Cow::Owned(gba.video_output().into()),
        width: WIDTH as u32,
        height: HEIGHT as u32,
        format: ClientFormat::U8U8U8,
      };
      texture.texture.write(glium::Rect { left: 0, bottom: 0, width: WIDTH, height: HEIGHT }, raw);
      imgui::Image::new(video_output_texture_id, [WIDTH as f32, HEIGHT as f32]).build(ui);
    });
    ui.window("CPU State")
      .size([210.0, 305.0], Condition::Appearing)
      .resizable(false)
      .position([0.0, 0.0], Condition::Appearing)
      .build(|| {
        ui.columns(2, "a", true);
        let cpsr = gba.cpsr();
        ui.text(format!(
          "N:{} \nC:{} \nZ:{} \nV:{} \nI:{} \nF:{} \nT:{} \nmode:{:x}\n{}",
          cpsr.negative_flag(),
          cpsr.carry_flag(),
          cpsr.zero_flag(),
          cpsr.overflow_flag(),
          cpsr.irq_disabled_flag(),
          cpsr.fiq_disabled_flag(),
          cpsr.thumb_state_flag(),
          cpsr.mode(),
          cpsr.mode()
        ));
        ui.set_column_offset(1, 80.0);
        ui.next_column();
        const REGISTER_NAMES: [&str; 16] = [
          "a1", "a2", "a3", "a4", "v1", "v2", "v3", "v4", "v5", "sb", "sl", "fp", "ip", "sp", "lr",
          "pc",
        ];
        for (idx, (value, name)) in gba.registers().iter().zip(REGISTER_NAMES.iter()).enumerate() {
          ui.text(format!("{} r{:<2}:{:08x}", name, idx, *value));
        }
      });
    ui.window("Debug Buttons")
      .position([800.0, 0.0], Condition::Appearing)
      .always_auto_resize(true)
      .build(|| {
        let stop_continue_icon = if running { " " } else { " " };
        let icon_size = [30.0, 15.0];
        if ui.button_with_size(stop_continue_icon, icon_size) {
          running = !running;
          just_clicked_continue = true;
        }
        ui.same_line();
        ui.text(if running { "Stop" } else { "Continue" });
        if !running {
          if ui.button_with_size(" ", icon_size) {
            gba.run_one_instruction().unwrap();
            gba.update_video_output();
          }
          ui.same_line();
          ui.text("Step");
          if ui.button_with_size("#1", icon_size) {
            for _ in 0..10 {
              gba.run_one_instruction().unwrap();
            }
            gba.update_video_output();
          }
          ui.same_line();
          ui.text("Step(10)");
          if ui.button_with_size("#2", icon_size) {
            for _ in 0..100 {
              gba.run_one_instruction().unwrap();
            }
            gba.update_video_output();
          }
          ui.same_line();
          ui.text("Step(100)");
          if ui.button_with_size("#3", icon_size) {
            for _ in 0..1000 {
              gba.run_one_instruction().unwrap();
            }
            gba.update_video_output();
          }
          ui.same_line();
          ui.text("Step(1000)");
          if ui.button_with_size("#4", icon_size) {
            for _ in 0..10000 {
              gba.run_one_instruction().unwrap();
            }
            gba.update_video_output();
          }
          ui.same_line();
          ui.text("Step(10000)");
          if ui.button_with_size("#5", icon_size) {
            for _ in 0..100_000 {
              gba.run_one_instruction().unwrap();
            }
            gba.update_video_output();
          }
          ui.same_line();
          ui.text("Step(100000)");
          if ui.button_with_size("#6", icon_size) {
            for _ in 0..variable_step {
              gba.run_one_instruction().unwrap();
            }
            gba.update_video_output();
          }
          ui.same_line();
          ui.text("Step(");
          ui.same_line();
          let _pushed_width = ui.push_item_width(120.0);
          ui.input_int(")##7", &mut variable_step).step(0).build();
        }
        ui.separator();
        ui.text("Log Level");
        if ui.radio_button("None", &mut log_level, LogLevel::None) {
          gba.set_log_level(log_level);
        }
        ui.same_line();
        if ui.radio_button("Medium", &mut log_level, LogLevel::EveryInstruction) {
          gba.set_log_level(log_level);
        }
        ui.same_line();
        if ui.radio_button("Debug", &mut log_level, LogLevel::Debug) {
          gba.set_log_level(log_level);
        }
        ui.separator();
        scroll_memory_to_pc = ui.small_button("Goto PC");
      });
    ui.window("ROM Loading")
      .position([210.0, 0.0], Condition::Appearing)
      .size([260.0, 110.0], Condition::Appearing)
      .build(|| {
        if ui.small_button("Browse") {
          match nfd::open_file_dialog(None, None).unwrap_or(nfd::Response::Cancel) {
            nfd::Response::Okay(file_path) => current_rom_file = file_path,
            nfd::Response::OkayMultiple(_) => std::panic!("This should never happen"),
            nfd::Response::Cancel => (), // ignore
          }
        }
        ui.text("File: ");
        ui.same_line();
        ui.input_text(" ", &mut current_rom_file).build();
        if ui.small_button("Load and Reset") {
          let success = if let Ok(mut rom_file) = std::fs::File::open(&current_rom_file) {
            let mut contents = Vec::with_capacity(32 * 1024 * 1024);
            rom_file
              .read_to_end(&mut contents)
              .map(|_| gba.load(&contents[..]).is_ok())
              .unwrap_or(false)
          } else {
            false
          };
          if !success {
            rom_open_msg = Some("Error loading rom file");
          } else {
            rom_open_msg = None;
          }
        }
        if let Some(msg) = rom_open_msg {
          ui.text(msg);
        }
      });
    ui.window("Memory Viewer")
      .position([465.0, 0.0], Condition::Appearing)
      .always_auto_resize(true)
      .build(|| {
        if ui.radio_button("BIOS", &mut browsed_memory.mem, BrowsableMemory::BIOS) {
          update_currently_browsed_memory_string(&gba, &mut browsed_memory);
        }
        ui.same_line();
        if ui.radio_button("board-WRAM", &mut browsed_memory.mem, BrowsableMemory::BoardWRAM) {
          update_currently_browsed_memory_string(&gba, &mut browsed_memory);
        }
        ui.same_line();
        if ui.radio_button("chip-WRAM", &mut browsed_memory.mem, BrowsableMemory::ChipWRAM) {
          update_currently_browsed_memory_string(&gba, &mut browsed_memory);
        }
        if ui.radio_button("Palette", &mut browsed_memory.mem, BrowsableMemory::Palette) {
          update_currently_browsed_memory_string(&gba, &mut browsed_memory);
        }
        ui.same_line();
        if ui.radio_button("VRAM", &mut browsed_memory.mem, BrowsableMemory::VRAM) {
          update_currently_browsed_memory_string(&gba, &mut browsed_memory);
        }
        ui.same_line();
        if ui.radio_button("I/O", &mut browsed_memory.mem, BrowsableMemory::IO) {
          update_currently_browsed_memory_string(&gba, &mut browsed_memory);
        }
        ui.same_line();
        if ui.radio_button("OAM", &mut browsed_memory.mem, BrowsableMemory::OAM) {
          update_currently_browsed_memory_string(&gba, &mut browsed_memory);
        }
        ui.same_line();
        if ui.radio_button("ROM", &mut browsed_memory.mem, BrowsableMemory::ROM) {
          if gba.loaded_rom().is_some() {
            update_currently_browsed_memory_string(&gba, &mut browsed_memory);
          } else {
            browsed_memory.mem = BrowsableMemory::BIOS;
          }
        }
        ui.separator();
        if ui.radio_button("ARM style", &mut browsed_memory.chunk_size, 4) {
          update_currently_browsed_memory_string(&gba, &mut browsed_memory);
        }
        ui.same_line();
        if ui.radio_button("THUMB style", &mut browsed_memory.chunk_size, 2) {
          browsed_memory.chunk_size = 2;
          update_currently_browsed_memory_string(&gba, &mut browsed_memory);
        }
        ui.separator();
        if ui.radio_button("Big Endian", &mut browsed_memory.is_little_endian, false) {
          update_currently_browsed_memory_string(&gba, &mut browsed_memory);
        }
        ui.same_line();
        if ui.radio_button("Little Endian", &mut browsed_memory.is_little_endian, true) {
          update_currently_browsed_memory_string(&gba, &mut browsed_memory);
        }
        ui.separator();
        ui.checkbox("Auto Update", &mut browsed_memory.auto_update);
        ui.separator();
        ui.child_window("child frame")
          .size([400.0, 400.0])
          .border(true)
          .always_vertical_scrollbar(true)
          .build(|| {
            if browsed_memory.auto_update && browsed_memory.last_update.elapsed().as_millis() > 500
            {
              update_currently_browsed_memory_string(&gba, &mut browsed_memory);
              browsed_memory.last_update = std::time::Instant::now();
            }
            ui.columns(4, "memory contents", true);
            ui.set_column_offset(1, 0.0);
            ui.set_column_offset(2, 90.0);
            ui.set_column_offset(3, 160.0);
            ui.next_column();
            ui.text("Address");
            ui.next_column();
            ui.text("Value");
            ui.next_column();
            ui.text("Assembly");
            ui.next_column();
            if scroll_memory_to_pc {
              let pc = gba.registers()[15];
              let region = match pc {
                0..=0x01FF_FFFF => BrowsableMemory::BIOS,
                0x0200_0000..=0x02FF_FFFF => BrowsableMemory::BoardWRAM,
                0x0300_0000..=0x03FF_FFFF => BrowsableMemory::ChipWRAM,
                0x0400_0000..=0x04FF_FFFF => BrowsableMemory::IO,
                0x0500_0000..=0x05FF_FFFF => BrowsableMemory::Palette,
                0x0600_0000..=0x06FF_FFFF => BrowsableMemory::VRAM,
                0x0700_0000..=0x07FF_FFFF => BrowsableMemory::OAM,
                _ => BrowsableMemory::ROM,
              };
              browsed_memory.mem = region;
              update_currently_browsed_memory_string(&gba, &mut browsed_memory);
            }
            let (contents, breakpoints) =
              (&mut browsed_memory.contents, &mut browsed_memory.breakpoints);
            let pc = gba.registers()[15];
            let chunk_size = browsed_memory.chunk_size as u32;
            for (addr_string, value, disassembly, addr_u32, checkpoint_set) in contents {
              let this_line_is_current_pc = pc >= *addr_u32 && pc < (*addr_u32 + chunk_size);
              if scroll_memory_to_pc && this_line_is_current_pc {
                scroll_memory_to_pc = false;
                unsafe { imgui::sys::igSetScrollHereY(0.0) };
              }
              if this_line_is_current_pc {
                ui.selectable_config("###1")
                  .size([400.0, 20.0])
                  .flags(SelectableFlags::SPAN_ALL_COLUMNS)
                  .selected(true);
                ui.same_line();
              }
              ui.next_column();
              if ui.checkbox(addr_string, checkpoint_set) {
                if *checkpoint_set {
                  breakpoints.push(*addr_u32);
                } else {
                  breakpoints.retain(|x| *x != *addr_u32);
                }
              }
              ui.next_column();
              ui.text(value);
              ui.next_column();
              ui.text(disassembly);
              ui.next_column();
            }
          });
      });
    ui.window("Logs")
      .position([0.0, 305.0], Condition::Appearing)
      .size([1024.0, 285.0], Condition::Appearing)
      .always_vertical_scrollbar(true)
      .build(|| {
        for log_line in unsafe { &*GBA_LOGS.as_ref().unwrap().get() }.iter().rev() {
          ui.text(log_line);
        }
      });
    //ui.show_demo_window(_opened);
    //ui.show_metrics_window(opened);
  });
  Ok(())
}
