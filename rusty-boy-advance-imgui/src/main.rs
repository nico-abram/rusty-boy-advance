use glium::{
  backend::Facade,
  texture::{ClientFormat, RawImage2d},
  Texture2d,
};
use imgui::{im_str, Condition, ImString};
use rusty_boy_advance::{GBABox, LogLevel};
use std::{borrow::Cow, io::Read};

mod support;

#[derive(Copy, Clone, PartialEq)]
enum BrowsableMemory {
  BIOS,
  ROM,
  VRAM,
}
fn offset(mem: BrowsableMemory) -> u32 {
  match mem {
    BrowsableMemory::BIOS => 0,
    BrowsableMemory::ROM => 0x0800_0000,
    BrowsableMemory::VRAM => 0x0600_0000,
  }
}
fn get_memory(gba: &GBABox, mem: BrowsableMemory) -> &[u8] {
  match mem {
    BrowsableMemory::BIOS => gba.bios_bytes(),
    BrowsableMemory::ROM => gba.loaded_rom().unwrap().game_pak(),
    BrowsableMemory::VRAM => gba.vram(),
  }
}
struct BrowsedMemory {
  mem: BrowsableMemory,
  chunk_size: usize,
  auto_update: bool,
  contents: Vec<(ImString, String, u32, bool)>,
  breakpoints: Vec<u32>,
}
fn update_currently_browsed_memory_string(gba: &GBABox, mem: &mut BrowsedMemory) {
  mem.contents.clear();
  for (chunk_idx, chunks) in get_memory(gba, mem.mem).chunks(mem.chunk_size).enumerate() {
    let addr = ((chunk_idx * mem.chunk_size) as u32) + offset(mem.mem);
    mem.contents.push((
      ImString::new(format!("{:08x}:", addr)),
      chunks.iter().map(|x| format!("{:02x}", x)).collect::<Vec<_>>().join(""),
      addr,
      mem.breakpoints.iter().any(|x| *x == addr),
    ));
  }
}

fn main() -> std::result::Result<(), Box<dyn std::error::Error>> {
  const WIDTH: u32 = 240;
  const HEIGHT: u32 = 160;
  let mut gba = GBABox::new(LogLevel::None, None, Some(|x| print!("{}", x)));
  let mut system = support::init("Rusty Boy Advance ImGui");
  let gl_texture = {
    let raw = RawImage2d {
      data: Cow::Owned(gba.video_output().into()),
      width: WIDTH as u32,
      height: HEIGHT as u32,
      format: ClientFormat::U8U8U8U8,
    };
    Texture2d::new(system.display.get_context(), raw).unwrap()
  };
  let video_output_texture_id = system.renderer.textures().insert(std::rc::Rc::new(gl_texture));
  let mut current_rom_file = ImString::with_capacity(128);
  let mut running = false;
  let mut just_clicked_continue = false;
  let mut rom_open_msg = None;
  let mut browsed_memory = BrowsedMemory {
    mem: BrowsableMemory::BIOS,
    chunk_size: 4,
    auto_update: false,
    contents: Vec::with_capacity(1024 * 1024 * 16),
    breakpoints: Vec::new(),
  };

  update_currently_browsed_memory_string(&gba, &mut browsed_memory);
  system.main_loop(|opened, ui, renderer, _display| {
    if running {
      if browsed_memory.breakpoints.len() == 0 {
        gba.run_one_frame().unwrap();
      } else {
        for _ in 0..1000 {
          if !just_clicked_continue
            && browsed_memory.breakpoints.iter().any(|x| *x == gba.registers()[15])
          {
            running = false;
            break;
          }
          just_clicked_continue = false;
          gba.run_one_instruction();
        }
      }
      just_clicked_continue = false;
    }
    ui.window(unsafe {
      imgui::ImStr::from_utf8_with_nul_unchecked(
        format!("{}\0", gba.loaded_rom().map(|rom| rom.title()).unwrap_or("No ROM loaded"))
          .as_str()
          .as_bytes(),
      )
    })
    .position([210.0, 90.0], Condition::Appearing)
    .always_auto_resize(true)
    .build(|| {
      let texture = renderer.textures().get(video_output_texture_id).unwrap();
      let raw = RawImage2d {
        data: Cow::Owned(gba.video_output().into()),
        width: WIDTH as u32,
        height: HEIGHT as u32,
        format: ClientFormat::U8U8U8U8,
      };
      texture.write(glium::Rect { left: 0, bottom: 0, width: WIDTH, height: HEIGHT }, raw);
      ui.image(video_output_texture_id, [WIDTH as f32, HEIGHT as f32]).build();
    });
    ui.window(im_str!("CPU State"))
      .size([210.0, 305.0], Condition::Appearing)
      .resizable(false)
      .position([0.0, 0.0], Condition::Appearing)
      .build(|| {
        ui.columns(2, im_str!("a"), true);
        let cpsr = gba.cpsr();
        ui.text(format!(
          "N:{} \nC:{} \nZ:{} \nV:{} \nI:{} \nF:{} \nT:{} \nmode:{:x}",
          cpsr.negative_flag(),
          cpsr.carry_flag(),
          cpsr.zero_flag(),
          cpsr.overflow_flag(),
          cpsr.irq_disabled_flag(),
          cpsr.fiq_disabled_flag(),
          cpsr.thumb_state_flag(),
          cpsr.mode()
        ));
        ui.set_column_offset(1, 80.0);
        ui.next_column();
        const register_names: [&str; 16] = [
          "a1", "a2", "a3", "a4", "v1", "v2", "v3", "v4", "v5", "sb", "sl", "fp", "ip", "sp", "lr",
          "pc",
        ];
        for (idx, (value, name)) in gba.registers().iter().zip(register_names.iter()).enumerate() {
          ui.text(format!("{} r{:<2}:{:08x}", name, idx, *value));
        }
      });
    ui.window(im_str!("Debug Buttons"))
      .position([210.0, 285.0], Condition::Appearing)
      .always_auto_resize(true)
      .build(|| {
        let stop_continue_icon = if running { im_str!(" ") } else { im_str!(" ") };
        let icon_size = [30.0, 15.0];
        if ui.button(stop_continue_icon, icon_size) {
          running = !running;
          just_clicked_continue = true;
        }
        ui.same_line(0.0);
        ui.text(if running { im_str!("Stop") } else { im_str!("Continue") });
        if !running {
          if ui.button(im_str!(" "), icon_size) {
            gba.run_one_instruction().unwrap();
          }
          ui.same_line(0.0);
          ui.text("Step");
          if ui.button(im_str!(""), icon_size) {
            for _ in 0..10 {
              gba.run_one_instruction().unwrap();
            }
          }
          ui.same_line(0.0);
          ui.text("Step(10)");
        }
      });
    ui.window(im_str!("ROM Loading"))
      .position([210.0, 0.0], Condition::Appearing)
      .size([260.0, 90.0], Condition::Appearing)
      .build(|| {
        if ui.small_button(im_str!("Browse")) {
          match nfd::open_file_dialog(None, None).unwrap_or(nfd::Response::Cancel) {
            nfd::Response::Okay(file_path) => current_rom_file = ImString::new(file_path),
            nfd::Response::OkayMultiple(files) => std::panic!("This should never happen"),
            nfd::Response::Cancel => (), // ignore
          }
        }
        ui.text("File: ");
        ui.same_line(0.0);
        ui.input_text(im_str!(" "), &mut current_rom_file).build();
        if ui.small_button(im_str!("Load and Reset")) {
          let rom_file =
            unsafe { std::ffi::CStr::from_ptr(current_rom_file.as_ptr()) }.to_str().unwrap();
          let success = if let Ok(mut rom_file) = std::fs::File::open(rom_file) {
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
    ui.window(im_str!("Memory Viewer"))
      .position([465.0, 0.0], Condition::Appearing)
      .always_auto_resize(true)
      .build(|| {
        if ui.radio_button(im_str!("BIOS"), &mut browsed_memory.mem, BrowsableMemory::BIOS) {
          update_currently_browsed_memory_string(&gba, &mut browsed_memory);
        }
        ui.same_line(0.0);
        if ui.radio_button(im_str!("ROM"), &mut browsed_memory.mem, BrowsableMemory::ROM) {
          update_currently_browsed_memory_string(&gba, &mut browsed_memory);
        }
        ui.same_line(0.0);
        if ui.radio_button(im_str!("VRAM"), &mut browsed_memory.mem, BrowsableMemory::VRAM) {
          update_currently_browsed_memory_string(&gba, &mut browsed_memory);
        }
        if ui.small_button(im_str!("ARM style")) {
          browsed_memory.chunk_size = 4;
          update_currently_browsed_memory_string(&gba, &mut browsed_memory);
        }
        ui.same_line(0.0);
        if ui.small_button(im_str!("THUMB style")) {
          browsed_memory.chunk_size = 2;
          update_currently_browsed_memory_string(&gba, &mut browsed_memory);
        }
        ui.checkbox(im_str!("Auto Update"), &mut browsed_memory.auto_update);
        ui.separator();
        ui.child_frame(im_str!("child frame"), [320.0, 400.0])
          .show_borders(true)
          .always_show_vertical_scroll_bar(true)
          .build(|| {
            if browsed_memory.auto_update {
              update_currently_browsed_memory_string(&gba, &mut browsed_memory);
            }
            ui.columns(2, im_str!("memory contents"), true);
            ui.set_column_offset(1, 150.0);
            ui.text("Address");
            ui.next_column();
            ui.text("Value");
            ui.next_column();
            let (contents, breakpoints) =
              (&mut browsed_memory.contents, &mut browsed_memory.breakpoints);
            for (addr_string, value, addr_u32, checkpoint_set) in contents {
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
            }
          });
      });
    //ui.show_demo_window(opened);
    //ui.show_metrics_window(opened);
  });
  Ok(())
}
