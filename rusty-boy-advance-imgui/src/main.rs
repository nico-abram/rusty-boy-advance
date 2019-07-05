use glium::{
  backend::Facade,
  texture::{ClientFormat, RawImage2d},
  Texture2d,
};
use imgui::{im_str, Condition, ImString};
use rusty_boy_advance::{LogLevel, GBABox};
use std::{borrow::Cow, io::Read};

mod support;

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
  let texture_id = system.renderer.textures().insert(std::rc::Rc::new(gl_texture));
  let mut running = true;
  let mut current_browse_memory: fn(&GBABox) -> &[u8] = |gba| gba.bios_bytes();
  let mut memory_browse_chunk_size = 4;
  let mut current_rom_file = ImString::with_capacity(128);
  system.main_loop(|_, ui, renderer, _display| {
    if running {
      gba.run_one_frame().unwrap();
    }
    if let Some(rom_name) = gba.loaded_rom().map(|rom| rom.title()) {
      ui.window(unsafe {
        imgui::ImStr::from_utf8_with_nul_unchecked(format!("{}\0", rom_name).as_str().as_bytes())
      })
      .position([400.0, 0.0], Condition::Appearing)
      .always_auto_resize(true)
      .build(|| {
        let texture = renderer.textures().get(texture_id).unwrap();
        let raw = RawImage2d {
          data: Cow::Owned(gba.video_output().into()),
          width: WIDTH as u32,
          height: HEIGHT as u32,
          format: ClientFormat::U8U8U8U8,
        };
        texture.write(glium::Rect { left: 0, bottom: 0, width: WIDTH, height: HEIGHT }, raw);
        ui.image(texture_id, [WIDTH as f32, HEIGHT as f32]).build();
      });
    }
    ui.window(im_str!("CPU State"))
      .size([200.0, 305.0], Condition::Appearing)
      .resizable(false)
      .position([125.0, 0.0], Condition::Appearing)
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
        for (num, val) in gba.registers().iter().enumerate() {
          ui.text(format!("r{:<2}:{:08x}", num, *val));
        }
      });
    ui.window(im_str!("Debug Buttons"))
      .position([0.0, 0.0], Condition::Appearing)
      .always_auto_resize(true)
      .build(|| {
        let stop_continue_label = if running { im_str!("Stop") } else { im_str!("Continue") };
        if ui.button(stop_continue_label, [100.0, 50.0]) {
          running = !running;
        }
        if !running {
          if ui.button(im_str!("Step"), [100.0, 50.0]) {
            gba.run_one_instruction().unwrap();
          }
        }
      });
    ui.window(im_str!("ROM Loading"))
      .position([0.0, 350.0], Condition::Appearing)
      .always_auto_resize(true)
      .build(|| {
        ui.input_text(im_str!("file"), &mut current_rom_file).build();
        if ui.button(im_str!("Load and Reset"), [100.0, 50.0]) {
          let rom_file =
            unsafe { std::ffi::CStr::from_ptr(current_rom_file.as_ptr()) }.to_str().unwrap();
          if let Ok(mut rom_file) = std::fs::File::open(rom_file) {
            let mut contents = Vec::with_capacity(32 * 1024 * 1024);
            rom_file.read_to_end(&mut contents);
            gba.load(&contents[..]);
          }
        }
      });
    ui.window(im_str!("Memory"))
      .position([0.0, 350.0], Condition::Appearing)
      .always_auto_resize(true)
      .build(|| {
        if ui.button(im_str!("BIOS"), [100.0, 50.0]) {
          current_browse_memory = |gba| gba.bios_bytes();
        }
        ui.same_line(0.0);
        if ui.button(im_str!("ROM"), [100.0, 50.0]) {
          current_browse_memory = |gba| gba.loaded_rom().unwrap().game_pak();
        }
        ui.same_line(0.0);
        if ui.button(im_str!("VRAM"), [100.0, 50.0]) {
          unimplemented!()
        }
        ui.separator();
        if ui.button(im_str!("ARM style"), [100.0, 50.0]) {
          memory_browse_chunk_size = 4;
        }
        ui.same_line(0.0);
        if ui.button(im_str!("THUMB style"), [100.0, 50.0]) {
          memory_browse_chunk_size = 2;
        }
        ui.separator();
        ui.child_frame(im_str!("child frame"), [400.0, 100.0])
          .show_borders(true)
          .always_show_vertical_scroll_bar(true)
          .build(|| {
            for chunks in current_browse_memory(&gba).chunks(memory_browse_chunk_size) {
              ui.text(chunks.iter().map(|x| format!("{:x}", x)).collect::<Vec<_>>().join(""));
            }
          });
      });
    //ui.show_demo_window(opened);
    //ui.show_metrics_window(opened);
  });
  Ok(())
}
