use rusty_boy_advance::{GBABox, LogLevel};
#[cfg(target_arch = "wasm32")]
#[macro_use]
extern crate stdweb;

use quicksilver::{
  geom::Vector,
  graphics::{Background::Img, Color, Image, PixelFormat},
  lifecycle::{Settings, State, Window},
  Result,
};

fn polymorphic_print(x: &str) {
  #[cfg(target_arch = "wasm32")]
  {
    js! {
      console.log(@{x});
    }
  }
  #[cfg(not(target_arch = "wasm32"))]
  {
    println!("{}", x);
  }
}

struct GameState {
  gba: GBABox,
}

impl State for GameState {
  fn new() -> Result<Self> {
    unimplemented!()
  }

  fn draw(&mut self, window: &mut Window) -> Result<()> {
    window.set_title(
      format!("Rusty Boy Advance Quicksilver ({:.2} FPS)", window.average_fps()).as_str(),
    );
    window.clear(Color::WHITE)?;
    self
      .gba
      .run_one_frame()
      .map_err(|_| quicksilver::Error::ContextError(String::from("Error running a GBA frame")))?;
    let mut rgba_formatted = [255u8; 255 * 160 * 4];
    for (output, input) in rgba_formatted.chunks_mut(4).zip(self.gba.video_output().chunks(3)) {
      output[..3].clone_from_slice(input);
    }
    let img = Image::from_raw(&rgba_formatted[..], 240, 160, PixelFormat::RGBA)?;
    // This doesnt work in the browser for some reason (But does on desktop)
    // https://github.com/ryanisaacg/quicksilver/issues/512
    //let img = Image::from_raw(self.gba.video_output(), 240, 160, PixelFormat::RGB)?;
    window.draw(&img.area(), Img(&img));
    Ok(())
  }
}
fn main() -> std::result::Result<(), Box<dyn std::error::Error>> {
  let mut gba = GBABox::new(LogLevel::None, None, Some(polymorphic_print));
  let rom_bytes = include_bytes!("armwrestler.gba");
  gba
    .load(rom_bytes)
    .map_err(|_| quicksilver::Error::ContextError(String::from("Error loading ROM")))?;
  quicksilver::lifecycle::run_with(
    "Rusty Boy Advance Quicksilver",
    Vector::new(240, 160),
    Settings { vsync: false, ..Settings::default() },
    || Ok(GameState { gba }),
  );
  Ok(())
}
