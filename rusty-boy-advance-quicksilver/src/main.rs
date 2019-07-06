use rusty_boy_advance::{GBABox, LogLevel};

use quicksilver::{
  geom::Vector,
  graphics::{Background::Img, Color, Image, PixelFormat},
  lifecycle::{Asset, Settings, State, Window},
  Result,
};

struct GameState {
  gba: GBABox,
}

impl State for GameState {
  fn new() -> Result<Self> {
    unimplemented!()
  }

  fn draw(&mut self, window: &mut Window) -> Result<()> {
    window.clear(Color::WHITE)?;
    self.gba.run_one_frame().unwrap();
    let mut a = [0u8; 240 * 160 * 3];
    for (idx, x) in self.gba.video_output().iter().enumerate() {
      a[idx] = *x;
    }
    let img = Image::from_raw(&a[..], 240, 160, PixelFormat::RGB)?;
    window.draw(&img.area(), Img(&img));
    Ok(())
  }
}

fn main() -> std::result::Result<(), Box<dyn std::error::Error>> {
  let mut gba = GBABox::new(LogLevel::None, None, Some(|x| print!("{}", x)));
  let mut r = Asset::new(quicksilver::load_file("e.gba"));
  r.execute(|rom_file| {
    gba
      .load(&rom_file[..])
      .map_err(|_| quicksilver::Error::ContextError(String::from("Error loading ROM")))
  })?;
  quicksilver::lifecycle::run_with(
    "Rusty Boy Advance Quicksilver",
    Vector::new(240, 160),
    Settings::default(),
    || Ok(GameState { gba }),
  );
  Ok(())
}
