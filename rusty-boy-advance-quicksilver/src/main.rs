use rusty_boy_advance::{LogLevel, GBA};

use quicksilver::{
  geom::Vector,
  graphics::{Background::Img, Color, Image, PixelFormat},
  lifecycle::{Asset, Settings, State, Window},
  Result,
};

struct GameState {
  gba: GBA,
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
      a[idx * 3 + 0] = ((*x & 0x00FF_0000) >> 16) as u8;
      a[idx * 3 + 1] = ((*x & 0x0000_FF00) >> 8) as u8;
      a[idx * 3 + 2] = ((*x & 0x0000_00FF) >> 0) as u8;
    }
    let img = Image::from_raw(&a[..], 240, 160, PixelFormat::RGB)?;
    window.draw(&img.area(), Img(&img));
    Ok(())
  }
}

fn main() -> std::result::Result<(), Box<dyn std::error::Error>> {
  let mut gba = GBA::new(LogLevel::None, None, Some(|x| print!("{}", x)));
  let mut r = Asset::new(quicksilver::load_file("e.gba"));
  r.execute(|rom_file| {
    gba.load(&rom_file[..]).unwrap();
    Ok(())
  });
  quicksilver::lifecycle::run_with(
    "Rusty Boy Advance Quicksilver",
    Vector::new(240, 160),
    Settings::default(),
    || Ok(GameState { gba }),
  );
  Ok(())
}
