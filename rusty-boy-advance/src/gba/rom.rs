use alloc::{string::String, vec::Vec};

pub struct Rom {
  game_pak: [u8; 64 * 1024],
  title: String,
  code: String,
}
impl Rom {
  pub fn new(contents: Vec<u8>, title: String, code: String) -> Self {
    let mut rom = Rom { game_pak: [0u8; 64 * 1024], title, code };
    for (input, out) in contents.iter().zip(rom.game_pak.iter_mut()) {
      *out = *input;
    }
    rom
  }
  pub fn game_pak(&self) -> &[u8] {
    &self.game_pak[..]
  }
  pub fn title(&self) -> &str {
    self.title.as_str()
  }
  pub fn code(&self) -> &str {
    self.code.as_str()
  }
}
