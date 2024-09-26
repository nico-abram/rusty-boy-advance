use alloc::{string::String, vec::Vec};

pub struct Rom {
  game_pak: Vec<u8>,
  title: String,
  code: String,
}

impl Rom {
  pub fn new(contents: Vec<u8>, title: String, code: String) -> Self {
    Rom { game_pak: contents, title, code }
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
