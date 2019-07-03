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
}
