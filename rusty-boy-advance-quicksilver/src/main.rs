use rusty_boy_advance::{GBABox, GBAButton, LogLevel};
#[cfg(target_arch = "wasm32")]
#[macro_use]
extern crate stdweb;

use quicksilver::{
  geom::Vector,
  graphics::{Background::Img, Color, Image, PixelFormat},
  input::Key,
  lifecycle::{Settings, State, Window},
  Result,
};

use std::{cell::RefCell, rc::Rc};

#[cfg(target_arch = "wasm32")]
use stdweb::{
  unstable::TryInto,
  web::{
    document,
    event::{ChangeEvent, LoadEndEvent},
    html_element::InputElement,
    IElement, IEventTarget, INode,
  },
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
  gba: Rc<RefCell<GBABox>>,
}

impl State for GameState {
  fn new() -> Result<Self> {
    unimplemented!()
  }

  fn draw(&mut self, window: &mut Window) -> Result<()> {
    let kb = window.keyboard();
    let mut gba = self.gba.borrow_mut();
    if kb[Key::Left].is_down() {
      gba.input(GBAButton::Left);
    }
    if kb[Key::Right].is_down() {
      gba.input(GBAButton::Right);
    }
    if kb[Key::Up].is_down() {
      gba.input(GBAButton::Up);
    }
    if kb[Key::Down].is_down() {
      gba.input(GBAButton::Down);
    }
    if kb[Key::Return].is_down() {
      gba.input(GBAButton::Start);
    }
    window.set_title(
      format!("Rusty Boy Advance Quicksilver ({:.2} FPS)", window.average_fps()).as_str(),
    );
    window.clear(Color::WHITE)?;
    gba
      .run_one_frame()
      .map_err(|_| quicksilver::Error::ContextError(String::from("Error running a GBA frame")))?;
    let img = Image::from_raw(gba.video_output(), 240, 160, PixelFormat::RGB)?;
    window.draw(&img.area(), Img(&img));
    Ok(())
  }
}
fn main() -> std::result::Result<(), Box<dyn std::error::Error>> {
  let gba = Rc::new(RefCell::new(GBABox::new(LogLevel::None, None, Some(polymorphic_print))));
  let rom_bytes = include_bytes!("armwrestler.gba");
  gba
    .borrow_mut()
    .load(rom_bytes)
    .map_err(|_| quicksilver::Error::ContextError(String::from("Error loading ROM")))?;
  #[cfg(target_arch = "wasm32")]
  {
    let gba = Rc::clone(&gba);
    let document = document();
    let newline = document.create_element("br").unwrap();
    let input: InputElement = document.create_element("input").unwrap().try_into().unwrap();
    input.set_attribute("type", "file").unwrap();
    let body = document.body().unwrap();
    body.append_child(&input);
    body.append_child(&newline);
    let input_clone = input.clone();
    input_clone.add_event_listener(move |_event: ChangeEvent| {
      let files: stdweb::web::FileList = js!( return @{input.as_ref()}.files; ).try_into().unwrap();
      let mut files = files.iter();
      if let Some(file) = files.next() {
        let reader = stdweb::web::FileReader::new();
        js!( return @{reader.as_ref()}.readAsArrayBuffer( @{file.as_ref()} ));
        let gba = Rc::clone(&gba);
        reader.add_event_listener(move |event: LoadEndEvent| {
          let data: stdweb::web::ArrayBuffer =
            js!(return @{event}.target.result;).try_into().unwrap();
          let data: Vec<_> = data.into();
          gba.borrow_mut().load(&data[..]).map_err(|_| js!(alert("Invalid ROM");));
        });
      }
    });
  }
  quicksilver::lifecycle::run_with(
    "Rusty Boy Advance Quicksilver",
    Vector::new(240, 160),
    Settings { ..Settings::default() },
    || Ok(GameState { gba }),
  );
  Ok(())
}
