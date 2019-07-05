use clap::{App, Arg};
use rusty_boy_advance::{LogLevel, GBA};
use std::io::Read;

mod renderer;

#[allow(clippy::expect_fun_call)]
fn main() -> Result<(), Box<dyn std::error::Error>> {
  let matches = App::new("RGBA")
    .version("0.0")
    .author("Nick12")
    .about("GBA emulator in rust")
    .arg(
      Arg::with_name("rom")
        .short('r')
        .long("rom")
        .value_name("ROM file")
        .help("The rom file to run")
        .required(true)
        .takes_value(true),
    )
    .arg(
      Arg::with_name("bios")
        .short('b')
        .long("bios-file")
        .value_name("BIOS file")
        .help("If given, will use as BIOS")
        .takes_value(true)
        .required(false),
    )
    .arg(
      Arg::with_name("log-level")
        .short('l')
        .default_value("0")
        .long("log-level")
        .value_name("Log Level")
        .possible_values(&["0", "1", "2"])
        .help("Log Level. 0, 1 or 2")
        .takes_value(true)
        .required(false),
    )
    .arg(
      Arg::with_name("headless")
        .short('h')
        .long("headless")
        .value_name("Headless")
        .help("If given, will run without a graphical window")
        .takes_value(false)
        .required(false),
    )
    .arg(
      Arg::with_name("frames")
        .short('f')
        .long("frames")
        .value_name("Frames to run")
        .help("The number of frames to run before stopping (0 means none)")
        .default_value("0")
        .required(false)
        .takes_value(true),
    )
    .arg(
      Arg::with_name("instructions")
        .short('i')
        .default_value("0")
        .long("instructions-to-run")
        .value_name("Instructions")
        .help("Number of instructions to execute before stopping. Forces headless")
        .takes_value(true)
        .required(false),
    )
    .get_matches();

  let log_level = matches.value_of("log-level").unwrap().parse::<u32>().unwrap();
  let frames_to_run = matches.value_of("frames").unwrap().parse::<u32>().unwrap();
  let instructions_to_run = matches.value_of("instructions").unwrap().parse::<u32>().unwrap();
  let bios_file_name_option = matches.value_of("bios");
  let rom_filename = matches.value_of("rom").unwrap();
  let headless = matches.is_present("headless");

  let mut gba = {
    let log_level = match log_level {
      0 => LogLevel::None,
      1 => LogLevel::NormalizedEveryInstruction,
      2 => LogLevel::Debug,
      _ => unimplemented!(), // Clap ensures this doesnt happen
    };
    let bios_file = bios_file_name_option.map(|rom_filename| {
      std::fs::read(rom_filename).expect(format!("BIOS file {} not found", rom_filename).as_str())
    });
    let mut gba = GBA::new(log_level, bios_file.as_ref().map(|v| &v[..]), Some(|x| print!("{}", x)));
    let mut rom_file = std::fs::File::open(rom_filename)
      .expect(format!("ROM file {} not found", rom_filename).as_str());
     let mut rom_contents = Vec::with_capacity(32 * 1024 * 1024);
    rom_file.read_to_end(&mut rom_contents)?;
    gba.load(&rom_contents[..])?;
    gba 
  };

  if instructions_to_run > 0 {
    for _ in 0..instructions_to_run {
      gba.run_one_instruction()?;
    }
  } else if headless {
    if frames_to_run != 0 {
      for _ in 0..frames_to_run {
        gba.run_one_frame()?;
      }
    } else {
      gba.run_forever()?;
    }
  } else {
    renderer::run(gba, frames_to_run)?;
  }
  Ok(())
}
