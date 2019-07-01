#![feature(box_syntax)]
#![feature(test)]
#![allow(dead_code)]
#![allow(unused_variables)]

use clap::{App, Arg};

mod cpu;
mod renderer;

#[allow(clippy::expect_fun_call)]
fn main() -> Result<(), String> {
  let matches = App::new("RGBA")
    .version("0.0")
    .author("Nick12")
    .about("GBA emulator in rust")
    .arg(
      Arg::with_name("rom")
        .short("r")
        .long("rom")
        .value_name("ROM file")
        .help("The rom file to run")
        .required(true)
        .takes_value(true),
    )
    .arg(
      Arg::with_name("frames")
        .short("f")
        .long("frames")
        .value_name("Frames to run")
        .help("The number of frames to run before stopping (0 means none)")
        .default_value("0")
        .required(false)
        .takes_value(true),
    )
    .arg(
      Arg::with_name("headless")
        .short("h")
        .long("headless")
        .value_name("Headless")
        .help("If given, will run without a graphical window")
        .takes_value(false)
        .required(false),
    )
    .arg(
      Arg::with_name("bios")
        .short("b")
        .long("bios-file")
        .value_name("BIOS file")
        .help("If given, will use as BIOS")
        .takes_value(true)
        .required(false),
    )
    .arg(
      Arg::with_name("instructions")
        .short("i")
        .long("instructions-to-run")
        .value_name("Instructions")
        .help("Number of instructions to execute before stopping. Forces headless")
        .takes_value(true)
        .required(false),
    )
    .get_matches();

  let frames_to_run = matches.value_of("frames").unwrap_or("0").parse::<u32>().unwrap_or(0u32);
  let instructions_to_run =
    matches.value_of("instructions").unwrap_or("0").parse::<u32>().unwrap_or(0u32);
  //TODO: Actually use the given bios file
  let bios_file = matches.value_of("bios").map(|rom_filename| {
    std::fs::File::open(rom_filename)
      .expect(format!("BIOS file {} not found", rom_filename).as_str())
  });
  let rom_filename = matches.value_of("rom").unwrap();
  let headless = matches.is_present("headless");

  let mut cpu = cpu::Cpu::new();
  let file = std::fs::File::open(rom_filename)
    .expect(format!("ROM file {} not found", rom_filename).as_str());
  let reader = std::io::BufReader::new(file);
  cpu.load(reader);

  if instructions_to_run > 0 {
    for _ in 0..instructions_to_run {
      cpu.run_one_instruction();
    }
  } else if headless {
    if frames_to_run != 0 {
      for _ in 0..frames_to_run {
        cpu.run_one_frame();
      }
    } else {
      cpu.run_forever();
    }
  } else {
    renderer::run(&mut cpu, frames_to_run).unwrap();
  }
  Ok(())
}
