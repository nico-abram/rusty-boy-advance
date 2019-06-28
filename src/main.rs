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
        .author("Niko12")
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
        .get_matches();

    let frames_to_run = matches.value_of("frames").unwrap().parse::<u32>().unwrap_or(0u32);
    let rom_filename = matches.value_of("rom").unwrap();
    let headless = matches.is_present("headless");

    let mut cpu = cpu::Cpu::new();
    let file = std::fs::File::open(rom_filename)
        .expect(format!("File {} not found", rom_filename).as_str());
    let reader = std::io::BufReader::new(file);
    cpu.load(reader);

    if headless {
        if frames_to_run != 0 {
            for _ in 0..frames_to_run {
                cpu.run_once();
            }
        } else {
            cpu.run_forever();
        }
    } else {
        renderer::run(&mut cpu, frames_to_run).unwrap();
    }
    Ok(())
}
