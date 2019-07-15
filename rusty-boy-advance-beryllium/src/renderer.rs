use beryllium::*;
use rusty_boy_advance::GBABox;

#[allow(clippy::unneeded_field_pattern)]
pub fn run(mut gba: GBABox, frames_to_run: u32) -> Result<(), Box<dyn std::error::Error>> {
  let mut frames_left_to_run = frames_to_run;
  let sdl = unsafe { beryllium::init() }?;
  let mut surface = sdl.create_rgb_surface(240, 160, SurfaceFormat::DIRECT24_DEFAULT)?;
  let pitch = surface.pitch() as usize;

  let window = sdl.create_window(
    "Rusty Boy Advance Beryllium",
    WINDOW_POSITION_CENTERED,
    WINDOW_POSITION_CENTERED,
    240,
    160,
    WindowFlags::default(),
  )?;
  let renderer = unsafe {
    window
      .try_into_renderer(
        None,
        RendererFlags::default().with_accelerated(true).with_present_vsync(true),
      )
      .map_err(|(_win, msg)| msg)?
  };
  let beginning_of_time = std::time::Instant::now();
  'game_loop: loop {
    while let Some(event) = sdl.poll_event() {
      match event {
        Event::Keyboard {
          timestamp: _,
          window_id: _,
          is_key_down,
          repeat_count: _,
          key_info: KeyInfo { keycode: Some(code), .. },
        } => {
          if is_key_down {
            match code {
              //beryllium::event::KeyCode::Z => (),
              _ => (),
            }
          }
        }
        Event::Quit { timestamp: _ } => {
          break 'game_loop;
        }
        _ => (),
      }
    }
    if frames_to_run == 0 {
      gba.run_one_frame()?;
    } else {
      gba.run_one_frame()?;
      frames_left_to_run -= 1;
      if frames_left_to_run == 0 {
        println!(
          "Ran {} frames in {} milliseconds.",
          frames_to_run,
          beginning_of_time.elapsed().as_millis()
        );
        break 'game_loop;
      }
    };
    // Safety Rules: We have to lock the surface before it's safe to edit the
    // pixel data directly. We can't store this pointer past the closure's use,
    // and we also must follow standard 2D pixel buffer editing rules to not go
    // out of bounds, etc. This method doesn't know your target pixel format,
    // you just get a byte pointer and you have to cast it to the type for the
    // size of pixel data you're working with.
    let output = gba.video_output();
    unsafe {
      #[allow(clippy::cast_ptr_alignment)]
      surface.lock_edit(|ptr| {
        assert_eq!((ptr as usize) & 3, 0, "Got an unaligned pointer from the surface");
        for x in 0usize..240usize {
          for y in 0usize..160usize {
            // Note: pitch values are provided **in bytes**, so cast to the pixel
            // type after you offset to the start of the target row.
            let row_ptr = ptr.add(y * pitch) as *mut u8;
            let idx = x * 3;
            row_ptr.add(idx).write(output[y * 240 + idx + 2]);
            row_ptr.add(idx + 1).write(output[y * 240 + idx + 1]);
            #[allow(clippy::identity_op)]
            row_ptr.add(idx + 2).write(output[y * 240 + idx + 0]);
          }
        }
      })?;
    }
    renderer.clear()?;
    {
      let texture = renderer.create_texture_from_surface(&surface)?;
      renderer.copy(&texture, None, None)?;
    }
    renderer.present();
  }
  Ok(())
}
