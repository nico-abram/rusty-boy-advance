use crate::cpu::Cpu;
use beryllium::*;

pub fn run(cpu: &mut Cpu, frames_to_run: u32) -> Result<(), String> {
    let mut frames_left_to_run = frames_to_run;
    let sdl = unsafe { beryllium::init() }?;
    let mut surface = sdl.create_rgb_surface(240, 160, SurfaceFormat::DIRECT32_DEFAULT)?;
    let pitch = surface.pitch() as usize;

    let window = sdl.create_window(
        "RGBA",
        WINDOW_POSITION_CENTERED,
        WINDOW_POSITION_CENTERED,
        800,
        600,
        WindowFlags::default(),
    )?;
    let renderer = unsafe {
        window.create_renderer(
            None,
            RendererFlags::default().with_accelerated(true).with_present_vsync(true),
        )?
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
                Event::Quit { timestamp } => {
                    break 'game_loop;
                }
                _ => (),
            }
        }
        if frames_to_run == 0 {
            //cpu.run_one_frame();
        } else {
            cpu.run_one_frame();
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
        unsafe {
            #[allow(clippy::cast_ptr_alignment)]
            surface.lock_edit(|ptr| {
                assert_eq!((ptr as usize) & 3, 0, "Got an unaligned pointer from the surface");
                for x in 0usize..240usize {
                    for y in 0usize..160usize {
                        // Note: pitch values are provided **in bytes**, so cast to the pixel
                        // type after you offset to the start of the target row.
                        let row_ptr = ptr.add(y * pitch) as *mut u32;
                        row_ptr.add(x).write(cpu.output_texture[x * 160 + y]);
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
