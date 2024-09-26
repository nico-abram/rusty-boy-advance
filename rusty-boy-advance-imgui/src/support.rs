use glium::glutin::surface::WindowSurface;
use glium::{Display, Surface};
use imgui::{ConfigFlags, Context, FontConfig, FontGlyphRanges, FontSource, Ui};
use imgui_glium_renderer::Renderer;
use imgui_winit_support::winit;
use imgui_winit_support::{HiDpiMode, WinitPlatform};
use std::time::Instant;
use winit::event::{ElementState, Event, WindowEvent};
use winit::event_loop::EventLoop;
use winit::keyboard::PhysicalKey;
use winit::window::WindowBuilder;

use copypasta::{ClipboardContext, ClipboardProvider};
use imgui::ClipboardBackend;
pub struct ClipboardSupport(pub ClipboardContext);
pub fn clipboard_init() -> Option<ClipboardSupport> {
  ClipboardContext::new().ok().map(ClipboardSupport)
}
impl ClipboardBackend for ClipboardSupport {
  fn get(&mut self) -> Option<String> {
    self.0.get_contents().ok()
  }
  fn set(&mut self, text: &str) {
    // ignore errors?
    let _ = self.0.set_contents(text.to_owned());
  }
}

pub struct System {
  pub event_loop: EventLoop<()>,
  pub display: Display<WindowSurface>,
  pub imgui: Context,
  pub platform: WinitPlatform,
  pub renderer: Renderer,
  pub window: imgui_winit_support::winit::window::Window,
}

pub fn init(title: &str) -> System {
  let event_loop = EventLoop::new().expect("Failed to create EventLoop");
  let builder: WindowBuilder =
    WindowBuilder::new().with_maximized(true).with_title(title.to_owned());
  let (window, display) = glium::backend::glutin::SimpleWindowBuilder::new()
    .set_window_builder(builder)
    .build(&event_loop);

  let mut imgui = Context::create();
  imgui.set_ini_filename(None);

  let mut platform = WinitPlatform::init(&mut imgui);
  platform.attach_window(imgui.io_mut(), &window, HiDpiMode::Default);

  let mut icon_glyph_range = vec![0u32; 257];
  for (idx, x) in icon_glyph_range.iter_mut().enumerate() {
    *x = (idx + 0xe000) as u32;
  }
  icon_glyph_range[256] = 0;

  pub const FONT_SIZE: f32 = 26.0;
  // Fixed font size. Note imgui_winit_support uses "logical
  // pixels", which are physical pixels scaled by the devices
  // scaling factor. Meaning, 13.0 pixels should look the same size
  // on two different screens, and thus we do not need to scale this
  // value (as the scaling is handled by winit)
  imgui.fonts().add_font(&[
    FontSource::TtfData {
      data: include_bytes!("../resources/Roboto-Regular.ttf"),
      size_pixels: FONT_SIZE,
      config: Some(FontConfig {
        // As imgui-glium-renderer isn't gamma-correct with
        // it's font rendering, we apply an arbitrary
        // multiplier to make the font a bit "heavier". With
        // default imgui-glow-renderer this is unnecessary.
        rasterizer_multiply: 1.5,
        // Oversampling font helps improve text rendering at
        // expense of larger font atlas texture.
        oversample_h: 4,
        oversample_v: 4,
        ..FontConfig::default()
      }),
    },
    FontSource::TtfData {
      data: include_bytes!("../resources/mplus-1p-regular.ttf"),
      size_pixels: FONT_SIZE,
      config: Some(FontConfig {
        // Oversampling font helps improve text rendering at
        // expense of larger font atlas texture.
        oversample_h: 4,
        oversample_v: 4,
        // Range of glyphs to rasterize
        glyph_ranges: FontGlyphRanges::japanese(),
        ..FontConfig::default()
      }),
    },
    FontSource::TtfData {
      data: include_bytes!("../resources/OpenFontIcons.ttf"),
      size_pixels: FONT_SIZE,
      config: Some(FontConfig {
        oversample_h: 4,
        oversample_v: 4,
        glyph_ranges: FontGlyphRanges::from_slice(icon_glyph_range.leak()),
        rasterizer_multiply: 1.75,
        ..FontConfig::default()
      }),
    },
  ]);

  let renderer = Renderer::init(&mut imgui, &display).expect("Failed to initialize renderer");

  if let Some(backend) = clipboard_init() {
    imgui.set_clipboard_backend(backend);
  }

  System { event_loop, display, imgui, platform, renderer, window }
}

impl System {
  pub fn main_loop<
    F: FnMut(
      &mut bool,
      &mut Ui,
      &mut Renderer,
      &Display<WindowSurface>,
      f32,
      &Vec<(PhysicalKey, ElementState)>,
    ),
  >(
    self,
    mut run_ui: F,
  ) {
    let System { event_loop, display, mut imgui, mut platform, mut renderer, window, .. } = self;

    let mut last_frame = Instant::now();
    let mut key_events = Vec::with_capacity(64);
    let mut fps = 0.0;
    event_loop
      .run(move |event, window_target| match event {
        Event::NewEvents(_) => {
          let now = Instant::now();
          let io = imgui.io_mut();
          io.config_flags.set(ConfigFlags::DOCKING_ENABLE, true);
          fps = io.framerate;
          io.update_delta_time(now - last_frame);
          last_frame = now;
        }
        Event::AboutToWait => {
          platform.prepare_frame(imgui.io_mut(), &window).expect("Failed to prepare frame");
          window.request_redraw();
        }
        Event::DeviceEvent {
          event:
            imgui_winit_support::winit::event::DeviceEvent::Key(
              imgui_winit_support::winit::event::RawKeyEvent { physical_key, state },
            ),
          ..
        } => {
          key_events.push((physical_key, state));
        }
        Event::WindowEvent { event: WindowEvent::RedrawRequested, .. } => {
          let mut ui = imgui.frame();
          ui.dockspace_over_main_viewport();

          let mut run = true;
          run_ui(&mut run, &mut ui, &mut renderer, &display, fps, &key_events);
          key_events.clear();
          if !run {
            window_target.exit();
          }

          let mut target = display.draw();
          //target.clear_color_srgb(1.0, 1.0, 1.0, 1.0);
          target.clear_color_srgb(0.1, 0.1, 0.1, 1.0);
          platform.prepare_render(ui, &window);
          let draw_data = imgui.render();
          renderer.render(&mut target, draw_data).expect("Rendering failed");
          target.finish().expect("Failed to swap buffers");
        }
        Event::WindowEvent { event: WindowEvent::Resized(new_size), .. } => {
          if new_size.width > 0 && new_size.height > 0 {
            display.resize((new_size.width, new_size.height));
          }
          platform.handle_event(imgui.io_mut(), &window, &event);
        }
        Event::WindowEvent { event: WindowEvent::CloseRequested, .. } => window_target.exit(),
        event => {
          platform.handle_event(imgui.io_mut(), &window, &event);
        }
      })
      .expect("EventLoop error");
  }
}
