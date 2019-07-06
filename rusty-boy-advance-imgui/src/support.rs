use glium::{
  glutin::{self, Event, WindowEvent},
  Display, Surface,
};
use imgui::{Context, FontConfig, FontGlyphRanges, FontSource, Ui};
use imgui_glium_renderer::GliumRenderer;
use imgui_winit_support::{HiDpiMode, WinitPlatform};
use std::time::Instant;

pub struct System {
  pub events_loop: glutin::EventsLoop,
  pub display: glium::Display,
  pub imgui: Context,
  pub platform: WinitPlatform,
  pub renderer: GliumRenderer,
  pub font_size: f32,
}

// We can't make a const range array atm. Maybe make this less hideous
// With something like a lazy static, but it's probably not worth it
// as this works and is just a temporary hack until we can define it as const
static mut ICON_GLYPH_RANGE: [u16; 257] = [0u16; 257];

pub fn init(title: &str) -> System {
  unsafe {
    for (idx, x) in ICON_GLYPH_RANGE.iter_mut().enumerate() {
      *x = (idx + 0xe000) as u16;
    }
    ICON_GLYPH_RANGE[256] = 0;
  }
  let title = match title.rfind('/') {
    Some(idx) => title.split_at(idx + 1).1,
    None => title,
  };
  let events_loop = glutin::EventsLoop::new();
  let context = glutin::ContextBuilder::new().with_vsync(true);
  let builder = glutin::WindowBuilder::new()
    .with_title(title.to_owned())
    .with_dimensions(glutin::dpi::LogicalSize::new(1024f64, 768f64));
  let display = Display::new(builder, context, &events_loop).expect("Failed to initialize display");

  let mut imgui = Context::create();
  imgui.set_ini_filename(None);

  let mut platform = WinitPlatform::init(&mut imgui);
  {
    let gl_window = display.gl_window();
    let window = gl_window.window();
    platform.attach_window(imgui.io_mut(), &window, HiDpiMode::Rounded);
  }

  let hidpi_factor = platform.hidpi_factor();
  let font_size = (13.0 * hidpi_factor) as f32;
  imgui.fonts().add_font(&[
    FontSource::DefaultFontData {
      config: Some(FontConfig { size_pixels: font_size, ..FontConfig::default() }),
    },
    FontSource::TtfData {
      data: include_bytes!("../resources/OpenFontIcons.ttf"),
      size_pixels: font_size,
      config: Some(FontConfig {
        rasterizer_multiply: 1.75,
        glyph_ranges: FontGlyphRanges::from_slice(unsafe { &ICON_GLYPH_RANGE }),
        ..FontConfig::default()
      }),
    },
  ]);

  imgui.io_mut().font_global_scale = (1.0 / hidpi_factor) as f32;

  let renderer = GliumRenderer::init(&mut imgui, &display).expect("Failed to initialize renderer");

  System { events_loop, display, imgui, platform, renderer, font_size }
}

impl System {
  pub fn main_loop<F: FnMut(&mut bool, &mut Ui, &mut GliumRenderer, &glium::Display)>(
    self,
    mut run_ui: F,
  ) {
    let System { mut events_loop, display, mut imgui, mut platform, mut renderer, .. } = self;
    let gl_window = display.gl_window();
    let window = gl_window.window();
    let mut last_frame = Instant::now();
    let mut run = true;

    while run {
      events_loop.poll_events(|event| {
        platform.handle_event(imgui.io_mut(), &window, &event);

        if let Event::WindowEvent { event, .. } = event {
          if let WindowEvent::CloseRequested = event {
            run = false;
          }
        }
      });

      let io = imgui.io_mut();
      platform.prepare_frame(io, &window).expect("Failed to start frame");
      last_frame = io.update_delta_time(last_frame);
      let mut ui = imgui.frame();
      run_ui(&mut run, &mut ui, &mut renderer, &display);

      let mut target = display.draw();
      target.clear_color_srgb(1.0, 1.0, 1.0, 1.0);
      platform.prepare_render(&ui, &window);
      let draw_data = ui.render();
      renderer.render(&mut target, draw_data).expect("Rendering failed");
      target.finish().expect("Failed to swap buffers");
    }
  }
}
