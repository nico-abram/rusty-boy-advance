/// Fills three bytes starting at the given index in the given slice with the RGB values of the given
/// 16 bit GBA color. See http://problemkaputt.de/gbatek.htm#lcdcolorpalettes for details.
/// (the intensities 0-14 are practically all black, and only intensities 15-31 are resulting in visible medium.)
#[allow(clippy::identity_op)]
#[allow(clippy::redundant_closure_call)]
#[inline]
pub fn color_to_rgb(color: u16) -> (u8, u8, u8) {
  let r = ((color >> 0) & 0x001F) as u8;
  let g = ((color >> 5) & 0x001F) as u8;
  let b = ((color >> 10) & 0x001F) as u8;
  (r, g, b)
}
#[inline]
pub fn color_to_rgb_no_correction(color: u16) -> (u8, u8, u8) {
  let (r, g, b) = color_to_rgb(color);
  let col5_to_col8 = |x| (x << 3) | (x >> 2);
  (col5_to_col8(r), col5_to_col8(g), col5_to_col8(b))
}
#[inline]
pub fn color_to_rgb_simple(color: u16) -> (u8, u8, u8) {
  let (r, g, b) = color_to_rgb(color);
  color_correct_simple(r, g, b)
}
#[inline]
pub fn color_to_rgb_simple_fast(color: u16) -> (u8, u8, u8) {
  let (r, g, b) = color_to_rgb(color);
  color_correct_simple_fast(r, g, b)
}
#[inline]
pub fn color_to_rgb_simple_fast_funsafe(color: u16) -> (u8, u8, u8) {
  let (r, g, b) = color_to_rgb(color);
  unsafe { color_correct_simple_funsafe(r, g, b) }
}
#[inline]
pub fn color_to_rgb_correct(color: u16) -> (u8, u8, u8) {
  let (r, g, b) = color_to_rgb(color);
  color_correct(r, g, b)
}

#[inline]
fn color_correct_simple(r: u8, g: u8, b: u8) -> (u8, u8, u8) {
  let lcd_gamma = 4.0;
  let out_gamma = 2.2;
  let (r, g, b) = (r as f32 / 31.0, g as f32 / 31.0, b as f32 / 31.0);
  let (r, g, b) = (r.powf(lcd_gamma), g.powf(lcd_gamma), b.powf(lcd_gamma));
  let (r, g, b) = (
    (((r * 240.0 + 50.0 * g + b * 0.0) / 255.0).powf(1.0 / out_gamma) * (255.0 * 255.0 / 280.0))
      .clamp(0.0, 255.0),
    (((r * 10.0 + 230.0 * g + b * 10.0) / 255.0).powf(1.0 / out_gamma) * (255.0 * 255.0 / 280.0))
      .clamp(0.0, 255.0),
    (((r * 10.0 + 30.0 * g + b * 220.0) / 255.0).powf(1.0 / out_gamma) * (255.0 * 255.0 / 280.0))
      .clamp(0.0, 255.0),
  );
  (r as u8, g as u8, b as u8)
}
// source: https://github.com/nba-emu/NanoBoyAdvance/blob/master/src/platform/core/src/device/shader/color_agb.glsl.hpp
fn color_correct(r: u8, g: u8, b: u8) -> (u8, u8, u8) {
  let darken_screen = 1.0;
  let target_gamma = 2.2;
  let display_gamma = 2.2;
  let sat = 1.0;
  let lum = 0.94;
  let contrast = 1.0;
  //let blr = 0.0;
  //let blg = 0.0;
  //let blb = 0.0;
  let r_ = 0.82;
  let g_ = 0.665;
  let b_ = 0.73;
  let rg = 0.125;
  let rb = 0.195;
  let gr = 0.24;
  let gb = 0.075;
  let br = -0.06;
  let bg = 0.21;
  //let overscan_percent_x = 0.0;
  //let overscan_percent_y = 0.0;

  let (r, g, b) = (r as f64 / 31.0, g as f64 / 31.0, b as f64 / 31.0);

  let p = target_gamma + darken_screen;
  let (r, g, b) = (r.powf(p), g.powf(p), b.powf(p));

  fn lerp(a: f64, b: f64, c: f64) -> f64 {
    (1.0 - b) * a + b * c
  }
  let (r, g, b) =
    (lerp(r, 0.5, 1.0 - contrast), lerp(g, 0.5, 1.0 - contrast), lerp(b, 0.5, 1.0 - contrast));

  let (r, g, b) = ((r * lum).clamp(0.0, 1.0), (g * lum).clamp(0.0, 1.0), (b * lum).clamp(0.0, 1.0));

  let adjust_a = (1.0 - sat) * 0.3086;
  let adjust_b = (1.0 - sat) * 0.6094;
  let adjust_c = (1.0 - sat) * 0.0820;
  let (r, g, b) = (
    r * (adjust_a + sat) * r_ + r * adjust_b * rg + r * adjust_c * rb,
    g * adjust_a * gr + g * (adjust_b + sat) * g_ + g * adjust_c * gb,
    b * adjust_a * br + b * adjust_b * bg + b * (adjust_c + sat) * b_,
  );

  let (r, g, b) =
    (r.powf(1.0 / display_gamma), g.powf(1.0 / display_gamma), b.powf(1.0 / display_gamma));

  (
    (r * 255.0).clamp(0.0, 255.0) as u8,
    (g * 255.0).clamp(0.0, 255.0) as u8,
    (b * 255.0).clamp(0.0, 255.0) as u8,
  )
}

/// Less accurate but faster version of color_correct_simple using unstable intrinsics
#[inline]
unsafe fn color_correct_simple_funsafe(r: u8, g: u8, b: u8) -> (u8, u8, u8) {
  use std::intrinsics::*;
  let f1 = |x: u8| {
    //let lcd_gamma = 4.0;

    let x = fdiv_fast(x as f32, 31.0);
    fmul_fast(fmul_fast(fmul_fast(x, x), x), x)
    //x.powf(lcd_gamma)
  };

  let (r, g, b) = (f1(r), f1(g), f1(b));

  let f2 = |r: f32, g: f32, b: f32, rc: f32, gc: f32, bc: f32| {
    //let out_gamma = 2.2;
    //let out_gamma_c = 1.0 / out_gamma;
    let x =
      fdiv_fast(fadd_fast(fadd_fast(fmul_fast(r, rc), fmul_fast(g, gc)), fmul_fast(b, bc)), 255.0);
    //let x = x.powf(out_gamma_c);
    let x = x.sqrt();
    //let x_16 = rsqrt(rsqrt(rsqrt(x)));
    //let x = x * x_16 / rsqrt(rsqrt(x_16));
    let k = fdiv_fast(fmul_fast(255.0, 255.0), 280.0);
    fmul_fast(x, k) //.clamp(0.0, 255.0)
  };
  let (r, g, b) =
    (f2(r, g, b, 240.0, 50.0, 0.0), f2(r, g, b, 10.0, 230.0, 10.0), f2(r, g, b, 10.0, 30.0, 220.0));

  (r as u8, g as u8, b as u8)
}
// Less accurate but faster version of color_correct_simple
#[inline]
fn color_correct_simple_fast(r: u8, g: u8, b: u8) -> (u8, u8, u8) {
  let f1 = |x: u8| {
    //let lcd_gamma = 4.0;

    let x = x as f32 / 31.0;
    x.powi(4)
    //x.powf(lcd_gamma)
  };

  let (r, g, b) = (f1(r), f1(g), f1(b));

  let f2 = |r: f32, g: f32, b: f32, rc: f32, gc: f32, bc: f32| {
    //let out_gamma = 2.2;
    //let out_gamma_c = 1.0 / out_gamma;
    let x = r * (rc / 255.0) + g * (gc / 255.0) + b * (bc / 255.0);
    //let x = x.powf(out_gamma_c);
    let x = x.sqrt();
    let k = 255.0 * 255.0 / 280.0;
    x * k //.clamp(0.0, 255.0)
  };
  let (r, g, b) =
    (f2(r, g, b, 240.0, 50.0, 0.0), f2(r, g, b, 10.0, 230.0, 10.0), f2(r, g, b, 10.0, 30.0, 220.0));

  (r as u8, g as u8, b as u8)
}
