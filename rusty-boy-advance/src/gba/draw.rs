use core::any;

use super::gba::{GBA, VIDEO_HEIGHT, VIDEO_WIDTH};

const TILE_WIDTH: u32 = 8;
const TILE_HEIGHT: u32 = 8;

const SCREEN_Y_TILE_COUNT: u32 = (VIDEO_HEIGHT as u32) / TILE_HEIGHT;
const SCREEN_X_TILE_COUNT: u32 = (VIDEO_WIDTH as u32) / TILE_WIDTH;

const REG_BG0HOFS: u32 = 0x0400_0010;
const REG_BG0VOFS: u32 = 0x0400_0012;
#[inline]
fn bgscroll(gba: &mut GBA, bg_num: u32) -> (u32, u32) {
  let (bg_hofs, bg_vofs) = (REG_BG0HOFS + bg_num * 4, REG_BG0VOFS + bg_num * 4);

  ((gba.fetch_u16(bg_hofs) & 0x1FF) as u32, (gba.fetch_u16(bg_vofs) & 0x1FF) as u32)
}

const BG0CNT: u32 = 0x0400_0008;
#[inline]
fn bgcnt(gba: &mut GBA, bg_num: u32) -> u32 {
  let bgcnt = BG0CNT + bg_num * 2;
  gba.fetch_u16(bgcnt) as u32
}

const TILE_MAP_PAGE_SIZE: u32 = 2 * 1024;
const TILE_DATA_PAGE_SIZE: u32 = 16 * 1024;
pub const TILE_DATA_PAGE_TILE_COUNT_BPP8: u32 = TILE_DATA_PAGE_SIZE / (64 * 64);
pub const TILE_DATA_PAGE_TILE_COUNT_BPP4: u32 = TILE_DATA_PAGE_TILE_COUNT_BPP8 / 2;
#[inline]
fn parse_bgcnt(bg_control_flags: u32) -> (u32, bool, u32, u32, bool, (u32, u32)) {
  let priority = bg_control_flags & 0x3;
  let mosaic = bg_control_flags & 0x40 != 0;
  // The tile data contains colours, format depends on full palette
  // (1 byte per colour) or not (4 bits a colour)
  let tile_data_page = u32::from((bg_control_flags >> 2) & 0x3);
  let tile_data_base_addr = tile_data_page * TILE_DATA_PAGE_SIZE;

  // The tilemap contains the attributes for the tile(Like being flipped)
  // and the tile index into the tile data
  let tile_map_page = u32::from((bg_control_flags >> 8) & 0x1F);
  let tile_map_base_addr = tile_map_page * TILE_MAP_PAGE_SIZE;

  let full_palette = (bg_control_flags & 0x0000_0080) != 0;
  let screen_size_flag = ((bg_control_flags >> 14) & 0x3) as usize;
  let (bg_x_tile_count, bg_y_tile_count) = match screen_size_flag {
    0 => (32, 32),
    1 => (64, 32),
    2 => (32, 64),
    3 => (64, 64),
    _ => std::unreachable!(),
  };

  (
    priority,
    mosaic,
    tile_data_base_addr,
    tile_map_base_addr,
    full_palette,
    (bg_x_tile_count, bg_y_tile_count),
  )
}

#[inline]
fn bytes_per_tile(full_palette: bool) -> u32 {
  if full_palette { TILE_WIDTH * TILE_HEIGHT } else { TILE_WIDTH * (TILE_HEIGHT / 2) }
}

/// Fills three bytes starting at the given index in the given slice with the RGB values of the given
/// 16 bit GBA color. See http://problemkaputt.de/gbatek.htm#lcdcolorpalettes for details.
/// (the intensities 0-14 are practically all black, and only intensities 15-31 are resulting in visible medium.)
#[allow(clippy::identity_op)]
#[allow(clippy::redundant_closure_call)]
#[inline]
fn color_to_rgb(color: u16) -> (u8, u8, u8) {
  let r = ((color >> 0) & 0x001F) as u8;
  let g = ((color >> 5) & 0x001F) as u8;
  let b = ((color >> 10) & 0x001F) as u8;

  let col5_to_col8 = |x| (x << 3) | (x >> 2);
  (col5_to_col8(r), col5_to_col8(g), col5_to_col8(b))
}

fn write_color(output: &mut [u8], color: u16) {
  let color = color_to_rgb(color);
  output[0] = color.0;
  output[1] = color.1;
  output[2] = color.2;
}

fn fetch_u16(mem: &[u8], addr: usize) -> u16 {
  (mem[addr] as u16) + ((mem[addr + 1] as u16) << 8)
}

fn fetch_u16_vram(gba: &GBA, addr: u32) -> u16 {
  let addr = addr as usize;
  fetch_u16(&gba.vram[..], addr)
}

fn fetch_u16_palette_ram(gba: &GBA, addr: usize) -> u16 {
  fetch_u16(&gba.palette_ram[..], addr)
}

pub fn get_tile(
  gba: &mut GBA,
  tile_page: u32,
  tile_idx: u32,
  bpp8: bool,
  palette_number: u8,
) -> alloc::vec::Vec<u8> {
  let bytes_per_tile = bytes_per_tile(bpp8);

  let tile_data_base_addr = tile_page * TILE_DATA_PAGE_SIZE;
  let tile_addr = tile_data_base_addr + tile_idx * bytes_per_tile;

  let mut output = alloc::vec![0u8; 8 * 8*3];

  let mut out_transparency = [false; 8];
  for strip_idx in 0..8 {
    draw_mode0_bg_tile_hstrip::<false>(
      gba,
      &mut output[strip_idx * 8 * 3..],
      &mut out_transparency,
      (palette_number as u32) << 12,
      tile_addr,
      bpp8,
      strip_idx,
      // Unused with ::<false>
      0,
      0,
    );
  }

  output
}

/// Returns (rgb_bytes_vec_u8u8u8, tile_map_addr, tile_data_addr, tile_map_value, tile_data_index)
pub fn get_mode0_bg_tile(
  gba: &mut GBA,
  bg_num: u32,
  tile_x: u32,
  tile_y: u32,
) -> (alloc::vec::Vec<u8>, u32, u32, u32, u32) {
  let (scroll_x, scroll_y) = bgscroll(gba, bg_num);
  let (bg_first_tile_x, bg_first_tile_y) = (scroll_x / 8, scroll_y / 8);
  let (leftover_x_pxs_from_scroll, leftover_y_pxs_from_scroll) = (scroll_x % 8, (scroll_y % 8));

  let background_control_flags = bgcnt(gba, bg_num);
  let (
    priority,
    mosaic,
    tile_data_base_addr,
    tile_map_base_addr,
    full_palette,
    (bg_tile_count_x, bg_tile_count_y),
  ) = parse_bgcnt(background_control_flags);

  let bytes_per_tile = bytes_per_tile(full_palette);

  assert!(tile_x < bg_tile_count_x);
  assert!(tile_x < bg_tile_count_y);

  // The math here is pretty weird. The way the tiles are laid out in memory seems to be
  // the first (top-left) 32x32 tiles, then the bottom-left 32x32, then top-right
  // then bottom-right
  let tile_map_element_addr = tile_map_base_addr
    + (((tile_x & 0x1F) + tile_y * bg_tile_count_y) * 2)
    + if tile_x >= 32 { 32 * bg_tile_count_y * 2 } else { 0 };

  // See http://problemkaputt.de/gbatek.htm#lcdvrambgscreendataformatbgmap
  let tile_map_element = u32::from(fetch_u16_vram(gba, tile_map_element_addr));

  let tile_number = tile_map_element & (if full_palette { 0x3FF } else { 0x3FF });
  let tile_addr = tile_data_base_addr + tile_number * bytes_per_tile;

  let mut output = alloc::vec![0u8; 8 * 8*3];
  let mut out_transparency = [false; 8];
  for strip_idx in 0..8 {
    draw_mode0_bg_tile_hstrip::<false>(
      gba,
      &mut output[strip_idx * 8 * 3..],
      &mut out_transparency,
      tile_map_element,
      tile_addr,
      full_palette,
      strip_idx,
      // Unused with ::<false>
      0,
      0,
    );
  }

  (output, tile_map_element_addr, tile_addr, tile_map_element, tile_number)
}

/// Draws an 8 pixel horizontal strip into the output slice
/// Returns true if any pixels are transparent
pub fn draw_mode0_bg_tile_hstrip<const USE_GBA_TEXTURE: bool>(
  gba: &mut GBA,
  output: &mut [u8],
  transparency_output: &mut [bool; 8],
  tile_map_element: u32,
  tile_addr: u32,
  full_palette: bool,
  strip_idx_in_tile: usize,
  strip_idx_in_scanline: usize,
  scanline_first_px_idx: usize,
) -> bool {
  let output = if USE_GBA_TEXTURE {
    &mut gba.output_texture[scanline_first_px_idx + strip_idx_in_scanline * 8 * 3..]
  } else {
    output
  };
  let mut any_transparent_px = false;

  let h_flip = (tile_map_element & 0x0000_0400) != 0;
  let v_flip: bool = (tile_map_element & 0x0000_0800) != 0;

  if full_palette {
    // 1 byte per pixel
    let start_idx = strip_idx_in_tile * 8;
    let end_idx = (strip_idx_in_tile + 1) * 8;

    for i in start_idx..end_idx {
      let palette_idx = gba.vram[(tile_addr + i as u32) as usize] as usize;
      let color = fetch_u16(&gba.palette_ram[..], palette_idx);
      transparency_output[i] = palette_idx == 0;
      any_transparent_px |= palette_idx == 0;
      write_color(&mut output[((i & 0x7) * 3) as usize..], color);
    }
  } else {
    // 4 bits per pixel
    let palette_base = {
      let palette_number = (tile_map_element >> 12) & 0xF;
      // Each sub palette has 16 colors of 2 bytes each
      (palette_number * 16 * 2) as usize
    };
    let start_idx = strip_idx_in_tile * 4;
    let end_idx = start_idx + 4;

    let mut transparency_out_it = transparency_output.iter_mut();
    for i in start_idx..end_idx {
      let (mut px_x_within_tile, mut px_y_within_tile) = ((i & 0x3) * 2, (i >> 2));

      // TODO: Things seem to be wrong when moving along both ways mid-tile when flipping?
      if h_flip {
        px_x_within_tile = 7 - px_x_within_tile;
      };
      if v_flip {
        px_y_within_tile = 7 - px_y_within_tile;
      };

      // First 4 bits are the first px's palette idx, next 4 are the next color's
      let (palette_idx1, palette_idx2) = {
        let palette_idxs = gba.vram[(tile_addr + i as u32) as usize] as usize;
        #[allow(clippy::identity_op)]
        ((palette_idxs >> 0) & 0xF, (palette_idxs >> 4) & 0xF)
      };

      let transparent_px1 = palette_idx1 == 0;
      *transparency_out_it.next().unwrap() = transparent_px1;
      //transparency_output[px_x_within_tile] = transparent_px1;
      any_transparent_px |= transparent_px1;
      let palette_idx1 = if !transparent_px1 { palette_base + (palette_idx1 * 2) } else { 0 };
      let color = fetch_u16(&gba.palette_ram[..], palette_idx1);
      write_color(&mut output[px_x_within_tile * 3 as usize..], color);

      let px2_x_within_tile =
        if h_flip { px_x_within_tile - 1 } else { px_x_within_tile.saturating_add(1) };

      let transparent_px2 = palette_idx2 == 0;
      *transparency_out_it.next().unwrap() = transparent_px2;
      //transparency_output[px2_x_within_tile] = transparent_px2;
      any_transparent_px |= transparent_px2;
      let palette_idx2 = if !transparent_px2 { palette_base + (palette_idx2 * 2) } else { 0 };
      let color = fetch_u16(&gba.palette_ram[..], palette_idx2);
      write_color(&mut output[px2_x_within_tile * 3 as usize..], color);
    }
  }
  any_transparent_px
}

fn draw_scanline_bgmode0(gba: &mut GBA, scanline: u32, display_control: u16) {
  let bg_num = 0;

  let bg_hofs = REG_BG0HOFS + bg_num * 4;
  let bg_vofs = REG_BG0VOFS + bg_num * 4;
  let (scroll_x, scroll_y) =
    ((gba.fetch_u16(bg_hofs) & 0x1FF) as u32, (gba.fetch_u16(bg_vofs) & 0x1FF) as u32);
  let (bg_first_tile_x, bg_first_tile_y) = (scroll_x / 8, scroll_y / 8);
  let (leftover_x_pxs_from_scroll, leftover_y_pxs_from_scroll) = (scroll_x % 8, (scroll_y % 8));

  const BG0CNT: u32 = 0x0400_0008;
  const BG1CNT: u32 = 0x0400_000A;
  const BG2CNT: u32 = 0x0400_000C;
  const BG3CNT: u32 = 0x0400_000E;
  //let bgcnt = BG0CNT + bg_num * 2;
  //let bg_control_flags = gba.fetch_u16(bgcnt);

  let bg_control_flags =
    [gba.fetch_u16(BG0CNT), gba.fetch_u16(BG1CNT), gba.fetch_u16(BG2CNT), gba.fetch_u16(BG3CNT)];

  // The tile data contains colours, format depends on full palette
  // (1 byte per colour) or not (4 bits a colour)
  //let tile_data_page = u32::from((bg_control_flags >> 2) & 0x3);
  //let tile_data_base_addr = tile_data_page * TILE_DATA_PAGE_SIZE;
  let tile_data_base_addrs = bg_control_flags.map(|bgcnt| {
    let tile_data_page = u32::from((bgcnt >> 2) & 0x3);
    tile_data_page * TILE_DATA_PAGE_SIZE
  });

  // 4 is a sentinel value indicating no bg
  let mut bgs_to_draw = [4, 4, 4, 4, 4, 4];
  for (idx, bgcnt) in bg_control_flags.iter().enumerate() {
    if display_control & (1 << (8 + idx)) != 0 {
      let priority = *bgcnt & 0x3;
      if bgs_to_draw[priority as usize] == 4 {
        bgs_to_draw[priority as usize] = idx;
      } else if bgs_to_draw[priority as usize + 1] == 4 {
        bgs_to_draw[priority as usize + 1] = idx;
      } else if bgs_to_draw[priority as usize + 2] == 4 {
        bgs_to_draw[priority as usize + 2] = idx;
      } else {
        unimplemented!("matching BG priority is unimplemented")
      }
    }
  }

  // The tilemap contains the attributes for the tile(Like being flipped)
  // and the tile index into the tile data
  //let tile_map_page = u32::from((bg_control_flags >> 8) & 0x1F);
  //let tile_map_base_addr = tile_map_page * TILE_MAP_PAGE_SIZE;

  //let full_palette = (bg_control_flags & 0x0000_0080) != 0;
  let full_palette_bools = bg_control_flags.map(|bgcnt| (bgcnt & 0x0000_0080) != 0);
  /*
  let screen_size_flag = ((bg_control_flags >> 14) & 0x3) as usize;
  let (bg_x_tile_count, bg_y_tile_count) = match screen_size_flag {
    0 => (32, 32),
    1 => (64, 32),
    2 => (32, 64),
    3 => (64, 64),
    _ => std::unreachable!(),
  };

  let bytes_per_tile =
    if full_palette { TILE_WIDTH * TILE_HEIGHT } else { TILE_WIDTH * (TILE_HEIGHT / 2) };
  */
  let screen_sizes = bg_control_flags.map(|bgcnt| {
    let screen_size_flag = ((bgcnt >> 14) & 0x3) as usize;
    match screen_size_flag {
      0 => (32, 32),
      1 => (64, 32),
      2 => (32, 64),
      3 => (64, 64),
      _ => std::unreachable!(),
    }
  });

  let stripe_in_tile = ((scanline + scroll_y) % 8) as usize;
  let scanline_first_px_idx = (scanline * (VIDEO_WIDTH as u32) * 3) as usize;

  let bg_tile_y: u32 = ((scanline + scroll_y) / 8);
  //let tile_map_scanline_addr = tile_map_base_addr + (bg_tile_y * bg_y_tile_count) * 2;
  let tile_map_scanline_addrs: [_; 4] = std::array::from_fn(|idx| {
    let bgcnt = bg_control_flags[idx];
    let tile_map_page = u32::from((bgcnt >> 8) & 0x1F);
    let tile_map_base_addr = tile_map_page * TILE_MAP_PAGE_SIZE;
    let bg_y_tile_count = screen_sizes[idx].1;
    tile_map_base_addr + (bg_tile_y * bg_y_tile_count) * 2
  });

  const TILE_HSTRIP_COUNT_IN_SCANLINE: usize = VIDEO_WIDTH / 8;
  let mut output_transparency = [false; 8];
  for strip_idx in 0..TILE_HSTRIP_COUNT_IN_SCANLINE {
    /*
    let bg_tile_x = (bg_first_tile_x + strip_idx as u32) % bg_x_tile_count;
    let tile_map_element_addr = tile_map_scanline_addr
      + (bg_tile_x & 0x1F) * 2
      + if bg_tile_x >= 32 { 32 * bg_y_tile_count * 2 } else { 0 };

    // See http://problemkaputt.de/gbatek.htm#lcdvrambgscreendataformatbgmap
    let tile_map_element = fetch_u16_vram(gba, tile_map_element_addr) as u32;

    let tile_number = tile_map_element & 0x3FF;
    let tile_addr = tile_data_base_addr + tile_number * bytes_per_tile;
    */

    let mut first_iter = true;
    for bg_num in bgs_to_draw.iter().filter(|bg| **bg != 4) {
      let bg_tile_x = (bg_first_tile_x + strip_idx as u32) % screen_sizes[*bg_num].0;

      let tile_map_element_addr = tile_map_scanline_addrs[*bg_num]
        + (bg_tile_x & 0x1F) * 2
        + if bg_tile_x >= 32 { 32 * screen_sizes[*bg_num].1 * 2 } else { 0 };
      let tile_map_element = fetch_u16_vram(gba, tile_map_element_addr) as u32;

      let full_palette = full_palette_bools[*bg_num];
      let bytes_per_tile =
        if full_palette { TILE_WIDTH * TILE_HEIGHT } else { TILE_WIDTH * (TILE_HEIGHT / 2) };
      let tile_number = tile_map_element & 0x3FF;
      let tile_addr = tile_data_base_addrs[*bg_num] + tile_number * bytes_per_tile;

      //TODO: scroll_x
      let any_transparent = if first_iter {
        draw_mode0_bg_tile_hstrip::<true>(
          gba,
          &mut [],
          &mut output_transparency,
          tile_map_element,
          tile_addr,
          full_palette,
          stripe_in_tile,
          strip_idx,
          scanline_first_px_idx,
        )
      } else {
        let mut out = [0u8; 8 * 3];
        let output_transparency2 = output_transparency;
        let any_transparent = draw_mode0_bg_tile_hstrip::<false>(
          gba,
          &mut out[..],
          &mut output_transparency,
          tile_map_element,
          tile_addr,
          full_palette,
          stripe_in_tile,
          strip_idx,
          scanline_first_px_idx,
        );
        for (idx, _) in output_transparency2.iter().enumerate().filter(|(_, b)| **b) {
          gba.output_texture[scanline_first_px_idx + strip_idx * 8 * 3 + idx * 3 + 0] =
            out[idx * 3 + 0];
          gba.output_texture[scanline_first_px_idx + strip_idx * 8 * 3 + idx * 3 + 1] =
            out[idx * 3 + 1];
          gba.output_texture[scanline_first_px_idx + strip_idx * 8 * 3 + idx * 3 + 2] =
            out[idx * 3 + 2];
        }
        for i in 0..8 {
          output_transparency[i] = output_transparency2[i] && output_transparency[i];
        }
        any_transparent
      };
      if !any_transparent {
        break;
      }
      first_iter = false;
    }
  }
}

pub fn draw_scanline(gba: &mut GBA, scanline: u32) {
  let display_control = gba.fetch_u16(super::gba::DISPCNT_ADDR);
  let bg_mode = display_control & 0x0000_0007;
  let start_px = scanline * VIDEO_WIDTH as u32;

  match bg_mode {
    0 => draw_scanline_bgmode0(gba, scanline, display_control),
    1 => {
      gba.print_fn.map(|f| f("Unimplemented: bg mode 1"));
    }
    2 => {
      gba.print_fn.map(|f| f("Unimplemented: bg mode 2"));
    }
    3 => {
      // 16 bit color bitmap. One frame buffer (240x160 pixels, 32768 colors)
      let start_idx = (start_px * 2) as usize;
      let start_idx_out = (start_px * 3) as usize;
      #[allow(clippy::match_ref_pats)]
      for (idx, slice) in gba.vram[start_idx..].chunks_exact(2).take(VIDEO_WIDTH).enumerate() {
        if let &[high_byte, low_byte] = slice {
          let color = u16::from(low_byte) + (u16::from(high_byte) << 8);
          write_color(&mut gba.output_texture[(start_idx_out + idx * 3)..], color);
        }
      }
    }
    4 => {
      // 8 bit palette indexed bitmap. Two frame buffers (240x160 pixels, 256 colors)
      let second_frame = (gba.io_mem[0] & 0x0000_0008) != 0;
      let start_idx =
        (start_px * 1) as usize + if second_frame { VIDEO_WIDTH * VIDEO_HEIGHT } else { 0 };
      let start_idx_out = (start_px * 3) as usize;

      for (idx, palette_idx) in gba.vram[start_idx..].iter().take(VIDEO_WIDTH).enumerate() {
        let palette_idx = (*palette_idx as usize) * 2;
        let color = fetch_u16_palette_ram(gba, palette_idx);
        write_color(&mut gba.output_texture[(start_idx_out + idx * 3)..], color);
      }
    }
    5 => {
      // 16 bit color bitmap. Two frame buffers (160x128 pixels, 32768 colors)
      const BGMODE5_WIDTH: usize = 160;
      const BGMODE5_HEIGHT: usize = 128;
      const BGMODE5_FRAMEBUFFER_PX_COUNT: usize = BGMODE5_WIDTH * BGMODE5_HEIGHT;
      const BGMODE5_FIRST_X_PX: usize = (VIDEO_WIDTH - BGMODE5_WIDTH) / 2;
      const BGMODE5_FIRST_Y_PX: usize = (VIDEO_HEIGHT - BGMODE5_HEIGHT) / 2;

      let second_frame = (gba.io_mem[0] & 0x0000_0008) != 0; // TODO: Is this right?
      let start_idx =
        (start_px * 2) as usize + if second_frame { BGMODE5_FRAMEBUFFER_PX_COUNT * 2 } else { 0 };
      let mut start_idx_out = (start_px * 3) as usize;

      if scanline < (BGMODE5_FIRST_Y_PX as u32)
        || scanline < ((BGMODE5_FIRST_Y_PX + BGMODE5_HEIGHT) as u32)
      {
        for x in 0..VIDEO_WIDTH {
          // TODO: Is this right?
          write_color(&mut gba.output_texture[(x * 3)..], u16::max_value());
        }
      } else {
        for _x in 0..BGMODE5_FIRST_Y_PX {
          // TODO: Is this right?
          write_color(&mut gba.output_texture[start_idx_out..], u16::max_value());
          start_idx_out += 3;
        }
        let iter = gba.vram[start_idx..].chunks_exact(2).take(BGMODE5_WIDTH);
        for slice in iter {
          if let &[high_byte, low_byte] = slice {
            // TODO: Is this right?
            let color = u16::from(low_byte) + (u16::from(high_byte) << 8);
            write_color(&mut gba.output_texture[start_idx_out..], color);
            start_idx_out += 3;
          }
        }
        for _x in BGMODE5_WIDTH..(BGMODE5_WIDTH + BGMODE5_FIRST_Y_PX) {
          // TODO: Is this right?
          write_color(&mut gba.output_texture[start_idx_out..], u16::max_value());
          start_idx_out += 3;
        }
      }
    }
    _ => {
      gba.print_fn.map(|f| f(&alloc::format!("Unrecognized bg mode: {bg_mode}")));
    }
  }
}
