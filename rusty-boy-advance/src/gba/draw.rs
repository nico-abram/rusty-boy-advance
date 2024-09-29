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

const TILE_DATA_PAGE_SIZE: u32 = 16 * 1024;
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
  const TILE_MAP_PAGE_SIZE: u32 = 2 * 1024;
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

fn fetch_u16_vram(gba: &GBA, addr: u32) -> u16 {
  let addr = addr as usize;
  (gba.vram[addr] as u16) + ((gba.vram[addr + 1] as u16) << 8)
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
  let tile_count = TILE_DATA_PAGE_SIZE / bytes_per_tile;

  let tile_addr = tile_data_base_addr + tile_idx * bytes_per_tile;

  let mut output = alloc::vec![0u8; 8 * 8*3];

  let mut write_color = |color: u16, px_x: u32, px_y: u32| {
    let color = color_to_rgb(color);
    output[((px_x + px_y * 8) * 3 + 0) as usize] = color.0;
    output[((px_x + px_y * 8) * 3 + 1) as usize] = color.1;
    output[((px_x + px_y * 8) * 3 + 2) as usize] = color.2;
  };
  if bpp8 {
    // 1 byte per pixel
    for i in 0..64 {
      let palette_idx = gba.vram[(tile_addr + i) as usize] as usize;
      let color = u16::from(gba.palette_ram[palette_idx])
        + (u16::from(gba.palette_ram[palette_idx + 1]) << 8);

      let px_x = (i % 8);
      let px_y = (i / 8) * 8;

      write_color(color, px_x, px_y);
    }
  } else {
    // 4 bits per pixel
    let palette_base = {
      // Each sub palette has 16 colors of 2 bytes each
      (palette_number * 16 * 2) as usize
    };
    for i in 0..32u32 {
      let (mut px_x_within_tile, mut px_y_within_tile) = ((i & 0x3) * 2, (i >> 2));

      // First 4 bits are the first px's palette idx, next 4 are the next color's
      let (palette_idx1, palette_idx2) = {
        let palette_idxs = gba.vram[(tile_addr + i) as usize] as usize;
        #[allow(clippy::identity_op)]
        ((palette_idxs >> 0) & 0xF, (palette_idxs >> 4) & 0xF)
      };

      let palette_idx1 = if palette_idx1 != 0 { palette_base + (palette_idx1 * 2) } else { 0 };
      let color = u16::from(gba.palette_ram[palette_idx1])
        + (u16::from(gba.palette_ram[palette_idx1 + 1]) << 8);

      write_color(color, px_x_within_tile, px_y_within_tile);

      let px2_x_within_tile = px_x_within_tile.saturating_add(1);

      let palette_idx2 = if palette_idx2 != 0 { palette_base + (palette_idx2 * 2) } else { 0 };
      let color = u16::from(gba.palette_ram[palette_idx2])
        + (u16::from(gba.palette_ram[palette_idx2 + 1]) << 8);
      write_color(color, px2_x_within_tile, px_y_within_tile);
    }
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

  let h_flip = (tile_map_element & 0x0000_0400) != 0;
  let v_flip = (tile_map_element & 0x0000_0800) != 0;

  let mut output = alloc::vec![0u8; 8 * 8*3];

  let mut write_color = |color: u16, px_x: u32, px_y: u32| {
    let color = color_to_rgb(color);
    output[((px_x + px_y * 8) * 3 + 0) as usize] = color.0;
    output[((px_x + px_y * 8) * 3 + 1) as usize] = color.1;
    output[((px_x + px_y * 8) * 3 + 2) as usize] = color.2;
  };
  if full_palette {
    // 1 byte per pixel
    for i in 0..64 {
      let palette_idx = gba.vram[(tile_addr + i) as usize] as usize;
      let color = u16::from(gba.palette_ram[palette_idx])
        + (u16::from(gba.palette_ram[palette_idx + 1]) << 8);

      let px_x = (i % 8);
      let px_y = (i / 8) * 8;

      write_color(color, px_x, px_y);
    }
  } else {
    // 4 bits per pixel
    let palette_base = {
      let palette_number = (tile_map_element >> 12) & 0xF;
      // Each sub palette has 16 colors of 2 bytes each
      (palette_number * 16 * 2) as usize
    };
    for i in 0..32u32 {
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
        let palette_idxs = gba.vram[(tile_addr + i) as usize] as usize;
        #[allow(clippy::identity_op)]
        ((palette_idxs >> 0) & 0xF, (palette_idxs >> 4) & 0xF)
      };

      let palette_idx1 = if palette_idx1 != 0 { palette_base + (palette_idx1 * 2) } else { 0 };
      let color = u16::from(gba.palette_ram[palette_idx1])
        + (u16::from(gba.palette_ram[palette_idx1 + 1]) << 8);

      write_color(color, px_x_within_tile, px_y_within_tile);

      let px2_x_within_tile =
        if h_flip { px_x_within_tile - 1 } else { px_x_within_tile.saturating_add(1) };

      let palette_idx2 = if palette_idx2 != 0 { palette_base + (palette_idx2 * 2) } else { 0 };
      let color = u16::from(gba.palette_ram[palette_idx2])
        + (u16::from(gba.palette_ram[palette_idx2 + 1]) << 8);
      write_color(color, px2_x_within_tile, px_y_within_tile);
    }
  }
  (output, tile_map_element_addr, tile_addr, tile_map_element, tile_number)
}
