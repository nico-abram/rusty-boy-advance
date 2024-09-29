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

#[inline]
fn parse_bgcnt(bg_control_flags: u32) -> (u32, bool, u32, u32, bool, (u32, u32)) {
  let priority = bg_control_flags & 0x3;
  let mosaic = bg_control_flags & 0x40 != 0;
  // The tile data contains colours, format depends on full palette
  // (1 byte per colour) or not (4 bits a colour)
  const TILE_DATA_PAGE_SIZE: u32 = 16 * 1024;
  let tile_data_page = u32::from((bg_control_flags >> 2) & 0x3);
  let tile_data_base_addr = 0x0600_0000 + tile_data_page * TILE_DATA_PAGE_SIZE;

  // The tilemap contains the attributes for the tile(Like being flipped)
  // and the tile index into the tile data
  const TILE_MAP_PAGE_SIZE: u32 = 2 * 1024;
  let tile_map_page = u32::from((bg_control_flags >> 8) & 0x1F);
  let tile_map_base_addr = 0x0600_0000 + tile_map_page * TILE_MAP_PAGE_SIZE;

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

pub fn get_mode0_tile(
  gba: &mut GBA,
  bg_num: u32,
  tile_x: u32,
  tile_y: u32,
) -> impl std::iter::Iterator<Item = u8> {
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
  /*
  let tile_map_element_addr = tile_map_base_addr
    + (((bg_tile_x & 0x1F) + bg_tile_y * bg_x_tile_count) * 2)
    + if bg_tile_x >= 32 { 32 * bg_y_tile_count * 2 } else { 0 };

  // See http://problemkaputt.de/gbatek.htm#lcdvrambgscreendataformatbgmap
  let tile_map_element = u32::from(gba.fetch_u16(tile_map_element_addr));

  let tile_number = tile_map_element & (if full_palette { 0x3FF } else { 0x1FF });
  let tile_addr = 0x0600_0000 + tile_number * bytes_per_tile;

  let h_flip = (tile_map_element & 0x0000_0400) != 0;
  let v_flip = (tile_map_element & 0x0000_0800) != 0;

  if full_palette {
    // 1 byte per pixel
    for i in 0..64 {
      let palette_idx = gba.fetch_byte(tile_data_base_addr + tile_number + i) as usize;
      let color = u16::from(gba.palette_ram[palette_idx])
        + (u16::from(gba.palette_ram[palette_idx + 1]) << 8);
      let px_idx = (screen_tile_x * 8
        + (screen_tile_y * bg_x_tile_count * 8)
        + (i % 8)
        + (i / 8) * bg_x_tile_count * 8) as usize;
      if px_idx < (&gba.output_texture[..]).len() {
        Self::fill_output_color(&mut gba.output_texture[..], px_idx, color);
      }
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

      let (px_x_idx, overflows_x) = ((px_x_within_tile + screen_tile_x * 8) as usize)
        .overflowing_sub(leftover_x_pxs_from_scroll as usize);
      let (px_y_idx, overflows_y) = ((px_y_within_tile + screen_tile_y * 8) as usize)
        .overflowing_sub(leftover_y_pxs_from_scroll as usize);
      if overflows_x || overflows_y {
        continue;
      }

      if px_y_idx >= VIDEO_HEIGHT {
        continue;
      }

      // First 4 bits are the first px's palette idx, next 4 are the next color's
      let (palette_idx1, palette_idx2) = {
        let palette_idxs = gba.fetch_byte(tile_addr + i) as usize;
        #[allow(clippy::identity_op)]
        ((palette_idxs >> 0) & 0xF, (palette_idxs >> 4) & 0xF)
      };

      if px_x_idx < VIDEO_WIDTH {
        let px_idx1 = px_x_idx.wrapping_add(px_y_idx * VIDEO_WIDTH);
        let palette_idx1 = palette_base + (palette_idx1 * 2);

        let color = u16::from(gba.palette_ram[palette_idx1])
          + (u16::from(gba.palette_ram[palette_idx1 + 1]) << 8);
        Self::fill_output_color(&mut gba.output_texture[..], px_idx1, color);
      }
      if if h_flip { px_x_idx - 1 } else { px_x_idx.saturating_add(1) } < VIDEO_WIDTH {
        let px_idx2 = (if h_flip { px_x_idx - 1 } else { px_x_idx.saturating_add(1) })
          .wrapping_add(px_y_idx * VIDEO_WIDTH);
        let palette_idx2 = palette_base + (palette_idx2 * 2);
        let color = u16::from(gba.palette_ram[palette_idx2])
          + (u16::from(gba.palette_ram[palette_idx2 + 1]) << 8);
        Self::fill_output_color(&mut gba.output_texture[..], px_idx2, color);
      }
    }
  }
  */

  std::iter::from_fn(|| {
    //f();
    Some(0)
  })
}
