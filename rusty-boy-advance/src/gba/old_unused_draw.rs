// original get_tile: (Used for debug tile viewer)

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
    write_color(&mut output[((px_x + px_y * 8) * 3) as usize..], color);
  };
  if bpp8 {
    // 1 byte per pixel
    for i in 0..64 {
      let palette_idx = gba.vram[(tile_addr + i) as usize] as usize;
      let color = fetch_u16_palette_ram(gba, palette_idx);

      let px_x = (i % 8);
      let px_y = (i / 8) * 8;

      write_color(color, px_x, px_y);
    }
  } else {
    // 4 bits per pixel
    // Each sub palette has 16 colors of 2 bytes each
    let palette_base = palette_number as usize * 16 * 2;
    for i in 0..32u32 {
      let (mut px_x_within_tile, mut px_y_within_tile) = ((i & 0x3) * 2, (i >> 2));

      // First 4 bits are the first px's palette idx, next 4 are the next color's
      let (palette_idx1, palette_idx2) = {
        let palette_idxs = gba.vram[(tile_addr + i) as usize] as usize;
        #[allow(clippy::identity_op)]
        ((palette_idxs >> 0) & 0xF, (palette_idxs >> 4) & 0xF)
      };

      let palette_idx1 = if palette_idx1 != 0 { palette_base + (palette_idx1 * 2) } else { 0 };
      let color = fetch_u16_palette_ram(gba, palette_idx1);

      write_color(color, px_x_within_tile, px_y_within_tile);

      let px2_x_within_tile = px_x_within_tile.saturating_add(1);

      let palette_idx2 = if palette_idx2 != 0 { palette_base + (palette_idx2 * 2) } else { 0 };
      let color = fetch_u16_palette_ram(gba, palette_idx2);
      write_color(color, px2_x_within_tile, px_y_within_tile);
    }
  }
  output
}

// original get bg tile for debug view:

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
    write_color(&mut output[((px_x + px_y * 8) * 3) as usize..], color);
  };
  if full_palette {
    // 1 byte per pixel
    for i in 0..64 {
      let palette_idx = gba.vram[(tile_addr + i) as usize] as usize;
      let color = fetch_u16_palette_ram(gba, palette_idx);

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
      let color = fetch_u16_palette_ram(gba, palette_idx1);

      write_color(color, px_x_within_tile, px_y_within_tile);

      let px2_x_within_tile =
        if h_flip { px_x_within_tile - 1 } else { px_x_within_tile.saturating_add(1) };

      let palette_idx2 = if palette_idx2 != 0 { palette_base + (palette_idx2 * 2) } else { 0 };
      let color = fetch_u16_palette_ram(gba, palette_idx2);
      write_color(color, px2_x_within_tile, px_y_within_tile);
    }
  }
  (output, tile_map_element_addr, tile_addr, tile_map_element, tile_number)
}

// First scanline based  rendering function in gba.rs:

pub(crate) fn draw_scanline(&mut self, scanline: u32) {
  let display_control = self.fetch_u16(DISPCNT_ADDR);
  let bg_mode = display_control & 0x0000_0007;
  if bg_mode != 0 {
    return;
  }

  const TILE_WIDTH: u32 = 8;
  const TILE_HEIGHT: u32 = 8;

  let bg_num = 0;
  const REG_BG0HOFS: u32 = 0x0400_0010;
  const REG_BG0VOFS: u32 = 0x0400_0012;
  let bg_hofs = REG_BG0HOFS + bg_num * 4;
  let bg_vofs = REG_BG0VOFS + bg_num * 4;
  let (scroll_x, scroll_y) =
    ((self.fetch_u16(bg_hofs) & 0x1FF) as u32, (self.fetch_u16(bg_vofs) & 0x1FF) as u32);
  let (bg_first_tile_x, bg_first_tile_y) = (scroll_x / 8, scroll_y / 8);
  let (leftover_x_pxs_from_scroll, leftover_y_pxs_from_scroll) = (scroll_x % 8, (scroll_y % 8));

  const BG0CNT: u32 = 0x0400_0008;
  const BG1CNT: u32 = 0x0400_000A;
  const BG2CNT: u32 = 0x0400_000C;
  const BG3CNT: u32 = 0x0400_000E;
  let bgcnt = BG0CNT + bg_num * 2;
  let bg_control_flags = self.fetch_u16(bgcnt);

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

  let bytes_per_tile =
    if full_palette { TILE_WIDTH * TILE_HEIGHT } else { TILE_WIDTH * (TILE_HEIGHT / 2) };
  let draw_tile = |gba: &mut GBA, bg_tile_x, bg_tile_y, screen_tile_x, screen_tile_y| {
    // The math here is pretty weird. The way the tiles are laid out in memory seems to be
    // the first (top-left) 32x32 tiles, then the bottom-left 32x32, then top-right
    // then bottom-right
    let tile_map_element_addr = tile_map_base_addr
      + (((bg_tile_x & 0x1F) + bg_tile_y * bg_y_tile_count) * 2)
      + if bg_tile_x >= 32 { 32 * bg_y_tile_count * 2 } else { 0 };

    // See http://problemkaputt.de/gbatek.htm#lcdvrambgscreendataformatbgmap
    let tile_map_element = u32::from(gba.fetch_u16(tile_map_element_addr));

    let tile_number = tile_map_element & (if full_palette { 0x3FF } else { 0x3FF });
    let tile_addr = tile_data_base_addr + tile_number * bytes_per_tile;

    let h_flip = (tile_map_element & 0x0000_0400) != 0;
    let v_flip = (tile_map_element & 0x0000_0800) != 0;

    if full_palette {
      // 1 byte per pixel
      for i in 0..64 {
        let i = i;

        let palette_idx = gba.fetch_byte(tile_addr + i) as usize;
        let color = u16::from(gba.palette_ram[palette_idx])
          + (u16::from(gba.palette_ram[palette_idx + 1]) << 8);

        let px_y_idx = screen_tile_y * 8 + (i / 8);
        if px_y_idx as u32 != scanline {
          continue;
        }

        let px_idx = (screen_tile_x * 8 + (i % 8) + px_y_idx * bg_x_tile_count) as usize;
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
        // clarification: it is when we have a non zero scroll on x, and without moving through x we move
        // through y, the left-most scrollx pixels are not being drawn to, it seems
        // probably need to draw from 1 more tile?
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

        if px_y_idx as u32 != scanline {
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
          let palette_idx1 = if palette_idx1 != 0 { palette_base + (palette_idx1 * 2) } else { 0 };

          let color = u16::from(gba.palette_ram[palette_idx1])
            + (u16::from(gba.palette_ram[palette_idx1 + 1]) << 8);
          Self::fill_output_color(&mut gba.output_texture[..], px_idx1, color);
        }
        if if h_flip { px_x_idx - 1 } else { px_x_idx.saturating_add(1) } < VIDEO_WIDTH {
          let px_idx2 = (if h_flip { px_x_idx - 1 } else { px_x_idx.saturating_add(1) })
            .wrapping_add(px_y_idx * VIDEO_WIDTH);
          let palette_idx2 = if palette_idx2 != 0 { palette_base + (palette_idx2 * 2) } else { 0 };
          let color = u16::from(gba.palette_ram[palette_idx2])
            + (u16::from(gba.palette_ram[palette_idx2 + 1]) << 8);
          Self::fill_output_color(&mut gba.output_texture[..], px_idx2, color);
        }
      }
    }
  };
  const SCREEN_Y_TILE_COUNT: u32 = (VIDEO_HEIGHT as u32) / TILE_HEIGHT;
  const SCREEN_X_TILE_COUNT: u32 = (VIDEO_WIDTH as u32) / TILE_WIDTH;
  // We use an extra one for the case were we are mid-scroll and are displaying N+1 tiles
  let y = scanline / TILE_HEIGHT;
  for y in 0..=SCREEN_Y_TILE_COUNT {
    for x in 0..=SCREEN_X_TILE_COUNT {
      draw_tile(
        self,
        (bg_first_tile_x + x) % bg_x_tile_count,
        (bg_first_tile_y + y) % bg_y_tile_count,
        x,
        y,
      );
    }
  }
}

// first draw implementation  ever in gba.rs:

#[allow(clippy::unused_unit)]
pub(crate) fn update_video_output(&mut self) {
  for i in 0..VIDEO_WIDTH {
    super::draw::draw_scanline(self, i);
  }
  let display_control = self.fetch_u16(DISPCNT_ADDR);
  let bg_mode = display_control & 0x0000_0007;
  match bg_mode {
    0 => {
      const TILE_WIDTH: u32 = 8;
      const TILE_HEIGHT: u32 = 8;

      let bg_num = 0;
      const REG_BG0HOFS: u32 = 0x0400_0010;
      const REG_BG0VOFS: u32 = 0x0400_0012;
      let bg_hofs = REG_BG0HOFS + bg_num * 4;
      let bg_vofs = REG_BG0VOFS + bg_num * 4;
      let (scroll_x, scroll_y) =
        ((self.fetch_u16(bg_hofs) & 0x1FF) as u32, (self.fetch_u16(bg_vofs) & 0x1FF) as u32);
      let (bg_first_tile_x, bg_first_tile_y) = (scroll_x / 8, scroll_y / 8);
      let (leftover_x_pxs_from_scroll, leftover_y_pxs_from_scroll) = (scroll_x % 8, (scroll_y % 8));

      const BG0CNT: u32 = 0x0400_0008;
      const BG1CNT: u32 = 0x0400_000A;
      const BG2CNT: u32 = 0x0400_000C;
      const BG3CNT: u32 = 0x0400_000E;
      let bgcnt = BG0CNT + bg_num * 2;
      let bg_control_flags = self.fetch_u16(bgcnt);

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

      let bytes_per_tile =
        if full_palette { TILE_WIDTH * TILE_HEIGHT } else { TILE_WIDTH * (TILE_HEIGHT / 2) };
      let draw_tile = |gba: &mut GBA, bg_tile_x, bg_tile_y, screen_tile_x, screen_tile_y| {
        // The math here is pretty weird. The way the tiles are laid out in memory seems to be
        // the first (top-left) 32x32 tiles, then the bottom-left 32x32, then top-right
        // then bottom-right
        let tile_map_element_addr = tile_map_base_addr
          + (((bg_tile_x & 0x1F) + bg_tile_y * bg_y_tile_count) * 2)
          + if bg_tile_x >= 32 { 32 * bg_y_tile_count * 2 } else { 0 };

        // See http://problemkaputt.de/gbatek.htm#lcdvrambgscreendataformatbgmap
        let tile_map_element = u32::from(gba.fetch_u16(tile_map_element_addr));

        let tile_number = tile_map_element & (if full_palette { 0x3FF } else { 0x3FF });
        let tile_addr = tile_data_base_addr + tile_number * bytes_per_tile;

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
              let palette_idx1 =
                if palette_idx1 != 0 { palette_base + (palette_idx1 * 2) } else { 0 };

              let color = u16::from(gba.palette_ram[palette_idx1])
                + (u16::from(gba.palette_ram[palette_idx1 + 1]) << 8);
              Self::fill_output_color(&mut gba.output_texture[..], px_idx1, color);
            }
            if if h_flip { px_x_idx - 1 } else { px_x_idx.saturating_add(1) } < VIDEO_WIDTH {
              let px_idx2 = (if h_flip { px_x_idx - 1 } else { px_x_idx.saturating_add(1) })
                .wrapping_add(px_y_idx * VIDEO_WIDTH);
              let palette_idx2 =
                if palette_idx2 != 0 { palette_base + (palette_idx2 * 2) } else { 0 };
              let color = u16::from(gba.palette_ram[palette_idx2])
                + (u16::from(gba.palette_ram[palette_idx2 + 1]) << 8);
              Self::fill_output_color(&mut gba.output_texture[..], px_idx2, color);
            }
          }
        }
      };
      const SCREEN_Y_TILE_COUNT: u32 = (VIDEO_HEIGHT as u32) / TILE_HEIGHT;
      const SCREEN_X_TILE_COUNT: u32 = (VIDEO_WIDTH as u32) / TILE_WIDTH;
      // We use an extra one for the case were we are mid-scroll and are displaying N+1 tiles
      for y in 0..=SCREEN_Y_TILE_COUNT {
        for x in 0..=SCREEN_X_TILE_COUNT {
          draw_tile(
            self,
            (bg_first_tile_x + x) % bg_x_tile_count,
            (bg_first_tile_y + y) % bg_y_tile_count,
            x,
            y,
          );
        }
      }
    }
    1 => (),
    2 => (),
    3 => {
      // 16 bit color bitmap. One frame buffer (240x160 pixels, 32768 colors)
      #[allow(clippy::match_ref_pats)]
      for (idx, slice) in self.vram.chunks_exact(2).take(VIDEO_WIDTH * VIDEO_HEIGHT).enumerate() {
        if let &[high_byte, low_byte] = slice {
          let color = u16::from(low_byte) + (u16::from(high_byte) << 8);
          Self::fill_output_color(&mut self.output_texture[..], idx, color);
        }
      }
    }
    4 => {
      // 8 bit palette indexed bitmap. Two frame buffers (240x160 pixels, 256 colors)
      let second_frame = (self.io_mem[0] & 0x0000_0008) != 0;
      for (idx, palette_idx) in self
        .vram
        .iter()
        .skip(if second_frame { VIDEO_WIDTH * VIDEO_HEIGHT } else { 0 })
        .take(VIDEO_WIDTH * VIDEO_HEIGHT)
        .enumerate()
      {
        let palette_idx = (*palette_idx as usize) * 2;
        let color = u16::from(self.palette_ram[palette_idx])
          + (u16::from(self.palette_ram[palette_idx + 1]) << 8);
        Self::fill_output_color(&mut self.output_texture[..], idx, color);
      }
    }
    5 => {
      // 16 bit color bitmap. Two frame buffers (160x128 pixels, 32768 colors)
      const BGMODE5_WIDTH: usize = 160;
      const BGMODE5_HEIGHT: usize = 128;
      const BGMODE5_FRAMEBUFFER_PX_COUNT: usize = BGMODE5_WIDTH * BGMODE5_HEIGHT;
      const BGMODE5_FIRST_X_PX: usize = (VIDEO_WIDTH - BGMODE5_WIDTH) / 2;
      const BGMODE5_FIRST_Y_PX: usize = (VIDEO_HEIGHT - BGMODE5_HEIGHT) / 2;
      let second_frame = (self.io_mem[0] & 0x0000_0008) != 0; // TODO: Is this right?
      let mut iter = self
        .vram
        .chunks_exact(2)
        .skip(if second_frame { BGMODE5_FRAMEBUFFER_PX_COUNT } else { 0 })
        .take(BGMODE5_FRAMEBUFFER_PX_COUNT);
      for x in 0..VIDEO_WIDTH {
        for y in 0..VIDEO_HEIGHT {
          let idx = x + y * VIDEO_WIDTH;
          if x < BGMODE5_FIRST_X_PX
            || x > BGMODE5_FIRST_X_PX + BGMODE5_WIDTH
            || y < BGMODE5_FIRST_Y_PX
            || y > BGMODE5_FIRST_Y_PX + BGMODE5_HEIGHT
          {
            // TODO: Is this right?
            Self::fill_output_color(&mut self.output_texture[..], idx, u16::max_value());
          } else if let &[high_byte, low_byte] = iter.next().unwrap() {
            // TODO: Is this right?
            let color = u16::from(low_byte) + (u16::from(high_byte) << 8);
            Self::fill_output_color(&mut self.output_texture[..], idx, color);
          }
        }
      }
    }
    _ => (),
  }
}
