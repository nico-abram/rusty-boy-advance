use super::gba::{GBA, VIDEO_HEIGHT, VIDEO_WIDTH};

const TILE_WIDTH: u32 = 8;
const TILE_HEIGHT: u32 = 8;

const REG_BG0HOFS: u32 = 0x0400_0010;
const REG_BG0VOFS: u32 = 0x0400_0012;

pub const H_FLIP_MAP_BIT: u32 = 0x0000_0400;
pub const V_FLIP_MAP_BIT: u32 = 0x0000_0800;

#[inline]
fn bgscroll(gba: &mut GBA, bg_num: u32) -> (u16, u16) {
  (fetch_u16_iomem(gba, REG_BG0HOFS + bg_num * 4), fetch_u16_iomem(gba, REG_BG0VOFS + bg_num * 4))
}

const BG0CNT: u32 = 0x0400_0008;
#[inline]
fn bgcnt(gba: &mut GBA, bg_num: u32) -> u32 {
  fetch_u16_iomem(gba, BG0CNT + bg_num * 2) as u32
}

const TILE_DATA_PAGE_SP0: u32 = 4;
const TILE_MAP_PAGE_SIZE: u32 = 2 * 1024;
const TILE_DATA_PAGE_SIZE: u32 = 16 * 1024;
pub const TILE_DATA_PAGE_TILE_COUNT_BPP8: u32 = TILE_DATA_PAGE_SIZE / (64 * 64);
pub const TILE_DATA_PAGE_TILE_COUNT_BPP4: u32 = TILE_DATA_PAGE_TILE_COUNT_BPP8 / 2;
#[inline]
fn parse_bgcnt(bg_control_flags: u32) -> (u8, bool, u32, u32, bool, (u8, u8)) {
  let priority = (bg_control_flags & 0x3) as u8;
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
    (bg_x_tile_count as u8, bg_y_tile_count as u8),
  )
}

#[inline]
fn bytes_per_tile(full_palette: bool) -> u32 {
  if full_palette { TILE_WIDTH * TILE_HEIGHT } else { TILE_WIDTH * (TILE_HEIGHT / 2) }
}

fn write_color(output: &mut [u16], color: u16) {
  output[0] = color;
}

fn fetch_u16(mem: &[u8], addr: usize) -> u16 {
  (mem[addr] as u16) + ((mem[addr + 1] as u16) << 8)
}

fn fetch_u16_vram(gba: &GBA, addr: u16) -> u16 {
  let addr = addr as usize;
  fetch_u16(&gba.vram[..], addr)
}

fn fetch_u16_palette_ram(gba: &GBA, addr: usize) -> u16 {
  fetch_u16(&gba.palette_ram[..], addr)
}

#[inline]
fn fetch_u16_iomem(gba: &GBA, addr: u32) -> u16 {
  fetch_u16(&gba.io_mem[..], (addr - 0x0400_0000) as usize)
}

pub fn get_tile(
  gba: &mut GBA,
  tile_page: u32,
  tile_idx: u32,
  bpp8: bool,
  palette_number: u8,
) -> alloc::vec::Vec<u16> {
  let bytes_per_tile = bytes_per_tile(bpp8);

  let tile_data_base_addr = tile_page * TILE_DATA_PAGE_SIZE;
  let tile_addr = tile_data_base_addr + tile_idx * bytes_per_tile;

  let mut output = alloc::vec![0u16; 8 * 8];

  for strip_idx in 0..8 {
    draw_mode0_bg_tile_hstrip(
      gba,
      &mut output[strip_idx * 8..],
      false,
      false,
      palette_number,
      tile_addr,
      bpp8,
      strip_idx,
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
) -> (alloc::vec::Vec<u16>, u32, u32, u32, u16) {
  let background_control_flags = bgcnt(gba, bg_num);
  let (
    _priority,
    _mosaic,
    tile_data_base_addr,
    tile_map_base_addr,
    full_palette,
    (bg_tile_count_x, bg_tile_count_y),
  ) = parse_bgcnt(background_control_flags);

  let bytes_per_tile = bytes_per_tile(full_palette);

  assert!(tile_x < bg_tile_count_x as u32);
  assert!(tile_x < bg_tile_count_y as u32);

  // The math here is pretty weird. The way the tiles are laid out in memory seems to be
  // the first (top-left) 32x32 tiles, then the bottom-left 32x32, then top-right
  // then bottom-right
  let tile_map_element_addr = tile_map_base_addr
    + (((tile_x & 0x1F) + tile_y * bg_tile_count_y as u32) * 2)
    + if tile_x >= 32 { 32 * bg_tile_count_y as u32 * 2 } else { 0 };

  // See http://problemkaputt.de/gbatek.htm#lcdvrambgscreendataformatbgmap
  let tile_map_element = u32::from(fetch_u16_vram(gba, tile_map_element_addr as u16));

  let (h_flip, v_flip, palette_number, tile_number) = parse_tile_map_element(tile_map_element);
  let tile_addr = tile_data_base_addr + tile_number as u32 * bytes_per_tile;

  let mut output = alloc::vec![0u16; 8 * 8];
  for strip_idx in 0..8 {
    draw_mode0_bg_tile_hstrip(
      gba,
      &mut output[strip_idx * 8..],
      h_flip,
      v_flip,
      palette_number,
      tile_addr,
      full_palette,
      strip_idx,
    );
  }

  (output, tile_map_element_addr, tile_addr, tile_map_element, tile_number)
}

/// (h_flip, v_flip, palette_number, tile_number)
fn parse_tile_map_element(tile_map_element: u32) -> (bool, bool, u8, u16) {
  let h_flip = (tile_map_element & H_FLIP_MAP_BIT) != 0;
  let v_flip: bool = (tile_map_element & V_FLIP_MAP_BIT) != 0;
  let palette_number = (tile_map_element >> 12) & 0x1F;
  let tile_number = tile_map_element & 0x3FF;
  (h_flip, v_flip, palette_number as u8, tile_number as u16)
}
/// Draws an 8 pixel horizontal strip into the output slice
/// Returns true if any pixels are transparent
pub fn draw_mode0_bg_tile_hstrip(
  gba: &mut GBA,
  output: &mut [u16],
  h_flip: bool,
  v_flip: bool,
  palette_number: u8,
  tile_addr: u32,
  full_palette: bool,
  mut strip_idx_in_tile: usize,
) {
  //let h_flip = (tile_map_element & H_FLIP_MAP_BIT) != 0;
  //let v_flip: bool = (tile_map_element & V_FLIP_MAP_BIT) != 0;
  if v_flip {
    strip_idx_in_tile = 7 - strip_idx_in_tile;
  }

  if full_palette {
    // 1 byte per pixel
    let start_idx = strip_idx_in_tile * 8;
    let end_idx = (strip_idx_in_tile + 1) * 8;
    let h_flip_offs = 2 * start_idx + 7;

    for i in start_idx..end_idx {
      let lookup_idx = if h_flip { h_flip_offs - i } else { i };
      let palette_idx = gba.vram[tile_addr as usize + lookup_idx] as usize;

      let color = fetch_u16(&gba.palette_ram[..], palette_idx * 2);
      write_color(&mut output[(i & 0x7) as usize..], color);
    }
  } else {
    // 4 bits per pixel
    let palette_base = {
      //let palette_number = (tile_map_element >> 12) & 0x1F;
      // Each sub palette has 16 colors of 2 bytes each
      (palette_number as u32 * 16 * 2) as usize
    };
    let start_idx = strip_idx_in_tile * 4;
    let end_idx = start_idx + 4;

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
      let palette_idx1 = if !transparent_px1 { palette_base + (palette_idx1 * 2) } else { 0 };
      let color = fetch_u16(&gba.palette_ram[..], palette_idx1);
      write_color(&mut output[px_x_within_tile as usize..], color);

      let px2_x_within_tile =
        if h_flip { px_x_within_tile - 1 } else { px_x_within_tile.saturating_add(1) };

      let transparent_px2 = palette_idx2 == 0;
      let palette_idx2 = if !transparent_px2 { palette_base + (palette_idx2 * 2) } else { 0 };
      let color = fetch_u16(&gba.palette_ram[..], palette_idx2);
      write_color(&mut output[px2_x_within_tile as usize..], color);
    }
  }
}

type BgData = (u8, u32, [u16; 2], bool, (u16, u16));
fn draw_scanline_bgmode0(gba: &mut GBA, scanline: u32, display_control: u16) {
  let bg_data_arr: [_; 4] = std::array::from_fn(|bg_num| {
    let bg_num: u32 = bg_num as u32;
    let bg_ctrl_flags = bgcnt(gba, bg_num);

    let (scroll_x, scroll_y) = bgscroll(gba, bg_num);
    let bg_raw_y = scroll_y + scanline as u16;
    let bg_raw_tile_y = bg_raw_y / 8;
    let is_second_vertical_screenblock = bg_raw_tile_y & 32 != 0;

    let (
      priority,
      _mosaic,
      tile_data_base_addr,
      tile_map_base_addr,
      full_palette,
      (bg_tile_count_x, bg_tile_count_y),
    ) = parse_bgcnt(bg_ctrl_flags);
    let tile_map_base_addr = tile_map_base_addr as u16;
    // array indexed by 32x32 bg slice (also called a screenblock)
    // indices:
    // [00 11]
    // [22 33]
    let tile_map_base_addrs = if is_second_vertical_screenblock && bg_tile_count_y == 64 {
      if bg_tile_count_x == 64 {
        [tile_map_base_addr + 0x800 * 2, tile_map_base_addr + 0x800 * 3]
      } else {
        // 32 horiz X tiles
        [tile_map_base_addr + 0x800, tile_map_base_addr + 0x800]
      }
    } else {
      if bg_tile_count_x == 64 {
        [tile_map_base_addr, tile_map_base_addr + 0x800]
      } else {
        // 32 horiz X tiles
        [tile_map_base_addr, tile_map_base_addr]
      }
    };

    (priority, tile_data_base_addr, tile_map_base_addrs, full_palette, (scroll_x, scroll_y))
  });

  struct PriorityDrawLayer {
    implicit_priority: u8,
    priority: u8,
    layer: u8,
  }
  impl PriorityDrawLayer {
    fn is_bg(&self) -> bool {
      (self.layer & 0x80) == 0
    }
    fn idx(&self) -> usize {
      (self.layer & 0x7F) as usize
    }
  }
  let mut priority_array: [_; 128 + 4] =
    core::array::from_fn(|_| PriorityDrawLayer { implicit_priority: 0, priority: 0, layer: 0 });
  let mut priority_array_fill_size = 0;
  for (idx, bg_data) in bg_data_arr.iter().enumerate() {
    let enabled = display_control & (1 << (8 + idx)) != 0;
    if enabled {
      let elem = &mut priority_array[priority_array_fill_size];
      elem.priority = bg_data.0;
      elem.implicit_priority = 4 - idx as u8;
      elem.layer = idx as u8;
      priority_array_fill_size += 1;
    }
  }

  let mut sprite_data_arr: [_; 128] = core::array::from_fn(|_| (0u16, 0u16, 0u16));
  for sprite_idx in 0..128 {
    let sprite_data = fetch_sprite(gba, sprite_idx);
    sprite_data_arr[sprite_idx as usize] = sprite_data;
    let (y, obj_mode, _, _, _, shape) = parse_sprite_attr0(sprite_data.0);
    let (_, _, _, _, size) = parse_sprite_attr1(sprite_data.1);
    let (_size_x, size_y) = sprite_size(size, shape);

    let not_hidden = obj_mode != 0b10;
    let (y_min, y_max) = (y as u32, y as u32 + (size_y - 1) as u32);
    let in_scanline =
      if y_max <= 255 { scanline >= y_min && scanline <= y_max } else { scanline <= (y_max % 255) };
    if not_hidden && in_scanline {
      let (_, priority, _) = parse_sprite_attr2(sprite_data.2);
      let elem = &mut priority_array[priority_array_fill_size];
      elem.priority = priority;
      elem.implicit_priority = (127 - sprite_idx as u8) | 0x80;
      elem.layer = sprite_idx as u8 | 0x80;
      priority_array_fill_size += 1;
    }
  }
  priority_array[..priority_array_fill_size].sort_unstable_by(|a, b| {
    a.priority.cmp(&b.priority).then(a.implicit_priority.cmp(&b.implicit_priority).reverse())
  });

  let draw_scanline_for_bg = |gba: &mut GBA,
                              bg_data: &BgData,
                              first_bg: bool,
                              mut scanline_first_px_idx: usize| {
    let transparent_color = fetch_u16(&gba.palette_ram[..], 0);

    let (_priority, tile_data_base_addr, tile_map_base_addrs, full_palette, (scroll_x, scroll_y)) =
      bg_data;
    let bytes_per_tile = bytes_per_tile(*full_palette);

    let raw_bg_y = scroll_y + scanline as u16;
    let scroll_x_within_tile = (scroll_x % 8) as usize;
    let raw_bg_tile_y = raw_bg_y / 8;
    let bg_tile_y_in_screenblock = raw_bg_tile_y % 32;
    let strip_in_tile = (raw_bg_y % 8) as usize;

    let adjust_first_stripe_by_xscroll = scroll_x_within_tile != 0;
    let raw_bg_x = scroll_x; // + (strip_idx as u16 * 8);
    let mut raw_bg_tile_x = raw_bg_x / 8;

    let needs_extra_strip = adjust_first_stripe_by_xscroll;
    let mut output_pxs = [0u16; 8];
    let mut first_strip = true;
    const EXTRA_STRIP_IDX: usize = VIDEO_WIDTH / 8;
    for strip_idx in 0..((VIDEO_WIDTH / 8) + if needs_extra_strip { 1 } else { 0 }) {
      let bg_tile_x_in_screenblock = raw_bg_tile_x % 32;
      // 32 = 0x20
      let screenblock_in_x_idx = (raw_bg_tile_x & 32) >> 5;

      let tile_map_element_addr = tile_map_base_addrs[screenblock_in_x_idx as usize]
        + (bg_tile_x_in_screenblock * 2)
        + (bg_tile_y_in_screenblock * 2 * 32);
      let tile_map_element = fetch_u16_vram(gba, tile_map_element_addr as u16) as u32;

      let (h_flip, v_flip, palette_number, tile_number) = parse_tile_map_element(tile_map_element);
      let tile_addr = tile_data_base_addr + tile_number as u32 * bytes_per_tile;

      draw_mode0_bg_tile_hstrip(
        gba,
        &mut output_pxs,
        h_flip,
        v_flip,
        palette_number,
        tile_addr,
        *full_palette,
        strip_in_tile,
      );

      let (out_slice, painted_slice) = if first_strip && adjust_first_stripe_by_xscroll {
        let out_texture_slice =
          &mut gba.output_texture[scanline_first_px_idx..][..(8 - scroll_x_within_tile)];
        let painted_slice = &output_pxs[scroll_x_within_tile..];

        scanline_first_px_idx -= scroll_x_within_tile;

        (out_texture_slice, painted_slice)
      } else if strip_idx == EXTRA_STRIP_IDX {
        let out_texture_slice = &mut gba.output_texture
          [scanline_first_px_idx + EXTRA_STRIP_IDX * 8..][..scroll_x_within_tile];
        let painted_slice = &output_pxs[..scroll_x_within_tile];

        (out_texture_slice, painted_slice)
      } else {
        let out_texture_slice =
          &mut gba.output_texture[scanline_first_px_idx + strip_idx * 8..][..8];
        (out_texture_slice, &output_pxs[..])
      };
      if first_bg {
        out_slice.copy_from_slice(painted_slice);
      } else {
        assert!(out_slice.len() == painted_slice.len());
        for (out, in_) in out_slice.iter_mut().zip(painted_slice.iter()) {
          if *out == transparent_color {
            *out = *in_;
          }
        }
      }

      first_strip = false;
      raw_bg_tile_x += 1;
    }
    if needs_extra_strip {}
  };

  let transparent_color = fetch_u16(&gba.palette_ram[..], 0);
  let scanline_first_px_idx = (scanline * (VIDEO_WIDTH as u32)) as usize;
  for x in gba.output_texture[scanline_first_px_idx..][..VIDEO_WIDTH].iter_mut() {
    *x = transparent_color;
  }
  let first_bg = false;
  for draw_layer in &priority_array[..priority_array_fill_size] {
    if draw_layer.is_bg() {
      let bg_data = &bg_data_arr[draw_layer.idx()];
      draw_scanline_for_bg(gba, bg_data, first_bg, scanline_first_px_idx);
    } else {
      // sprite
      let &(attr0, attr1, attr2) = &sprite_data_arr[draw_layer.idx()];

      let (y, obj_mode, gfx_mode, mosaic, bpp8, shape) = parse_sprite_attr0(attr0);

      let (x, affine_idx, h_flip, v_flip, size) = parse_sprite_attr1(attr1);

      let (tile_idx, priority, palette_number) = parse_sprite_attr2(attr2);

      let (size_x, size_y) = sprite_size(size, shape);
      let tile_count_x = size_x as u32 / 8;

      let bytes_per_bpp4_tile = bytes_per_tile(false);
      let tile_data_base_addr =
        TILE_DATA_PAGE_SP0 * TILE_DATA_PAGE_SIZE + tile_idx as u32 * bytes_per_bpp4_tile;
      let bytes_per_tile = bytes_per_tile(bpp8);

      let stride_1d = display_control & 0x40 != 0;
      let char_stride_y_tiles = if stride_1d { tile_count_x } else { 32 } * bytes_per_bpp4_tile;

      let mut output_pxs = [0u16; 8];
      let y_within_sprite = scanline - if y > VIDEO_HEIGHT as u8 { 0 } else { y as u32 };
      let sprite_tile_y = y_within_sprite / 8;
      let sprite_tile_y =
        if v_flip { (size_y as u32 / 8) - 1 - sprite_tile_y } else { sprite_tile_y };
      let strip_in_tile = y_within_sprite % 8;
      let size_x_in_tiles = size_x / 8;
      for tile_x in 0..size_x_in_tiles {
        let in_tile_x = if h_flip { size_x_in_tiles - 1 - tile_x } else { tile_x };
        let tile_addr = tile_data_base_addr
          + in_tile_x as u32 * bytes_per_bpp4_tile
          + sprite_tile_y * char_stride_y_tiles;

        draw_mode0_bg_tile_hstrip(
          gba,
          &mut output_pxs,
          h_flip,
          v_flip,
          palette_number + 0x10,
          tile_addr,
          bpp8,
          strip_in_tile as usize,
        );
        let x_offset = x as usize + tile_x as usize * 8;
        if x_offset + 8 < VIDEO_WIDTH {
          let out_slice = &mut gba.output_texture[scanline_first_px_idx + x_offset..][..8];
          for (out, in_) in out_slice.iter_mut().zip(output_pxs.iter()) {
            if *out == transparent_color {
              *out = *in_;
            }
          }
        } else if (x_offset % 512) < VIDEO_WIDTH {
          let x_offset = x_offset % 512;
          // TODO: Partial last tile
          let pixels_to_draw = VIDEO_WIDTH - x_offset;
          let out_slice =
            &mut gba.output_texture[scanline_first_px_idx + x_offset..][..pixels_to_draw];
          for (out, in_) in out_slice.iter_mut().zip(output_pxs.iter()) {
            if *out == transparent_color {
              *out = *in_;
            }
          }
        } else if (x_offset + 8) % 512 < VIDEO_WIDTH {
          let last_x_offset = (x_offset + 8) % 512;
          let out_slice =
            &mut gba.output_texture[scanline_first_px_idx..][..last_x_offset as usize];
          for (out, in_) in out_slice.iter_mut().rev().zip(output_pxs.iter().rev()) {
            if *out == transparent_color {
              *out = *in_;
            }
          }
        }
        //gba.output_texture[scanline_first_px_idx..][..8 * 3].copy_from_slice(&output_pxs);
      }
      /*
      let tile_idx_in_page = tile_idx as u32 & (0x4000 - 1);
      let sp_page0 = 4;
      let tile_page = sp_page0 + if tile_idx & 0x4000 != 0 { 1 } else { 0 };
      let mut out_bytes = alloc::vec![0u8; size_x  as usize * size_y as usize * 3];
      for tile_x in 0..(size_x / 8) {
        for tile_y in 0..(size_y / 8) {
          let char_stride_y_tiles = if stride_1d { tile_count_x } else { 32 };
          let tile_rgb = get_tile(
            gba,
            tile_page,
            tile_idx_in_page + tile_x + tile_y * char_stride_y_tiles,
            bpp8,
            (palette_idx + 0x10) as u8,
          );
          let stride_y = size_x * 3;
          let stride_x = 8 * 3;
          let start_idx = tile_x * stride_x + tile_y * stride_y * 8;
          let start_idx = start_idx as usize;
          for y_within_tile in 0..8 {
            out_bytes[start_idx + y_within_tile * stride_y as usize..][..8 * 3]
              .copy_from_slice(&tile_rgb[y_within_tile * 8 * 3..][..8 * 3]);
          }
        }
      }
      */
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
      let start_idx_out = (start_px) as usize;
      #[allow(clippy::match_ref_pats)]
      for (idx, slice) in gba.vram[start_idx..].chunks_exact(2).take(VIDEO_WIDTH).enumerate() {
        if let &[low_byte, high_byte] = slice {
          let color = (u16::from(low_byte)) + (u16::from(high_byte) << 8);
          write_color(&mut gba.output_texture[(start_idx_out + idx)..], color);
        }
      }
    }
    4 => {
      // 8 bit palette indexed bitmap. Two frame buffers (240x160 pixels, 256 colors)
      let second_frame = (gba.io_mem[0] & 0x0000_0008) != 0;
      let start_idx =
        (start_px * 1) as usize + if second_frame { VIDEO_WIDTH * VIDEO_HEIGHT } else { 0 };
      let start_idx_out = (start_px) as usize;

      for (idx, palette_idx) in gba.vram[start_idx..].iter().take(VIDEO_WIDTH).enumerate() {
        let palette_idx = (*palette_idx as usize) * 2;
        let color = fetch_u16_palette_ram(gba, palette_idx);
        write_color(&mut gba.output_texture[(start_idx_out + idx)..], color);
      }
    }
    5 => {
      // 16 bit color bitmap. Two frame buffers (160x128 pixels, 32768 colors)
      const BGMODE5_WIDTH: usize = 160;
      const BGMODE5_HEIGHT: usize = 128;
      const BGMODE5_FRAMEBUFFER_PX_COUNT: usize = BGMODE5_WIDTH * BGMODE5_HEIGHT;
      const BGMODE5_FIRST_Y_PX: usize = (VIDEO_HEIGHT - BGMODE5_HEIGHT) / 2;

      let second_frame = (gba.io_mem[0] & 0x0000_0008) != 0; // TODO: Is this right?
      let start_idx =
        (start_px * 2) as usize + if second_frame { BGMODE5_FRAMEBUFFER_PX_COUNT * 2 } else { 0 };
      let mut start_idx_out = (start_px) as usize;

      if scanline < (BGMODE5_FIRST_Y_PX as u32)
        || scanline < ((BGMODE5_FIRST_Y_PX + BGMODE5_HEIGHT) as u32)
      {
        for x in 0..VIDEO_WIDTH {
          // TODO: Is this right?
          write_color(&mut gba.output_texture[(x)..], u16::max_value());
        }
      } else {
        for _x in 0..BGMODE5_FIRST_Y_PX {
          // TODO: Is this right?
          write_color(&mut gba.output_texture[start_idx_out..], u16::max_value());
          start_idx_out += 1;
        }
        let iter = gba.vram[start_idx..].chunks_exact(2).take(BGMODE5_WIDTH);
        for slice in iter {
          if let &[low_byte, high_byte] = slice {
            // TODO: Is this right?
            let color = u16::from(low_byte) + (u16::from(high_byte) << 8);
            write_color(&mut gba.output_texture[start_idx_out..], color);
            start_idx_out += 1;
          }
        }
        for _x in BGMODE5_WIDTH..(BGMODE5_WIDTH + BGMODE5_FIRST_Y_PX) {
          // TODO: Is this right?
          write_color(&mut gba.output_texture[start_idx_out..], u16::max_value());
          start_idx_out += 1;
        }
      }
    }
    _ => {
      //gba.print_fn.map(|f| f(&alloc::format!("Unrecognized bg mode: {bg_mode}")));
    }
  }
}

fn fetch_sprite(gba: &mut GBA, sprite_idx: u8) -> (u16, u16, u16) {
  let oam_idx = sprite_idx as usize * 8;
  let attr0 = fetch_u16(&gba.oam[..], oam_idx + 0);
  let attr1 = fetch_u16(&gba.oam[..], oam_idx + 2);
  let attr2 = fetch_u16(&gba.oam[..], oam_idx + 4);
  (attr0, attr1, attr2)
}

/// (y, obj_mode, gfx_mode, mosaic, bpp8, shape)
fn parse_sprite_attr0(attr0: u16) -> (u8, u8, u8, bool, bool, u8) {
  let y = attr0 & 0x00FF;
  let obj_mode = (attr0 & 0x0300) >> 8;
  let gfx_mode = (attr0 & 0x0C00) >> 10;
  let mosaic = attr0 & 0x1000 != 0;
  let bpp8 = attr0 & 0x2000 != 0;
  let shape = (attr0 & 0xC000) >> 14;
  (y as u8, obj_mode as u8, gfx_mode as u8, mosaic, bpp8, shape as u8)
}
/// (x, affine_idx, hflip, vflip, size)
fn parse_sprite_attr1(attr1: u16) -> (u16, u8, bool, bool, u8) {
  let x = attr1 & 0x01FF;
  let affine_idx = (attr1 & 0x3E00) >> 9;
  let hflip = attr1 & 0x1000 != 0;
  let vflip = attr1 & 0x2000 != 0;
  let size = (attr1 & 0xC000) >> 14;
  (x as u16, affine_idx as u8, hflip, vflip, size as u8)
}
/// (tile_idx, priority, palette_idx)
fn parse_sprite_attr2(attr2: u16) -> (u16, u8, u8) {
  let tile_idx = attr2 & 0x03FF;
  let priority = (attr2 & 0x0C00) >> 10;
  let palette_idx = (attr2 & 0xF000) >> 12;
  (tile_idx as u16, priority as u8, palette_idx as u8)
}
fn sprite_size(size: u8, shape: u8) -> (u8, u8) {
  let base_size = 8 << size;
  let base_size2 = 8 << size.saturating_sub(1);
  let base_size3 = 16 << ((size + 1) >> 1);
  if shape == 0b00 {
    (base_size, base_size)
  } else if shape == 0x01 {
    (base_size3, base_size2)
  } else {
    // 10
    (base_size2, base_size3)
  }
}

pub fn draw_sprite(
  gba: &mut GBA,
  sprite_idx: u8,
) -> (alloc::vec::Vec<u16>, u32, u32, u32, u32, u32, u32, u32, u8, u8, u8) {
  let (attr0, attr1, attr2) = fetch_sprite(gba, sprite_idx);

  let (y, obj_mode, gfx_mode, mosaic, bpp8, shape) = parse_sprite_attr0(attr0);

  let (x, affine_idx, hflip, vflip, size) = parse_sprite_attr1(attr1);

  let (tile_idx, priority, palette_idx) = parse_sprite_attr2(attr2);

  let (size_x, size_y) = sprite_size(size, shape);
  let size_x = size_x as u32;
  let size_y = size_y as u32;
  let tile_count_x = size_x / 8;

  let stride_1d = gba.fetch_u16(super::gba::DISPCNT_ADDR) & 0x40 != 0;

  let tile_idx_in_page = tile_idx as u32 & (0x4000 - 1);
  let tile_page = TILE_DATA_PAGE_SP0 + if tile_idx & 0x4000 != 0 { 1 } else { 0 };
  let mut out_bytes = alloc::vec![0u16; size_x  as usize * size_y as usize ];
  for tile_x in 0..(size_x / 8) {
    for tile_y in 0..(size_y / 8) {
      let char_stride_y_tiles = if stride_1d { tile_count_x } else { 32 };
      let tile_rgb = get_tile(
        gba,
        tile_page,
        tile_idx_in_page + tile_x + tile_y * char_stride_y_tiles,
        bpp8,
        (palette_idx + 0x10) as u8,
      );
      let stride_y = size_x;
      let stride_x = 8;
      let start_idx = tile_x * stride_x + tile_y * stride_y * 8;
      let start_idx = start_idx as usize;
      for y_within_tile in 0..8 {
        out_bytes[start_idx + y_within_tile * stride_y as usize..][..8]
          .copy_from_slice(&tile_rgb[y_within_tile * 8..][..8]);
      }
    }
  }

  (
    out_bytes,
    y as u32,
    x as u32,
    size_x,
    size_y,
    tile_idx as u32,
    tile_idx as u32 * bytes_per_tile(false),
    palette_idx as u32,
    size,
    shape,
    priority,
  )
}
