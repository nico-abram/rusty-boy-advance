use std::{format, string::String};

use alloc::string::ToString;

/// Utility impl on u8's for some kind of "bit pattern matching".
///
/// Consider performance implications (If any) later.
trait AsBoolSlice {
  fn is_set(self, mask: u8) -> bool;
  fn is_set_n(self, n: u8) -> bool;
  fn as_bools(self) -> [bool; 8];
}

impl AsBoolSlice for u8 {
  #[inline]
  fn is_set(self, mask: u8) -> bool {
    (self & mask) == mask
  }

  #[inline]
  fn is_set_n(self, bit_number: u8) -> bool {
    self.is_set(1u8 << bit_number)
  }

  #[inline]
  fn as_bools(self) -> [bool; 8] {
    [
      self.is_set_n(7),
      self.is_set_n(6),
      self.is_set_n(5),
      self.is_set_n(4),
      self.is_set_n(3),
      self.is_set_n(2),
      self.is_set_n(1),
      self.is_set_n(0),
    ]
  }
}

// bit fiddling

#[inline]
fn as_11th_bit(opcode: u16) -> bool {
  (opcode & 0x0800) != 0
}

/// 11 bit value (0-1023)  
#[inline]
fn as_low_11bits(opcode: u16) -> u16 {
  opcode & 0x07FF
}

/// 3 bit value (0-7)
#[inline]
fn as_bits_8_to_10(opcode: u16) -> usize {
  ((opcode >> 8) & 0x0007) as usize
}

/// 5 bit value (0-31)
#[inline]
fn as_bits_6_to_10(opcode: u16) -> u16 {
  (opcode >> 6) & 0x001F
}

/// 3 bit values (0-7)
/// Since these are often used as register numbers we return them as usizes
#[inline]
#[allow(clippy::identity_op)]
fn as_lower_3bit_values(opcode: u16) -> (usize, usize, usize, usize) {
  (
    ((opcode & 0x0E00) >> 9) as usize,
    ((opcode & 0x01C0) >> 6) as usize,
    ((opcode & 0x0038) >> 3) as usize,
    ((opcode & 0x0007) >> 0) as usize,
  )
}

/// 8 bit value
#[inline]
fn as_low_byte(opcode: u16) -> u8 {
  opcode as u8
}

/// 2 bit value (0-3)
#[inline]
fn as_bits_11_and_12(opcode: u16) -> u16 {
  (opcode >> 11) & 0x3
}

// disasm

pub fn disasm_thumb(opcode: u16) -> String {
  const T: bool = true;
  const F: bool = false;

  let bits15_8 = (opcode >> 8) as u8;

  match bits15_8.as_bools() {
    [F, F, F, T, T, _, _, _] => add_or_sub(opcode),
    [F, F, F, _, _, _, _, _] => move_shifted_register(opcode),
    [F, F, T, _, _, _, _, _] => immediate_operation(opcode),
    [F, T, F, F, F, F, _, _] => alu_operation(opcode),
    [F, T, F, F, F, T, _, _] => high_register_operations_or_bx(opcode),
    [F, T, F, F, T, _, _, _] => pc_relative_load(opcode),
    [F, T, F, T, _, _, F, _] => load_or_store_with_relative_offset(opcode),
    [F, T, F, T, _, _, T, _] => load_or_store_sign_extended_byte_or_halfword(opcode),
    [F, T, T, _, _, _, _, _] => load_or_store_with_immediate_offset(opcode),
    [T, F, F, F, _, _, _, _] => load_or_store_halfword(opcode),
    [T, F, F, T, _, _, _, _] => stack_pointer_relative_load_or_store(opcode),
    [T, F, T, F, _, _, _, _] => load_address(opcode),
    [T, F, T, T, F, F, F, F] => add_or_sub_offset_to_stack_pointer(opcode),
    [T, F, T, T, _, T, F, _] => push_or_pop(opcode),
    [T, T, F, F, _, _, _, _] => multiple_loads_or_stores(opcode),
    [T, T, F, T, T, T, T, T] => software_interrupt(opcode),
    [T, T, F, T, _, _, _, _] => conditional_branch(opcode),
    [T, T, T, F, F, _, _, _] => branch(opcode),
    [T, T, T, T, F, _, _, _] => branch_and_link_or_link_and_exchange_first_opcode(opcode),
    [T, T, T, T, T, _, _, _] => branch_and_link_or_link_and_exchange_second_opcode(opcode),
    _ => "INVALID".into(),
  }
}

/// ADD/SUB
fn add_or_sub(opcode: u16) -> String {
  let (_, rn, rs, rd) = as_lower_3bit_values(opcode);
  let immediate = (opcode & 0x0400) != 0;
  let is_substraction = (opcode & 0x0200) != 0;

  let verb = if is_substraction { "SUB" } else { "ADD" };

  if immediate {
    format!("{verb} R{rd},R{rs},#${rn:02X}")
  } else {
    format!("{verb} R{rd},R{rs},R{rn}")
  }
}

/// MOV ... ASR/LSL/LSR/ASR/RRX
fn move_shifted_register(opcode: u16) -> String {
  let operation = as_bits_11_and_12(opcode);
  let (_, _, rs, rd) = as_lower_3bit_values(opcode);
  let offset = as_bits_6_to_10(opcode);

  let op_s = match operation {
    0 => "LSL",
    1 => "LSR",
    2 => "ASR",
    3 => "UNKNOWN",
    _ => std::unreachable!(), // We're matching on 2 bits
  };

  if offset != 0 {
    format!("{op_s} R{rd}, R{rs}, #{offset}")
  } else {
    format!("MOV R{rd}, R{rs}, RRX")
  }
}

/// Move, compare, add and substract immediate
fn immediate_operation(opcode: u16) -> String {
  let rd = as_bits_8_to_10(opcode);
  let offset = u32::from(as_low_byte(opcode));
  let operation = as_bits_11_and_12(opcode);

  match operation {
    0 => {
      format!("MOV R{rd},#${offset:02X}")
    }
    1 => {
      format!("CMP R{rd},#${offset:02X}")
    }
    2 => {
      format!("ADD R{rd},R{rd},$#{offset:02X}")
    }
    3 => {
      format!("SUB R{rd},R{rd},$#{offset:02X}")
    }
    _ => std::unreachable!(), // It's 2 bits
  }
}

fn alu_operation(_opcode: u16) -> String {
  // TODO
  "ALU?".into()
}
/// HiReg/BX
fn high_register_operations_or_bx(opcode: u16) -> String {
  let (_, _, rs, rd) = as_lower_3bit_values(opcode);
  let rs_most_significant_bit = (opcode & 0x0040) >> 6;
  let rd_most_significant_bit = (opcode & 0x0080) >> 7;
  let operation = (opcode & 0x0300) >> 8;
  let rs = rs + ((rs_most_significant_bit as usize) << 3);
  let rd = rd + ((rd_most_significant_bit as usize) << 3);

  match operation {
    0 => {
      format!("ADD R{rd},R{rs}")
    }
    1 => {
      format!("CMP R{rd},R{rs}")
    }
    2 => {
      format!("MOV R{rd},R{rs}")
    }
    3 => {
      format!("BX R{rs}")
    }
    _ => std::unreachable!(), // It's 2 bits
  }
}

/// LDR PC
fn pc_relative_load(opcode: u16) -> String {
  let rd = as_bits_8_to_10(opcode);
  let byte = u32::from(as_low_byte(opcode));
  let offset = byte << 2; // In steps of 4

  format!("LDR R{rd}, [PC+${offset:06X}]")
}

/// LDR/STR
/// 08000fac
fn load_or_store_with_relative_offset(opcode: u16) -> String {
  let (_, ro, rb, rd) = as_lower_3bit_values(opcode);
  let is_load_else_store = as_11th_bit(opcode);
  let is_byte_else_word = (opcode & 0x0400) != 0;

  let verb = if is_load_else_store { "LDR" } else { "STR" };
  let byte_s = if is_byte_else_word { "B" } else { "" };

  format!("{verb}{byte_s} R{rd}, [R{ro}+R{rb}]")
}

/// LDR/STR H/SB/SH
fn load_or_store_sign_extended_byte_or_halfword(opcode: u16) -> String {
  let is_halfword_else_byte = as_11th_bit(opcode);
  let (_, ro, rb, rd) = as_lower_3bit_values(opcode);
  let sign_extend = (opcode & 0x0400) != 0;

  if !is_halfword_else_byte && !sign_extend {
    format!("STRH R{rd}, [R{rb}+R{ro}]")
  } else {
    let verb =
      if is_halfword_else_byte { if sign_extend { "LDSH" } else { "LDRH" } } else { "LDSB" };
    format!("{verb} R{rd}, [R{rb}+R{ro}]")
  }
}

/// LDR/STR {B}
fn load_or_store_with_immediate_offset(opcode: u16) -> String {
  let is_byte_else_word = (opcode & 0x1000) != 0;
  let is_load_else_store = as_11th_bit(opcode);
  let (_, _, rb, rd) = as_lower_3bit_values(opcode);
  let mut offset = u32::from(as_bits_6_to_10(opcode));
  if !is_byte_else_word {
    offset = offset << 2;
  }

  let verb = if is_load_else_store { "LDR" } else { "STR" };
  let byte_s = if is_byte_else_word { "B" } else { "" };

  format!("{verb}{byte_s} R{rd}, [R{rb}+#${offset:02X}]")
}

/// LDR/STR H
fn load_or_store_halfword(opcode: u16) -> String {
  let is_load_else_store = as_11th_bit(opcode);
  let (_, _, rb, rd) = as_lower_3bit_values(opcode);
  let offset = as_bits_6_to_10(opcode);
  let offset = u32::from(offset) << 1;

  let verb = if is_load_else_store { "LDRH" } else { "STRH" };

  format!("{verb} R{rd}, [R{rb}+#${offset:02X}]")
}

/// LDR/STR SP
/// 80016A0
fn stack_pointer_relative_load_or_store(opcode: u16) -> String {
  let is_load_else_store = as_11th_bit(opcode);
  let byte = as_low_byte(opcode);
  let offset = u32::from(byte) << 2;
  let rd = as_bits_8_to_10(opcode);

  let verb = if is_load_else_store { "LDR" } else { "STR" };

  if offset & 0xFF00 != 0 {
    format!("{verb} R{rd}, [SP, #${offset:04X}]")
  } else {
    format!("{verb} R{rd}, [SP, #${offset:02X}]")
  }
}

/// LOAD PC/SP
fn load_address(opcode: u16) -> String {
  let load_sp_else_pc = as_11th_bit(opcode);
  let byte = as_low_byte(opcode);
  let nn = u32::from(byte) << 2;
  let rd = as_bits_8_to_10(opcode);

  let name = if load_sp_else_pc { "SP" } else { "PC" };
  if nn & 0xFF00 != 0 {
    format!("LDR R{rd}, [{name}, #${nn:04X}]")
  } else {
    format!("LDR R{rd}, [{name}, #${nn:02X}]")
  }
}

/// ADD/SUB SP,nn
fn add_or_sub_offset_to_stack_pointer(opcode: u16) -> String {
  let byte = as_low_byte(opcode);
  let substract_else_add = (byte & 0x80) != 0;
  let byte7 = byte & 0x0000_007F;
  let offset = u32::from(byte7) << 2;

  let verb = if substract_else_add { "SUB" } else { "ADD" };

  if offset & 0xFF00 != 0 {
    format!("{verb} SP,#${offset:04X}")
  } else {
    format!("{verb} SP,#${offset:02X}")
  }
}

fn rlist_to_string(rlist_s: &mut String, mut rlist: u8) {
  use std::fmt::Write;

  while rlist != 0 {
    let lz = rlist.trailing_zeros();
    let n_down = lz;
    let l1 = (rlist >> lz).trailing_ones();
    let n_up = n_down + l1 - 1;
    if n_up != n_down {
      write!(rlist_s, "R{n_down}-R{n_up},").unwrap();
    } else {
      write!(rlist_s, "R{n_down},").unwrap();
    }
    // clear bits processed
    let bits_done = lz + l1;
    rlist = rlist.checked_shr(bits_done).unwrap_or(0).checked_shl(bits_done).unwrap_or(0);
    //rlist = (rlist >> bits_done) << bits_done;
  }
  rlist_s.pop();
}
/// PUSH/POP
fn push_or_pop(opcode: u16) -> String {
  let is_pop_else_push = as_11th_bit(opcode);
  let pc_or_lr_flag = (opcode & 0x0100) != 0; // PUSH LR or POP PC
  let rlist = as_low_byte(opcode);

  let verb = if is_pop_else_push { "POP {" } else { "PUSH {" };

  let mut ret = verb.to_string();
  rlist_to_string(&mut ret, rlist);

  if pc_or_lr_flag {
    if is_pop_else_push {
      ret.push_str(",PC");
    } else {
      ret.push_str(",LR");
    }
  }
  ret.push_str("}");

  ret
}

/// STM/LDM
fn multiple_loads_or_stores(opcode: u16) -> String {
  let rb = as_bits_8_to_10(opcode);
  let is_load_else_store = as_11th_bit(opcode);
  let rlist = as_low_byte(opcode);

  let verb = if is_load_else_store { "LDMIA" } else { "STMIA" };

  let mut rlist_s = String::new();
  rlist_to_string(&mut rlist_s, rlist as u8);

  format!("{verb} R{rb}!, {{{rlist_s}}}")
}

/// SWI
fn software_interrupt(opcode: u16) -> String {
  let comment = opcode & 0xFF;
  format!("SWI #${comment:02X}")
}

fn opcode_to_cond(opcode: u16) -> &'static str {
  match (opcode) as u8 {
    0x0 => "EQ",
    0x1 => "NE",
    0x2 => "CS_HS",
    0x3 => "CC_LO",
    0x4 => "MI",
    0x5 => "PL",
    0x6 => "VS",
    0x7 => "VC",
    0x8 => "HI",
    0x9 => "LS",
    0xA => "GE",
    0xB => "LT",
    0xC => "GT",
    0xD => "LE",
    0xE => "",
    0xF => "NV",
    _ => std::unreachable!(), // We're matching on 4 bits
  }
}
/// B{COND}
fn conditional_branch(opcode: u16) -> String {
  let offset = u32::from(as_low_byte(opcode)) << 1;
  let (is_negative, mut offset) = ((offset & 0x0000_0100) != 0, offset & 0x0000_00FF);
  let cond = (opcode >> 8) & 0x000F;

  let cond_s = opcode_to_cond(cond);
  let sign_s = if is_negative { "-" } else { "+" };
  if is_negative {
    offset = (!offset) & 0x0000_00FE;
  } else {
    offset += 2;
  }
  format!("B{cond_s} {sign_s}#${offset:02X}")
}

/// B
fn branch(opcode: u16) -> String {
  let is_negative = (opcode & 0x0000_0400) != 0; // 11th bit
  let absolute_offset = u32::from(opcode & 0x0000_03FF) << 1; // first 10 bits
  let sign = if is_negative { "-" } else { "+" };

  format!("B {sign}#${absolute_offset:04X}")
}

/// BL/BLX
fn branch_and_link_or_link_and_exchange_first_opcode(opcode: u16) -> String {
  let upper_offset = as_low_11bits(opcode);
  format!("BLL #${upper_offset:04X}")
}

/// BL/BLX
fn branch_and_link_or_link_and_exchange_second_opcode(opcode: u16) -> String {
  let lower_offset = as_low_11bits(opcode);
  format!("BLH #${lower_offset:04X}")
}
