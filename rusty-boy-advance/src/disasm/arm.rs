use std::{format, string::String};

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
fn opcode_to_cond(opcode: u32) -> &'static str {
  match (opcode >> 28) as u8 {
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

/// Get the lower 5 nibbles (2.5 bytes) as bytes
#[inline]
#[allow(clippy::identity_op)]
fn as_u8_nibbles(opcode: u32) -> (u8, u8, u8, u8, u8) {
  (
    ((opcode & 0x000F_0000) >> 16) as u8,
    ((opcode & 0x0000_F000) >> 12) as u8,
    ((opcode & 0x0000_0F00) >> 8) as u8,
    ((opcode & 0x0000_00F0) >> 4) as u8,
    ((opcode & 0x0000_000F) >> 0) as u8,
  )
}

/// Get the lower 5 nibbles (2.5 bytes) as usizes
#[inline]
fn as_usize_nibbles(opcode: u32) -> (usize, usize, usize, usize, usize) {
  let (b0, b1, b2, b3, b4) = as_u8_nibbles(opcode);
  (b0 as usize, b1 as usize, b2 as usize, b3 as usize, b4 as usize)
}

/// Get bits 24-20 as bools
#[inline]
fn as_flags(opcode: u32) -> (bool, bool, bool, bool, bool) {
  (
    (opcode & 0x0100_0000) != 0,
    (opcode & 0x0080_0000) != 0,
    (opcode & 0x0040_0000) != 0,
    (opcode & 0x0020_0000) != 0,
    (opcode & 0x0010_0000) != 0,
  )
}

/// Get bit 25 as bool
#[inline]
fn as_extra_flag(opcode: u32) -> bool {
  (opcode & 0x0200_0000) != 0
}

// disasm:
pub fn disasm_arm(opcode: u32) -> String {
  const T: bool = true;
  const F: bool = false;

  let bits27_20 = (opcode >> 20) as u8;
  let bits11_4 = (opcode >> 4) as u8;

  match (bits27_20.as_bools(), bits11_4.as_bools()) {
    ([F, T, T, _, _, _, _, _], [_, _, _, _, _, _, _, T]) => "UNDEF".into(),
    ([F, T, _, _, _, _, _, _], _) => single_data_transfer(opcode),
    ([F, F, F, F, F, F, _, _], [_, _, _, _, T, F, F, T]) => multiply(opcode),
    ([F, F, F, F, T, _, _, _], [_, _, _, _, T, F, F, T]) => multiply_long(opcode),
    ([F, F, F, T, F, _, F, F], [F, F, F, F, T, F, F, T]) => single_data_swap(opcode),
    ([F, F, F, _, _, F, _, _], [F, F, F, F, T, _, _, T]) => {
      // register
      halfword_data_transfer_immediate_or_register_offset(opcode)
    }
    ([F, F, F, _, _, T, _, _], [_, _, _, _, T, _, _, T]) => {
      // imm
      halfword_data_transfer_immediate_or_register_offset(opcode)
    }
    ([T, F, F, _, _, _, _, _], _) => block_data_transfer(opcode),
    ([T, T, F, _, _, _, _, _], _) => coprocessor_data_transfer(opcode),
    ([T, T, T, F, _, _, _, _], [_, _, _, F, _, _, _, _]) => coprocessor_data_operation(opcode),
    ([T, T, T, F, _, _, _, _], [_, _, _, T, _, _, _, _]) => coprocessor_register_transfer(opcode),
    ([T, T, T, T, _, _, _, _], _) => software_interrupt(opcode),
    ([T, F, T, _, _, _, _, _], _) => branch_or_branch_and_link(opcode),
    ([F, F, F, T, F, F, T, F], [T, T, T, T, F, F, F, T])
      if (opcode & 0x000F_F000) == 0x000F_F000 =>
    {
      branch_and_exchange(opcode)
    }
    ([F, F, _, T, F, _, F, F], [F, F, F, F, F, F, F, F]) => {
      move_to_register_from_status_register(opcode)
    }
    ([F, F, _, T, F, _, T, F], _) => move_to_status_register(opcode),
    ([F, F, _, _, _, _, _, _], _) => alu_operation(opcode),
  }
}

/// Multiply (MUL)
fn multiply(opcode: u32) -> String {
  let spred = opcode_to_cond(opcode);

  let (_, _, _, accumulate, set_cond_flags) = as_flags(opcode);
  let (rd, rn, rs, _, rm) = as_usize_nibbles(opcode);

  let verb = if accumulate { "MLA" } else { "MUL" };
  let scond = if set_cond_flags { "S" } else { "" };

  if accumulate {
    format!("{verb}{scond}{spred} R{rd},R{rm},R{rs},R{rn}")
  } else {
    format!("{verb}{scond}{spred} R{rd},R{rm},R{rs}")
  }
}
/// Multiply long (MULL)
fn multiply_long(opcode: u32) -> String {
  let spred = opcode_to_cond(opcode);

  let (is_unsupported, _store_double_word_result, signed, accumulate, set_cond_flags) =
    as_flags(opcode);
  let (rdhigh_aka_rd, rdlow_aka_rn, rs, _, rm) = as_usize_nibbles(opcode);

  if is_unsupported {
    return "UNDEX SML".into();
  }

  let verb = if signed {
    if accumulate { "SMLAL" } else { "SMULL" }
  } else {
    if accumulate { "UMLAL" } else { "UMULL" }
  };
  let scond = if set_cond_flags { "S" } else { "" };

  return format!("{verb}{scond}{spred} R{rdlow_aka_rn},R{rdhigh_aka_rd},R{rm},R{rs}");
}

/// Single Data Transfer (SDT)
fn single_data_transfer(opcode: u32) -> String {
  let spred = opcode_to_cond(opcode);

  let shifted_register = as_extra_flag(opcode);
  let (_is_pre_offseted, is_up, is_byte_size, bit_21, is_load) = as_flags(opcode);
  let (rn, rd, _third_byte, _second_byte, _first_byte) = as_usize_nibbles(opcode);

  let verb = if is_load { "LDR" } else { "STR" };
  let size_s = if is_byte_size { "B" } else { "" };
  let sign_s = if is_up { "+" } else { "-" };
  let writeback_s = if bit_21 { "!" } else { "" };

  if !shifted_register {
    //TODO: pre/post offset?
    let offset = opcode & 0x0000_0FFF;
    format!("{verb}{size_s}{spred}{writeback_s} R{rd}, [R{rn}, {sign_s}#${offset:04X}]")
  } else {
    format!("SDT shift?")
  }
}

/// Single data swap (SWP)
fn single_data_swap(opcode: u32) -> String {
  let spred = opcode_to_cond(opcode);

  let (_, _, is_byte_else_word, _, _) = as_flags(opcode);
  let (rn, rd, _, _, rm) = as_usize_nibbles(opcode);

  let size_s = if is_byte_else_word { "B" } else { "" };
  format!("SWP{size_s}{spred} R{rd},R{rm},[R{rn}]")
}

/// Halfword Data Transfer Register Offset (HDT_RO or HDT_IO)
fn halfword_data_transfer_immediate_or_register_offset(opcode: u32) -> String {
  let _spred = opcode_to_cond(opcode);

  "HDT?".into()
}

/// Block Data Transfer (BDT)
fn block_data_transfer(_opcode: u32) -> String {
  "BDT?".into()
}

/// Coprocessor Data Transfer (CDT) (Unimplemented)
fn coprocessor_data_transfer(_opcode: u32) -> String {
  "CDT?".into()
}

/// Coprocessor Data Operation (CDO) (Unimplemented)
fn coprocessor_data_operation(_opcode: u32) -> String {
  "CDO?".into()
}

/// Coprocessor Register Transfer (CRT) (Unimplemented)
fn coprocessor_register_transfer(_opcode: u32) -> String {
  "CRT?".into()
}

/// Single data swap (SWP)
fn software_interrupt(_opcode: u32) -> String {
  "SWI".into()
}

/// Branch or Branch and link (B or BL)
fn branch_or_branch_and_link(opcode: u32) -> String {
  let spred = opcode_to_cond(opcode);

  let offset = (opcode & 0x007F_FFFF) << 2; // * 4
  let is_positive = (opcode & 0x0080_0000) == 0;
  let is_branch_and_link = (opcode & 0x0100_0000) != 0;

  let verb = if is_branch_and_link { "BL" } else { "B" };
  let sign = if is_positive { "+" } else { "-" };

  return format!("{verb}{spred} {sign}{offset}");
}

/// Branch and Exchange (BX)
fn branch_and_exchange(opcode: u32) -> String {
  let spred = opcode_to_cond(opcode);

  let reg_n = opcode & 0x0000_000F;
  format!("BX{spred} R{reg_n}")
}

/// Move to register from status register (MRS)
fn move_to_register_from_status_register(opcode: u32) -> String {
  let _spred = opcode_to_cond(opcode);

  let (_, _, use_spsr, _, _) = as_flags(opcode);
  let reg = ((opcode & 0x0000_F000) >> 12) as usize;
  let status_reg = if use_spsr { "SPSR" } else { "CPSR" };

  format!("MRS R{reg}, {status_reg}")
}

/// Move to Status Register (MSR)
fn move_to_status_register(opcode: u32) -> String {
  let _spred = opcode_to_cond(opcode);

  "MSR?".into()
}

/// Single data swap (SWP)
fn alu_operation(opcode: u32) -> String {
  let spred = opcode_to_cond(opcode);

  let immediate = as_extra_flag(opcode);
  let (rn_num, rd, _, _, _) = as_usize_nibbles(opcode);
  let (_, _, third_byte, second_byte, lowest_byte) = as_u8_nibbles(opcode);
  let set_cond_flags = (opcode & 0x0010_0000) != 0;

  let operation = ((opcode & 0x01E0_0000) >> 21) as u8;

  let verb = match operation as u8 {
    0 => "AND",
    1 => "EOR",
    2 => "SUB",
    3 => "RSB",
    4 => "ADD",
    5 => "ADC",
    6 => "SBC",
    7 => "RSC",
    8 => "TST",
    9 => "TEQ",
    10 => "CMP",
    11 => "CMN",
    12 => "ORR",
    13 => "MOV",
    14 => "BIC",
    15 => "MVN",
    _ => unimplemented!("Impossible. We AND'ed 4 bits, there are 16 posibilities"),
  };
  let scond = if set_cond_flags { "S" } else { "" };

  if immediate {
    let ror_shift = u32::from(third_byte) << 1;
    let mut val = u32::from(lowest_byte + second_byte * 16);

    val = val.rotate_right(ror_shift);
    if operation == 13 {
      format!("{verb}{scond}{spred} R{rd},  #${val:04X}")
    } else {
      format!("{verb}{scond}{spred} R{rd}, R{rn_num}, #${val:04X}")
    }
  } else {
    if operation == 13 {
      format!("{verb}{scond}{spred} R{rd}, R{lowest_byte}")
    } else {
      format!("{verb}{scond}{spred} R{rd}, R{rn_num}, R{lowest_byte}")
    }
  }
}
