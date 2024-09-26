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
    format!("{verb} R{rs}, #${rn:02X}")
  } else {
    format!("{verb} R{rs}, R{rn}")
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
  "IMM_OP?".into()
}

fn alu_operation(opcode: u16) -> String {
  "ALU?".into()
}
/// HiReg/BX
fn high_register_operations_or_bx(opcode: u16) -> String {
  "BX?".into()
}

/// LDR PC
fn pc_relative_load(opcode: u16) -> String {
  "LDR PC?".into()
}

/// LDR/STR
fn load_or_store_with_relative_offset(opcode: u16) -> String {
  "LDR/STR?".into()
}

/// LDR/STR H/SB/SH
fn load_or_store_sign_extended_byte_or_halfword(opcode: u16) -> String {
  "LDR/STR H/SB/SH?".into()
}

/// LDR/STR {B}
fn load_or_store_with_immediate_offset(opcode: u16) -> String {
  "LDR/STR {B}?".into()
}

/// LDR/STR H
fn load_or_store_halfword(opcode: u16) -> String {
  "LDR/STR H?".into()
}

/// LDR/STR SP
fn stack_pointer_relative_load_or_store(opcode: u16) -> String {
  "LDR/STR SP?".into()
}

/// LOAD PC/SP
fn load_address(opcode: u16) -> String {
  "LOAD PC/SP?".into()
}

/// ADD/SUB SP,nn
fn add_or_sub_offset_to_stack_pointer(opcode: u16) -> String {
  "ADD/SUB SP,nn?".into()
}

/// PUSH/POP
fn push_or_pop(opcode: u16) -> String {
  "PUSH/POP?".into()
}

/// STM/LDM
fn multiple_loads_or_stores(opcode: u16) -> String {
  "STM/LDM?".into()
}

/// SWI
fn software_interrupt(opcode: u16) -> String {
  "SWI".into()
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
  format!("B{cond_s} {sign_s}#${offset:04X}")
}

/// B
fn branch(opcode: u16) -> String {
  "B?".into()
}

/// BL/BLX
fn branch_and_link_or_link_and_exchange_first_opcode(opcode: u16) -> String {
  "BL/BLX?".into()
}

/// BL/BLX
fn branch_and_link_or_link_and_exchange_second_opcode(opcode: u16) -> String {
  "BL/BLX?".into()
}
