#![allow(non_snake_case)]
#![allow(non_camel_case_types)]
#![allow(dead_code)]

use super::super::{
  cpsr::{borrow_from, carry_from},
  utils::AsBoolSlice,
  gba::GBA,
};

pub type ThumbError = String;
pub type ThumbResult = Result<(), ThumbError>;
pub type ThumbInstruction = fn(&mut GBA, u16) -> ThumbResult;

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
#[inline]
fn sign_flag(N: u32) -> bool {
  (N >> 31) == 1
}

fn move_shifted_register(GBA: &mut GBA, opcode: u16) -> ThumbResult {
  let operation = as_bits_11_and_12(opcode);
  let (_, _, rs, rd) = as_lower_3bit_values(opcode);
  let offset = as_bits_6_to_10(opcode);
  let operand = GBA.regs[rs];
  // TODO: Special 0 shifts
  let res = match operation {
    0 => operand << offset,
    1 => operand >> offset,
    2 => ((operand as i32) >> offset) as u32,
    3 => unimplemented!("reserved"),
    _ => unimplemented!("Impossible?"), //std::hint::unreachable_unchecked()
  };
  GBA.regs[rd] = res;
  GBA.cpsr.set_all_status_flags(
    res,
    if offset == 0 && operation == 0 { None } else { Some((operand >> (32 - offset)) & 1 != 0) },
    None,
  );
  GBA.clocks += 0; // TODO: clocks
  Ok(())
}
/// ADD/SUB
fn add_or_sub(GBA: &mut GBA, opcode: u16) -> ThumbResult {
  let (_, rn, rs, rd) = as_lower_3bit_values(opcode);
  let immediate = (opcode & 0x0400) != 0;
  let is_substraction = (opcode & 0x0200) != 0;
  let first_operand = GBA.regs[rs];
  let second_operand = if immediate { rn as u32 } else { GBA.regs[rn] };
  GBA.regs[rd] = if is_substraction {
    let (result, overflow) = first_operand.overflowing_sub(second_operand);
    //second_operand > first_operand
    GBA.cpsr.set_all_status_flags(
      result,
      Some(borrow_from(first_operand, second_operand)),
      Some(overflow),
    );
    result
  } else {
    let (result, overflow) = first_operand.overflowing_add(second_operand);
    GBA.cpsr.set_all_status_flags(result, Some(overflow), Some(false));
    /*
    GBA.cpsr.set_all_status_flags_for_addition(
      GBA.regs[rd],
      first_operand,
      second_operand,
      Some(overflow),
    );*/
    result
  };
  GBA.clocks += 0; // TODO: clocks
  Ok(())
}
/// Move, compare, add and substract immediate
fn immediate_operation(GBA: &mut GBA, opcode: u16) -> ThumbResult {
  let rd = as_bits_8_to_10(opcode);
  let offset = u32::from(as_low_byte(opcode));
  let operation = as_bits_11_and_12(opcode);
  let rd_val = GBA.regs[rd];
  if operation == 1 {
    // CMP handled separately since it doesnt store the result
    let (res, overflow) = rd_val.overflowing_sub(offset);
    GBA.cpsr.set_all_status_flags(res, Some(offset <= rd_val), Some(overflow));
    GBA.clocks += 0; // TODO: clocks
    return Ok(());
  }
  GBA.regs[rd] = match operation {
    0 => {
      // MOV
      GBA.cpsr.set_all_status_flags(offset, None, None);
      offset
    }
    2 => {
      // ADD
      let (res, overflow) = rd_val.overflowing_add(offset);
      GBA.cpsr.set_all_status_flags_for_addition(res, rd_val, offset, Some(overflow));
      res
    }
    3 => {
      // SUB
      let (res, overflow) = rd_val.overflowing_sub(offset);
      GBA.cpsr.set_all_status_flags(res, Some(offset <= rd_val), Some(overflow));
      res
    }
    _ => unimplemented!(), //std::hint::unreachable_unchecked()
  };
  GBA.clocks += 0; // TODO: clocks
  Ok(())
}
fn alu_operation(GBA: &mut GBA, opcode: u16) -> ThumbResult {
  let (_, _, rs, rd) = as_lower_3bit_values(opcode);
  let operation = ((opcode >> 6) & 0x000F) as u8;
  let (rs_val, rd_val) = (GBA.regs[rs], GBA.regs[rd]);
  match operation {
    0x0 => {
      //AND
      let res = rd_val & rs_val;
      GBA.regs[rd] = res;
      GBA.cpsr.set_all_status_flags(res, None, None);
    }
    0x1 => {
      //EOR/XOR
      let res = rd_val ^ rs_val;
      GBA.regs[rd] = res;
      GBA.cpsr.set_all_status_flags(res, None, None);
    }
    0x2 => {
      //LSL (Logical Shift Left)
      let rs_val = rs_val & 0x00FF;
      let (result, _) = rd_val.overflowing_shl(rs_val);
      GBA.regs[rd] = result;
      GBA.cpsr.set_all_status_flags(
        result,
        if rs_val == 0 { None } else { Some((rd_val >> (32 - rs_val)) & 1 != 0) },
        None,
      );
    }
    0x3 => {
      //LSR (Logical Shift Right)
      let rs_val = rs_val & 0x00FF;
      let result = rd_val >> rs_val;
      GBA.regs[rd] = result;
      GBA.cpsr.set_all_status_flags(
        result,
        if rs_val == 0 { None } else { Some((rd_val >> (rs_val - 1)) & 1 != 0) },
        None,
      );
    }
    0x4 => {
      //ASR (Arithmetic Shift Right)
      let rs_val = rs_val & 0x00FF;
      let result = ((rd_val as i32) << rs_val) as u32;
      GBA.regs[rd] = result;
      GBA.cpsr.set_all_status_flags(
        result,
        if rs_val == 0 { None } else { Some((rd_val >> (rs_val - 1)) & 1 != 0) },
        None,
      );
    }
    0x5 => {
      // ADC (Add With Carry)
      let (result, overflow) = rd_val.overflowing_add(rs_val);
      let (result, overflow2) = result.overflowing_add(GBA.cpsr.C() as u32);
      GBA.regs[rd] = result;
      GBA.cpsr.set_all_status_flags(
        result,
        Some(carry_from(rd_val, rs_val + (GBA.cpsr.C() as u32), result)), // TODO: Account for overflow in rs+C
        Some(overflow || overflow2),
      );
    }
    0x6 => {
      // SBC (Substract With Carry)
      let (result, overflow) = rd_val.overflowing_sub(rs_val);
      let (result, overflow2) = result.overflowing_sub((!GBA.cpsr.C()) as u32);
      GBA.regs[rd] = result;
      GBA.cpsr.set_all_status_flags(
        result,
        Some(borrow_from(rd_val, rs_val.overflowing_add((!GBA.cpsr.C()) as u32).0) || overflow2),
        Some(overflow || overflow2),
      );
    }
    0x7 => {
      // ROR (Rotate Right)
      let rs_val = rs_val & 0x00FF;
      if rs_val == 0 {
        GBA.cpsr.set_neutral_flags(rd_val);
      } else {
        let r4 = rs_val & 0x1F;
        if r4 > 0 {
          let result = rd_val.rotate_right(r4);
          GBA.regs[rd] = result;
          GBA.cpsr.set_all_status_flags(result, Some(((rd_val >> (r4 - 1)) & 1) != 0), None);
        } else {
          GBA.cpsr.set_neutral_flags(rd_val);
          GBA.cpsr.set_C((rd_val >> 31) != 0);
        }
      }
    }
    0x8 => {
      // TST (Test)
      GBA.cpsr.set_all_status_flags(rd_val & rs_val, None, None);
    }
    0x9 => {
      // NEG (Negate)
      let (result, overflow) = 0u32.overflowing_sub(rs_val);
      GBA.regs[rd] = result;
      GBA.cpsr.set_all_status_flags(result, Some(borrow_from(0, rs_val)), Some(overflow));
    }
    0xA => {
      // CMP (Compare)
      let (res, overflow) = rd_val.overflowing_sub(rs_val);
      GBA.cpsr.set_all_status_flags(res, Some(rs_val > rd_val), Some(overflow));
    }
    0xB => {
      // CMN (Compare Negative)
      let (res, overflow) = rd_val.overflowing_add(rs_val);
      GBA.cpsr.set_all_status_flags(res, Some(carry_from(rd_val, rs_val, res)), Some(overflow));
    }
    0xC => {
      // ORR (Logical OR)
      let res = rd_val | rs_val;
      GBA.regs[rd] = res;
      GBA.cpsr.set_all_status_flags(res, None, None);
    }
    0xD => {
      // MUL (Multiply)
      let res = rd_val.overflowing_mul(rs_val).0;
      GBA.regs[rd] = res;
      GBA.cpsr.set_all_status_flags(res, Some(false), None);
    }
    0xE => {
      // BIC (Bit clear)
      let res = rd_val & (!rs_val);
      GBA.regs[rd] = res;
      GBA.cpsr.set_all_status_flags(res, None, None);
    }
    0xF => {
      // MVN (Not)
      let res = !rs_val;
      GBA.regs[rd] = res;
      GBA.cpsr.set_all_status_flags(res, None, None);
    }
    _ => unimplemented!("Impossible"),
  }
  GBA.clocks += 0; // TODO: clocks
  Ok(())
}
/// HiReg/BX
fn high_register_operations_or_bx(GBA: &mut GBA, opcode: u16) -> ThumbResult {
  let (_, _, rs, rd) = as_lower_3bit_values(opcode);
  let rs_most_significant_bit = (opcode & 0x0040) >> 6;
  let rd_most_significant_bit = (opcode & 0x0080) >> 7;
  let operation = (opcode & 0x0300) >> 8;
  let rs = rs + ((rs_most_significant_bit as usize) << 3);
  let rd = rd + ((rd_most_significant_bit as usize) << 3);
  match operation {
    0 => {
      // ADD
      let (res, _) = GBA.regs[rd].overflowing_add(GBA.regs[rs]);
      GBA.regs[rd] = res;
    }
    1 => {
      //CMP
      let (op1, op2) = (GBA.regs[rd], GBA.regs[rs]);
      let (res, overflow) = op1.overflowing_sub(op2);
      // TODO: Is this check right?
      GBA.cpsr.set_all_status_flags(res, Some(borrow_from(op1, op2)), Some(overflow));
    }
    2 => {
      // MOV
      GBA.regs[rd] = if rs == 15 { GBA.regs[15] + 2 } else { GBA.regs[rs] };
    }
    3 => {
      // Ignore BLX since it's ARMv9
      // BX
      // Change to ARM mode if bit 0 is 0
      let rs_val = GBA.regs[rs];
      GBA.cpsr.set_T((rs_val & 0x0000_0001) != 0);
      if rs == 15 {
        GBA.regs[15] = rs_val & 0xFFFF_FFFC;
      } else {
        GBA.regs[15] = rs_val & 0xFFFF_FFFE;
      }
    }
    _ => unimplemented!("Impossible"), //std::hint::unreachable_unchecked()
  }
  GBA.clocks += 0; // TODO: clocks
  Ok(())
}
/// LDR PC
fn pc_relative_load(GBA: &mut GBA, opcode: u16) -> ThumbResult {
  let rd = as_bits_8_to_10(opcode);
  let byte = u32::from(as_low_byte(opcode));
  let offset = byte << 2; // In steps of 4
  GBA.regs[rd] = GBA.fetch_u32(((GBA.regs[15] + 2) & 0xFFFF_FFFC).overflowing_add(offset).0);
  GBA.clocks += 0; // TODO: clocks
  Ok(())
}
/// LDR/STR
fn load_or_store_with_relative_offset(GBA: &mut GBA, opcode: u16) -> ThumbResult {
  let (_, ro, rb, rd) = as_lower_3bit_values(opcode);
  let is_load = as_11th_bit(opcode);
  let is_byte_else_word = (opcode & 0x0400) != 0;
  let (addr, _) = GBA.regs[ro].overflowing_add(GBA.regs[rb]);
  if is_load {
    if is_byte_else_word {
      // TODO: Does this zero extend the 32bit value?
      GBA.write_u8(addr, GBA.regs[rd] as u8);
    } else {
      GBA.write_u32(addr, GBA.regs[rd]);
    }
  } else {
    GBA.regs[rd] =
      if is_byte_else_word { u32::from(GBA.fetch_byte(addr)) } else { GBA.fetch_u32(addr) };
  }
  GBA.clocks += 0; // TODO: clocks
  Ok(())
}
/// LDR/STR H/SB/SH
fn load_or_store_sign_extended_byte_or_halfword(GBA: &mut GBA, opcode: u16) -> ThumbResult {
  println!("ohayo");
  let H = as_11th_bit(opcode);
  let (_, ro, rb, rd) = as_lower_3bit_values(opcode);
  let sign_extend = (opcode & 0x0400) != 0;
  let addr = GBA.regs[rb].overflowing_add(GBA.regs[ro]).0;
  if !H && !sign_extend {
    GBA.write_u16(addr, GBA.regs[rd] as u16);
    GBA.clocks += 0; // TODO: clocks
    return Ok(());
  }
  GBA.regs[rd] = if H {
    // Half-word(16 bits)
    if sign_extend {
      (i32::from(GBA.fetch_u16(addr) as i16) as u32) // Sign extend
    } else {
      u32::from(GBA.fetch_u16(addr))
    }
  } else {
    (i32::from(GBA.fetch_byte(addr) as i8) as u32) // Sign extend
  };
  GBA.clocks += 0; // TODO: clocks
  Ok(())
}
/// LDR/STR {B}
fn load_or_store_with_immediate_offset(GBA: &mut GBA, opcode: u16) -> ThumbResult {
  let is_byte_else_word = (opcode & 0x1000) != 0;
  let is_load_else_store = as_11th_bit(opcode);
  let (_, _, rb, rd) = as_lower_3bit_values(opcode);
  let offset = u32::from(as_bits_6_to_10(opcode));
  let rb_val = GBA.regs[rb];
  if is_byte_else_word {
    let addr = offset + rb_val;
    if is_load_else_store {
      GBA.regs[rd] = u32::from(GBA.fetch_byte(addr));
    } else {
      GBA.write_u8(addr, GBA.regs[rd] as u8);
    }
  } else {
    let offset = offset << 2;
    let addr = offset + rb_val;
    if is_load_else_store {
      GBA.regs[rd] = GBA.fetch_u32(addr);
    } else {
      GBA.write_u32(addr, GBA.regs[rd]);
    }
  }
  GBA.clocks += 0; // TODO: clocks
  Ok(())
}
/// LDR/STR H
fn load_or_store_halfword(GBA: &mut GBA, opcode: u16) -> ThumbResult {
  let is_load_else_store = as_11th_bit(opcode);
  let (_, _, rb, rd) = as_lower_3bit_values(opcode);
  let offset = as_bits_6_to_10(opcode);
  let offset = u32::from(offset) << 1;
  let addr = offset + GBA.regs[rb];
  if is_load_else_store {
    GBA.regs[rd] = u32::from(GBA.fetch_u16(addr));
    GBA.write_u16(addr, GBA.regs[rd] as u16);
  }
  GBA.clocks += 0; // TODO: clocks
  Ok(())
}
/// LDR/STR SP
fn stack_pointer_relative_load_or_store(GBA: &mut GBA, opcode: u16) -> ThumbResult {
  let is_load_else_store = as_11th_bit(opcode);
  let byte = as_low_byte(opcode);
  let offset = u32::from(byte) << 2;
  let rd = as_bits_8_to_10(opcode);
  let addr = GBA.regs[13] + offset;
  if is_load_else_store {
    GBA.regs[rd] = GBA.fetch_u32(addr);
  } else {
    GBA.write_u32(addr, GBA.regs[rd]);
  }
  GBA.clocks += 0; // TODO: clocks
  Ok(())
}
/// LOAD PC/SP
fn load_address(GBA: &mut GBA, opcode: u16) -> ThumbResult {
  let load_sp_else_pc = as_11th_bit(opcode);
  let byte = as_low_byte(opcode);
  let nn = u32::from(byte) << 2;
  let rd = as_bits_8_to_10(opcode);
  GBA.regs[rd] = if load_sp_else_pc {
    GBA.regs[13] + nn
  } else {
    //TODO: Is this right????
    ((GBA.regs[15] + 4) & !2) + nn
  };
  GBA.clocks += 0; // TODO: clocks
  Ok(())
}
/// ADD/SUB SP,nn
fn add_or_sub_offset_to_stack_pointer(GBA: &mut GBA, opcode: u16) -> ThumbResult {
  let byte = as_low_byte(opcode);
  let substract_else_add = (byte & 0x80) != 0;
  let byte7 = byte & 0x0000_007F;
  let offset = u32::from(byte7) << 2;
  let sp = GBA.regs[13];
  GBA.regs[13] =
    if substract_else_add { sp.overflowing_sub(offset).0 } else { sp.overflowing_add(offset).0 };
  GBA.clocks += 0; // TODO: clocks
  Ok(())
}
/// PUSH/POP
fn push_or_pop(GBA: &mut GBA, opcode: u16) -> ThumbResult {
  let is_pop_else_push = as_11th_bit(opcode);
  // PUSH LR or POP PC
  let pc_or_lr_flag = (opcode & 0x0100) != 0;
  let rlist = as_low_byte(opcode);
  let mut sp = GBA.regs[13];
  if is_pop_else_push {
    // TODO: Does this need reversing (So unreversed)? (After the enumerate)
    for (idx, _) in rlist.as_bools().iter().enumerate().filter(|(_, &boolean)| boolean).rev() {
      sp += 4;
      //println!("pop {:x}:{:x}({})", sp, GBA.fetch_u32(sp), 7 - idx);
      GBA.regs[7 - idx] = GBA.fetch_u32(sp);
    }
    if pc_or_lr_flag {
      sp += 4;
      //println!("OWO");
      //println!("pop {:x}:{:x}({})", sp, GBA.fetch_u32(sp), 15);
      GBA.regs[15] = GBA.fetch_u32(sp) & (!0x0000_0001); // POP PC
    }
  } else {
    if pc_or_lr_flag {
      //println!("push {:x}:{:x}({})", sp, GBA.regs[14], 14);
      GBA.write_u32(sp, GBA.regs[14]); // PUSH LR
      sp -= 4;
    }
    // TODO: Does this need reversing? (After the enumerate)
    for (idx, _) in rlist.as_bools().iter().enumerate().filter(|(_, &boolean)| boolean) {
      //println!("push {:x}:{:x}({})", sp, GBA.regs[7 - idx], 7 - idx);
      GBA.write_u32(sp, GBA.regs[7 - idx]);
      sp -= 4;
    }
  }
  GBA.regs[13] = sp;
  GBA.clocks += 0; // TODO: clocks
  Ok(())
}
/// STM/LDM
fn multiple_loads_or_stores(GBA: &mut GBA, opcode: u16) -> ThumbResult {
  let rb = as_bits_8_to_10(opcode);
  let is_load_else_store = as_11th_bit(opcode);
  let rlist = as_low_byte(opcode);
  let mut addr = GBA.regs[rb];
  // TODO: Should this be reversed?
  if is_load_else_store {
    for (idx, _) in rlist.as_bools().iter().enumerate().filter(|(_, &boolean)| boolean) {
      GBA.write_u32(addr, GBA.regs[7 - idx]);
      addr += 4;
    }
  } else {
    for (idx, _) in rlist.as_bools().iter().enumerate().filter(|(_, &boolean)| boolean) {
      GBA.regs[7 - idx] = GBA.fetch_u32(addr);
      addr += 4;
    }
  }
  GBA.clocks += 0; // TODO: clocks
  Ok(())
}
/// B{COND}
fn conditional_branch(GBA: &mut GBA, opcode: u16) -> ThumbResult {
  let offset = u32::from(as_low_byte(opcode)) << 1;
  let (is_negative, offset) = ((offset * 0x0000_0100) != 0, offset & 0x0000_00FE);
  let cond = (opcode >> 8) & 0x000F;
  let should_not_jump = super::arm::check_cond(GBA, u32::from(cond) << 28);
  if should_not_jump {
    GBA.clocks += 0; // TODO: clocks
    return Ok(());
  }
  GBA.regs[15] = if is_negative {
    GBA.regs[15].overflowing_sub((!offset) & 0x0000_00FE).0
  } else {
    GBA.regs[15].overflowing_add(offset + 2).0
  };
  GBA.clocks += 0; // TODO: clocks
  Ok(())
}
/// SWI
fn software_interrupt(GBA: &mut GBA, opcode: u16) -> ThumbResult {
  super::arm::SWI(GBA, u32::from(opcode) << 16)?;
  GBA.clocks += 0; // TODO: clocks
  Ok(())
}
/// B
fn branch(GBA: &mut GBA, opcode: u16) -> ThumbResult {
  //let offset = (opcode & 0x01FF) as u32;
  let offset = u32::from(as_low_11bits(opcode)) << 1;
  let is_negative = (opcode & 0x0200) != 0;
  let pc = (*GBA.pc()).overflowing_add(2).0;
  *GBA.pc() = if is_negative { pc.overflowing_sub(offset).0 } else { pc.overflowing_add(offset).0 };
  GBA.clocks += 0; // TODO: clocks
  Ok(())
}
/// BL/BLX
///
/// This is actually a 32 bit instruction (2 thumb instructions).
///
/// The first one has the upper 11 bits and the second the lower 11
fn branch_and_link_or_link_and_exchange_first_opcode(GBA: &mut GBA, opcode: u16) -> ThumbResult {
  let upper_offset = (i32::from((as_low_11bits(opcode) as i16) << 5) << 7) as u32;
  let pc = GBA.regs[15];
  GBA.regs[14] = pc.overflowing_add(upper_offset).0.overflowing_add(2).0;
  GBA.clocks += 0; // TODO: clocks
  Ok(())
}
fn branch_and_link_or_link_and_exchange_second_opcode(GBA: &mut GBA, opcode: u16) -> ThumbResult {
  //let H = as_11th_bit(opcode);// I *think* this is not needed since it's ARM9 (BLX)
  let lower_offset = u32::from(as_low_11bits(opcode) << 1);
  let pc = GBA.regs[15] + 2;
  GBA.regs[15] = GBA.regs[14].overflowing_add(lower_offset).0;
  GBA.regs[14] = pc - 1;
  GBA.clocks += 0; // TODO: clocks
  Ok(())
}
fn decode_thumb(opcode: u16) -> Result<ThumbInstruction, ThumbError> {
  let bits15_8 = (opcode >> 8) as u8;
  const T: bool = true;
  const F: bool = false;
  Ok(match bits15_8.as_bools() {
    [F, F, F, T, T, _, _, _] => add_or_sub, /* TODO: The ARM7TDMI manual says 0b000111 but GBATEK 0x00011 */
    [F, F, F, _, _, _, _, _] => move_shifted_register,
    [F, F, T, _, _, _, _, _] => immediate_operation,
    [F, T, F, F, F, F, _, _] => alu_operation,
    [F, T, F, F, F, T, _, _] => high_register_operations_or_bx,
    [F, T, F, F, T, _, _, _] => pc_relative_load,
    [F, T, F, T, _, _, F, _] => load_or_store_with_relative_offset,
    [F, T, F, T, _, _, T, _] => load_or_store_sign_extended_byte_or_halfword,
    [F, T, T, _, _, _, _, _] => load_or_store_with_immediate_offset,
    [T, F, F, F, _, _, _, _] => load_or_store_halfword,
    [T, F, F, T, _, _, _, _] => stack_pointer_relative_load_or_store,
    [T, F, T, F, _, _, _, _] => load_address,
    [T, F, T, T, F, F, F, F] => add_or_sub_offset_to_stack_pointer,
    [T, F, T, T, _, T, F, _] => push_or_pop,
    [T, T, F, F, _, _, _, _] => multiple_loads_or_stores,
    [T, T, F, T, T, T, T, T] => software_interrupt,
    [T, T, F, T, _, _, _, _] => conditional_branch,
    [T, T, T, F, F, _, _, _] => branch,
    [T, T, T, T, F, _, _, _] => branch_and_link_or_link_and_exchange_first_opcode,
    [T, T, T, T, T, _, _, _] => branch_and_link_or_link_and_exchange_second_opcode,
    _ => return Err(format!("Invalid thumb opcode: {:x}", opcode)),
  })
}

pub(crate) fn execute_one_instruction(GBA: &mut GBA) -> ThumbResult {
  let pc = *GBA.pc();
  let opcode = GBA.fetch_u16(pc);
  *GBA.pc() += 2;
  (GBA.instruction_hook_with_opcode)(GBA, u32::from(opcode));
  let instruction = decode_thumb(opcode)?;
  instruction(GBA, opcode)?;
  Ok(())
}

#[cfg(test)]
mod tests {
  use super::{AsBoolSlice, *};

  extern crate test;
  use test::Bencher;
  fn none(_: &mut GBA, _: u16) -> ThumbResult {
    Ok(())
  }
  fn decode_thumb(opcode: u16) -> ThumbInstruction {
    let bits15_8 = (opcode >> 8) as u8;
    const T: bool = true;
    const F: bool = false;
    match bits15_8.as_bools() {
      [F, F, F, T, T, T, _, _] => add_or_sub,
      [F, F, F, _, _, _, _, _] => move_shifted_register,
      [F, F, T, _, _, _, _, _] => immediate_operation,
      [F, T, F, F, F, F, _, _] => alu_operation,
      [F, T, F, F, F, T, _, _] => high_register_operations_or_bx,
      [F, T, F, F, T, _, _, _] => pc_relative_load,
      [F, T, F, T, _, _, F, _] => load_or_store_with_relative_offset,
      [F, T, F, T, _, _, T, _] => load_or_store_sign_extended_byte_or_halfword,
      [F, T, T, _, _, _, _, _] => load_or_store_with_immediate_offset,
      [T, F, F, F, _, _, _, _] => load_or_store_halfword,
      [T, F, F, T, _, _, _, _] => stack_pointer_relative_load_or_store,
      [T, F, T, F, _, _, _, _] => load_address,
      [T, F, T, T, F, F, F, F] => add_or_sub_offset_to_stack_pointer,
      [T, F, T, T, _, T, F, _] => push_or_pop,
      [T, T, F, F, _, _, _, _] => multiple_loads_or_stores,
      [T, T, F, T, T, T, T, T] => software_interrupt,
      [T, T, F, T, _, _, _, _] => conditional_branch,
      [T, T, T, F, F, _, _, _] => branch,
      [T, T, T, T, _, _, _, _] => branch_and_link_or_link_and_exchange_first_opcode,
      _ => none,
    }
  }
  #[allow(clippy::identity_op)]
  fn decode_thumb_raw(opcode: u16) -> ThumbInstruction {
    let bits15_8 = (opcode >> 8) as u8;
    const T: bool = true;
    const F: bool = false;
    match bits15_8 {
      x if (x & 0b1111_1100) == 0b0001_1100 => add_or_sub,
      x if (x & 0b1110_0000) == 0b0000_0000 => move_shifted_register,
      x if (x & 0b1110_0000) == 0b0010_0000 => immediate_operation,
      x if (x & 0b1111_1100) == 0b0100_0000 => alu_operation,
      x if (x & 0b1111_1100) == 0b0100_0100 => high_register_operations_or_bx,
      x if (x & 0b1111_1000) == 0b0100_1000 => pc_relative_load,
      x if (x & 0b1111_0010) == 0b0101_0000 => load_or_store_with_relative_offset,
      x if (x & 0b1111_0010) == 0b0101_0010 => load_or_store_sign_extended_byte_or_halfword,
      x if (x & 0b1110_0000) == 0b0110_0000 => load_or_store_with_immediate_offset,
      x if (x & 0b1111_0000) == 0b1000_0000 => load_or_store_halfword,
      x if (x & 0b1111_0000) == 0b1001_0000 => stack_pointer_relative_load_or_store,
      x if (x & 0b1111_0000) == 0b1010_0000 => load_address,
      x if (x & 0b1111_1111) == 0b1011_0000 => add_or_sub_offset_to_stack_pointer,
      x if (x & 0b1011_0110) == 0b1011_0100 => push_or_pop,
      x if (x & 0b1111_0000) == 0b1100_0000 => multiple_loads_or_stores,
      x if (x & 0b1111_1111) == 0b0110_1111 => software_interrupt,
      x if (x & 0b1111_0000) == 0b1101_0000 => conditional_branch,
      x if (x & 0b1111_1000) == 0b1110_0000 => branch,
      x if (x & 0b1111_0000) == 0b1111_0000 => branch_and_link_or_link_and_exchange_first_opcode,
      _ => none,
    }
  }
  #[bench]
  fn bench_decode_thumb(b: &mut Bencher) {
    b.iter(|| {
      test::black_box((0..100_000u32).fold(0u32, |old, x| {
        test::black_box(old);
        test::black_box(decode_thumb(x as u16));
        0u32
      }))
    });
  }
  #[bench]
  fn bench_decode_thumb_raw(b: &mut Bencher) {
    b.iter(|| {
      test::black_box((0..100_000u32).fold(0u32, |old, x| {
        test::black_box(old);
        test::black_box(decode_thumb_raw(x as u16));
        0u32
      }))
    });
  }
}
