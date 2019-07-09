#![allow(non_snake_case)]
#![allow(non_camel_case_types)]
#![allow(dead_code)]

use super::super::{
  cpsr::{borrow_from, carry_from},
  gba::GBA,
  utils::AsBoolSlice,
};

use alloc::{format, string::String};

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

fn move_shifted_register(gba: &mut GBA, opcode: u16) -> ThumbResult {
  gba.debug_print_fn.map(|f| f("move_shifted_register\n"));
  let operation = as_bits_11_and_12(opcode);
  let (_, _, rs, rd) = as_lower_3bit_values(opcode);
  let offset = as_bits_6_to_10(opcode);
  let operand = gba.regs[rs];
  gba.debug_print_fn.map(|f| f(format!("rs:{}\n", rs).as_str()));
  gba.debug_print_fn.map(|f| f(format!("rd:{}\n", rd).as_str()));
  gba.debug_print_fn.map(|f| f(format!("operand:{:x}\n", operand).as_str()));
  gba.debug_print_fn.map(|f| f(format!("offset:{:x}\n", offset).as_str()));
  gba.debug_print_fn.map(|f| f(format!("operation:{}\n", operation).as_str()));
  // TODO: Special 0 shifts
  let res = match operation {
    0 => operand << offset,
    1 => operand >> offset,
    2 => ((operand as i32) >> offset) as u32,
    3 => unimplemented!("reserved"),
    _ => unreachable!(), // We're matching on 2 bits
  };
  gba.regs[rd] = res;
  gba.cpsr.set_all_status_flags(
    res,
    if offset == 0 && operation == 0 {
      None
    } else {
      Some(match operation {
        0 => ((operand >> (32 - offset)) & 1) != 0,
        1 | 2 => ((operand >> (offset - 1)) & 1) != 0,
        _ => unreachable!(), // We crashed in the match above if we arent 0,1 or 2 already
      })
    },
    None,
  );
  gba.clocks += 0; // TODO: clocks
  Ok(())
}
/// ADD/SUB
fn add_or_sub(gba: &mut GBA, opcode: u16) -> ThumbResult {
  let (_, rn, rs, rd) = as_lower_3bit_values(opcode);
  let immediate = (opcode & 0x0400) != 0;
  let is_substraction = (opcode & 0x0200) != 0;
  let first_operand = gba.regs[rs];
  let second_operand = if immediate { rn as u32 } else { gba.regs[rn] };
  gba.regs[rd] = if is_substraction {
    let (result, _overflow) = first_operand.overflowing_sub(second_operand);
    //second_operand > first_operand
    gba.cpsr.set_all_status_flags(
      result,
      Some(borrow_from(first_operand, second_operand)),
      Some(
        ((first_operand ^ second_operand) as i32) < 0 && ((second_operand ^ result) as i32) >= 0,
      ),
    );
    result
  } else {
    let (result, overflow) = first_operand.overflowing_add(second_operand);
    gba.cpsr.set_all_status_flags(result, Some(overflow), Some(false));
    /*
    gba.cpsr.set_all_status_flags_for_addition(
      gba.regs[rd],
      first_operand,
      second_operand,
      Some(overflow),
    );*/
    result
  };
  gba.clocks += 0; // TODO: clocks
  Ok(())
}
/// Move, compare, add and substract immediate
fn immediate_operation(gba: &mut GBA, opcode: u16) -> ThumbResult {
  let rd = as_bits_8_to_10(opcode);
  let offset = u32::from(as_low_byte(opcode));
  let operation = as_bits_11_and_12(opcode);
  let rd_val = gba.regs[rd];
  if operation == 1 {
    // CMP handled separately since it doesnt store the result
    let (res, _overflow) = rd_val.overflowing_sub(offset);
    gba.cpsr.set_all_status_flags(
      res,
      Some(offset <= rd_val),
      Some(((rd_val ^ offset) as i32) < 0 && ((offset ^ res) as i32) >= 0),
    );
    gba.clocks += 0; // TODO: clocks
    return Ok(());
  }
  gba.regs[rd] = match operation {
    0 => {
      // MOV
      gba.cpsr.set_all_status_flags(offset, None, None);
      offset
    }
    2 => {
      // ADD
      let (res, overflow) = rd_val.overflowing_add(offset);
      gba.cpsr.set_all_status_flags_for_addition(res, rd_val, offset, Some(overflow));
      res
    }
    3 => {
      // SUB
      let (res, overflow) = rd_val.overflowing_sub(offset);
      gba.cpsr.set_all_status_flags(res, Some(offset <= rd_val), Some(overflow));
      res
    }
    _ => unreachable!(), // It's 2 bits
  };
  gba.clocks += 0; // TODO: clocks
  Ok(())
}
fn alu_operation(gba: &mut GBA, opcode: u16) -> ThumbResult {
  let (_, _, rs, rd) = as_lower_3bit_values(opcode);
  let operation = ((opcode >> 6) & 0x000F) as u8;
  let (rs_val, rd_val) = (gba.regs[rs], gba.regs[rd]);
  match operation {
    0x0 => {
      //AND
      let res = rd_val & rs_val;
      gba.regs[rd] = res;
      gba.cpsr.set_all_status_flags(res, None, None);
    }
    0x1 => {
      //EOR/XOR
      let res = rd_val ^ rs_val;
      gba.regs[rd] = res;
      gba.cpsr.set_all_status_flags(res, None, None);
    }
    0x2 => {
      //LSL (Logical Shift Left)
      gba.debug_print_fn.map(|f| f("LSL\n"));
      let rs_val = rs_val & 0x00FF;
      let (result, _) = rd_val.overflowing_shl(rs_val);
      gba.regs[rd] = result;
      gba.debug_print_fn.map(|f| f(format!("result:{:x}\n", result).as_str()));
      gba.debug_print_fn.map(|f| f(format!("rs_val:{:x}\n", rs_val).as_str()));
      gba.debug_print_fn.map(|f| f(format!("rd_val:{:x}\n", rd_val).as_str()));
      gba
        .debug_print_fn
        .map(|f| f(format!("carry:{}\n", ((rd_val >> (32 - rs_val)) & 1) != 0).as_str()));
      gba.cpsr.set_all_status_flags(
        result,
        if rs_val == 0 { None } else { Some(((rd_val >> (32 - rs_val)) & 1) != 0) },
        None,
      );
    }
    0x3 => {
      //LSR (Logical Shift Right)
      let rs_val = rs_val & 0x00FF;
      let result = rd_val >> rs_val;
      gba.regs[rd] = result;
      gba.cpsr.set_all_status_flags(
        result,
        if rs_val == 0 { None } else { Some(((rd_val >> (rs_val - 1)) & 1) != 0) },
        None,
      );
    }
    0x4 => {
      //ASR (Arithmetic Shift Right)
      let rs_val = rs_val & 0x00FF;
      let result = ((rd_val as i32) << rs_val) as u32;
      gba.regs[rd] = result;
      gba.cpsr.set_all_status_flags(
        result,
        if rs_val == 0 { None } else { Some((rd_val >> (rs_val - 1)) & 1 != 0) },
        None,
      );
    }
    0x5 => {
      // ADC (Add With Carry)
      let (op2_plus_carry, overflow_when_adding_op2_and_carry) =
        rs_val.overflowing_add(gba.cpsr.carry_flag() as u32);
      let (result, overflow) = rd_val.overflowing_add(op2_plus_carry);
      gba.regs[rd] = result;
      gba.cpsr.set_all_status_flags(
        result,
        Some(
          overflow_when_adding_op2_and_carry
            || carry_from(rd_val, rs_val + (gba.cpsr.carry_flag() as u32), result),
        ),
        Some(overflow_when_adding_op2_and_carry || overflow),
      );
    }
    0x6 => {
      // SBC (Substract With Carry)
      let (result, overflow) = rd_val.overflowing_sub(rs_val);
      let (result, overflow2) = result.overflowing_sub((!gba.cpsr.carry_flag()) as u32);
      gba.regs[rd] = result;
      gba.cpsr.set_all_status_flags(
        result,
        Some(
          borrow_from(rd_val, rs_val.overflowing_add((!gba.cpsr.carry_flag()) as u32).0)
            || overflow2,
        ),
        Some(overflow || overflow2),
      );
    }
    0x7 => {
      // ROR (Rotate Right)
      let rs_val = rs_val & 0x00FF;
      if rs_val == 0 {
        gba.cpsr.set_neutral_flags(rd_val);
      } else {
        let r4 = rs_val & 0x1F;
        if r4 > 0 {
          let result = rd_val.rotate_right(r4);
          gba.regs[rd] = result;
          gba.cpsr.set_all_status_flags(result, Some(((rd_val >> (r4 - 1)) & 1) != 0), None);
        } else {
          gba.cpsr.set_neutral_flags(rd_val);
          gba.cpsr.set_carry_flag((rd_val >> 31) != 0);
        }
      }
    }
    0x8 => {
      // TST (Test)
      gba.cpsr.set_all_status_flags(rd_val & rs_val, None, None);
    }
    0x9 => {
      // NEG (Negate)
      let (result, overflow) = 0u32.overflowing_sub(rs_val);
      gba.regs[rd] = result;
      gba.cpsr.set_all_status_flags(result, Some(borrow_from(0, rs_val)), Some(overflow));
    }
    0xA => {
      // CMP (Compare)
      let (res, _) = rd_val.overflowing_sub(rs_val);
      gba.cpsr.set_all_status_flags(
        res,
        Some(borrow_from(rd_val, rs_val)),
        Some(((rd_val ^ rs_val) as i32) < 0 && ((rs_val ^ res) as i32) >= 0),
      );
    }
    0xB => {
      // CMN (Compare Negative)
      let (res, overflow) = rd_val.overflowing_add(rs_val);
      gba.cpsr.set_all_status_flags(res, Some(carry_from(rd_val, rs_val, res)), Some(overflow));
    }
    0xC => {
      // ORR (Logical OR)
      let res = rd_val | rs_val;
      gba.regs[rd] = res;
      gba.cpsr.set_all_status_flags(res, None, None);
    }
    0xD => {
      // MUL (Multiply)
      let res = rd_val.overflowing_mul(rs_val).0;
      gba.regs[rd] = res;
      gba.cpsr.set_all_status_flags(res, Some(false), None);
    }
    0xE => {
      // BIC (Bit clear)
      let res = rd_val & (!rs_val);
      gba.regs[rd] = res;
      gba.cpsr.set_all_status_flags(res, None, None);
    }
    0xF => {
      // MVN (Not)
      let res = !rs_val;
      gba.regs[rd] = res;
      gba.cpsr.set_all_status_flags(res, None, None);
    }
    _ => unimplemented!("Impossible"),
  }
  gba.clocks += 0; // TODO: clocks
  Ok(())
}
/// HiReg/BX
fn high_register_operations_or_bx(gba: &mut GBA, opcode: u16) -> ThumbResult {
  let (_, _, rs, rd) = as_lower_3bit_values(opcode);
  let rs_most_significant_bit = (opcode & 0x0040) >> 6;
  let rd_most_significant_bit = (opcode & 0x0080) >> 7;
  let operation = (opcode & 0x0300) >> 8;
  let rs = rs + ((rs_most_significant_bit as usize) << 3);
  let rd = rd + ((rd_most_significant_bit as usize) << 3);
  match operation {
    0 => {
      // ADD
      let (res, _) = gba.regs[rd].overflowing_add(gba.regs[rs]);
      gba.regs[rd] = res;
    }
    1 => {
      //CMP
      let (op1, op2) = (gba.regs[rd], gba.regs[rs]);
      let (res, _overflow) = op1.overflowing_sub(op2);
      // TODO: Is this check right?
      gba.cpsr.set_all_status_flags(
        res,
        Some(borrow_from(op1, op2)),
        Some(((op1 ^ op2) as i32) < 0 && ((op2 ^ res) as i32) >= 0),
      );
    }
    2 => {
      // MOV
      gba.regs[rd] = if rs == 15 { gba.regs[15].overflowing_add(2).0 } else { gba.regs[rs] };
    }
    3 => {
      // Ignore BLX since it's ARMv9
      // BX
      // Change to ARM mode if bit 0 is 0
      let rs_val = gba.regs[rs];
      gba.cpsr.set_thumb_state_flag((rs_val & 0x0000_0001) != 0);
      if rs == 15 {
        gba.regs[15] = rs_val & 0xFFFF_FFFC;
      } else {
        gba.regs[15] = rs_val & 0xFFFF_FFFE;
      }
    }
    _ => unreachable!(), // It's 2 bits
  }
  gba.clocks += 0; // TODO: clocks
  Ok(())
}
/// LDR PC
fn pc_relative_load(gba: &mut GBA, opcode: u16) -> ThumbResult {
  let rd = as_bits_8_to_10(opcode);
  let byte = u32::from(as_low_byte(opcode));
  let offset = byte << 2; // In steps of 4
  gba.regs[rd] =
    gba.fetch_u32(((gba.regs[15].overflowing_add(2).0) & 0xFFFF_FFFC).overflowing_add(offset).0);
  gba.clocks += 0; // TODO: clocks
  Ok(())
}
/// LDR/STR
fn load_or_store_with_relative_offset(gba: &mut GBA, opcode: u16) -> ThumbResult {
  let (_, ro, rb, rd) = as_lower_3bit_values(opcode);
  let is_load = as_11th_bit(opcode);
  let is_byte_else_word = (opcode & 0x0400) != 0;
  let (addr, _) = gba.regs[ro].overflowing_add(gba.regs[rb]);
  if is_load {
    if is_byte_else_word {
      gba.write_u8(addr, gba.regs[rd] as u8);
    } else {
      gba.write_u32(addr, gba.regs[rd]);
    }
  } else {
    gba.regs[rd] =
      if is_byte_else_word { u32::from(gba.fetch_byte(addr)) } else { gba.fetch_u32(addr) };
  }
  gba.clocks += 0; // TODO: clocks
  Ok(())
}
/// LDR/STR H/SB/SH
fn load_or_store_sign_extended_byte_or_halfword(gba: &mut GBA, opcode: u16) -> ThumbResult {
  gba.debug_print_fn.map(|f| f("load_or_store_sign_extended_byte_or_halfword\n"));
  let H = as_11th_bit(opcode);
  let (_, ro, rb, rd) = as_lower_3bit_values(opcode);
  let sign_extend = (opcode & 0x0400) != 0;
  let addr = gba.regs[rb].overflowing_add(gba.regs[ro]).0;
  gba.debug_print_fn.map(|f| f(format!("addr:{}\n", addr).as_str()));
  if !H && !sign_extend {
    gba.write_u16(addr, gba.regs[rd] as u16);
    gba.clocks += 0; // TODO: clocks
    return Ok(());
  }
  gba.regs[rd] = if H {
    // Half-word(16 bits)
    if sign_extend {
      (i32::from(gba.fetch_u16(addr) as i16) as u32) // Sign extend
    } else {
      // zero extend
      u32::from(gba.fetch_u16(addr))
    }
  } else {
    (i32::from(gba.fetch_byte(addr) as i8) as u32) // Sign extend
  };
  gba.clocks += 0; // TODO: clocks
  Ok(())
}
/// LDR/STR {B}
fn load_or_store_with_immediate_offset(gba: &mut GBA, opcode: u16) -> ThumbResult {
  gba.debug_print_fn.map(|f| f("load_or_store_with_immediate_offset"));
  let is_byte_else_word = (opcode & 0x1000) != 0;
  let is_load_else_store = as_11th_bit(opcode);
  let (_, _, rb, rd) = as_lower_3bit_values(opcode);
  let offset = u32::from(as_bits_6_to_10(opcode));
  let rb_val = gba.regs[rb];
  if is_byte_else_word {
    let addr = offset.overflowing_add(rb_val).0;
    if is_load_else_store {
      gba.regs[rd] = u32::from(gba.fetch_byte(addr));
    } else {
      gba.write_u8(addr, gba.regs[rd] as u8);
    }
  } else {
    let offset = offset << 2;
    let addr = offset.overflowing_add(rb_val).0;
    if is_load_else_store {
      gba.regs[rd] = gba.fetch_u32(addr);
    } else {
      gba.write_u32(addr, gba.regs[rd]);
    }
  }
  gba.clocks += 0; // TODO: clocks
  Ok(())
}
/// LDR/STR H
fn load_or_store_halfword(gba: &mut GBA, opcode: u16) -> ThumbResult {
  gba.debug_print_fn.map(|f| f("load_or_store_halfword"));
  let is_load_else_store = as_11th_bit(opcode);
  let (_, _, rb, rd) = as_lower_3bit_values(opcode);
  let offset = as_bits_6_to_10(opcode);
  let offset = u32::from(offset) << 1;
  let addr = offset.overflowing_add(gba.regs[rb]).0;
  if is_load_else_store {
    gba.regs[rd] = u32::from(gba.fetch_u16(addr));
  } else {
    gba.write_u16(addr, gba.regs[rd] as u16);
  }
  gba.clocks += 0; // TODO: clocks
  Ok(())
}
/// LDR/STR SP
fn stack_pointer_relative_load_or_store(gba: &mut GBA, opcode: u16) -> ThumbResult {
  let is_load_else_store = as_11th_bit(opcode);
  let byte = as_low_byte(opcode);
  let offset = u32::from(byte) << 2;
  let rd = as_bits_8_to_10(opcode);
  let addr = gba.regs[13].overflowing_add(offset).0;
  if is_load_else_store {
    gba.regs[rd] = gba.fetch_u32(addr);
  } else {
    gba.write_u32(addr, gba.regs[rd]);
  }
  gba.clocks += 0; // TODO: clocks
  Ok(())
}
/// LOAD PC/SP
fn load_address(gba: &mut GBA, opcode: u16) -> ThumbResult {
  let load_sp_else_pc = as_11th_bit(opcode);
  let byte = as_low_byte(opcode);
  let nn = u32::from(byte) << 2;
  let rd = as_bits_8_to_10(opcode);
  gba.regs[rd] = if load_sp_else_pc {
    gba.regs[13].overflowing_add(nn).0
  } else {
    (gba.regs[15].overflowing_add(2).0 & !3).overflowing_add(nn).0
  };
  gba.clocks += 0; // TODO: clocks
  Ok(())
}
/// ADD/SUB SP,nn
fn add_or_sub_offset_to_stack_pointer(gba: &mut GBA, opcode: u16) -> ThumbResult {
  let byte = as_low_byte(opcode);
  let substract_else_add = (byte & 0x80) != 0;
  let byte7 = byte & 0x0000_007F;
  let offset = u32::from(byte7) << 2;
  let sp = gba.regs[13];
  gba.regs[13] =
    if substract_else_add { sp.overflowing_sub(offset).0 } else { sp.overflowing_add(offset).0 };
  gba.clocks += 0; // TODO: clocks
  Ok(())
}
/// PUSH/POP
fn push_or_pop(gba: &mut GBA, opcode: u16) -> ThumbResult {
  let is_pop_else_push = as_11th_bit(opcode);
  // PUSH LR or POP PC
  let pc_or_lr_flag = (opcode & 0x0100) != 0;
  let rlist = as_low_byte(opcode);
  let mut sp = gba.regs[13];
  // GBATEK says "In THUMB mode stack is always meant to be 'full descending',
  // ie. PUSH is equivalent to 'STMFD/STMDB' and POP to 'LDMFD/LDMIA' in ARM mode."
  // And http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.dui0068b/Cacbgchh.html says
  // "The stack pointer can either point to the last item in the stack (a full stack), or the next
  // free space on the stack (an empty stack)."
  // So we assume sp is "empty" a.k.a pop= post-index and push = pre-index
  if is_pop_else_push {
    for (idx, _) in rlist.as_bools().iter().rev().enumerate().filter(|(_, &boolean)| boolean) {
      gba.regs[idx] = gba.fetch_u32(sp);
      sp = sp.overflowing_add(4).0;
    }
    if pc_or_lr_flag {
      gba.regs[15] = gba.fetch_u32(sp) & (!0x0000_0001); // POP PC
      sp = sp.overflowing_add(4).0;
    }
  } else {
    if pc_or_lr_flag {
      sp = sp.overflowing_sub(4).0;
      gba.write_u32(sp, gba.regs[14]); // PUSH LR
    }
    for (idx, _) in rlist.as_bools().iter().rev().enumerate().filter(|(_, &boolean)| boolean).rev()
    {
      sp = sp.overflowing_sub(4).0;
      gba.write_u32(sp, gba.regs[idx]);
    }
  }
  gba.regs[13] = sp;
  gba.clocks += 0; // TODO: clocks
  Ok(())
}
/// STM/LDM
fn multiple_loads_or_stores(gba: &mut GBA, opcode: u16) -> ThumbResult {
  let rb = as_bits_8_to_10(opcode);
  let is_load_else_store = as_11th_bit(opcode);
  let rlist = as_low_byte(opcode);
  let mut addr = gba.regs[rb];
  let operation: fn(&mut GBA, u32, usize) = if is_load_else_store {
    |gba: &mut GBA, addr, reg| {
      gba.regs[reg] = gba.fetch_u32(addr);
    }
  } else {
    |gba: &mut GBA, addr, reg| {
      gba.write_u32(addr, gba.regs[reg]);
    }
  };
  for (idx, _) in rlist.as_bools().iter().rev().enumerate().filter(|(_, &boolean)| boolean) {
    operation(gba, addr, idx);
    addr = addr.overflowing_add(4).0;
  }
  gba.regs[rb] = addr;
  gba.clocks += 0; // TODO: clocks
  Ok(())
}
/// B{COND}
fn conditional_branch(gba: &mut GBA, opcode: u16) -> ThumbResult {
  let offset = u32::from(as_low_byte(opcode)) << 1;
  let (is_negative, offset) = ((offset & 0x0000_0100) != 0, offset & 0x0000_00FF);
  let cond = (opcode >> 8) & 0x000F;
  let should_not_jump = super::arm::check_cond(gba, u32::from(cond) << 28);
  if should_not_jump {
    gba.clocks += 0; // TODO: clocks
    return Ok(());
  }
  gba.regs[15] = if is_negative {
    gba.regs[15].overflowing_sub((!offset) & 0x0000_00FE).0
  } else {
    gba.regs[15].overflowing_add(offset.overflowing_add(2).0).0
  };
  gba.clocks += 0; // TODO: clocks
  Ok(())
}
/// SWI
fn software_interrupt(gba: &mut GBA, opcode: u16) -> ThumbResult {
  super::arm::software_interrupt(gba, u32::from(opcode) << 16)?;
  gba.clocks += 0; // TODO: clocks
  Ok(())
}
/// B
fn branch(gba: &mut GBA, opcode: u16) -> ThumbResult {
  let is_negative = (opcode & 0x0000_0400) != 0; // 11th bit
  let absolute_offset = ((opcode & 0x0000_03FF) as u32) << 1; // first 10 bits
  let pc = gba.regs[15];
  gba.regs[15] = if is_negative {
    // pc.overflowing_sub(absolute_offset).0
    pc.overflowing_sub((!absolute_offset) & 0x0000_07FE).0
  } else {
    pc.overflowing_add(absolute_offset).0.overflowing_add(2).0
  };
  gba.clocks += 0; // TODO: clocks
  Ok(())
}
/// BL/BLX
///
/// This is actually a 32 bit instruction (2 thumb instructions).
///
/// The first one has the upper 11 bits and the second the lower 11
fn branch_and_link_or_link_and_exchange_first_opcode(gba: &mut GBA, opcode: u16) -> ThumbResult {
  let upper_offset = (i32::from((as_low_11bits(opcode) as i16) << 5) << 7) as u32;
  let pc = gba.regs[15];
  gba.regs[14] = pc.overflowing_add(upper_offset).0.overflowing_add(2).0;
  gba.clocks += 0; // TODO: clocks
  Ok(())
}
fn branch_and_link_or_link_and_exchange_second_opcode(gba: &mut GBA, opcode: u16) -> ThumbResult {
  //let H = as_11th_bit(opcode);// I *think* this is not needed since it's ARM9 (BLX)
  let lower_offset = u32::from(as_low_11bits(opcode) << 1);
  let pc = gba.regs[15].overflowing_add(2).0;
  gba.regs[15] = gba.regs[14].overflowing_add(lower_offset).0;
  gba.regs[14] = pc.overflowing_sub(1).0;
  gba.clocks += 0; // TODO: clocks
  Ok(())
}
fn decode_thumb(opcode: u16) -> Result<ThumbInstruction, ThumbError> {
  let bits15_8 = (opcode >> 8) as u8;
  const T: bool = true;
  const F: bool = false;
  Ok(match bits15_8.as_bools() {
    [F, F, F, T, T, _, _, _] => add_or_sub, /* The ARM7TDMI manual says 0b000111 but GBATEK 0x00011. In GBATEK we trust */
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

pub(crate) fn execute_one_instruction(gba: &mut GBA) -> ThumbResult {
  let pc = *gba.pc();
  let opcode = gba.fetch_u16(pc);
  *gba.pc() = pc.overflowing_add(2).0;
  (gba.instruction_hook_with_opcode)(gba, u32::from(opcode));
  let instruction = decode_thumb(opcode)?;
  instruction(gba, opcode)?;
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
