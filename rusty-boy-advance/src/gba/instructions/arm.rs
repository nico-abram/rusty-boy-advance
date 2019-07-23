use alloc::{format, string::String};

use super::super::{
  cpsr::CPSR,
  cpu_mode::CpuMode,
  gba::{GBA, SWI_HANDLER},
  utils::AsBoolSlice,
};

pub type ARMError = String;
pub type ARMResult = Result<(), ARMError>;
pub type ARMInstruction = fn(&mut GBA, u32) -> ARMResult;

#[allow(non_camel_case_types)]
#[derive(Debug)]
pub(crate) enum Cond {
  /// Equal (Z)
  EQ,
  /// Not Equal (Not Z)
  NE,
  /// Unsigned higher or same (C)
  CS_HS,
  /// Unsigned lower (not C)
  CC_LO,
  /// Negative (N)
  MI,
  /// Positive or zero (N)
  PL,
  /// Overflowe (V)
  VS,
  /// No overflow (V)
  VC,
  /// Unsigned higher (C and not Z)
  HI,
  /// Unsigned lower or same (not C=0 or Z)
  LS,
  /// Greater or equal (N=V)
  GE,
  /// Less than (N!=V)
  LT,
  /// Greater than (not Z and N=V)
  GT,
  /// Less or equal (Z or N!=V)
  LE,
  /// Always (true)
  AL,
  /// Never (ARMv1,v2 only) (Reserved ARMv3 and up) (false)
  NV,
}
/// Map the opcode's upper nibble to the Cond enum
#[inline]
pub(crate) fn opcode_to_cond(opcode: u32) -> Cond {
  match (opcode >> 28) as u8 {
    0x0 => Cond::EQ,
    0x1 => Cond::NE,
    0x2 => Cond::CS_HS,
    0x3 => Cond::CC_LO,
    0x4 => Cond::MI,
    0x5 => Cond::PL,
    0x6 => Cond::VS,
    0x7 => Cond::VC,
    0x8 => Cond::HI,
    0x9 => Cond::LS,
    0xA => Cond::GE,
    0xB => Cond::LT,
    0xC => Cond::GT,
    0xD => Cond::LE,
    0xE => Cond::AL,
    0xF => Cond::NV,
    _ => unreachable!(), // We're matching on 4 bits
  }
}
/// Check if the condition for the given opcode is true
#[inline]
pub(crate) fn check_cond(gba: &mut GBA, opcode: u32) -> bool {
  let applies = match opcode_to_cond(opcode) {
    Cond::EQ => {
      gba.debug_print_fn.map(|f| f(format!("eq zero:{}", gba.cpsr.zero_flag()).as_str()));
      gba.cpsr.zero_flag()
    }
    Cond::NE => !gba.cpsr.zero_flag(),
    Cond::CS_HS => gba.cpsr.carry_flag(),
    Cond::CC_LO => !gba.cpsr.carry_flag(),
    Cond::MI => gba.cpsr.negative_flag(),
    Cond::PL => !gba.cpsr.negative_flag(),
    Cond::VS => gba.cpsr.overflow_flag(),
    Cond::VC => !gba.cpsr.overflow_flag(),
    Cond::HI => gba.cpsr.carry_flag() && !gba.cpsr.zero_flag(),
    Cond::LS => !gba.cpsr.carry_flag() || gba.cpsr.zero_flag(),
    Cond::GE => gba.cpsr.negative_flag() == gba.cpsr.overflow_flag(),
    Cond::LT => gba.cpsr.negative_flag() != gba.cpsr.overflow_flag(),
    Cond::GT => gba.cpsr.negative_flag() == gba.cpsr.overflow_flag() && !gba.cpsr.zero_flag(),
    Cond::LE => gba.cpsr.negative_flag() != gba.cpsr.overflow_flag() || gba.cpsr.zero_flag(),
    Cond::AL => true,
    Cond::NV => false,
  };
  if applies {
    gba.clocks += gba.sequential_cycle();
  }
  !applies
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
/// Branch and Exchange (BX)
/// If the condition is true, branch and switch mode
fn branch_and_exchange(gba: &mut GBA, opcode: u32) -> ARMResult {
  // Address to jump to is in a register given by lowest nibble
  let reg_n = opcode & 0x0000_000F;
  let mut jmp_addr = gba.regs[reg_n as usize];
  // It doesn't *actually* switch mode, a bit tells us what to set it to
  let switch_to_thumb = (jmp_addr & 0x0000_0001) != 0;
  if switch_to_thumb {
    jmp_addr -= 1;
    gba.cpsr.set_thumb_state_flag(true);
  }
  gba.regs[15] = jmp_addr;
  gba.clocks += gba.sequential_cycle() + gba.sequential_cycle() + gba.nonsequential_cycle();
  Ok(())
}
/// Branch or Branch and link (B or BL)
///
/// BL is similar to CALL, it stores the return PC (PC+4) in the
/// LR (Register 14). If using nested functions, that requires pushing LR onto the stack.
fn branch_or_branch_and_link(gba: &mut GBA, opcode: u32) -> ARMResult {
  gba.debug_print_fn.map(|f| f("branch_or_branch_and_link\n"));
  let offset = (opcode & 0x007F_FFFF) << 2; //*4
  let is_positive = (opcode & 0x0080_0000) == 0;
  let is_branch_and_link = (opcode & 0x0100_0000) != 0;
  let pc = gba.regs[15];
  gba.debug_print_fn.map(|f| f(format!("is_branch_and_link:{}\n", is_branch_and_link).as_str()));
  if is_branch_and_link {
    gba.regs[14] = pc;
  }
  gba.regs[15] = 4u32
    .overflowing_add(if is_positive {
      pc.overflowing_add(offset).0
    } else {
      pc.overflowing_sub(!(offset | 0xFF00_0003) + 4).0
    })
    .0;
  gba.clocks += gba.sequential_cycle() + gba.sequential_cycle() + gba.nonsequential_cycle();
  Ok(())
}
/// Software Interrupt (SWI)
///
/// Used to call BIOS functions
pub(crate) fn software_interrupt(gba: &mut GBA, _: u32) -> ARMResult {
  let ret_pc = gba.regs[15];
  gba.set_mode(CpuMode::Supervisor);
  gba.regs[14] = ret_pc;
  gba.regs[15] = SWI_HANDLER;
  gba.cpsr.set_thumb_state_flag(false); // Set ARM state
  gba.cpsr.set_irq_disabled_flag(true);
  gba.clocks += gba.sequential_cycle() + gba.sequential_cycle() + gba.nonsequential_cycle();
  Ok(())
}
/// Single data swap (SWP)
fn single_data_swap(gba: &mut GBA, opcode: u32) -> ARMResult {
  let (_, _, is_byte_else_word, _, _) = as_flags(opcode);
  let (rn, rd, _, _, rm) = as_usize_nibbles(opcode);
  if is_byte_else_word {
    // TODO: Is this right? GBATEK mentions zero extending
    // But is it the most significant LE byte?
    // It refers to STR/LDR, which says
    // "When reading a byte from memory, upper 24 bits of Rd are zero-extended."
    // I'm assuming that literally means the upper bits and not the most significant ones
    gba.regs[rd] = gba.regs[rn] & 0x0000_00FF;
    gba.regs[rn] = gba.regs[rm] & 0x0000_00FF;
  } else {
    gba.regs[rd] = gba.regs[rn];
    gba.regs[rn] = gba.regs[rm];
  }
  gba.clocks += gba.sequential_cycle()
    + gba.nonsequential_cycle()
    + gba.nonsequential_cycle()
    + gba.internal_cycle();
  Ok(())
}
/// Multiply long (MULL)
fn multiply_long(gba: &mut GBA, opcode: u32) -> ARMResult {
  let (is_unsupported, store_double_word_result, signed, accumulate, set_cond_flags) =
    as_flags(opcode);
  let (rdhigh_aka_rd, rdlow_aka_rn, rs, _, rm) = as_usize_nibbles(opcode);
  if is_unsupported {
    return Err(String::from(
      "No support for SMLAxy, SMLAWy, SMLALxy, SMULxy and SMULWy (Signed Halfword Multiply)",
    ));
  }
  let mut res: u64 = if signed {
    (i64::from(gba.regs[rs] as i32) * i64::from(gba.regs[rm] as i32)) as u64
  } else {
    u64::from(gba.regs[rs]) * u64::from(gba.regs[rm])
  };
  if accumulate {
    if store_double_word_result {
      let val = u64::from(gba.regs[rdlow_aka_rn]) + (u64::from(gba.regs[rdhigh_aka_rd]) << 32);
      if signed {
        res = ((res as i64) + (val as i64)) as u64;
      } else {
        res += val;
      }
    } else {
      let val = gba.regs[rdhigh_aka_rd];
      if signed {
        res = ((res as i64) + i64::from(val as i32)) as u64;
      } else {
        res += u64::from(val);
      }
    }
  }
  if store_double_word_result {
    gba.regs[rdlow_aka_rn] = (res) as u32;
    gba.regs[rdhigh_aka_rd] = (res >> 32) as u32;
  } else {
    gba.regs[rdhigh_aka_rd] = (res) as u32;
  }
  if set_cond_flags {
    // TODO: Are Z and N set using the 64bit value
    // or just the upper word or lower word?
    gba.cpsr.set_zero_flag(res == 0);
    gba.cpsr.set_negative_flag((res & 0x8000_0000_0000_0000) != 0);
    gba.cpsr.set_carry_flag(false); //C=destroyed (ARMv4 and below)
  }
  let internal_cycles = 1
    + if res & 0x0000_FF00 != 0 || (signed && res & 0x0000_FF00 == 0x0000_FF00) { 1 } else { 0 }
    + if res & 0x00FF_0000 != 0 || (signed && res & 0x00FF_0000 == 0x00FF_0000) { 1 } else { 0 }
    + if res & 0xFF00_0000 != 0 || (signed && res & 0xFF00_0000 == 0xFF00_0000) { 1 } else { 0 };
  gba.clocks += gba.sequential_cycle()
    + gba.internal_cycle() * (internal_cycles + if accumulate { 2 } else { 1 });
  Ok(())
}
/// Multiply (MUL)
fn multiply(gba: &mut GBA, opcode: u32) -> ARMResult {
  let (_, _, _, accumulate, set_cond_flags) = as_flags(opcode);
  let (rd, rn, rs, _, rm) = as_usize_nibbles(opcode);
  let res = if accumulate {
    gba.regs[rs].overflowing_mul(gba.regs[rm]).0.overflowing_add(gba.regs[rn]).0
  } else {
    gba.regs[rs].overflowing_mul(gba.regs[rm]).0
  };
  gba.regs[rd] = res;
  if set_cond_flags {
    //C=destroyed (ARMv4 and below)
    gba.cpsr.set_all_status_flags(res, Some(false), None);
  }
  let internal_cycles = 1
    + if res & 0x0000_FF00 != 0 || res & 0x0000_FF00 == 0x0000_FF00 { 1 } else { 0 }
    + if res & 0x00FF_0000 != 0 || res & 0x00FF_0000 == 0x00FF_0000 { 1 } else { 0 }
    + if res & 0xFF00_0000 != 0 || res & 0xFF00_0000 == 0xFF00_0000 { 1 } else { 0 };
  gba.clocks += gba.sequential_cycle()
    + gba.internal_cycle() * (internal_cycles + if accumulate { 1 } else { 0 });
  Ok(())
}
/// Halfword Data Transfer Register Offset (HDT_RO or HDT_IO)
fn halfword_data_transfer_immediate_or_register_offset(gba: &mut GBA, opcode: u32) -> ARMResult {
  gba.debug_print_fn.map(|f| f("halfword_data_transfer_immediate_or_register_offset\n"));
  let (is_pre_offseted, is_up, is_immediate_offset_else_register, write_back, is_load_else_store) =
    as_flags(opcode);
  let (rn, rd, immediate_offset_upper, opcode, lowest_nibble) = as_usize_nibbles(opcode);
  let opcode = (opcode >> 1) & 3;
  gba.debug_print_fn.map(|f| f(format!("opcode:{}\n", opcode).as_str()));
  gba.debug_print_fn.map(|f| f(format!("is_load_else_store:{}\n", is_load_else_store).as_str()));
  let offset = if is_immediate_offset_else_register {
    gba.debug_print_fn.map(|f| {
      f(format!(
        "immediate offset high:{:x} low:{:x} val:{:x}\n",
        immediate_offset_upper,
        lowest_nibble,
        ((immediate_offset_upper as u32) << 4) + (lowest_nibble as u32)
      )
      .as_str())
    });
    ((immediate_offset_upper as u32) << 4) + (lowest_nibble as u32)
  } else {
    gba.debug_print_fn.map(|f| f(format!("reg offset lowest_nibble:{}\n", lowest_nibble).as_str()));
    gba.regs[lowest_nibble]
  };
  gba.debug_print_fn.map(|f| f(format!("offset:{:x}\n", offset).as_str()));
  let mut addr = gba.regs[rn];
  let add_offset: fn(u32, u32) -> u32 =
    if is_up { |addr, offset| addr + offset } else { |addr, offset| addr - offset };
  if is_pre_offseted {
    addr = add_offset(addr, offset);
  }
  if is_load_else_store {
    match opcode {
      1 => {
        // Load Unsigned halfword (zero-extended)
        gba.debug_print_fn.map(|f| {
          f(format!(
            "loading Load Unsigned halfword (zero-extended) addr:{:x} va:{:x} extended:{:x}\n",
            addr,
            gba.fetch_u16(addr),
            gba.fetch_u16(addr) as u32
          )
          .as_str())
        });
        gba.regs[rd] = gba.fetch_u16(addr) as u32;
      }
      2 => {
        // Load Signed byte (sign extended)
        gba.regs[rd] = gba.fetch_byte(addr) as i8 as i32 as u32;
      }
      3 => {
        // Load Signed halfword (sign extended)
        gba.regs[rd] = gba.fetch_u16(addr) as i16 as i32 as u32;
      }
      0 => core::panic!(
        "Invalid instruction: Reserved 0 opcode load halfword_data_transfer_immediate_or_register_offset"
      ),
      _ => unreachable!(), // It's 2 bits
    }
  } else {
    match opcode {
      1 => {
        // Store halfword
        gba.debug_print_fn.map(|f| {
          f(format!(
            "storing halfword addr:{:x} value:{:x} extended:{:x}\n",
            addr, gba.regs[rd], gba.regs[rd] as u16
          )
          .as_str())
        });
        gba.write_u16(addr, gba.regs[rd] as u16);
      }
      2 => {
        // Load Doubleword
        gba.regs[rd] = gba.fetch_u32(addr);
        gba.regs[rd + 1] = gba.fetch_u32(addr.overflowing_add(4).0);
      }
      3 => {
        // Store Doubleword
        gba.write_u32(addr, gba.regs[rd]);
        gba.write_u32(addr.overflowing_add(4).0, gba.regs[rd + 1]);
      }
      0 => {
        // Reserved for SWP. Unreachable (Ensure we match SWP before this in decoding)
        unreachable!()
      }
      _ => unreachable!(), // It's 2 bits
    }
  }
  if !is_pre_offseted {
    addr = add_offset(addr, offset);
  }
  if !is_pre_offseted || write_back {
    gba.regs[rn] = addr;
  }
  // TODO: Do clocks properly here
  gba.clocks += gba.sequential_cycle() + gba.nonsequential_cycle() + gba.internal_cycle();
  Ok(())
}
/// Single Data Transfer (SDT)
fn single_data_transfer(gba: &mut GBA, opcode: u32) -> ARMResult {
  gba.clocks += gba.sequential_cycle() + gba.nonsequential_cycle() + gba.internal_cycle();
  gba.debug_print_fn.map(|f| f("single_data_transfer"));
  let shifted_register = as_extra_flag(opcode);
  let (is_pre_offseted, is_up, is_byte_size, bit_21, is_load) = as_flags(opcode);
  let (rn, rd, third_byte, second_byte, first_byte) = as_usize_nibbles(opcode);
  gba.debug_print_fn.map(|f| f(format!("shifted_register:{}\n", shifted_register).as_str()));
  gba.debug_print_fn.map(|f| f(format!("rn:{}\n", rn).as_str()));
  gba.debug_print_fn.map(|f| f(format!("rd:{}\n", rd).as_str()));
  gba.debug_print_fn.map(|f| f(format!("first_byte:{:x}\n", first_byte).as_str()));
  gba.debug_print_fn.map(|f| f(format!("second_byte:{:x}\n", second_byte).as_str()));
  gba.debug_print_fn.map(|f| f(format!("third_byte:{:x}\n", third_byte).as_str()));
  let offset = if !shifted_register {
    gba
      .debug_print_fn
      .map(|f| f(format!("immediate offset:{:x}\n", opcode & 0x0000_0FFF).as_str()));
    opcode & 0x0000_0FFF
  } else {
    let shift_amount = (third_byte as u32) * 2 + (((second_byte as u32) & 0x8) >> 3);
    let shift_type = second_byte;
    let register_value = gba.regs[first_byte];
    gba.debug_print_fn.map(|f| f(format!("shifted reg shift_type:{}\n", shift_type).as_str()));
    gba
      .debug_print_fn
      .map(|f| f(format!("shifted reg shift_amount:{:x}\n", shift_amount).as_str()));
    gba
      .debug_print_fn
      .map(|f| f(format!("shifted reg register_value:{:x}\n", register_value).as_str()));
    match shift_type & 6 {
      0 => register_value.overflowing_shl(shift_amount).0,
      2 => register_value.overflowing_shr(shift_amount).0,
      4 => (register_value as i32).overflowing_shr(shift_amount).0 as u32,
      6 => register_value.rotate_right(shift_amount),
      _ => {
        return Err(format!(
          "Invalid instruction SDis_thumb_state_flag(Single Data Transfer) shift type {}",
          shift_type & 6
        ));
      }
    }
  };
  let mut addr = gba.regs[rn];
  let add_offset =
    |addr| (i64::from(addr) + if is_up { i64::from(offset) } else { -i64::from(offset) }) as u32;
  gba.debug_print_fn.map(|f| {
    f(format!("offset i64:{}\n", if is_up { i64::from(offset) } else { -i64::from(offset) })
      .as_str())
  });
  gba.debug_print_fn.map(|f| f(format!("is_pre_offseted:{}\n", is_pre_offseted).as_str()));
  if is_pre_offseted {
    addr = add_offset(addr);
  }
  if rn == 15 {
    addr += 4;
  }
  gba.debug_print_fn.map(|f| f(format!("addr:{}\n", addr).as_str()));
  gba.debug_print_fn.map(|f| f(format!("addr:{:x}\n", addr).as_str()));
  gba.debug_print_fn.map(|f| f(format!("is_load:{}\n", is_load).as_str()));
  gba.debug_print_fn.map(|f| f(format!("is_byte_size:{}\n", is_byte_size).as_str()));
  if is_load {
    gba.regs[rd] = if is_byte_size {
      gba.debug_print_fn.map(|f| f(format!("byte_addr:{}\n", addr).as_str()));
      gba
        .debug_print_fn
        .map(|f| f(format!("byte_addr div:{:x}\n", ((addr) / 4u32) * 4u32).as_str()));
      gba.debug_print_fn.map(|f| f(format!("byte:{:x}\n", gba.fetch_byte(addr)).as_str()));
      gba
        .debug_print_fn
        .map(|f| f(format!("byte_addr div add:{:x}\n", (((addr) / 4u32) * 4u32) + 3).as_str()));
      gba.debug_print_fn.map(|f| {
        f(format!("byte div add:{:x}\n", gba.fetch_byte((((addr) / 4u32) * 4u32) + 3)).as_str())
      });
      gba
        .debug_print_fn
        .map(|f| f(format!("byte div:{:x}\n", gba.fetch_byte(((addr) / 4u32) * 4u32)).as_str()));
      // Least significant byte
      u32::from(gba.fetch_byte(addr))
    } else {
      gba.fetch_u32(addr)
    };
  } else if is_byte_size {
    gba.write_u8(addr, gba.regs[rd] as u8);
  } else {
    gba.write_u32(addr, gba.regs[rd]);
  }
  if !is_pre_offseted {
    addr = add_offset(addr);
  }
  if !is_pre_offseted || bit_21 {
    gba.regs[rn] = addr;
  }
  Ok(())
}
/// Block Data Transfer (BDT)
fn block_data_transfer(gba: &mut GBA, opcode: u32) -> ARMResult {
  let (is_pre_offseted, is_up, psr_or_user_mode, write_back, is_load_else_store) = as_flags(opcode);
  let (rn, _, _, _, _) = as_usize_nibbles(opcode);
  let (rlist2, rlist1) = (((opcode & 0x0000_FF00) >> 8) as u8, (opcode & 0x0000_00FF) as u8);
  let rlists = [rlist1, rlist2];
  let is_psr = psr_or_user_mode && is_load_else_store && ((rlist2 & 0x80) != 0);
  let get_mut_reg: fn(&mut GBA, usize) -> &mut u32 = if psr_or_user_mode && !is_psr {
    // http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.dui0489g/Cihcadda.html says
    // "You must not use it in User mode or System mode.". How strict is that requirement?
    |gba, x| match x {
      0..=7 => &mut gba.regs[x],
      8..=12 => {
        if gba.cpsr.mode() == CpuMode::FIQ {
          &mut gba.fiq_only_banks[0][x - 8]
        } else {
          &mut gba.regs[x]
        }
      }
      13..=15 => &mut gba.all_modes_banks[CpuMode::User.as_usize()][x - 13],
      _ => unimplemented!(
        "This should never happen. Attempt to access an invalid register number ({})",
        x
      ),
    }
  } else {
    |gba, x| &mut gba.regs[x]
  };
  let mut sp = *get_mut_reg(gba, rn);
  let offseting_operation: fn(&mut u32, u32) = if !is_up {
    |sp, offset| {
      *sp -= offset;
    }
  } else {
    |sp, offset| {
      *sp += offset;
    }
  };
  let is_push = !is_load_else_store;
  let push_or_pop_operation: fn(&mut GBA, usize, u32) = if is_push {
    |gba, reg, sp| gba.write_u32(sp, gba.regs[reg])
  } else {
    |gba, reg, sp| gba.regs[reg] = gba.fetch_u32(sp)
  };
  let mut n = 0;
  // IntoIterator is not implemented for arrays (https://github.com/rust-lang/rust/issues/25725)
  // So we use regular fors (For now)
  if is_pre_offseted {
    for (byte_number, byte) in rlists.iter().enumerate().rev() {
      for (bit_in_byte, &bit_is_set) in byte.as_bools().iter().rev().enumerate().rev() {
        if bit_is_set {
          n += 1;
          let bit_number = byte_number * 8 + bit_in_byte;
          offseting_operation(&mut sp, 4);
          push_or_pop_operation(gba, bit_number, sp);
        }
      }
    }
  } else {
    for (byte_number, byte) in rlists.iter().enumerate() {
      for (bit_in_byte, &bit_is_set) in byte.as_bools().iter().rev().enumerate() {
        if bit_is_set {
          n += 1;
          let bit_number = byte_number * 8 + bit_in_byte;
          push_or_pop_operation(gba, bit_number, sp);
          offseting_operation(&mut sp, 4);
        }
      }
    }
  }
  if write_back {
    *get_mut_reg(gba, rn) = sp;
  }
  if is_psr {
    gba.cpsr = *gba
      .get_spsr_mut()
      .ok_or("Cannot use ^(PSR) on arm BDT while in User or Priviledged(System) mode")?;
  }
  gba.clocks += gba.nonsequential_cycle()
    + if is_push {
      gba.nonsequential_cycle() + (core::cmp::max(n, 1) - 1) * gba.sequential_cycle()
    } else {
      gba.internal_cycle()
        + if opcode & 0x0000_0080 != 0 {
          // LDM PC
          gba.nonsequential_cycle() + (1 + n) * gba.sequential_cycle()
        } else {
          n * gba.sequential_cycle()
        }
    };
  Ok(())
}
/// Data Processing or PSR transfer
/// This handles all ALU operations
fn alu_operation(gba: &mut GBA, opcode: u32) -> ARMResult {
  gba.debug_print_fn.map(|f| f("alu_operation\n"));
  let immediate = as_extra_flag(opcode);
  let (rn_num, rd, _, _, _) = as_usize_nibbles(opcode);
  let (_, _, third_byte, second_byte, lowest_byte) = as_u8_nibbles(opcode);
  let set_condition_flags = (opcode & 0x0010_0000) != 0 || rd == 15;
  let mut rn = gba.regs[rn_num];
  if rn_num == 15 {
    rn = rn.overflowing_add(4).0; // Account for PC pipelining
  }
  let operation = ((opcode & 0x01E0_0000) >> 21) as u8;
  let (op2, shift_carry) = if immediate {
    let ror_shift = u32::from(third_byte) << 1;
    let val = u32::from(lowest_byte + second_byte * 16);
    gba.debug_print_fn.map(|f| f(format!("immediate ror_shift:{}\n", ror_shift).as_str()));
    gba.debug_print_fn.map(|f| f(format!("immediate val dec :{}\n", val).as_str()));
    gba.debug_print_fn.map(|f| f(format!("immediate val hex :{:x}\n", val).as_str()));
    (
      val.rotate_right(ror_shift),
      if ror_shift == 0 { None } else { Some(((val >> (ror_shift - 1)) & 1) != 0) },
    )
  } else {
    let mut register_value: u32 = gba.regs[lowest_byte as usize];
    gba.debug_print_fn.map(|f| f(format!("register val n :{}\n", lowest_byte).as_str()));
    gba.debug_print_fn.map(|f| f(format!("register val hex :{:x}\n", register_value).as_str()));
    if lowest_byte == 15 {
      register_value += 4;
    }
    gba.debug_print_fn.map(|f| f(format!("register val hex :{:x}\n", register_value).as_str()));
    let shift_by_register = (opcode & 0x0000_0010) != 0;
    let mut imm_shift_zero = false;
    let shift_amount = if shift_by_register {
      gba.debug_print_fn.map(|f| f(format!("shift by register n :{}\n", third_byte).as_str()));
      (gba.regs[third_byte as usize] & 0x0000_00FF)
        .overflowing_add(if third_byte == 15 { 4 } else { 0 })
        .0
    } else {
      let shift_amount = (opcode & 0x0000_0F80) >> 7;
      imm_shift_zero = shift_amount == 0;
      shift_amount
    };
    let shift_type = ((opcode & 0x0000_0060) >> 5) as u8;
    let (op2, shift_carry) = if imm_shift_zero {
      gba.debug_print_fn.map(|f| f(format!("imm_shift_zero shift_type:{}\n", shift_type).as_str()));
      // Handle special shift by 0 case
      match shift_type {
        0 => (register_value, None),
        1 => (0, Some((register_value & 0x8000_0000) != 0)),
        2 => (
          (register_value as i32).overflowing_shr(31).0 as u32,
          Some((register_value & 0x8000_0000) != 0),
        ),
        3 => (
          (register_value as i32).overflowing_shr(1).0 as u32
            | if gba.cpsr.carry_flag() { 0x8000_0000 } else { 0 },
          Some((register_value & 0x0000_0001) != 0),
        ),
        _ => unimplemented!("Impossible. We AND'ed 2 bits, there are 4 posibilities"),
      }
    } else {
      gba.debug_print_fn.map(|f| f(format!("shift_type:{}\n", shift_type).as_str()));
      match shift_type {
        0 => (
          register_value.overflowing_shl(shift_amount).0,
          if shift_amount == 0 {
            None
          } else {
            Some(((register_value >> (32 - shift_amount)) & 1) != 0)
          },
        ),
        1 => (
          register_value.overflowing_shr(shift_amount).0,
          if shift_amount != 0 {
            Some(((register_value >> (shift_amount - 1)) & 1) != 0)
          } else {
            None
          },
        ),
        2 => (
          (register_value as i32).overflowing_shr(shift_amount).0 as u32,
          if shift_amount != 0 {
            Some(((register_value >> (core::cmp::max(shift_amount, 32) - 1)) & 1) != 0)
          } else {
            None
          },
        ),
        3 => (
          register_value.rotate_right(shift_amount),
          if shift_amount != 0 {
            Some(((register_value.overflowing_shr(shift_amount - 1).0) & 1) != 0)
          } else {
            None
          },
        ),
        _ => unimplemented!("Impossible. We AND'ed 2 bits, there are 4 posibilities"),
      }
    };
    (op2, if shift_by_register && third_byte == 0 { None } else { shift_carry })
  };
  gba.debug_print_fn.map(|f| f(format!("op2:{}\n", op2).as_str()));
  let res = &mut gba.regs[rd];
  let cpsr = gba.cpsr;
  let ref_cpsr = &mut gba.cpsr;
  let mut set_all_flags = move |res: u32, carry: Option<bool>, overflow: Option<bool>| {
    if set_condition_flags {
      ref_cpsr.set_all_status_flags(res, carry, overflow);
    }
  };
  match operation as u8 {
    0 => {
      //AND
      *res = rn & op2;
      set_all_flags(*res, shift_carry, None);
    }
    1 => {
      //EOR/XOR
      *res = rn ^ op2;
      set_all_flags(*res, shift_carry, None);
    }
    2 => {
      //sub
      let (result, _overflow) = rn.overflowing_sub(op2);
      *res = result;
      set_all_flags(
        *res,
        Some(op2 <= rn),
        Some(((rn ^ op2) as i32) < 0 && ((op2 ^ result) as i32) >= 0),
      );
    }
    3 => {
      //rsb
      let (result, _overflow) = op2.overflowing_sub(rn);
      *res = result;
      set_all_flags(
        *res,
        Some(rn <= op2),
        Some(((op2 ^ rn) as i32) < 0 && ((rn ^ result) as i32) >= 0),
      );
    }
    4 => {
      //add
      let (result, overflow) = op2.overflowing_add(rn);
      *res = result;
      set_all_flags(
        result,
        Some(CPSR::addition_carries(result, op2, rn)),
        Some(((!(rn ^ result)) & rn ^ result) & 0x8000_0000 != 0),
      );
    }
    5 => {
      //adc
      let (result, overflow) = op2.overflowing_add(rn);
      let (result, overflow2) = result.overflowing_add(cpsr.carry_flag() as u32);
      *res = result;
      set_all_flags(
        result,
        Some(CPSR::addition_carries(result, op2, rn)),
        Some(overflow || overflow2), // TODO: This is probably wrong
      );
    }
    6 => {
      //sbc
      let (result, overflow) = rn.overflowing_sub(op2);
      let (result, overflow2) = result.overflowing_sub(1 - (cpsr.carry_flag() as u32));
      *res = result;
      set_all_flags(
        *res,
        Some(op2 < rn || (cpsr.carry_flag() && op2 == rn)),
        Some(overflow || overflow2),
      );
    }
    7 => {
      //rsc
      let (result, overflow) = op2.overflowing_sub(rn);
      let (result, overflow2) = result.overflowing_sub(1 - (cpsr.carry_flag() as u32));
      *res = result;
      set_all_flags(*res, Some(rn <= op2), Some(overflow || overflow2));
    }
    8 => {
      //tst
      let res = rn & op2;
      if rd == 15 {
        set_all_flags(res, None, None);
      } else {
        set_all_flags(res, shift_carry, None);
      }
    }
    9 => {
      //teq
      let res = rn ^ op2;
      if rd == 15 {
        set_all_flags(res, None, None);
      } else {
        set_all_flags(res, shift_carry, None);
      }
    }
    10 => {
      //cmp
      let (res, _) = rn.overflowing_sub(op2);
      // set_all_flags(res, Some(op2 <= rn), Some(v));
      set_all_flags(
        res,
        Some(op2 <= rn),
        Some(((rn ^ op2) as i32) < 0 && ((op2 ^ res) as i32) >= 0),
      );
    }
    11 => {
      //cmn
      let (res, overflow) = rn.overflowing_add(op2);
      set_all_flags(res, Some(CPSR::addition_carries(res, rn, op2)), Some(!overflow));
    }
    12 => {
      //or
      *res = rn | op2;
      set_all_flags(*res, shift_carry, None);
    }
    13 => {
      //mov
      *res = op2;
      set_all_flags(*res, shift_carry, None);
    }
    14 => {
      //bic/bit clear
      *res = rn & (!op2);
      set_all_flags(*res, shift_carry, None);
    }
    15 => {
      //not/mvn
      *res = !op2;
      set_all_flags(*res, shift_carry, None);
    }
    _ => unimplemented!("Impossible. We AND'ed 4 bits, there are 16 posibilities"),
  }
  // See http://problemkaputt.de/gbatek.htm#armopcodesdataprocessingalu
  // (1+p)S+rI+pN. Whereas r=1 if I=0 and R=1 (ie. shift by register); otherwise r=0. And p=1 if Rd=R15; otherwise p=0.
  gba.clocks += gba.sequential_cycle()
    + if !immediate && ((opcode & 0x0000_0010) != 0) { gba.internal_cycle() } else { 0 }
    + if rd == 15 { gba.sequential_cycle() + gba.nonsequential_cycle() } else { 0 };
  Ok(())
}
/// Move to Status Register (MSR)
fn move_to_status_register(gba: &mut GBA, opcode: u32) -> ARMResult {
  const FLAGS_MASK: u32 = 0xFF00_0000;
  const STATE_MASK: u32 = 0x0000_0020;
  const PRIVACY_MASK: u32 = 0x0000_00CF;
  let immediate = as_extra_flag(opcode);
  let (_, _, use_spsr, _, _) = as_flags(opcode);
  let change_flags_fields = (opcode & 0x0008_0000) != 0; // i.e overflow/zero/etc
  let change_control_fields = (opcode & 0x0001_0000) != 0; // mode, thumb, etc
  let operand = if immediate {
    let (_, _, shift, second, first) = as_u8_nibbles(opcode);
    (u32::from(first) + u32::from(second) * 16).rotate_right(2 * u32::from(shift))
  } else {
    gba.regs[(opcode & 0x0000_000F) as usize]
  };

  let current_mode = gba.cpsr.mode();
  if use_spsr {
    // If modifying the spsr we can directly modify it
    let spsr =
      gba.get_spsr_mut().ok_or("MSR with use_spsr in invalid mode (User or priviledged)")?;
    let old = spsr.0;
    let mut valid_bits_mask: u32 = (if change_control_fields { STATE_MASK } else { 0 })
      | (if change_flags_fields { FLAGS_MASK } else { 0 });
    valid_bits_mask &= FLAGS_MASK | STATE_MASK | PRIVACY_MASK;
    let invalid_bits_mask = !valid_bits_mask;
    // I *think* I don't need to check for priviledged mode since user
    // mode has no spsr
    *spsr = CPSR((old & invalid_bits_mask) | (operand & valid_bits_mask));
  } else {
    // If modifying the cpsr, we must call the GBA's set_mode
    // to change banks if necessary
    let old = gba.cpsr.0;
    let valid_bits_mask: u32 = (if change_control_fields { STATE_MASK } else { 0 })
      | (if change_flags_fields { FLAGS_MASK } else { 0 });
    let invalid_bits_mask = !valid_bits_mask;
    gba.cpsr = CPSR((old & invalid_bits_mask) | (operand & valid_bits_mask));
    if current_mode != CpuMode::User && change_control_fields {
      gba.set_mode(CpuMode::from_byte(((operand & 0x0000_000F) | 0x0000_0010) as u8));
      gba.cpsr = CPSR((gba.cpsr.0 & (!PRIVACY_MASK)) | (operand & PRIVACY_MASK));
    }
  };
  gba.clocks += gba.sequential_cycle();
  Ok(())
}
/// Move to register from status register (MRS)
fn move_to_register_from_status_register(gba: &mut GBA, opcode: u32) -> ARMResult {
  let (_, _, use_spsr, _, _) = as_flags(opcode);
  gba.regs[((opcode & 0x0000_F000) >> 12) as usize] = if use_spsr {
    gba
      .get_spsr_mut()
      .ok_or("Cannot use ^(PSR) on arm MRS while in User or Priviledged(System) mode")?
      .0
  } else {
    gba.cpsr.0
  };
  gba.clocks += gba.sequential_cycle();
  Ok(())
}
/// Coprocessor Data Transfer (CDT) (Unimplemented)
fn coprocessor_data_transfer(_: &mut GBA, _: u32) -> ARMResult {
  unimplemented!("Coprocessor instructions are not supported")
}
/// Coprocessor Data Operation (CDO) (Unimplemented)
fn coprocessor_data_operation(_: &mut GBA, _: u32) -> ARMResult {
  unimplemented!("Coprocessor instructions are not supported")
}
/// Coprocessor Register Transfer (CRT) (Unimplemented)
fn coprocessor_register_transfer(_: &mut GBA, _: u32) -> ARMResult {
  unimplemented!("Coprocessor instructions are not supported")
}

/// Map an opcode to an instruction (An fn(&mut GBA, u32))
///
/// Panics on undefined or invalid opcode.
fn decode_arm(opcode: u32) -> Result<ARMInstruction, ARMError> {
  let bits27_20 = (opcode >> 20) as u8;
  let bits11_4 = (opcode >> 4) as u8;
  const T: bool = true;
  const F: bool = false;
  Ok(match (bits27_20.as_bools(), bits11_4.as_bools()) {
    ([F, T, T, _, _, _, _, _], [_, _, _, _, _, _, _, T]) => unimplemented!(
      "Unimplemented: Undefined instruction {:b} ({:x}) bits 27-20: {:x} bits 11-4: {:x}",
      opcode,
      opcode,
      bits27_20,
      bits11_4 // TODO: Jump to undef handler
    ),
    ([F, T, _, _, _, _, _, _], _) => single_data_transfer,
    ([F, F, F, F, F, F, _, _], [_, _, _, _, T, F, F, T]) => multiply,
    ([F, F, F, F, T, _, _, _], [_, _, _, _, T, F, F, T]) => multiply_long,
    ([F, F, F, T, F, _, F, F], [F, F, F, F, T, F, F, T]) => single_data_swap,
    ([F, F, F, _, _, _, _, _], [F, F, F, F, T, _, _, T]) => {
      halfword_data_transfer_immediate_or_register_offset
    }
    ([T, F, F, _, _, _, _, _], _) => block_data_transfer,
    ([T, T, F, _, _, _, _, _], _) => coprocessor_data_transfer,
    ([T, T, T, F, _, _, _, _], [_, _, _, F, _, _, _, _]) => coprocessor_data_operation,
    ([T, T, T, F, _, _, _, _], [_, _, _, T, _, _, _, _]) => coprocessor_register_transfer,
    ([T, T, T, T, _, _, _, _], _) => software_interrupt,
    ([T, F, T, _, _, _, _, _], _) => branch_or_branch_and_link,
    ([F, F, F, T, F, F, T, F], [T, T, T, T, F, F, F, T])
      if (opcode & 0x000F_F000) == 0x000F_F000 =>
    {
      branch_and_exchange
    }
    ([F, F, _, T, F, _, F, F], [F, F, F, F, F, F, F, F]) => move_to_register_from_status_register,
    ([F, F, _, T, F, _, T, F], _) => move_to_status_register,
    ([F, F, _, _, _, _, _, _], _) => alu_operation,
  })
}

pub(crate) fn execute_one_instruction(gba: &mut GBA) -> ARMResult {
  let pc = gba.regs[15];
  let opcode = gba.fetch_u32(pc);
  gba.regs[15] += 4;
  (gba.instruction_hook_with_opcode)(gba, opcode);
  if check_cond(gba, opcode) {
    return Ok(());
  }
  let instruction = decode_arm(opcode)?;
  instruction(gba, opcode)?;
  Ok(())
}
