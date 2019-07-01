#![allow(non_snake_case)]
#![allow(non_camel_case_types)]
#![allow(dead_code)]
#![allow(unused_variables)]

use super::utils::AsBoolSlice;
use super::Cpu;

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

fn move_shifted_register(cpu: &mut Cpu, opcode: u16) {
    let operation = as_bits_11_and_12(opcode);
    let (_, _, rs, rd) = as_lower_3bit_values(opcode);
    let offset = as_bits_6_to_10(opcode);
    let operand = cpu.regs[rs];
    // TODO: Special 0 shifts
    cpu.regs[rd] = match operation {
        0 => operand << offset,
        1 => operand >> offset,
        2 => ((operand as i32) >> offset) as u32,
        3 => unimplemented!("reserved"),
        _ => unimplemented!("Impossible?"), //std::hint::unreachable_unchecked()
    };
    //TODO: Z=zeroflag, N=sign, C=carry (except LSL#0: C=unchanged), V=unchanged.
    cpu.clocks += 0; // TODO: clocks
}
/// ADD/SUB
fn add_or_sub(cpu: &mut Cpu, opcode: u16) {
    let (_, rn, rs, rd) = as_lower_3bit_values(opcode);
    let immediate = (opcode & 0x0400) != 0;
    let is_substraction = (opcode & 0x0200) != 0;
    let first_operand = cpu.regs[rs];
    let second_operand = if immediate { rn as u32 } else { cpu.regs[rn] };
    cpu.regs[rd] = if is_substraction {
        let (result, overflow) = first_operand.overflowing_sub(second_operand);
        cpu.cpsr.set_all_status_flags(
            cpu.regs[rd],
            Some(second_operand > first_operand),
            Some(overflow),
        );
        result
    } else {
        let (result, overflow) = first_operand.overflowing_add(second_operand);
        cpu.cpsr.set_all_status_flags_for_addition(
            cpu.regs[rd],
            first_operand,
            second_operand,
            Some(overflow),
        );
        result
    };
    cpu.clocks += 0; // TODO: clocks
}
/// Move, compare, add and substract immediate
fn immediate_operation(cpu: &mut Cpu, opcode: u16) {
    let rd = as_bits_8_to_10(opcode);
    let offset = u32::from(as_low_byte(opcode));
    let operation = as_bits_11_and_12(opcode);
    println!("{:x}", opcode);
    println!("{:x}", operation);
    let rd_val = cpu.regs[rd];
    if operation == 1 {
        // CMP handled separately since it doesnt store the result
        let (res, overflow) = rd_val.overflowing_sub(offset);
        cpu.cpsr.set_all_status_flags(res, Some(offset <= rd_val), Some(overflow));
        cpu.clocks += 0; // TODO: clocks
        return;
    }
    dbg!(rd);
    println!("{:x}", offset);
    cpu.regs[rd] = match operation {
        0 => {
            // MOV
            cpu.cpsr.set_all_status_flags(offset, None, None);
            offset
        }
        2 => {
            // ADD
            let (res, overflow) = rd_val.overflowing_add(offset);
            cpu.cpsr.set_all_status_flags_for_addition(res, rd_val, offset, Some(overflow));
            res
        }
        3 => {
            // SUB
            let (res, overflow) = rd_val.overflowing_sub(offset);
            cpu.cpsr.set_all_status_flags(res, Some(offset <= rd_val), Some(overflow));
            res
        }
        _ => unimplemented!(), //std::hint::unreachable_unchecked()
    };
    cpu.clocks += 0; // TODO: clocks
}
fn carry_from(op1: u32, op2: u32, result: u32) -> bool {
    ((op1 >> 31) + (op2 >> 31)) > (result >> 31)
}
fn borrow_from(positive: u32, negative: u32, result: u32) -> bool {
    positive >= negative
}
fn sign_flag(N: u32) -> bool {
    (N >> 31) == 1
}
fn alu_operation(cpu: &mut Cpu, opcode: u16) {
    let (_, _, rs, rd) = as_lower_3bit_values(opcode);
    let operation = ((opcode >> 6) & 0x000F) as u8;
    let (rs_val, rd_val) = (cpu.regs[rs], cpu.regs[rd]);
    match operation {
        0x0 => {
            //AND
            let res = rd_val & rs_val;
            cpu.regs[rd] = res;
            cpu.cpsr.set_all_status_flags(res, None, None);
        }
        0x1 => {
            //EOR/XOR
            let res = rd_val ^ rs_val;
            cpu.regs[rd] = res;
            cpu.cpsr.set_all_status_flags(res, None, None);
        }
        0x2 => {
            //LSL (Logical Shift Left)
            let rs_val = rs_val & 0x00FF;
            let (result, _) = rd_val.overflowing_shl(rs_val);
            cpu.regs[rd] = result;
            cpu.cpsr.set_all_status_flags(
                result,
                if rs_val == 0 { None } else { Some((rd_val >> (32 - rs_val)) & 1 != 0) },
                None,
            );
        }
        0x3 => {
            //LSR (Logical Shift Right)
            let rs_val = rs_val & 0x00FF;
            let result = rd_val >> rs_val;
            cpu.regs[rd] = result;
            cpu.cpsr.set_all_status_flags(
                result,
                if rs_val == 0 { None } else { Some((rd_val >> (rs_val - 1)) & 1 != 0) },
                None,
            );
        }
        0x4 => {
            //ASR (Arithmetic Shift Right)
            let rs_val = rs_val & 0x00FF;
            let result = ((rd_val as i32) << rs_val) as u32;
            cpu.regs[rd] = result;
            cpu.cpsr.set_all_status_flags(
                result,
                if rs_val == 0 { None } else { Some((rd_val >> (rs_val - 1)) & 1 != 0) },
                None,
            );
        }
        0x5 => {
            // ADC (Add With Carry)
            let (result, overflow) = rd_val.overflowing_add(rs_val);
            let (result, overflow2) = result.overflowing_add(cpu.cpsr.C() as u32);
            cpu.regs[rd] = result;
            cpu.cpsr.set_all_status_flags(
                result,
                Some(carry_from(rd_val, rs_val, result)),
                Some(overflow || overflow2),
            );
        }
        0x6 => {
            // SBC (Substract With Carry)
            let (result, overflow) = rd_val.overflowing_sub(rs_val);
            let (result, overflow2) = result.overflowing_sub((!cpu.cpsr.C()) as u32);
            cpu.regs[rd] = result;
            cpu.cpsr.set_all_status_flags(
                result,
                Some(borrow_from(rd_val, rs_val + ((!cpu.cpsr.C()) as u32), result)),
                Some(overflow || overflow2),
            );
        }
        0x7 => {
            // ROR (Rotate Right)
            let rs_val = rs_val & 0x00FF;
            if rs_val == 0 {
                cpu.cpsr.set_neutral_flags(rd_val);
            } else {
                let r4 = rs_val & 0x1F;
                if r4 > 0 {
                    let result = rd_val.rotate_right(r4);
                    cpu.regs[rd] = result;
                    cpu.cpsr.set_all_status_flags(
                        result,
                        Some(((rd_val >> (r4 - 1)) & 1) != 0),
                        None,
                    );
                } else {
                    cpu.cpsr.set_neutral_flags(rd_val);
                    cpu.cpsr.set_C((rd_val >> 31) != 0);
                }
            }
        }
        0x8 => {
            // TST (Test)
            cpu.cpsr.set_all_status_flags(rd_val & rs_val, None, None);
        }
        0x9 => {
            // NEG (Negate)
            let (result, overflow) = 0u32.overflowing_sub(rs_val);
            cpu.regs[rd] = result;
            cpu.cpsr.set_all_status_flags(
                result,
                Some(borrow_from(0, rs_val, result)),
                Some(overflow),
            );
        }
        0xA => {
            // CMP (Compare)
            let (res, overflow) = rd_val.overflowing_sub(rs_val);
            cpu.cpsr.set_all_status_flags(res, Some(rs_val > rd_val), Some(overflow));
        }
        0xB => {
            // CMN (Compare Negative)
            let (res, overflow) = rd_val.overflowing_add(rs_val);
            cpu.cpsr.set_all_status_flags(
                res,
                Some(carry_from(rd_val, rs_val, res)),
                Some(overflow),
            );
        }
        0xC => {
            // ORR (Logical OR)
            let res = rd_val | rs_val;
            cpu.regs[rd] = res;
            cpu.cpsr.set_all_status_flags(res, None, None);
        }
        0xD => {
            // MUL (Multiply)
            let (res, overflow) = rd_val.overflowing_mul(rs_val);
            cpu.regs[rd] = res;
            //TODO: Is overflow right there as carry?
            cpu.cpsr.set_all_status_flags(res, Some(false), None);
        }
        0xE => {
            // BIC (Bit clear)
            let res = rd_val & (!rs_val);
            cpu.regs[rd] = res;
            cpu.cpsr.set_all_status_flags(res, None, None);
        }
        0xF => {
            // MVN (Not)
            let res = !rs_val;
            cpu.regs[rd] = res;
            cpu.cpsr.set_all_status_flags(res, None, None);
        }
        _ => unimplemented!("Impossible"),
    }
    cpu.clocks += 0; // TODO: clocks
}
/// HiReg/BX
fn high_register_operations_or_bx(cpu: &mut Cpu, opcode: u16) {
    let (_, _, rs, rd) = as_lower_3bit_values(opcode);
    let msbs = (opcode & 0x0040) >> 6;
    let msbd = (opcode & 0x0080) >> 7;
    let operation = (opcode & 0x0300) >> 8;
    let rs = rs + ((msbs as usize) << 3);
    let rd = rd + ((msbd as usize) << 3);
    match operation {
        0 => {
            // ADD
            let (res, _) = cpu.regs[rd].overflowing_add(cpu.regs[rs]);
            cpu.regs[rd] = res;
        }
        1 => {
            //CMP
            let (op1, op2) = (cpu.regs[rd], cpu.regs[rs]);
            let (res, overflow) = op1.overflowing_sub(op2);
            // TODO: Is this check right?
            cpu.cpsr.set_all_status_flags(res, Some(op2 > op1), Some(overflow));
        }
        2 => {
            // MOV
            cpu.regs[rd] = if rs == 15 { cpu.regs[15] + 2 } else { cpu.regs[rs] };
        }
        3 => {
            // Ignore BLX since it's ARMv9
            // BX
            // Change to ARM mode if bit 0 is 0
            let rs_val = cpu.regs[rs];
            cpu.cpsr.set_T((rs_val & 0x0000_0001) != 0);
            if rs == 15 {
                cpu.regs[15] = rs_val & 0xFFFF_FFFC;
            } else {
                cpu.regs[15] = rs_val & 0xFFFF_FFFE;
            }
            return;
        }
        _ => unimplemented!("Impossible"), //std::hint::unreachable_unchecked()
    }
    cpu.clocks += 0; // TODO: clocks
}
/// LDR PC
fn pc_relative_load(cpu: &mut Cpu, opcode: u16) {
    let rd = as_bits_8_to_10(opcode);
    let byte = u32::from(as_low_byte(opcode));
    let offset = byte << 2; // In steps of 4
    cpu.regs[rd] = cpu.fetch_u32(((cpu.regs[15] + 2) & 0xFFFF_FFFC).overflowing_add(offset).0);
    cpu.clocks += 0; // TODO: clocks
}
/// LDR/STR
fn load_or_store_with_relative_offset(cpu: &mut Cpu, opcode: u16) {
    let (_, ro, rb, rd) = as_lower_3bit_values(opcode);
    let is_load = as_11th_bit(opcode);
    let is_byte_else_word = (opcode & 0x0400) != 0;
    let (addr, _) = cpu.regs[ro].overflowing_add(cpu.regs[rb]);
    if is_load {
        if is_byte_else_word {
            // TODO: Does this zero extend the 32bit value?
            cpu.write_u8(addr, cpu.regs[rd] as u8);
        } else {
            cpu.write_u32(addr, cpu.regs[rd]);
        }
    } else {
        cpu.regs[rd] =
            if is_byte_else_word { u32::from(cpu.fetch_byte(addr)) } else { cpu.fetch_u32(addr) };
    }
    cpu.clocks += 0; // TODO: clocks
}
/// LDR/STR H/SB/SH
fn load_or_store_sign_extended_byte_or_halfword(cpu: &mut Cpu, opcode: u16) {
    let H = as_11th_bit(opcode);
    let (_, ro, rb, rd) = as_lower_3bit_values(opcode);
    let sign_extend = (opcode & 0x0400) != 0;
    let addr = cpu.regs[rb].overflowing_add(cpu.regs[ro]).0;
    if !H && !sign_extend {
        cpu.write_u16(addr, cpu.regs[rd] as u16);
        cpu.clocks += 0; // TODO: clocks
        return;
    }
    cpu.regs[rd] = if H {
        // Half-word(16 bits)
        if sign_extend {
            (i32::from(cpu.fetch_u16(addr) as i16) as u32) // Sign extend
        } else {
            u32::from(cpu.fetch_u16(addr))
        }
    } else {
        (i32::from(cpu.fetch_byte(addr) as i8) as u32) // Sign extend
    };
    cpu.clocks += 0; // TODO: clocks
}
/// LDR/STR {B}
fn load_or_store_with_immediate_offset(cpu: &mut Cpu, opcode: u16) {
    let is_byte_else_word = (opcode & 0x1000) != 0;
    let is_load_else_store = as_11th_bit(opcode);
    let (_, _, rb, rd) = as_lower_3bit_values(opcode);
    let offset = u32::from(as_bits_6_to_10(opcode));
    let rb_val = cpu.regs[rb];
    if is_byte_else_word {
        let addr = offset + rb_val;
        if is_load_else_store {
            cpu.regs[rd] = u32::from(cpu.fetch_byte(addr));
        } else {
            cpu.write_u8(addr, cpu.regs[rd] as u8);
        }
    } else {
        let offset = offset << 2;
        let addr = offset + rb_val;
        if is_load_else_store {
            cpu.regs[rd] = cpu.fetch_u32(addr);
        } else {
            cpu.write_u32(addr, cpu.regs[rd]);
        }
    }
    cpu.clocks += 0; // TODO: clocks
}
/// LDR/STR {H}
fn load_or_store_halfword(cpu: &mut Cpu, opcode: u16) {
    let is_load_else_store = as_11th_bit(opcode);
    let (_, _, rb, rd) = as_lower_3bit_values(opcode);
    let offset = as_bits_6_to_10(opcode);
    let offset = u32::from(offset) << 1;
    let addr = offset + cpu.regs[rb];
    if is_load_else_store {
        cpu.regs[rd] = u32::from(cpu.fetch_u16(addr));
    } else {
        cpu.write_u16(addr, cpu.regs[rd] as u16);
    }
    cpu.clocks += 0; // TODO: clocks
}
/// LDR/STR SP
fn stack_pointer_relative_load_or_store(cpu: &mut Cpu, opcode: u16) {
    let is_load_else_store = as_11th_bit(opcode);
    let byte = as_low_byte(opcode);
    let offset = u32::from(byte) << 2;
    let rd = as_bits_8_to_10(opcode);
    let addr = cpu.regs[13] + offset;
    if is_load_else_store {
        cpu.regs[rd] = cpu.fetch_u32(addr);
    } else {
        cpu.write_u32(addr, cpu.regs[rd]);
    }
    cpu.clocks += 0; // TODO: clocks
}
/// LOAD PC/SP
fn load_address(cpu: &mut Cpu, opcode: u16) {
    let load_sp_else_pc = as_11th_bit(opcode);
    let byte = as_low_byte(opcode);
    let nn = u32::from(byte) << 2;
    let rd = as_bits_8_to_10(opcode);
    cpu.regs[rd] = if load_sp_else_pc {
        cpu.regs[13] + nn
    } else {
        //TODO: Is this right????
        ((cpu.regs[15] + 4) & !2) + nn
    };
    cpu.clocks += 0; // TODO: clocks
}
/// ADD/SUB SP,nn
fn add_or_sub_offset_to_stack_pointer(cpu: &mut Cpu, opcode: u16) {
    let byte = as_low_byte(opcode);
    let substract_else_add = (byte & 0x80) != 0;
    let byte7 = byte & 0x0000_007F;
    let offset = u32::from(byte7) << 2;
    dbg!(offset);
    let sp = cpu.regs[13];
    cpu.regs[13] = if substract_else_add {
        sp.overflowing_sub(offset).0
    } else {
        sp.overflowing_add(offset).0
    };
    cpu.clocks += 0; // TODO: clocks
}
/// PUSH/POP
fn push_or_pop(cpu: &mut Cpu, opcode: u16) {
    let is_pop_else_push = as_11th_bit(opcode);
    // PUSH LR or POP PC
    let pc_or_lr_flag = (opcode & 0x0100) != 0;
    let rlist = as_low_byte(opcode);
    let mut sp = cpu.regs[13];
    if is_pop_else_push {
        // TODO: Does this need reversing?
        for (idx, _) in rlist.as_bools().iter().enumerate().filter(|(idx, &boolean)| boolean) {
            cpu.regs[7 - idx] = cpu.fetch_u32(sp);
            sp += 4;
        }
        if pc_or_lr_flag {
            cpu.regs[15] = cpu.fetch_u32(sp); // POP PC
            sp += 4;
        }
    } else {
        // TODO: Does this need reversing?
        for (idx, _) in rlist.as_bools().iter().enumerate().filter(|(idx, &boolean)| boolean) {
            cpu.write_u32(sp, cpu.regs[7 - idx]);
            dbg!(7 - idx);
            sp -= 4;
        }
        if pc_or_lr_flag {
            dbg!("LR");
            cpu.write_u32(sp, cpu.regs[14]); // PUSH LR
            sp -= 4;
        }
    }
    cpu.regs[13] = sp;
    cpu.clocks += 0; // TODO: clocks
}
/// STM/LDM
fn multiple_loads_or_stores(cpu: &mut Cpu, opcode: u16) {
    let rb = as_bits_8_to_10(opcode);
    let is_load_else_store = as_11th_bit(opcode);
    let rlist = as_low_byte(opcode);
    let mut addr = cpu.regs[rb];
    // TODO: Should this be reversed?
    if is_load_else_store {
        for (idx, _) in rlist.as_bools().iter().enumerate().filter(|(idx, &boolean)| boolean) {
            cpu.write_u32(addr, cpu.regs[7 - idx]);
            addr += 4;
        }
    } else {
        for (idx, _) in rlist.as_bools().iter().enumerate().filter(|(idx, &boolean)| boolean) {
            cpu.regs[7 - idx] = cpu.fetch_u32(addr);
            addr += 4;
        }
    }
    cpu.clocks += 0; // TODO: clocks
}
/// B{COND}
fn conditional_branch(cpu: &mut Cpu, opcode: u16) {
    let offset = u32::from(as_low_byte(opcode)) << 1;
    dbg!(offset);
    println!("{:x}", offset);
    let (is_negative, offset) = ((offset * 0x0000_0100) != 0, offset & 0x0000_00FE);
    dbg!(offset);
    println!("{:x}", offset);
    let cond = (opcode << 4) & 0xF000;
    let should_not_jump = super::arm::check_cond(cpu, u32::from(cond) << 16);
    dbg!(should_not_jump);
    dbg!(is_negative);
    if should_not_jump {
        cpu.clocks += 0; // TODO: clocks
        return;
    }
    cpu.regs[15] = if is_negative {
        println!("{:x}", (!offset) & 0x0000_00FE);
        cpu.regs[15].overflowing_sub((!offset) & 0x0000_00FE).0
    } else {
        println!("{:x}", offset);
        cpu.regs[15].overflowing_add(offset + 2).0
    };
    cpu.clocks += 0; // TODO: clocks
}
/// SWI
fn software_interrupt(cpu: &mut Cpu, opcode: u16) {
    super::arm::SWI(cpu, u32::from(opcode) << 16);
    cpu.clocks += 0; // TODO: clocks
}
/// B
fn branch(cpu: &mut Cpu, opcode: u16) {
    dbg!("branch");
    //let offset = (opcode & 0x01FF) as u32;
    let offset = u32::from(as_low_11bits(opcode)) << 1;
    println!("{:x}", offset);
    let is_negative = (opcode & 0x0200) != 0;
    dbg!(is_negative);
    let pc = (*cpu.pc()).overflowing_add(2).0;
    *cpu.pc() =
        if is_negative { pc.overflowing_sub(offset).0 } else { pc.overflowing_add(offset).0 };
    cpu.clocks += 0; // TODO: clocks
}
/// BL/BLX
///
/// This is actually a 32 bit instruction (2 thumb instructions).
///
/// The first one has the upper 11 bits and the second the lower 11
fn branch_and_link_or_link_and_exchange(cpu: &mut Cpu, opcode: u16) {
    //let H = as_11th_bit(opcode);// I *think* this is not needed since it's ARM9 (BLX)
    let upper_offset = (i32::from((as_low_11bits(opcode) as i16) << 5) << 7) as u32;
    let pc = cpu.regs[15];
    cpu.regs[14] = pc.overflowing_add(upper_offset).0.overflowing_add(2).0;
    let next_instruction = cpu.fetch_u16(pc);
    cpu.regs[15] += 2;
    let lower_offset = u32::from(as_low_11bits(next_instruction) << 1);
    let pc = cpu.regs[15] + 2;
    cpu.regs[15] = cpu.regs[14] + lower_offset;
    cpu.regs[14] = pc - 1;
    cpu.clocks += 0; // TODO: clocks
}
fn decode_thumb(opcode: u16) -> fn(&mut Cpu, u16) -> () {
    let bits15_8 = (opcode >> 8) as u8;
    const T: bool = true;
    const F: bool = false;
    match bits15_8.as_bools() {
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
        [T, T, T, T, _, _, _, _] => branch_and_link_or_link_and_exchange,
        _ => unimplemented!(),
    }
}

pub(crate) fn execute_one_instruction(cpu: &mut Cpu) {
    let pc = *cpu.pc();
    let opcode = cpu.fetch_u16(pc);
    *cpu.pc() += 2;
    println!("{:x}", opcode);
    let instruction = decode_thumb(opcode);
    instruction(cpu, opcode);
}

#[cfg(test)]
mod tests {
    use super::AsBoolSlice;
    use super::*;

    extern crate test;
    use test::Bencher;
    fn none(cpu: &mut Cpu, x: u16) {}
    fn decode_thumb(opcode: u16) -> fn(&mut Cpu, u16) -> () {
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
            [T, T, T, T, _, _, _, _] => branch_and_link_or_link_and_exchange,
            _ => none,
        }
    }
    #[allow(clippy::identity_op)]
    fn decode_thumb_raw(opcode: u16) -> fn(&mut Cpu, u16) -> () {
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
            x if (x & 0b1111_0000) == 0b1111_0000 => branch_and_link_or_link_and_exchange,
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
