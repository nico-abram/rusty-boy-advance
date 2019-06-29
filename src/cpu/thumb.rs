#![allow(non_snake_case)]
#![allow(non_camel_case_types)]
#![allow(dead_code)]
#![allow(unused_variables)]

use super::utils::AsBoolSlice;
use super::Cpu;

fn as_11th_bit(opcode: u16) -> bool {
    (opcode & 0x0800) != 0
}
/// 11 bit value (0-1023)
fn as_low_11bits(opcode: u16) -> u16 {
    opcode & 0x07FF
}
/// 3 bit value (0-7)
fn as_bits_8_to_10(opcode: u16) -> usize {
    ((opcode >> 8) & 0x0003) as usize
}
/// 5 bit value (0-31)
fn as_bits_6_to_10(opcode: u16) -> u16 {
    (opcode >> 6) & 0x000F
}
/// 3 bit values (0-7)
/// Since these are often used as register numbers we return them as usizes
fn as_lower_3bit_values(opcode: u16) -> (usize, usize, usize, usize) {
    (
        ((opcode & 0x0E00) >> 9) as usize,
        ((opcode & 0x01C0) >> 6) as usize,
        ((opcode & 0x0038) >> 3) as usize,
        ((opcode & 0x0007) >> 0) as usize,
    )
}
/// 8 bit value
fn as_low_word(opcode: u16) -> u8 {
    opcode as u8
}
/// 2 bit value (0-3)
fn as_bits_11_and_12(opcode: u16) -> u16 {
    (opcode >> 10) & 0x3
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
    let offset = as_low_word(opcode) as u32;
    let operation = as_bits_11_and_12(opcode);
    let rd_val = cpu.regs[rd];
    if operation == 1 {
        // CMP handled separately since it doesnt store the result
        let (res, overflow) = rd_val.overflowing_sub(offset);
        cpu.cpsr.set_all_status_flags(res, Some(offset <= rd_val), Some(overflow));
        cpu.clocks += 0; // TODO: clocks
        return;
    }
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
fn alu_operation(cpu: &mut Cpu, opcode: u16) {
    let (_, _, rs, rd) = as_lower_3bit_values(opcode);
    let operation = (opcode >> 6) & 0x000F;
    unimplemented!()
}
/// HiReg/BX
fn high_register_operations_or_bx(cpu: &mut Cpu, opcode: u16) {
    let (_, _, rs, rd) = as_lower_3bit_values(opcode);
    let operation = opcode & 0x0300;
    let h1 = (opcode & 0x0080) != 0;
    let h2 = (opcode & 0x0040) != 0;
    unimplemented!()
}
/// LDR PC
fn pc_relative_load(cpu: &mut Cpu, opcode: u16) {
    let rd = as_bits_8_to_10(opcode);
    let word = as_low_word(opcode) as u32;
    let word = word * 4; // In steps of 4
    cpu.regs[rd] = cpu.fetch_u32(cpu.regs[15] + word);
    cpu.clocks += 0; // TODO: clocks
}
/// LDR/STR
fn load_or_store_with_relative_offset(cpu: &mut Cpu, opcode: u16) {
    let (_, ro, rb, rd) = as_lower_3bit_values(opcode);
    let is_load = as_11th_bit(opcode);
    let is_byte = (opcode & 0x0400) != 0; // else word(32bit)
    dbg!(ro);
    dbg!(rb);
    println!("{:b}", opcode);
    println!("{:x?}", opcode);
    println!("{:x?}", cpu.regs[ro]);
    println!("{:x?}", cpu.regs[rb]);
    dbg!(cpu.regs[ro]);
    dbg!(cpu.regs[rb]);
    dbg!(rd);
    let (addr, _) = cpu.regs[ro].overflowing_add(cpu.regs[rb]);
    println!("{:x?}", addr);
    println!("{}", addr);
    println!("{}", is_load);
    if is_load {
        if is_byte {
            // TODO: Does this zero extend the 32bit value?
            cpu.write_u8(addr, cpu.regs[rd] as u8);
        } else {
            cpu.write_u32(addr, cpu.regs[rd]);
        }
    } else {
        cpu.regs[rd] = if is_byte { cpu.fetch_byte(addr) as u32 } else { cpu.fetch_u32(addr) };
    }
    cpu.clocks += 0; // TODO: clocks
}
/// LDR/STR H/SB/SH
fn load_or_store_sign_extended_byte_or_halfword(cpu: &mut Cpu, opcode: u16) {
    let H = as_11th_bit(opcode);
    let (_, ro, rb, rd) = as_lower_3bit_values(opcode);
    let S = (opcode & 0x0400) != 0;
    unimplemented!()
}
/// LDR/STR {B}
fn load_or_store_with_immediate_offset(cpu: &mut Cpu, opcode: u16) {
    let B = (opcode & 0x1000) != 0;
    let L = as_11th_bit(opcode);
    let (_, _, rb, rd) = as_lower_3bit_values(opcode);
    let offset = as_bits_6_to_10(opcode);
    unimplemented!()
}
/// LDR/STR {H}
fn load_or_store_halfword(cpu: &mut Cpu, opcode: u16) {
    let L = as_11th_bit(opcode);
    let (_, _, rb, rd) = as_lower_3bit_values(opcode);
    let offset = as_bits_6_to_10(opcode);
    unimplemented!()
}
/// LDR/STR SP
fn stack_pointer_relative_load_or_store(cpu: &mut Cpu, opcode: u16) {
    let L = as_11th_bit(opcode);
    let word = as_low_word(opcode);
    let rd = as_bits_8_to_10(opcode);
    unimplemented!()
}
/// LOAD PC/SP
fn load_address(cpu: &mut Cpu, opcode: u16) {
    let SP = as_11th_bit(opcode);
    let word = as_low_word(opcode);
    let rd = as_bits_8_to_10(opcode);
    unimplemented!()
}
/// ADD SP,nn
fn add_offset_to_stack_pointer(cpu: &mut Cpu, opcode: u16) {
    let word = as_low_word(opcode);
    let S = (word & 0x80) != 0;
    let word7 = word >> 1;
    unimplemented!()
}
/// PUSH/POP
fn push_or_pop(cpu: &mut Cpu, opcode: u16) {
    let L = as_11th_bit(opcode);
    let R = (opcode & 0x0100) != 0;
    let rlist = as_low_word(opcode);
    unimplemented!()
}
/// STM/LDM
fn multiple_loads_or_stores(cpu: &mut Cpu, opcode: u16) {
    let rb = as_bits_8_to_10(opcode);
    let L = as_11th_bit(opcode);
    let rlist = as_low_word(opcode);
    unimplemented!()
}
/// B{COND}
fn conditional_branch(cpu: &mut Cpu, opcode: u16) {
    let offset = as_low_word(opcode);
    let cond = (opcode >> 8) & 0x000F;
    unimplemented!()
}
/// SWI
fn software_interrupt(cpu: &mut Cpu, opcode: u16) {
    let user_data = as_low_word(opcode);
    unimplemented!()
}
/// B
fn branch(cpu: &mut Cpu, opcode: u16) {
    let offset = (opcode & 0x01FF) as u32;
    let offset = offset << 1;
    let is_negative = (opcode & 0x0200) != 0;
    let pc = *cpu.pc();
    *cpu.pc() = if is_negative { pc - offset } else { pc + offset };
    cpu.clocks += 0; // TODO: clocks
}
/// BL/BLX
///
/// This is actually a 32 bit instruction (2 thumb instructions).
///
/// The first one has the upper 11 bits and the second the lower 11
fn branch_and_link_or_link_and_exchange(cpu: &mut Cpu, opcode: u16) {
    //let H = as_11th_bit(opcode);// I *think* this is not needed since it's ARM9 (BLX)
    let upper_offset = as_low_11bits(opcode) as u32;
    let pc = *cpu.pc();
    let next_instruction = cpu.fetch_u16(pc);
    let lower_offset = as_low_11bits(next_instruction) as u32;
    cpu.regs[14] = (pc + 2) | 1u32;
    *cpu.pc() = pc + 2 + (lower_offset << 1) + (upper_offset << 12);
    cpu.clocks += 0; // TODO: clocks
}
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
        [T, F, T, T, F, F, F, F] => add_offset_to_stack_pointer,
        [T, F, T, T, _, T, F, _] => push_or_pop,
        [T, T, F, F, _, _, _, _] => multiple_loads_or_stores,
        [T, T, F, T, T, T, T, T] => software_interrupt,
        [T, T, F, T, _, _, _, _] => conditional_branch,
        [T, T, T, F, F, _, _, _] => branch,
        [T, T, T, T, _, _, _, _] => branch_and_link_or_link_and_exchange,
        //TODO: the rest
        _ => unimplemented!(),
    }
}

pub(crate) fn execute_one_instruction(cpu: &mut Cpu) {
    let pc = *cpu.pc();
    let opcode = cpu.fetch_u16(pc);
    *cpu.pc() += 2;
    println!("{:x}", opcode);
    /*
    if check_cond(cpu, opcode) {
      return;
    }
    */
    let instruction = decode_thumb(opcode);
    instruction(cpu, opcode);
}
