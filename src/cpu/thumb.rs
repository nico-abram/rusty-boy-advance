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
fn as_bits_8_to_10(opcode: u16) -> u16 {
    (opcode >> 8) & 0x0003
}
/// 5 bit value (0-31)
fn as_bits_6_to_10(opcode: u16) -> u16 {
    (opcode >> 6) & 0x000F
}
/// 3 bit values (0-7)
fn as_lower_3bit_values(opcode: u16) -> (u8, u8, u8, u8) {
    (
        (opcode & 0x0E00) as u8,
        (opcode & 0x01C0) as u8,
        (opcode & 0x0038) as u8,
        (opcode & 0x0007) as u8,
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
    unimplemented!()
}
/// ADD/SUB
fn add_or_sub(cpu: &mut Cpu, opcode: u16) {
    let (_, rn, rs, rd) = as_lower_3bit_values(opcode);
    let operation = (opcode & 0x0200) != 0;
    unimplemented!()
}
/// Move, compare, add and substract immediate
fn immediate_operation(cpu: &mut Cpu, opcode: u16) {
    let rd = as_bits_8_to_10(opcode);
    let offset = as_low_word(opcode);
    let operation = as_bits_11_and_12(opcode);
    unimplemented!()
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
    let word = as_low_word(opcode);
    unimplemented!()
}
/// LDR/STR
fn load_or_store_with_relative_offset(cpu: &mut Cpu, opcode: u16) {
    let (_, ro, rb, rd) = as_lower_3bit_values(opcode);
    let L = as_11th_bit(opcode);
    let B = (opcode & 0x0400) != 0;
    unimplemented!()
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
    let offset = as_low_11bits(opcode);
    unimplemented!()
}
/// BL/BLX
fn branch_and_link_or_link_and_exchange(cpu: &mut Cpu, opcode: u16) {
    let offset = as_low_11bits(opcode);
    let H = as_11th_bit(opcode);
    unimplemented!()
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
