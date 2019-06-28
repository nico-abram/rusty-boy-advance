#![allow(non_snake_case)]
#![allow(non_camel_case_types)]
//split
#![allow(dead_code)]
#![allow(unused_variables)]

use super::utils::AsBoolSlice;
use super::Cpu;

fn low_11bits(opcode: u16) -> u16 {
    unimplemented!()
}
fn as_3bit_values(opcode: u16) -> (u8, u8, u8, u8) {
    unimplemented!()
}
fn low_word(opcode: u16) -> u8 {
    unimplemented!()
}

fn decode_thumb(opcode: u16) -> fn(&mut Cpu, u16) -> () {
    let bits15_8 = (opcode >> 8) as u8;
    const T: bool = true;
    const F: bool = false;
    match bits15_8.as_bools() {
        [F, F, F, T, T, _, _, _] => unimplemented!("add/sub"),
        [F, F, F, _, _, _, _, _] => unimplemented!("shifted"),
        [F, F, T, _, _, _, _, _] => unimplemented!("Immedi"),
        [F, T, F, F, F, F, _, _] => unimplemented!("AluOp"),
        [F, T, F, F, F, T, _, _] => unimplemented!("HiReg/BX"),
        [F, T, F, F, T, _, _, _] => unimplemented!("LDR PC"),
        [F, T, F, T, _, _, F, _] => unimplemented!("LDR/STR"),
        [F, T, F, T, _, _, T, _] => unimplemented!("H/SB/SH"),
        [F, T, T, _, _, _, _, _] => unimplemented!("{{B}}"),
        [T, F, F, F, _, _, _, _] => unimplemented!("{{H}}"),
        [T, F, F, T, _, _, _, _] => unimplemented!("SP"),
        [T, F, T, F, _, _, _, _] => unimplemented!("ADD PC/SP"),
        [T, F, T, T, F, F, F, F] => unimplemented!("ADD SP,nn"),
        [T, F, T, T, _, T, F, _] => unimplemented!("PUSH/POP"),
        [T, T, F, F, _, _, _, _] => unimplemented!("STM/LDM"),
        [T, T, F, T, T, T, T, T] => unimplemented!("SWI"),
        [T, T, F, T, _, _, _, _] => unimplemented!("B{{cond}}"),
        [T, T, T, F, F, _, _, _] => unimplemented!("B"),
        [T, T, T, T, _, _, _, _] => unimplemented!("BL,BLX"),
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
