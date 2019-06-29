#![allow(non_snake_case)]
#![allow(non_camel_case_types)]
#![allow(dead_code)]
#![allow(unused_variables)]

use super::cpsr::CPSR;
use super::utils::AsBoolSlice;
use super::{Cpu, CpuMode, SWI_HANDLER};

/// Print debug value as hex
macro_rules! dbgx {
    ($val:expr) => {
        dbg!(format!("{:x?}", $val))
    };
}

enum Cond {
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
fn opcode_to_cond(opcode: u32) -> Cond {
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
        _ => panic!("This is impossible"), //std::hint::unreachable_unchecked()
    }
}
/// Check if the condition for the given opcode is true
fn check_cond(cpu: &mut Cpu, opcode: u32) -> bool {
    let applies = match opcode_to_cond(opcode) {
        Cond::EQ => cpu.cpsr.Z(),
        Cond::NE => !cpu.cpsr.Z(),
        Cond::CS_HS => cpu.cpsr.C(),
        Cond::CC_LO => !cpu.cpsr.C(),
        Cond::MI => cpu.cpsr.N(),
        Cond::PL => !cpu.cpsr.N(),
        Cond::VS => cpu.cpsr.V(),
        Cond::VC => !cpu.cpsr.V(),
        Cond::HI => cpu.cpsr.C() && !cpu.cpsr.Z(),
        Cond::LS => !cpu.cpsr.C() || cpu.cpsr.Z(),
        Cond::GE => cpu.cpsr.N() == cpu.cpsr.V(),
        Cond::LT => cpu.cpsr.N() != cpu.cpsr.V(),
        Cond::GT => cpu.cpsr.N() == cpu.cpsr.V() && !cpu.cpsr.Z(),
        Cond::LE => cpu.cpsr.N() != cpu.cpsr.V() && cpu.cpsr.Z(),
        Cond::AL => true,
        Cond::NV => false,
    };
    if applies {
        cpu.clocks += 1;
    }
    !applies
}

/// Get the lower 5 nibbles (2.5 bytes) as bytes
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
fn as_usize_nibbles(opcode: u32) -> (usize, usize, usize, usize, usize) {
    let (b0, b1, b2, b3, b4) = as_u8_nibbles(opcode);
    (b0 as usize, b1 as usize, b2 as usize, b3 as usize, b4 as usize)
}
/// Get bits 23-27 as bools
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
fn as_extra_flag(opcode: u32) -> bool {
    (opcode & 0x0200_0000) != 0
}
/// Branch and Exchange
/// If the condition is true, branch and switch mode
fn BX(cpu: &mut Cpu, opcode: u32) {
    cpu.clocks += 3; // TODO: Clocks
                     // Address to jump to is in a register given by lowest nibble
    let reg_n = opcode & 0x0000_000F;
    let mut jmp_addr = cpu.regs[reg_n as usize];
    // It doesn't *actually* switch mode, a bit tells us what to set it to
    let switch_to_thumb = (jmp_addr & 0x0000_0001) != 0;
    if switch_to_thumb {
        jmp_addr -= 1;
        cpu.cpsr.set_T(true);
    }
    *cpu.pc() = jmp_addr;
}
/// Branch or Branch and link
///
/// BL is similar to CALL, it stores the return PC (PC+4) in the
/// LR (Register 14). If using nested functions, that requires pushing LR onto the stack.
fn B(cpu: &mut Cpu, opcode: u32) {
    cpu.clocks += 3; // TODO: Clocks
    let offset = opcode & 0x00FF_FFFF;
    let is_branch_and_link = (opcode & 0x0100_0000) != 0;
    if is_branch_and_link {
        *cpu.LR() = *cpu.pc();
    }
    *cpu.pc() += 4 + offset * 4;
}
/// Software Interrupt
///
/// Used to call BIOS functions
fn SWI(cpu: &mut Cpu, opcode: u32) {
    cpu.set_mode(CpuMode::Supervisor);
    *cpu.pc() = SWI_HANDLER;
    cpu.cpsr.set_T(false);
    cpu.cpsr.set_I(true);
    cpu.clocks += 0; // todo:clocks
}
/// Single data swap
fn SWP(cpu: &mut Cpu, opcode: u32) {
    let (_, _, is_byte_else_word, _, _) = as_flags(opcode);
    let (rn, rd, _, _, rm) = as_usize_nibbles(opcode);
    if is_byte_else_word {
        // TODO: Is this right? GBATEK mentions zero extending
        // But is it the most significant LE byte?
        // It refers to STR/LDR, which says
        // "When reading a byte from memory, upper 24 bits of Rd are zero-extended."
        // I'm assuming that literally means the upper bits and not the most significant ones
        *cpu.reg_mut(rd) = (*cpu.reg_mut(rn)) & 0x0000_00FF;
        *cpu.reg_mut(rn) = (*cpu.reg_mut(rm)) & 0x0000_00FF;
    } else {
        *cpu.reg_mut(rd) = *cpu.reg_mut(rn);
        *cpu.reg_mut(rn) = *cpu.reg_mut(rm);
    }
    cpu.clocks += 0; // todo:clocks
}
/// Multiply long
fn MULL(cpu: &mut Cpu, opcode: u32) {
    let (_, _, signed, accumulate, set_cond_flags) = as_flags(opcode);
    let (rdhigh, rdlow, rn, _, rm) = as_usize_nibbles(opcode);
    let res: u64 = if signed {
        unimplemented!("No support for signed long multiplication")
    } else {
        u64::from(cpu.fetch_reg(rn)) * u64::from(cpu.fetch_reg(rm))
    };
    *cpu.reg_mut(rdhigh) = (res) as u32;
    *cpu.reg_mut(rdlow) = (res >> 32) as u32;
    if set_cond_flags {
        cpu.cpsr.set_Z(res == 0);
        cpu.cpsr.set_N((res & 0x8000_0000) != 0);
        cpu.cpsr.set_V(false); //TODO
        cpu.cpsr.set_C(false); //TODO
    }
    cpu.clocks += 0; // todo:clocks
}
/// Multiply
fn MUL(cpu: &mut Cpu, opcode: u32) {
    let (_, _, _, accumulate, set_cond_flags) = as_flags(opcode);
    let (rd, rn, rs, _, rm) = as_usize_nibbles(opcode);
    let res = if accumulate {
        cpu.fetch_reg(rn) * cpu.fetch_reg(rm) + cpu.fetch_reg(rd)
    } else {
        cpu.fetch_reg(rn) * cpu.fetch_reg(rm)
    };
    *cpu.reg_mut(rs) = res;
    if set_cond_flags {
        cpu.cpsr.set_Z(res == 0);
        cpu.cpsr.set_N((res & 0x8000_0000) != 0);
        cpu.cpsr.set_V(false);
        cpu.cpsr.set_C(false);
    }
    cpu.clocks += 0; // todo:clocks
}
/// Halfword Data Transfer Register Offset
fn HDT_RO(cpu: &mut Cpu, opcode: u32) {
    let (p, o, _, w, l) = as_flags(opcode);
    let (rn, rd, _, sh, rm) = as_usize_nibbles(opcode);
    let s = (sh & 0b0100) != 0;
    let h = (sh & 0b0010) != 0;
    // TODO: the thing
    cpu.clocks += 0; // todo:clocks
    unimplemented!()
}
// Halfword Data Transfer Immediate Offset
fn HDT_IO(cpu: &mut Cpu, opcode: u32) {
    let (p, o, _, w, l) = as_flags(opcode);
    let (rn, rd, offset, sh, rm) = as_usize_nibbles(opcode);
    let s = (sh & 0b0100) != 0;
    let h = (sh & 0b0010) != 0;
    // TODO: the thing
    cpu.clocks += 0; // todo:clocks
    unimplemented!()
}
/// Single Data Transfer
fn SDT(cpu: &mut Cpu, opcode: u32) {
    let shifted_register = as_extra_flag(opcode);
    let (is_pre_offseted, is_up, is_byte_size, bit_21, is_load) = as_flags(opcode);
    let (rn, rd, third_byte, second_byte, first_byte) = as_usize_nibbles(opcode);
    let offset = if !shifted_register {
        opcode & 0x0000_0FFF
    } else {
        let shift_amount = (third_byte as u32) * 2 + (((second_byte as u32) & 0x8) >> 3);
        let shift_type = second_byte;
        let register_value = cpu.regs[first_byte];
        //TODO: Handle 0 special cases (Same as ALU operations)
        match shift_type & 6 {
            0 => register_value.overflowing_shl(shift_amount).0,
            2 => register_value.overflowing_shr(shift_amount).0,
            4 => (register_value as i32).overflowing_shr(shift_amount).0 as u32,
            6 => register_value.rotate_right(shift_amount),
            _ => unimplemented!("Invalid instruction"),
        }
    };
    let mut addr =
        ((cpu.regs[rn] as i64) + if is_up { offset as i64 } else { -(offset as i64) }) as u32;
    if rn == 15 {
        addr += 4;
    }
    if is_load {
        cpu.regs[rd] = if is_byte_size {
            // Least significant byte
            (*cpu.fetch_byte(((addr) / 4u32) * 4u32)) as u32
        } else {
            cpu.fetch_u32(addr)
        };
    } else if is_byte_size {
        cpu.write_u8(addr, cpu.regs[rd] as u8);
    } else {
        cpu.write_u32(addr, cpu.regs[rd]);
    }
    println!(
        "data trans load:{} rn:{} rd:{} offset:{:x?} rn:{:x?} rd:{:x?} addr:{:x?}",
        is_load, rn, rd, offset, cpu.regs[rn], cpu.regs[rd], addr
    );
    if !is_pre_offseted || bit_21 {
        cpu.regs[rn] = addr;
    }
    cpu.clocks += 0; // todo:clocks
}
/// Block Data Transfer
fn BDT(cpu: &mut Cpu, opcode: u32) {
    let (is_pre_offseted, is_up, psr_or_user_mode, write_back, is_load) = as_flags(opcode);
    let (rn, rd, _, _, _) = as_usize_nibbles(opcode);
    let register_list = opcode & 0x000F_FFFF;
    // TODO: the thing
    cpu.clocks += 0; // todo:clocks
    unimplemented!()
}
/// Data Processing or PSR transfer
/// This handles all ALU operations
fn ALU(cpu: &mut Cpu, opcode: u32) {
    let immediate = as_extra_flag(opcode);
    let (rn_num, rd, _, _, _) = as_usize_nibbles(opcode);
    dbg!(rn_num);
    dbg!(rd);
    let (_, _, third_byte, second_byte, lowest_byte) = as_u8_nibbles(opcode);
    let set_condition_flags = (opcode & 0x0010_0000) != 0 || rd == 15;
    dbg!(set_condition_flags);
    dbg!(rd);
    let mut rn = cpu.regs[rn_num];
    if rn_num == 15 {
        rn += 4; // Account for PC pipelining
    }
    let operation = ((opcode & 0x01E0_0000) >> 21) as u8;
    let (op2, shift_carry) = if immediate {
        let ror_shift = (third_byte as u32) * 2;
        let val = (lowest_byte + second_byte * 16) as u32;
        (val.rotate_right(ror_shift), Some((val & (1u32 << ror_shift)) != 0))
    } else {
        let register_value: u32 = cpu.regs[lowest_byte as usize];
        let shift_by_register = (opcode & 0x0000_0010) != 0;
        let mut imm_shift_zero = false;
        let shift_amount = if shift_by_register {
            cpu.regs[third_byte as usize]
        } else {
            let shift_amount = (third_byte as u32) * 2 + ((second_byte & 0x8) as u32);
            imm_shift_zero = shift_amount == 0;
            shift_amount
        };
        let shift_type = ((opcode & 0x0000_0060) >> 6) as u8;
        let (op2, shift_carry) = if imm_shift_zero {
            let shift_carry = Some(false);
            match shift_type {
                0 => (register_value, None),
                1 => (0, Some((register_value & 0x8000_0000) != 0)),
                2 => (
                    (register_value as i32).overflowing_shr(32).0 as u32,
                    Some((register_value & 0x8000_0000) != 0),
                ),
                3 => unimplemented!("TODO: RRX"),
                _ => unimplemented!("Impossible"),
            }
        } else {
            //TODO:carry(Are they right??)
            match shift_type {
                0 => (
                    register_value.overflowing_shl(shift_amount).0,
                    Some(
                        (register_value & (1 << std::cmp::max(shift_amount - 1, 0))) != 0
                            || shift_amount == 0,
                    ),
                ),
                1 => (
                    register_value.overflowing_shr(shift_amount).0,
                    Some(
                        (register_value & (0x8000_0000 >> std::cmp::max(shift_amount - 1, 0))) != 0
                            || shift_amount == 0,
                    ),
                ),
                2 => ((register_value as i32).overflowing_shr(shift_amount).0 as u32, Some(false)),
                3 => (
                    register_value.rotate_right(shift_amount),
                    Some(
                        (register_value & (1 << std::cmp::max(shift_amount - 1, 0))) != 0
                            || shift_amount == 0,
                    ),
                ),
                _ => unimplemented!("Impossible"),
            }
        };
        (op2, if lowest_byte == 0 { None } else { shift_carry })
    };
    let res = &mut cpu.regs[rd];
    let cpsr = cpu.cpsr;
    let ref_cpsr = &mut cpu.cpsr;
    // TODO: Flag setting
    let addition_carries = |res: u32, op1: u32, op2: u32| {
        ((op1 & 0x8000_0000) != 0 && (op2 & 0x8000_0000) != 0)
            || ((res & 0x8000_0000) != 0 && !(op1 & 0x8000_0000) != 0)
            || ((res & 0x8000_0000) != 0 && !(op2 & 0x8000_0000) != 0)
    };
    let mut set_all_flags = move |res: u32, carry: Option<bool>, overflow: Option<bool>| {
        if set_condition_flags {
            if let Some(carry) = carry {
                ref_cpsr.set_C(carry);
            }
            if let Some(overflow) = overflow {
                ref_cpsr.set_V(overflow);
            }
            ref_cpsr.set_N((res & 0x8000_0000) != 0);
            dbg!("setting Z to");
            dbg!(res);
            ref_cpsr.set_Z(res == 0x0000_0000);
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
            let (result, overflow) = rn.overflowing_sub(op2);
            *res = result;
            set_all_flags(*res, Some(op2 <= rn), Some(overflow));
        }
        3 => {
            //rsb
            let (result, overflow) = op2.overflowing_sub(rn);
            *res = result;
            set_all_flags(*res, Some(rn <= op2), Some(overflow));
        }
        4 => {
            //add
            let (result, overflow) = op2.overflowing_add(rn);
            *res = result;
            set_all_flags(*res, Some(addition_carries(result, rn, op2)), Some(overflow));
        }
        5 => {
            //adc
            let (result, overflow) = op2.overflowing_add(rn);
            let (result, overflow2) = result.overflowing_add(cpsr.C() as u32);
            *res = result;
            set_all_flags(
                *res,
                Some(addition_carries(result, rn, op2)), //TODO: consider carry add
                Some(overflow || overflow2),
            );
        }
        6 => {
            //sbc
            let (result, overflow) = rn.overflowing_sub(op2);
            let (result, overflow2) = result.overflowing_sub(1 - (cpsr.C() as u32));
            *res = result;
            set_all_flags(
                *res,
                Some(op2 <= rn), //TODO: consider carry add
                Some(overflow || overflow2),
            );
        }
        7 => {
            //rsc
            let (result, overflow) = op2.overflowing_sub(rn);
            let (result, overflow2) = result.overflowing_sub(1 - (cpsr.C() as u32));
            *res = result;
            set_all_flags(
                *res,
                Some(rn <= op2), //TODO: consider carry add
                Some(overflow || overflow2),
            );
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
            dbg!("teq");
            dbg!(rn);
            dbg!(op2);
            //teq
            let res = rn ^ op2;
            dbg!(res);
            if rd == 15 {
                set_all_flags(res, None, None);
            } else {
                set_all_flags(res, shift_carry, None);
            }
        }
        10 => {
            //cmp
            let (res, v) = rn.overflowing_sub(op2);
            set_all_flags(res, Some(op2 <= rn), Some(v));
        }
        11 => {
            //cmn
            let (res, v) = rn.overflowing_add(op2);
            set_all_flags(res, Some(addition_carries(res, rn, op2)), Some(v));
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
        _ => unimplemented!("Impossible"),
    }
    cpu.clocks += 0; // todo:clocks
}
/// Move to Status Register
fn MSR(cpu: &mut Cpu, opcode: u32) {
    dbg!("msr");
    let immediate = as_extra_flag(opcode);
    let (_, _, use_spsr, _, _) = as_flags(opcode);
    let change_flags_fields = (opcode & 0x0008_0000) != 0; // i.e overflow/zero/etc
    let change_control_fields = (opcode & 0x0001_0000) != 0; // mode, thumb, etc
    let operand = if immediate {
        let (_, _, shift, second, first) = as_u8_nibbles(opcode);
        ((first as u32) + (second as u32) * 16).rotate_right(2 * (shift as u32))
    } else {
        cpu.regs[(opcode & 0x0000_000F) as usize]
    };
    //let operand = cpu.regs[(opcode & 0x0000_000F) as usize];
    let old = cpu.cpsr.0;
    // user mode => cpsr

    let dest = if use_spsr { cpu.get_spsr_mut().unwrap() } else { &mut cpu.cpsr };
    // TODO: priviledge check for allowance bits
    const FLAGS_MASK: u32 = 0xFF000000;
    const STATE_MASK: u32 = 0x00000020;
    let valid_bits_mask: u32 = (if change_control_fields { STATE_MASK } else { 0 })
        | (if change_flags_fields { FLAGS_MASK } else { 0 });
    let invalid_bits_mask = !valid_bits_mask;
    *dest = CPSR((old & invalid_bits_mask) | (operand & valid_bits_mask));
    // Only set control flags of the cpsr/spsr if the `f` bit of the opcode is set
    // and we're in priviledged mode (Not User)
    if cpu.cpsr.mode() != CpuMode::User && change_control_fields {
        dbg!("changing mode maybe");
        dbg!(cpu.regs[14]);
        cpu.set_mode(CpuMode::from_byte(((operand & 0x0000000F) | 0x00000010) as u8));
        const PRIVACY_MASK: u32 = 0x0000_00CF;
        cpu.cpsr = CPSR((cpu.cpsr.0 & (!PRIVACY_MASK)) | (operand & PRIVACY_MASK));
        dbg!(cpu.regs[14]);
    }
}
/// Move to register from status register
fn MRS(cpu: &mut Cpu, opcode: u32) {
    let (_, _, use_spsr, _, _) = as_flags(opcode);
    cpu.regs[((opcode & 0x0000_F000) >> 12) as usize] =
        if use_spsr { cpu.get_spsr_mut().unwrap().0 } else { cpu.cpsr.0 };
}
/// Coprocessor Data Transfer (Unimplemented)
fn CDT(cpu: &mut Cpu, opcode: u32) {
    unimplemented!("Coprocessor instructions are not supported")
}
/// Coprocessor Data Operation (Unimplemented)
fn CDO(cpu: &mut Cpu, opcode: u32) {
    unimplemented!("Coprocessor instructions are not supported")
}
/// Coprocessor Register Transfer (Unimplemented)
fn CRT(cpu: &mut Cpu, opcode: u32) {
    unimplemented!("Coprocessor instructions are not supported")
}

/// Map an opcode to an instruction (An fn(&mut Cpu, u32))
///
/// Panics on undefined or invalid opcode.
fn decode_arm(opcode: u32) -> fn(&mut Cpu, u32) {
    let bits27_20 = (opcode >> 20) as u8;
    let bits11_4 = (opcode >> 4) as u8;
    const T: bool = true;
    const F: bool = false;
    match (bits27_20.as_bools(), bits11_4.as_bools()) {
        ([F, T, T, _, _, _, _, _], [_, _, _, _, _, _, _, T]) => unimplemented!(
            "Unimplemented: Undefined instruction {:b} ({:x}) bits 27-20: {:x} bits 11-4: {:x}",
            opcode,
            opcode,
            bits27_20,
            bits11_4
        ),
        ([F, T, _, _, _, _, _, _], _) => SDT,
        ([F, F, F, F, F, F, _, _], [_, _, _, _, T, F, F, T]) => MUL,
        ([F, F, F, F, T, _, _, _], [_, _, _, _, T, F, F, T]) => MULL,
        ([F, F, F, T, F, _, F, F], [F, F, F, F, T, F, F, T]) => SWP,
        ([F, F, F, _, _, F, _, _], [F, F, F, F, T, _, _, T]) => HDT_RO,
        ([F, F, F, _, _, T, _, _], [F, F, F, F, T, _, _, T]) => HDT_IO,
        ([T, F, F, _, _, _, _, _], _) => BDT,
        ([T, T, F, _, _, _, _, _], _) => CDT,
        ([T, T, T, F, _, _, _, _], [_, _, _, F, _, _, _, _]) => CDO,
        ([T, T, T, F, _, _, _, _], [_, _, _, T, _, _, _, _]) => CRT,
        ([T, T, T, T, _, _, _, _], _) => SWI,
        ([T, F, T, _, _, _, _, _], _) => B,
        ([F, F, F, T, F, F, T, F], [T, T, T, T, F, F, F, T])
            if (opcode & 0x000F_F000) == 0x000F_F000 =>
        {
            BX
        }
        ([F, F, _, T, F, _, F, F], [F, F, F, F, F, F, F, F]) => MRS,
        ([F, F, _, T, F, _, T, F], _) => MSR,
        ([F, F, _, _, _, _, _, _], _) => ALU,
        /*_ => unimplemented!(
          "Invalid opcode {:b} ({:x}) bits 27-20: {:x} bits 11-4: {:x} at pc {:x}",
          opcode,
          opcode,
          bits27_20,
          bits11_4,
          *self.pc()
        ),*/
    }
}

pub(crate) fn execute_one_instruction(cpu: &mut Cpu) {
    let pc = *cpu.pc();
    let opcode = cpu.fetch_u32(pc);
    *cpu.pc() += 4;
    println!("{:x}", opcode);
    if check_cond(cpu, opcode) {
        return;
    }
    let instruction = decode_arm(opcode);
    instruction(cpu, opcode);
}
