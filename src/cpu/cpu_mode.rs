/// Cpu Mode (NOT whether we're in thumb or arm, this
/// is related to priviledges and currently in use
/// register banks)
#[derive(PartialEq, Copy, Clone, Debug)]
pub(crate) enum CpuMode {
    User,
    FIQ,
    IRQ,
    Supervisor,
    Abort,
    Undef,
    Privileged,
}
impl CpuMode {
    /// Returns a number in the 0-5 range. Both User and Priviledged(Systen) use 0.
    ///
    /// Used to index the array of register banks.
    pub(crate) fn as_usize(self) -> usize {
        match self {
            CpuMode::User => 0,
            CpuMode::Privileged => 0,
            CpuMode::FIQ => 1,
            CpuMode::IRQ => 2,
            CpuMode::Supervisor => 3,
            CpuMode::Abort => 4,
            CpuMode::Undef => 5,
        }
    }
    /// In the CPSR status register representation:
    ///
    /// 0b10000 0x10h 16 - User (non-privileged)
    ///
    /// 0b10001 0x11 17 - FIQ (Fast interript)
    ///
    /// 0b10010 0x12 18 - IRQ (Interrupt)
    ///
    /// 0b10011 0x13 19 - Supervisor (SWI/BIOS call)
    ///
    /// 0b10111 0x17 23 - Abort (??? TODO:what is this)
    ///
    /// 0b11011 0x1B 27 - Undefined (Undefined instruction handler)
    ///
    /// 0b11111 0x1F 31 - System (privileged 'User' mode) (ARMv4 and up)
    pub(crate) fn as_byte(self) -> u8 {
        match self {
            CpuMode::User => 0b10000,
            CpuMode::FIQ => 0b10001,
            CpuMode::IRQ => 0b10010,
            CpuMode::Supervisor => 0b10011,
            CpuMode::Abort => 0b10111,
            CpuMode::Undef => 0b11011,
            CpuMode::Privileged => 0b11111,
        }
    }
    /// From the CPSR status register representation.
    ///
    /// See [as_byte][CpuMode::as_byte] for details.
    pub(crate) fn from_byte(byte: u8) -> CpuMode {
        match byte {
            0b10000 => CpuMode::User,
            0b10001 => CpuMode::FIQ,
            0b10010 => CpuMode::IRQ,
            0b10011 => CpuMode::Supervisor,
            0b10111 => CpuMode::Abort,
            0b11011 => CpuMode::Undef,
            0b11111 => CpuMode::Privileged,
            x => unimplemented!("Invalid cpu mode flag {:x?}", x),
        }
    }
}
