/// Cpu Mode (NOT whether we're in thumb or arm, this
/// is related to priviledges and currently in use
/// register banks)
#[derive(PartialEq, Copy, Clone, Debug)]
pub enum CpuMode {
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
      Self::User | Self::Privileged => 0,
      Self::FIQ => 1,
      Self::IRQ => 2,
      Self::Supervisor => 3,
      Self::Abort => 4,
      Self::Undef => 5,
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
      Self::User => 0b10000,
      Self::FIQ => 0b10001,
      Self::IRQ => 0b10010,
      Self::Supervisor => 0b10011,
      Self::Abort => 0b10111,
      Self::Undef => 0b11011,
      Self::Privileged => 0b11111,
    }
  }

  /// From the CPSR status register representation.
  ///
  /// See [as_byte][CpuMode::as_byte] for details.
  pub(crate) fn from_byte(byte: u8) -> Self {
    match byte {
      0b10000 => Self::User,
      0b10001 => Self::FIQ,
      0b10010 => Self::IRQ,
      0b10011 => Self::Supervisor,
      0b10111 => Self::Abort,
      0b11011 => Self::Undef,
      0b11111 => Self::Privileged,
      x => unimplemented!("Invalid cpu mode flag {:x?}", x),
    }
  }
}

impl core::fmt::Display for CpuMode {
  fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
    core::fmt::Debug::fmt(self, f)
  }
}

impl core::fmt::LowerHex for CpuMode {
  fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
    write!(f, "{:x}", self.as_byte())
  }
}
