use super::cpu_mode::CpuMode;

/// The CPSR register contains the status flags (Like carry, overfllow, and zero)
/// , the execution mode (Thumb or Arm) and the cpu mode/priviledges (See [CpuMode])
#[derive(Copy, Clone)]
pub struct CPSR(pub(crate) u32);
fn is_set(x: u32, bit: u32) -> bool {
  (x & bit) == bit
}
#[inline]
pub(crate) fn carry_from(op1: u32, op2: u32, res: u32) -> bool {
  ((op1 >> 31) + (op2 >> 31)) > (res >> 31)
}
#[inline]
pub(crate) fn borrow_from(positive: u32, negative: u32) -> bool {
  positive >= negative
}
impl CPSR {
  #[inline]
  fn is_set(self, bit: u32) -> bool {
    is_set(self.0, bit)
  }
  #[inline]
  fn set(&mut self, bit: u32, set_it_to_true: bool) {
    self.0 = (self.0 & (!bit)) | (if set_it_to_true { bit } else { 0 });
  }
  /// Sign(Negative) (N)
  #[inline]
  pub fn negative_flag(self) -> bool {
    self.is_set(0x8000_0000)
  }
  /// Sign(Negative) (N)
  #[inline]
  pub fn set_negative_flag(&mut self, v: bool) {
    self.set(0x8000_0000, v)
  }
  /// Zero (Z)
  #[inline]
  pub fn zero_flag(self) -> bool {
    self.is_set(0x4000_0000)
  }
  /// Zero (Z)
  #[inline]
  pub fn set_zero_flag(&mut self, v: bool) {
    self.set(0x4000_0000, v)
  }
  /// Carry (C)
  #[inline]
  pub fn carry_flag(self) -> bool {
    self.is_set(0x2000_0000)
  }
  /// Carry (C)
  #[inline]
  pub fn set_carry_flag(&mut self, v: bool) {
    self.set(0x2000_0000, v)
  }
  /// Overflow (V)
  #[inline]
  pub fn overflow_flag(self) -> bool {
    self.is_set(0x1000_0000)
  }
  /// Overflow (V)
  #[inline]
  pub fn set_overflow_flag(&mut self, v: bool) {
    self.set(0x1000_0000, v)
  }
  /// IRQ disabled (I)
  #[inline]
  pub fn irq_disabled_flag(self) -> bool {
    self.is_set(0x0000_0080)
  }
  /// Disable IRQ (I)
  #[inline]
  pub fn set_irq_disabled_flag(&mut self, v: bool) {
    self.set(0x0000_0080, v)
  }
  /// Fiq disabled (F)
  #[inline]
  pub fn fiq_disabled_flag(self) -> bool {
    self.is_set(0x0000_0040)
  }
  /// Disable Fiq (F)
  #[inline]
  pub fn set_fiq_disabled_flag(&mut self, v: bool) {
    self.set(0x0000_0040, v)
  }
  /// State/Thumb (thumb=1/true)
  #[inline]
  pub fn thumb_state_flag(self) -> bool {
    self.is_set(0x0000_0020)
  }
  /// State/Thumb (thumb=1/true)
  #[inline]
  pub fn set_thumb_state_flag(&mut self, v: bool) {
    self.set(0x0000_0020, v)
  }
  /// Get priviledge mode
  #[inline]
  pub fn mode(self) -> CpuMode {
    CpuMode::from_byte((self.0 & 0x0000_001F) as u8)
  }
  /// Set priviledge mode
  #[inline]
  pub fn set_mode(&mut self, new_mode: CpuMode) {
    self.0 = (self.0 & 0xFFFF_FFE0) | u32::from(new_mode.as_byte());
  }

  #[inline]
  pub fn addition_carries(res: u32, op1: u32, op2: u32) -> bool {
    carry_from(op1, op2, res)
    /*
    ((op1 & 0x8000_0000) != 0 && (op2 & 0x8000_0000) != 0)
      || ((res & 0x8000_0000) != 0 && !(op1 & 0x8000_0000) != 0)
      || ((res & 0x8000_0000) != 0 && !(op2 & 0x8000_0000) != 0)
      */
  }
  #[inline]
  pub(crate) fn set_all_status_flags_for_addition(
    &mut self,
    res: u32,
    op1: u32,
    op2: u32,
    overflow: Option<bool>,
  ) {
    let carry = Some(CPSR::addition_carries(res, op1, op2));
    self.set_all_status_flags(res, carry, overflow);
  }
  #[inline]
  pub(crate) fn set_neutral_flags(&mut self, res: u32) {
    self.set_negative_flag((res & 0x8000_0000) != 0);
    self.set_zero_flag(res == 0x0000_0000);
  }
  #[inline]
  pub(crate) fn set_all_status_flags(
    &mut self,
    res: u32,
    carry: Option<bool>,
    overflow: Option<bool>,
  ) {
    if let Some(carry) = carry {
      self.set_carry_flag(carry);
    }
    if let Some(overflow) = overflow {
      self.set_overflow_flag(overflow);
    }
    self.set_neutral_flags(res);
  }
}
impl std::fmt::Display for CPSR {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    write!(
      f,
      "N:{} C:{} Z:{} V:{} I:{} F:{} T:{} mode:{:x?}",
      self.negative_flag(),
      self.carry_flag(),
      self.zero_flag(),
      self.overflow_flag(),
      self.irq_disabled_flag(),
      self.fiq_disabled_flag(),
      self.thumb_state_flag(),
      self.mode().as_byte()
    )
  }
}
