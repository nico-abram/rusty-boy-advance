/// Utility impl on u8's for some kind of "bit pattern matching".
///
/// Consider performance implications (If any) later.
///
/// # Examples
///
/// ```
/// use rusty_boy_advance::cpu::utils::AsBoolSlice;
/// fn disassemble(x: u8) -> Option<&'static str> {
///   const T: bool = true;
///   const F: bool = false;
///   match x.as_bools() {
///     [T, T, T, _, _, _, _, _] => Some("First three bits set"),
///     [_, _, F, _, _, _, _, _] => Some("Third bit not set"),
///     _ => None,
///   }
/// }
/// assert!(disassemble(0b1110_0000u8) == Some("First three bits set"));
/// assert!(disassemble(0b1110_1100u8) == Some("First three bits set"));
/// assert!(disassemble(0b1100_1100u8) == Some("Third bit not set"));
/// assert!(disassemble(0b0000_1100u8) == Some("Third bit not set"));
/// assert!(disassemble(0b0010_1100u8) == None);
/// ```
pub trait AsBoolSlice {
  fn is_set(self, mask: u8) -> bool;
  fn is_set_n(self, n: u8) -> bool;
  fn as_bools(self) -> [bool; 8];
}
impl AsBoolSlice for u8 {
  #[inline]
  fn is_set(self, mask: u8) -> bool {
    (self & mask) == mask
  }
  #[inline]
  fn is_set_n(self, bit_number: u8) -> bool {
    self.is_set(1u8 << bit_number)
  }
  #[inline]
  fn as_bools(self) -> [bool; 8] {
    [
      self.is_set_n(7),
      self.is_set_n(6),
      self.is_set_n(5),
      self.is_set_n(4),
      self.is_set_n(3),
      self.is_set_n(2),
      self.is_set_n(1),
      self.is_set_n(0),
    ]
  }
}
