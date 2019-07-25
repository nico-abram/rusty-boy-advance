/// Utility impl on u8's for some kind of "bit pattern matching".
///
/// Consider performance implications (If any) later.
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
