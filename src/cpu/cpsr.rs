use super::cpu_mode::CpuMode;

/// The CPSR register contains the status flags (Like carry, overfllow, and zero)
/// , the execution mode (Thumb or Arm) and the cpu mode/priviledges (See [CpuMode])
#[derive(Copy, Clone)]
pub(crate) struct CPSR(pub(crate) u32);
fn is_set(x: u32, bit: u32) -> bool {
    (x & bit) == bit
}
impl CPSR {
    pub(crate) fn to_string(&self) -> String {
        format!(
            "N:{} C:{} Z:{} V:{} Q:{} I:{} F:{} T:{} mode:{:x?}",
            self.N(),
            self.C(),
            self.Z(),
            self.V(),
            self.Q(),
            self.I(),
            self.F(),
            self.T(),
            self.mode()
        )
    }
    fn is_set(&self, bit: u32) -> bool {
        is_set(self.0, bit)
    }
    fn set(&mut self, bit: u32, set_it_to_true: bool) {
        self.0 = (self.0 & (!bit)) | (if set_it_to_true { bit } else { 0 });
    }
    /// Sign(Negative)
    pub(crate) fn N(&self) -> bool {
        self.is_set(0x8000_0000)
    }
    /// Sign(Negative)
    pub(crate) fn set_N(&mut self, v: bool) {
        self.set(0x8000_0000, v)
    }
    /// Zero
    pub(crate) fn Z(&self) -> bool {
        self.is_set(0x4000_0000)
    }
    /// Zero
    pub(crate) fn set_Z(&mut self, v: bool) {
        self.set(0x4000_0000, v)
    }
    /// Carry
    pub(crate) fn C(&self) -> bool {
        self.is_set(0x2000_0000)
    }
    /// Carry
    pub(crate) fn set_C(&mut self, v: bool) {
        self.set(0x2000_0000, v)
    }
    /// Overflow
    pub(crate) fn V(&self) -> bool {
        self.is_set(0x1000_0000)
    }
    /// Overflow
    pub(crate) fn set_V(&mut self, v: bool) {
        self.set(0x1000_0000, v)
    }
    /// Sticky overflow
    pub(crate) fn Q(&self) -> bool {
        self.is_set(0x0800_0000)
    }
    // Sticky overflow
    pub(crate) fn set_Q(&mut self, v: bool) {
        self.set(0x0800_0000, v)
    }
    /// IRQ disabled
    pub(crate) fn I(&self) -> bool {
        self.is_set(0x0000_0080)
    }
    /// Disable IRQ
    pub(crate) fn set_I(&mut self, v: bool) {
        self.set(0x0000_0080, v)
    }
    /// Fiq disabled
    pub(crate) fn F(&self) -> bool {
        self.is_set(0x0000_0040)
    }
    /// Disable Fiq
    pub(crate) fn set_F(&mut self, v: bool) {
        self.set(0x0000_0040, v)
    }
    /// State/Thumb (thumb=1/true)
    pub(crate) fn T(&self) -> bool {
        self.is_set(0x0000_0020)
    }
    /// State/Thumb (thumb=1/true)
    pub(crate) fn set_T(&mut self, v: bool) {
        self.set(0x0000_0020, v)
    }
    /// Get priviledge mode
    pub(crate) fn mode(&self) -> CpuMode {
        CpuMode::from_byte((self.0 & 0x0000_001F) as u8)
    }
    /// Set priviledge mode
    pub(crate) fn set_mode(&mut self, new_mode: CpuMode) {
        self.0 = (self.0 & 0xFFFF_FFE0) | (new_mode.as_byte() as u32);
    }
}
