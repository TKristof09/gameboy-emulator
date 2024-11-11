open Uint
open Instruction

module Make (Mem : Addressable_intf.WordAddressable) = struct
  let fetch mem ~pc : Instruction_info.t =
      let op = Mem.read_byte mem pc |> Uint8.to_int in
      let read_byte_operand () = Mem.read_byte mem (Uint16.succ pc) in
      let read_signed_byte_operand () = read_byte_operand () |> Uint8.to_int8 in
      let read_word_operand () = Mem.read_word mem (Uint16.succ pc) in
      match op with
      | 0x00 -> { len = Uint16.one; instr = NOP }
      | 0x01 ->
          let n = read_word_operand () in
          { len = Uint16.of_int 3; instr = LD16 (Reg16 BC, Imm16 n) }
      | 0x02 -> { len = Uint16.one; instr = LD8 (PtrR BC, Reg8 A) }
      | 0x03 -> { len = Uint16.one; instr = INC16 (Reg16 BC) }
      | 0x04 -> { len = Uint16.one; instr = INC8 (Reg8 B) }
      | 0x05 -> { len = Uint16.one; instr = DEC8 (Reg8 B) }
      | 0x06 ->
          let n = read_byte_operand () in
          { len = Uint16.of_int 2; instr = LD8 (Reg8 B, Imm8 n) }
      | 0x07 -> { len = Uint16.one; instr = RLCA }
      | 0x08 ->
          let n = read_word_operand () in
          { len = Uint16.of_int 3; instr = LD16 (PtrImm16 n, SP) }
      | 0x09 -> { len = Uint16.one; instr = ADD16 (Reg16 HL, Reg16 BC) }
      | 0x0A -> { len = Uint16.one; instr = LD8 (Reg8 A, PtrR BC) }
      | 0x0B -> { len = Uint16.one; instr = DEC16 (Reg16 BC) }
      | 0x0C -> { len = Uint16.one; instr = INC8 (Reg8 C) }
      | 0x0D -> { len = Uint16.one; instr = DEC8 (Reg8 C) }
      | 0x0E ->
          let n = read_byte_operand () in
          { len = Uint16.of_int 2; instr = LD8 (Reg8 C, Imm8 n) }
      | 0x0F -> { len = Uint16.one; instr = RRCA }
      | 0x10 ->
          let _ = read_byte_operand () in
          (* TODO *)
          { len = Uint16.of_int 2; instr = STOP }
      | 0x11 ->
          let n = read_word_operand () in
          { len = Uint16.of_int 3; instr = LD16 (Reg16 DE, Imm16 n) }
      | 0x12 -> { len = Uint16.one; instr = LD8 (PtrR DE, Reg8 A) }
      | 0x13 -> { len = Uint16.one; instr = INC16 (Reg16 DE) }
      | 0x14 -> { len = Uint16.one; instr = INC8 (Reg8 D) }
      | 0x15 -> { len = Uint16.one; instr = DEC8 (Reg8 D) }
      | 0x16 ->
          let n = read_byte_operand () in
          { len = Uint16.of_int 2; instr = LD8 (Reg8 D, Imm8 n) }
      | 0x17 -> { len = Uint16.one; instr = RLA }
      | 0x18 ->
          let e = read_signed_byte_operand () in
          { len = Uint16.of_int 2; instr = JR (None, e) }
      | 0x19 -> { len = Uint16.one; instr = ADD16 (Reg16 HL, Reg16 DE) }
      | 0x1A -> { len = Uint16.one; instr = LD8 (Reg8 A, PtrR DE) }
      | 0x1B -> { len = Uint16.one; instr = DEC16 (Reg16 DE) }
      | 0x1C -> { len = Uint16.one; instr = INC8 (Reg8 E) }
      | 0x1D -> { len = Uint16.one; instr = DEC8 (Reg8 E) }
      | 0x1E ->
          let n = read_byte_operand () in
          { len = Uint16.of_int 2; instr = LD8 (Reg8 E, Imm8 n) }
      | 0x1F -> { len = Uint16.one; instr = RRA }
      | 0x20 ->
          let e = read_signed_byte_operand () in
          { len = Uint16.of_int 2; instr = JR (NZ, e) }
      | 0x21 ->
          let n = read_word_operand () in
          { len = Uint16.of_int 3; instr = LD16 (Reg16 HL, Imm16 n) }
      | 0x22 -> { len = Uint16.one; instr = LD8 (HL_i, Reg8 A) }
      | 0x23 -> { len = Uint16.one; instr = INC16 (Reg16 HL) }
      | 0x24 -> { len = Uint16.one; instr = INC8 (Reg8 H) }
      | 0x25 -> { len = Uint16.one; instr = DEC8 (Reg8 H) }
      | 0x26 ->
          let n = read_byte_operand () in
          { len = Uint16.of_int 2; instr = LD8 (Reg8 H, Imm8 n) }
      | 0x27 -> { len = Uint16.one; instr = DAA }
      | 0x28 ->
          let e = read_signed_byte_operand () in
          { len = Uint16.of_int 2; instr = JR (Z, e) }
      | 0x29 -> { len = Uint16.one; instr = ADD16 (Reg16 HL, Reg16 HL) }
      | 0x2A -> { len = Uint16.one; instr = LD8 (Reg8 A, PtrR HL) }
      | 0x2B -> { len = Uint16.one; instr = DEC16 (Reg16 HL) }
      | 0x2C -> { len = Uint16.one; instr = INC8 (Reg8 L) }
      | 0x2D -> { len = Uint16.one; instr = DEC8 (Reg8 L) }
      | 0x2E ->
          let n = read_byte_operand () in
          { len = Uint16.of_int 2; instr = LD8 (Reg8 L, Imm8 n) }
      | 0x2F -> { len = Uint16.one; instr = CPL }
      | 0x30 ->
          let e = read_signed_byte_operand () in
          { len = Uint16.of_int 2; instr = JR (NC, e) }
      | 0x31 ->
          let n = read_word_operand () in
          { len = Uint16.of_int 3; instr = LD16 (SP, Imm16 n) }
      | 0x32 -> { len = Uint16.one; instr = LD8 (HL_d, Reg8 A) }
      | 0x33 -> { len = Uint16.one; instr = INC16 SP }
      | 0x34 -> { len = Uint16.one; instr = INC8 (PtrR HL) }
      | 0x35 -> { len = Uint16.one; instr = DEC8 (PtrR HL) }
      | 0x36 ->
          let n = read_byte_operand () in
          { len = Uint16.of_int 2; instr = LD8 (PtrR HL, Imm8 n) }
      | 0x37 -> { len = Uint16.one; instr = SCF }
      | 0x38 ->
          let e = read_signed_byte_operand () in
          { len = Uint16.of_int 2; instr = JR (C, e) }
      | 0x39 -> { len = Uint16.one; instr = ADD16 (Reg16 HL, SP) }
      | 0x3A -> { len = Uint16.one; instr = LD8 (Reg8 A, HL_d) }
      | 0x3B -> { len = Uint16.one; instr = DEC16 SP }
      | 0x3C -> { len = Uint16.one; instr = INC8 (Reg8 A) }
      | 0x3D -> { len = Uint16.one; instr = DEC8 (Reg8 A) }
      | 0x3E ->
          let n = read_byte_operand () in
          { len = Uint16.of_int 2; instr = LD8 (Reg8 A, Imm8 n) }
      | 0x3F -> { len = Uint16.one; instr = CCF }
      | 0x40 -> { len = Uint16.one; instr = LD8 (Reg8 B, Reg8 B) }
      | 0x41 -> { len = Uint16.one; instr = LD8 (Reg8 B, Reg8 C) }
      | 0x42 -> { len = Uint16.one; instr = LD8 (Reg8 B, Reg8 D) }
      | 0x43 -> { len = Uint16.one; instr = LD8 (Reg8 B, Reg8 E) }
      | 0x44 -> { len = Uint16.one; instr = LD8 (Reg8 B, Reg8 H) }
      | 0x45 -> { len = Uint16.one; instr = LD8 (Reg8 B, Reg8 L) }
      | 0x46 -> { len = Uint16.one; instr = LD8 (Reg8 B, PtrR HL) }
      | 0x47 -> { len = Uint16.one; instr = LD8 (Reg8 B, Reg8 A) }
      | 0x48 -> { len = Uint16.one; instr = LD8 (Reg8 C, Reg8 B) }
      | 0x49 -> { len = Uint16.one; instr = LD8 (Reg8 C, Reg8 C) }
      | 0x4A -> { len = Uint16.one; instr = LD8 (Reg8 C, Reg8 D) }
      | 0x4B -> { len = Uint16.one; instr = LD8 (Reg8 C, Reg8 E) }
      | 0x4C -> { len = Uint16.one; instr = LD8 (Reg8 C, Reg8 H) }
      | 0x4D -> { len = Uint16.one; instr = LD8 (Reg8 C, Reg8 L) }
      | 0x4E -> { len = Uint16.one; instr = LD8 (Reg8 C, PtrR HL) }
      | 0x4F -> { len = Uint16.one; instr = LD8 (Reg8 C, Reg8 A) }
      | 0x50 -> { len = Uint16.one; instr = LD8 (Reg8 D, Reg8 B) }
      | 0x51 -> { len = Uint16.one; instr = LD8 (Reg8 D, Reg8 C) }
      | 0x52 -> { len = Uint16.one; instr = LD8 (Reg8 D, Reg8 D) }
      | 0x53 -> { len = Uint16.one; instr = LD8 (Reg8 D, Reg8 E) }
      | 0x54 -> { len = Uint16.one; instr = LD8 (Reg8 D, Reg8 H) }
      | 0x55 -> { len = Uint16.one; instr = LD8 (Reg8 D, Reg8 L) }
      | 0x56 -> { len = Uint16.one; instr = LD8 (Reg8 D, PtrR HL) }
      | 0x57 -> { len = Uint16.one; instr = LD8 (Reg8 D, Reg8 A) }
      | 0x58 -> { len = Uint16.one; instr = LD8 (Reg8 E, Reg8 B) }
      | 0x59 -> { len = Uint16.one; instr = LD8 (Reg8 E, Reg8 C) }
      | 0x5A -> { len = Uint16.one; instr = LD8 (Reg8 E, Reg8 D) }
      | 0x5B -> { len = Uint16.one; instr = LD8 (Reg8 E, Reg8 E) }
      | 0x5C -> { len = Uint16.one; instr = LD8 (Reg8 E, Reg8 H) }
      | 0x5D -> { len = Uint16.one; instr = LD8 (Reg8 E, Reg8 L) }
      | 0x5E -> { len = Uint16.one; instr = LD8 (Reg8 E, PtrR HL) }
      | 0x5F -> { len = Uint16.one; instr = LD8 (Reg8 E, Reg8 A) }
      | 0x60 -> { len = Uint16.one; instr = LD8 (Reg8 H, Reg8 B) }
      | 0x61 -> { len = Uint16.one; instr = LD8 (Reg8 H, Reg8 C) }
      | 0x62 -> { len = Uint16.one; instr = LD8 (Reg8 H, Reg8 D) }
      | 0x63 -> { len = Uint16.one; instr = LD8 (Reg8 H, Reg8 E) }
      | 0x64 -> { len = Uint16.one; instr = LD8 (Reg8 H, Reg8 H) }
      | 0x65 -> { len = Uint16.one; instr = LD8 (Reg8 H, Reg8 L) }
      | 0x66 -> { len = Uint16.one; instr = LD8 (Reg8 H, PtrR HL) }
      | 0x67 -> { len = Uint16.one; instr = LD8 (Reg8 H, Reg8 A) }
      | 0x68 -> { len = Uint16.one; instr = LD8 (Reg8 L, Reg8 B) }
      | 0x69 -> { len = Uint16.one; instr = LD8 (Reg8 L, Reg8 C) }
      | 0x6A -> { len = Uint16.one; instr = LD8 (Reg8 L, Reg8 D) }
      | 0x6B -> { len = Uint16.one; instr = LD8 (Reg8 L, Reg8 E) }
      | 0x6C -> { len = Uint16.one; instr = LD8 (Reg8 L, Reg8 H) }
      | 0x6D -> { len = Uint16.one; instr = LD8 (Reg8 L, Reg8 L) }
      | 0x6E -> { len = Uint16.one; instr = LD8 (Reg8 L, PtrR HL) }
      | 0x6F -> { len = Uint16.one; instr = LD8 (Reg8 L, Reg8 A) }
      | 0x70 -> { len = Uint16.one; instr = LD8 (PtrR HL, Reg8 B) }
      | 0x71 -> { len = Uint16.one; instr = LD8 (PtrR HL, Reg8 C) }
      | 0x72 -> { len = Uint16.one; instr = LD8 (PtrR HL, Reg8 D) }
      | 0x73 -> { len = Uint16.one; instr = LD8 (PtrR HL, Reg8 E) }
      | 0x74 -> { len = Uint16.one; instr = LD8 (PtrR HL, Reg8 H) }
      | 0x75 -> { len = Uint16.one; instr = LD8 (PtrR HL, Reg8 L) }
      | 0x76 -> { len = Uint16.one; instr = HALT }
      | 0x77 -> { len = Uint16.one; instr = LD8 (PtrR HL, Reg8 A) }
      | 0x78 -> { len = Uint16.one; instr = LD8 (Reg8 A, Reg8 B) }
      | 0x79 -> { len = Uint16.one; instr = LD8 (Reg8 A, Reg8 C) }
      | 0x7A -> { len = Uint16.one; instr = LD8 (Reg8 A, Reg8 D) }
      | 0x7B -> { len = Uint16.one; instr = LD8 (Reg8 A, Reg8 E) }
      | 0x7C -> { len = Uint16.one; instr = LD8 (Reg8 A, Reg8 H) }
      | 0x7D -> { len = Uint16.one; instr = LD8 (Reg8 A, Reg8 L) }
      | 0x7E -> { len = Uint16.one; instr = LD8 (Reg8 A, PtrR HL) }
      | 0x7F -> { len = Uint16.one; instr = LD8 (Reg8 A, Reg8 A) }
      | 0x80 -> { len = Uint16.one; instr = ADD8 (Reg8 A, Reg8 B) }
      | 0x81 -> { len = Uint16.one; instr = ADD8 (Reg8 A, Reg8 C) }
      | 0x82 -> { len = Uint16.one; instr = ADD8 (Reg8 A, Reg8 D) }
      | 0x83 -> { len = Uint16.one; instr = ADD8 (Reg8 A, Reg8 E) }
      | 0x84 -> { len = Uint16.one; instr = ADD8 (Reg8 A, Reg8 H) }
      | 0x85 -> { len = Uint16.one; instr = ADD8 (Reg8 A, Reg8 L) }
      | 0x86 -> { len = Uint16.one; instr = ADD8 (Reg8 A, PtrR HL) }
      | 0x87 -> { len = Uint16.one; instr = ADD8 (Reg8 A, Reg8 A) }
      | 0x88 -> { len = Uint16.one; instr = ADC (Reg8 A, Reg8 B) }
      | 0x89 -> { len = Uint16.one; instr = ADC (Reg8 A, Reg8 C) }
      | 0x8A -> { len = Uint16.one; instr = ADC (Reg8 A, Reg8 D) }
      | 0x8B -> { len = Uint16.one; instr = ADC (Reg8 A, Reg8 E) }
      | 0x8C -> { len = Uint16.one; instr = ADC (Reg8 A, Reg8 H) }
      | 0x8D -> { len = Uint16.one; instr = ADC (Reg8 A, Reg8 L) }
      | 0x8E -> { len = Uint16.one; instr = ADC (Reg8 A, PtrR HL) }
      | 0x8F -> { len = Uint16.one; instr = ADC (Reg8 A, Reg8 A) }
      | 0x90 -> { len = Uint16.one; instr = SUB (Reg8 A, Reg8 B) }
      | 0x91 -> { len = Uint16.one; instr = SUB (Reg8 A, Reg8 C) }
      | 0x92 -> { len = Uint16.one; instr = SUB (Reg8 A, Reg8 D) }
      | 0x93 -> { len = Uint16.one; instr = SUB (Reg8 A, Reg8 E) }
      | 0x94 -> { len = Uint16.one; instr = SUB (Reg8 A, Reg8 H) }
      | 0x95 -> { len = Uint16.one; instr = SUB (Reg8 A, Reg8 L) }
      | 0x96 -> { len = Uint16.one; instr = SUB (Reg8 A, PtrR HL) }
      | 0x97 -> { len = Uint16.one; instr = SUB (Reg8 A, Reg8 A) }
      | 0x98 -> { len = Uint16.one; instr = SBC (Reg8 A, Reg8 B) }
      | 0x99 -> { len = Uint16.one; instr = SBC (Reg8 A, Reg8 C) }
      | 0x9A -> { len = Uint16.one; instr = SBC (Reg8 A, Reg8 D) }
      | 0x9B -> { len = Uint16.one; instr = SBC (Reg8 A, Reg8 E) }
      | 0x9C -> { len = Uint16.one; instr = SBC (Reg8 A, Reg8 H) }
      | 0x9D -> { len = Uint16.one; instr = SBC (Reg8 A, Reg8 L) }
      | 0x9E -> { len = Uint16.one; instr = SBC (Reg8 A, PtrR HL) }
      | 0x9F -> { len = Uint16.one; instr = SBC (Reg8 A, Reg8 A) }
      | 0xA0 -> { len = Uint16.one; instr = AND (Reg8 A, Reg8 B) }
      | 0xA1 -> { len = Uint16.one; instr = AND (Reg8 A, Reg8 C) }
      | 0xA2 -> { len = Uint16.one; instr = AND (Reg8 A, Reg8 D) }
      | 0xA3 -> { len = Uint16.one; instr = AND (Reg8 A, Reg8 E) }
      | 0xA4 -> { len = Uint16.one; instr = AND (Reg8 A, Reg8 H) }
      | 0xA5 -> { len = Uint16.one; instr = AND (Reg8 A, Reg8 L) }
      | 0xA6 -> { len = Uint16.one; instr = AND (Reg8 A, PtrR HL) }
      | 0xA7 -> { len = Uint16.one; instr = AND (Reg8 A, Reg8 A) }
      | 0xA8 -> { len = Uint16.one; instr = XOR (Reg8 A, Reg8 B) }
      | 0xA9 -> { len = Uint16.one; instr = XOR (Reg8 A, Reg8 C) }
      | 0xAA -> { len = Uint16.one; instr = XOR (Reg8 A, Reg8 D) }
      | 0xAB -> { len = Uint16.one; instr = XOR (Reg8 A, Reg8 E) }
      | 0xAC -> { len = Uint16.one; instr = XOR (Reg8 A, Reg8 H) }
      | 0xAD -> { len = Uint16.one; instr = XOR (Reg8 A, Reg8 L) }
      | 0xAE -> { len = Uint16.one; instr = XOR (Reg8 A, PtrR HL) }
      | 0xAF -> { len = Uint16.one; instr = XOR (Reg8 A, Reg8 A) }
      | 0xB0 -> { len = Uint16.one; instr = OR (Reg8 A, Reg8 B) }
      | 0xB1 -> { len = Uint16.one; instr = OR (Reg8 A, Reg8 C) }
      | 0xB2 -> { len = Uint16.one; instr = OR (Reg8 A, Reg8 D) }
      | 0xB3 -> { len = Uint16.one; instr = OR (Reg8 A, Reg8 E) }
      | 0xB4 -> { len = Uint16.one; instr = OR (Reg8 A, Reg8 H) }
      | 0xB5 -> { len = Uint16.one; instr = OR (Reg8 A, Reg8 L) }
      | 0xB6 -> { len = Uint16.one; instr = OR (Reg8 A, PtrR HL) }
      | 0xB7 -> { len = Uint16.one; instr = OR (Reg8 A, Reg8 A) }
      | 0xB8 -> { len = Uint16.one; instr = CP (Reg8 A, Reg8 B) }
      | 0xB9 -> { len = Uint16.one; instr = CP (Reg8 A, Reg8 C) }
      | 0xBA -> { len = Uint16.one; instr = CP (Reg8 A, Reg8 D) }
      | 0xBB -> { len = Uint16.one; instr = CP (Reg8 A, Reg8 E) }
      | 0xBC -> { len = Uint16.one; instr = CP (Reg8 A, Reg8 H) }
      | 0xBD -> { len = Uint16.one; instr = CP (Reg8 A, Reg8 L) }
      | 0xBE -> { len = Uint16.one; instr = CP (Reg8 A, PtrR HL) }
      | 0xBF -> { len = Uint16.one; instr = CP (Reg8 A, Reg8 A) }
      | 0xC0 -> { len = Uint16.one; instr = RET NZ }
      | 0xC1 -> { len = Uint16.one; instr = POP (Reg16 BC) }
      | 0xC2 ->
          let n = read_word_operand () in
          { len = Uint16.of_int 3; instr = JP (NZ, Imm16 n) }
      | 0xC3 ->
          let n = read_word_operand () in
          { len = Uint16.of_int 3; instr = JP (None, Imm16 n) }
      | 0xC4 ->
          let n = read_word_operand () in
          { len = Uint16.of_int 3; instr = CALL (NZ, n) }
      | 0xC5 -> { len = Uint16.one; instr = PUSH (Reg16 BC) }
      | 0xC6 ->
          let n = read_byte_operand () in
          { len = Uint16.of_int 2; instr = ADD8 (Reg8 A, Imm8 n) }
      | 0xC7 -> { len = Uint16.one; instr = RST (Uint16.of_int 0x00) }
      | 0xC8 -> { len = Uint16.one; instr = RET Z }
      | 0xC9 -> { len = Uint16.one; instr = RET None }
      | 0xCA ->
          let n = read_word_operand () in
          { len = Uint16.of_int 3; instr = JP (Z, Imm16 n) }
      (* 0xCB prefix is handled later *)
      | 0xCC ->
          let n = read_word_operand () in
          { len = Uint16.of_int 3; instr = CALL (Z, n) }
      | 0xCD ->
          let n = read_word_operand () in
          { len = Uint16.of_int 3; instr = CALL (None, n) }
      | 0xCE ->
          let n = read_byte_operand () in
          { len = Uint16.of_int 2; instr = ADC (Reg8 A, Imm8 n) }
      | 0xCF -> { len = Uint16.one; instr = RST (Uint16.of_int 0x08) }
      | 0xD0 -> { len = Uint16.one; instr = RET NC }
      | 0xD1 -> { len = Uint16.one; instr = POP (Reg16 DE) }
      | 0xD2 ->
          let n = read_word_operand () in
          { len = Uint16.of_int 3; instr = JP (NC, Imm16 n) }
      | 0xD3 -> failwith "Illegal opcode 0xD3"
      | 0xD4 ->
          let n = read_word_operand () in
          { len = Uint16.of_int 3; instr = CALL (NC, n) }
      | 0xD5 -> { len = Uint16.one; instr = PUSH (Reg16 DE) }
      | 0xD6 ->
          let n = read_byte_operand () in
          { len = Uint16.of_int 2; instr = SUB (Reg8 A, Imm8 n) }
      | 0xD7 -> { len = Uint16.one; instr = RST (Uint16.of_int 0x10) }
      | 0xD8 -> { len = Uint16.one; instr = RET C }
      | 0xD9 -> { len = Uint16.one; instr = RETI }
      | 0xDA ->
          let n = read_word_operand () in
          { len = Uint16.of_int 3; instr = JP (C, Imm16 n) }
      | 0xDB -> failwith "Illegal opcode 0xDB"
      | 0xDC ->
          let n = read_word_operand () in
          { len = Uint16.of_int 3; instr = CALL (C, n) }
      | 0xDD -> failwith "Illegal opcode 0xDD"
      | 0xDE ->
          let n = read_byte_operand () in
          { len = Uint16.of_int 2; instr = SBC (Reg8 A, Imm8 n) }
      | 0xDF -> { len = Uint16.one; instr = RST (Uint16.of_int 0x18) }
      | 0xE0 ->
          let n = read_byte_operand () in
          { len = Uint16.of_int 2; instr = LD8 (Offset n, Reg8 A) }
      | 0xE1 -> { len = Uint16.one; instr = POP (Reg16 HL) }
      | 0xE2 -> { len = Uint16.one; instr = LD8 (C_offset, Reg8 A) }
      | 0xE3 -> failwith "Illegal opcode 0xE3"
      | 0xE4 -> failwith "Illegal opcode 0xE4"
      | 0xE5 -> { len = Uint16.one; instr = PUSH (Reg16 HL) }
      | 0xE6 ->
          let n = read_byte_operand () in
          { len = Uint16.of_int 2; instr = AND (Reg8 A, Imm8 n) }
      | 0xE7 -> { len = Uint16.one; instr = RST (Uint16.of_int 0x20) }
      | 0xE8 ->
          let e = read_signed_byte_operand () in
          { len = Uint16.of_int 2; instr = ADDSP e }
      | 0xE9 -> { len = Uint16.one; instr = JP (None, Reg16 HL) }
      | 0xEA ->
          let n = read_word_operand () in
          { len = Uint16.of_int 3; instr = LD8 (PtrImm n, Reg8 A) }
      | 0xEB -> failwith "Illegal opcode 0xEB"
      | 0xEC -> failwith "Illegal opcode 0xEC"
      | 0xED -> failwith "Illegal opcode 0xED"
      | 0xEE ->
          let n = read_byte_operand () in
          { len = Uint16.of_int 2; instr = XOR (Reg8 A, Imm8 n) }
      | 0xEF -> { len = Uint16.one; instr = RST (Uint16.of_int 0x28) }
      | 0xF0 ->
          let n = read_byte_operand () in
          { len = Uint16.of_int 2; instr = LD8 (Reg8 A, Offset n) }
      | 0xF1 -> { len = Uint16.one; instr = POP (Reg16 AF) }
      | 0xF2 -> { len = Uint16.one; instr = LD8 (Reg8 A, C_offset) }
      | 0xF3 -> { len = Uint16.one; instr = DI }
      | 0xF4 -> failwith "Illegal opcode 0xF4"
      | 0xF5 -> { len = Uint16.one; instr = PUSH (Reg16 AF) }
      | 0xF6 ->
          let n = read_byte_operand () in
          { len = Uint16.of_int 2; instr = OR (Reg8 A, Imm8 n) }
      | 0xF7 -> { len = Uint16.one; instr = RST (Uint16.of_int 0x30) }
      | 0xF8 ->
          let e = read_signed_byte_operand () in
          { len = Uint16.of_int 2; instr = LD16 (Reg16 HL, SP_offset e) }
      | 0xF9 -> { len = Uint16.one; instr = LD16 (SP, Reg16 HL) }
      | 0xFA ->
          let n = read_word_operand () in
          { len = Uint16.of_int 3; instr = LD8 (Reg8 A, PtrImm n) }
      | 0xFB -> { len = Uint16.one; instr = EI }
      | 0xFC -> failwith "Illegal opcode 0xFC"
      | 0xFD -> failwith "Illegal opcode 0xFD"
      | 0xFE ->
          let n = read_byte_operand () in
          { len = Uint16.of_int 2; instr = CP (Reg8 A, Imm8 n) }
      | 0xCB -> (
          let op = Mem.read_byte mem (Uint16.succ pc) |> Uint8.to_int in

          match op with
          | 0x00 -> { len = Uint16.of_int 2; instr = RLC (Reg8 B) }
          | 0x01 -> { len = Uint16.of_int 2; instr = RLC (Reg8 C) }
          | 0x02 -> { len = Uint16.of_int 2; instr = RLC (Reg8 D) }
          | 0x03 -> { len = Uint16.of_int 2; instr = RLC (Reg8 E) }
          | 0x04 -> { len = Uint16.of_int 2; instr = RLC (Reg8 H) }
          | 0x05 -> { len = Uint16.of_int 2; instr = RLC (Reg8 L) }
          | 0x06 -> { len = Uint16.of_int 2; instr = RLC (PtrR HL) }
          | 0x07 -> { len = Uint16.of_int 2; instr = RLC (Reg8 A) }
          | 0x08 -> { len = Uint16.of_int 2; instr = RRC (Reg8 B) }
          | 0x09 -> { len = Uint16.of_int 2; instr = RRC (Reg8 C) }
          | 0x0A -> { len = Uint16.of_int 2; instr = RRC (Reg8 D) }
          | 0x0B -> { len = Uint16.of_int 2; instr = RRC (Reg8 E) }
          | 0x0C -> { len = Uint16.of_int 2; instr = RRC (Reg8 H) }
          | 0x0D -> { len = Uint16.of_int 2; instr = RRC (Reg8 L) }
          | 0x0E -> { len = Uint16.of_int 2; instr = RRC (PtrR HL) }
          | 0x0F -> { len = Uint16.of_int 2; instr = RRC (Reg8 A) }
          | 0x10 -> { len = Uint16.of_int 2; instr = RL (Reg8 B) }
          | 0x11 -> { len = Uint16.of_int 2; instr = RL (Reg8 C) }
          | 0x12 -> { len = Uint16.of_int 2; instr = RL (Reg8 D) }
          | 0x13 -> { len = Uint16.of_int 2; instr = RL (Reg8 E) }
          | 0x14 -> { len = Uint16.of_int 2; instr = RL (Reg8 H) }
          | 0x15 -> { len = Uint16.of_int 2; instr = RL (Reg8 L) }
          | 0x16 -> { len = Uint16.of_int 2; instr = RL (PtrR HL) }
          | 0x17 -> { len = Uint16.of_int 2; instr = RL (Reg8 A) }
          | 0x18 -> { len = Uint16.of_int 2; instr = RR (Reg8 B) }
          | 0x19 -> { len = Uint16.of_int 2; instr = RR (Reg8 C) }
          | 0x1A -> { len = Uint16.of_int 2; instr = RR (Reg8 D) }
          | 0x1B -> { len = Uint16.of_int 2; instr = RR (Reg8 E) }
          | 0x1C -> { len = Uint16.of_int 2; instr = RR (Reg8 H) }
          | 0x1D -> { len = Uint16.of_int 2; instr = RR (Reg8 L) }
          | 0x1E -> { len = Uint16.of_int 2; instr = RR (PtrR HL) }
          | 0x1F -> { len = Uint16.of_int 2; instr = RR (Reg8 A) }
          | 0x20 -> { len = Uint16.of_int 2; instr = SLA (Reg8 B) }
          | 0x21 -> { len = Uint16.of_int 2; instr = SLA (Reg8 C) }
          | 0x22 -> { len = Uint16.of_int 2; instr = SLA (Reg8 D) }
          | 0x23 -> { len = Uint16.of_int 2; instr = SLA (Reg8 E) }
          | 0x24 -> { len = Uint16.of_int 2; instr = SLA (Reg8 H) }
          | 0x25 -> { len = Uint16.of_int 2; instr = SLA (Reg8 L) }
          | 0x26 -> { len = Uint16.of_int 2; instr = SLA (PtrR HL) }
          | 0x27 -> { len = Uint16.of_int 2; instr = SLA (Reg8 A) }
          | 0x28 -> { len = Uint16.of_int 2; instr = SRA (Reg8 B) }
          | 0x29 -> { len = Uint16.of_int 2; instr = SRA (Reg8 C) }
          | 0x2A -> { len = Uint16.of_int 2; instr = SRA (Reg8 D) }
          | 0x2B -> { len = Uint16.of_int 2; instr = SRA (Reg8 E) }
          | 0x2C -> { len = Uint16.of_int 2; instr = SRA (Reg8 H) }
          | 0x2D -> { len = Uint16.of_int 2; instr = SRA (Reg8 L) }
          | 0x2E -> { len = Uint16.of_int 2; instr = SRA (PtrR HL) }
          | 0x2F -> { len = Uint16.of_int 2; instr = SRA (Reg8 A) }
          | 0x30 -> { len = Uint16.of_int 2; instr = SWAP (Reg8 B) }
          | 0x31 -> { len = Uint16.of_int 2; instr = SWAP (Reg8 C) }
          | 0x32 -> { len = Uint16.of_int 2; instr = SWAP (Reg8 D) }
          | 0x33 -> { len = Uint16.of_int 2; instr = SWAP (Reg8 E) }
          | 0x34 -> { len = Uint16.of_int 2; instr = SWAP (Reg8 H) }
          | 0x35 -> { len = Uint16.of_int 2; instr = SWAP (Reg8 L) }
          | 0x36 -> { len = Uint16.of_int 2; instr = SWAP (PtrR HL) }
          | 0x37 -> { len = Uint16.of_int 2; instr = SWAP (Reg8 A) }
          | 0x38 -> { len = Uint16.of_int 2; instr = SRL (Reg8 B) }
          | 0x39 -> { len = Uint16.of_int 2; instr = SRL (Reg8 C) }
          | 0x3A -> { len = Uint16.of_int 2; instr = SRL (Reg8 D) }
          | 0x3B -> { len = Uint16.of_int 2; instr = SRL (Reg8 E) }
          | 0x3C -> { len = Uint16.of_int 2; instr = SRL (Reg8 H) }
          | 0x3D -> { len = Uint16.of_int 2; instr = SRL (Reg8 L) }
          | 0x3E -> { len = Uint16.of_int 2; instr = SRL (PtrR HL) }
          | 0x3F -> { len = Uint16.of_int 2; instr = SRL (Reg8 A) }
          | 0x40 -> { len = Uint16.of_int 2; instr = BIT (0, Reg8 B) }
          | 0x41 -> { len = Uint16.of_int 2; instr = BIT (0, Reg8 C) }
          | 0x42 -> { len = Uint16.of_int 2; instr = BIT (0, Reg8 D) }
          | 0x43 -> { len = Uint16.of_int 2; instr = BIT (0, Reg8 E) }
          | 0x44 -> { len = Uint16.of_int 2; instr = BIT (0, Reg8 H) }
          | 0x45 -> { len = Uint16.of_int 2; instr = BIT (0, Reg8 L) }
          | 0x46 -> { len = Uint16.of_int 2; instr = BIT (0, PtrR HL) }
          | 0x47 -> { len = Uint16.of_int 2; instr = BIT (0, Reg8 A) }
          | 0x48 -> { len = Uint16.of_int 2; instr = BIT (1, Reg8 B) }
          | 0x49 -> { len = Uint16.of_int 2; instr = BIT (1, Reg8 C) }
          | 0x4A -> { len = Uint16.of_int 2; instr = BIT (1, Reg8 D) }
          | 0x4B -> { len = Uint16.of_int 2; instr = BIT (1, Reg8 E) }
          | 0x4C -> { len = Uint16.of_int 2; instr = BIT (1, Reg8 H) }
          | 0x4D -> { len = Uint16.of_int 2; instr = BIT (1, Reg8 L) }
          | 0x4E -> { len = Uint16.of_int 2; instr = BIT (1, PtrR HL) }
          | 0x4F -> { len = Uint16.of_int 2; instr = BIT (1, Reg8 A) }
          | 0x50 -> { len = Uint16.of_int 2; instr = BIT (2, Reg8 B) }
          | 0x51 -> { len = Uint16.of_int 2; instr = BIT (2, Reg8 C) }
          | 0x52 -> { len = Uint16.of_int 2; instr = BIT (2, Reg8 D) }
          | 0x53 -> { len = Uint16.of_int 2; instr = BIT (2, Reg8 E) }
          | 0x54 -> { len = Uint16.of_int 2; instr = BIT (2, Reg8 H) }
          | 0x55 -> { len = Uint16.of_int 2; instr = BIT (2, Reg8 L) }
          | 0x56 -> { len = Uint16.of_int 2; instr = BIT (2, PtrR HL) }
          | 0x57 -> { len = Uint16.of_int 2; instr = BIT (2, Reg8 A) }
          | 0x58 -> { len = Uint16.of_int 2; instr = BIT (3, Reg8 B) }
          | 0x59 -> { len = Uint16.of_int 2; instr = BIT (3, Reg8 C) }
          | 0x5A -> { len = Uint16.of_int 2; instr = BIT (3, Reg8 D) }
          | 0x5B -> { len = Uint16.of_int 2; instr = BIT (3, Reg8 E) }
          | 0x5C -> { len = Uint16.of_int 2; instr = BIT (3, Reg8 H) }
          | 0x5D -> { len = Uint16.of_int 2; instr = BIT (3, Reg8 L) }
          | 0x5E -> { len = Uint16.of_int 2; instr = BIT (3, PtrR HL) }
          | 0x5F -> { len = Uint16.of_int 2; instr = BIT (3, Reg8 A) }
          | 0x60 -> { len = Uint16.of_int 2; instr = BIT (4, Reg8 B) }
          | 0x61 -> { len = Uint16.of_int 2; instr = BIT (4, Reg8 C) }
          | 0x62 -> { len = Uint16.of_int 2; instr = BIT (4, Reg8 D) }
          | 0x63 -> { len = Uint16.of_int 2; instr = BIT (4, Reg8 E) }
          | 0x64 -> { len = Uint16.of_int 2; instr = BIT (4, Reg8 H) }
          | 0x65 -> { len = Uint16.of_int 2; instr = BIT (4, Reg8 L) }
          | 0x66 -> { len = Uint16.of_int 2; instr = BIT (4, PtrR HL) }
          | 0x67 -> { len = Uint16.of_int 2; instr = BIT (4, Reg8 A) }
          | 0x68 -> { len = Uint16.of_int 2; instr = BIT (5, Reg8 B) }
          | 0x69 -> { len = Uint16.of_int 2; instr = BIT (5, Reg8 C) }
          | 0x6A -> { len = Uint16.of_int 2; instr = BIT (5, Reg8 D) }
          | 0x6B -> { len = Uint16.of_int 2; instr = BIT (5, Reg8 E) }
          | 0x6C -> { len = Uint16.of_int 2; instr = BIT (5, Reg8 H) }
          | 0x6D -> { len = Uint16.of_int 2; instr = BIT (5, Reg8 L) }
          | 0x6E -> { len = Uint16.of_int 2; instr = BIT (5, PtrR HL) }
          | 0x6F -> { len = Uint16.of_int 2; instr = BIT (5, Reg8 A) }
          | 0x70 -> { len = Uint16.of_int 2; instr = BIT (6, Reg8 B) }
          | 0x71 -> { len = Uint16.of_int 2; instr = BIT (6, Reg8 C) }
          | 0x72 -> { len = Uint16.of_int 2; instr = BIT (6, Reg8 D) }
          | 0x73 -> { len = Uint16.of_int 2; instr = BIT (6, Reg8 E) }
          | 0x74 -> { len = Uint16.of_int 2; instr = BIT (6, Reg8 H) }
          | 0x75 -> { len = Uint16.of_int 2; instr = BIT (6, Reg8 L) }
          | 0x76 -> { len = Uint16.of_int 2; instr = BIT (6, PtrR HL) }
          | 0x77 -> { len = Uint16.of_int 2; instr = BIT (6, Reg8 A) }
          | 0x78 -> { len = Uint16.of_int 2; instr = BIT (7, Reg8 B) }
          | 0x79 -> { len = Uint16.of_int 2; instr = BIT (7, Reg8 C) }
          | 0x7A -> { len = Uint16.of_int 2; instr = BIT (7, Reg8 D) }
          | 0x7B -> { len = Uint16.of_int 2; instr = BIT (7, Reg8 E) }
          | 0x7C -> { len = Uint16.of_int 2; instr = BIT (7, Reg8 H) }
          | 0x7D -> { len = Uint16.of_int 2; instr = BIT (7, Reg8 L) }
          | 0x7E -> { len = Uint16.of_int 2; instr = BIT (7, PtrR HL) }
          | 0x7F -> { len = Uint16.of_int 2; instr = BIT (7, Reg8 A) }
          | 0x80 -> { len = Uint16.of_int 2; instr = RES (0, Reg8 B) }
          | 0x81 -> { len = Uint16.of_int 2; instr = RES (0, Reg8 C) }
          | 0x82 -> { len = Uint16.of_int 2; instr = RES (0, Reg8 D) }
          | 0x83 -> { len = Uint16.of_int 2; instr = RES (0, Reg8 E) }
          | 0x84 -> { len = Uint16.of_int 2; instr = RES (0, Reg8 H) }
          | 0x85 -> { len = Uint16.of_int 2; instr = RES (0, Reg8 L) }
          | 0x86 -> { len = Uint16.of_int 2; instr = RES (0, PtrR HL) }
          | 0x87 -> { len = Uint16.of_int 2; instr = RES (0, Reg8 A) }
          | 0x88 -> { len = Uint16.of_int 2; instr = RES (1, Reg8 B) }
          | 0x89 -> { len = Uint16.of_int 2; instr = RES (1, Reg8 C) }
          | 0x8A -> { len = Uint16.of_int 2; instr = RES (1, Reg8 D) }
          | 0x8B -> { len = Uint16.of_int 2; instr = RES (1, Reg8 E) }
          | 0x8C -> { len = Uint16.of_int 2; instr = RES (1, Reg8 H) }
          | 0x8D -> { len = Uint16.of_int 2; instr = RES (1, Reg8 L) }
          | 0x8E -> { len = Uint16.of_int 2; instr = RES (1, PtrR HL) }
          | 0x8F -> { len = Uint16.of_int 2; instr = RES (1, Reg8 A) }
          | 0x90 -> { len = Uint16.of_int 2; instr = RES (2, Reg8 B) }
          | 0x91 -> { len = Uint16.of_int 2; instr = RES (2, Reg8 C) }
          | 0x92 -> { len = Uint16.of_int 2; instr = RES (2, Reg8 D) }
          | 0x93 -> { len = Uint16.of_int 2; instr = RES (2, Reg8 E) }
          | 0x94 -> { len = Uint16.of_int 2; instr = RES (2, Reg8 H) }
          | 0x95 -> { len = Uint16.of_int 2; instr = RES (2, Reg8 L) }
          | 0x96 -> { len = Uint16.of_int 2; instr = RES (2, PtrR HL) }
          | 0x97 -> { len = Uint16.of_int 2; instr = RES (2, Reg8 A) }
          | 0x98 -> { len = Uint16.of_int 2; instr = RES (3, Reg8 B) }
          | 0x99 -> { len = Uint16.of_int 2; instr = RES (3, Reg8 C) }
          | 0x9A -> { len = Uint16.of_int 2; instr = RES (3, Reg8 D) }
          | 0x9B -> { len = Uint16.of_int 2; instr = RES (3, Reg8 E) }
          | 0x9C -> { len = Uint16.of_int 2; instr = RES (3, Reg8 H) }
          | 0x9D -> { len = Uint16.of_int 2; instr = RES (3, Reg8 L) }
          | 0x9E -> { len = Uint16.of_int 2; instr = RES (3, PtrR HL) }
          | 0x9F -> { len = Uint16.of_int 2; instr = RES (3, Reg8 A) }
          | 0xA0 -> { len = Uint16.of_int 2; instr = RES (4, Reg8 B) }
          | 0xA1 -> { len = Uint16.of_int 2; instr = RES (4, Reg8 C) }
          | 0xA2 -> { len = Uint16.of_int 2; instr = RES (4, Reg8 D) }
          | 0xA3 -> { len = Uint16.of_int 2; instr = RES (4, Reg8 E) }
          | 0xA4 -> { len = Uint16.of_int 2; instr = RES (4, Reg8 H) }
          | 0xA5 -> { len = Uint16.of_int 2; instr = RES (4, Reg8 L) }
          | 0xA6 -> { len = Uint16.of_int 2; instr = RES (4, PtrR HL) }
          | 0xA7 -> { len = Uint16.of_int 2; instr = RES (4, Reg8 A) }
          | 0xA8 -> { len = Uint16.of_int 2; instr = RES (5, Reg8 B) }
          | 0xA9 -> { len = Uint16.of_int 2; instr = RES (5, Reg8 C) }
          | 0xAA -> { len = Uint16.of_int 2; instr = RES (5, Reg8 D) }
          | 0xAB -> { len = Uint16.of_int 2; instr = RES (5, Reg8 E) }
          | 0xAC -> { len = Uint16.of_int 2; instr = RES (5, Reg8 H) }
          | 0xAD -> { len = Uint16.of_int 2; instr = RES (5, Reg8 L) }
          | 0xAE -> { len = Uint16.of_int 2; instr = RES (5, PtrR HL) }
          | 0xAF -> { len = Uint16.of_int 2; instr = RES (5, Reg8 A) }
          | 0xB0 -> { len = Uint16.of_int 2; instr = RES (6, Reg8 B) }
          | 0xB1 -> { len = Uint16.of_int 2; instr = RES (6, Reg8 C) }
          | 0xB2 -> { len = Uint16.of_int 2; instr = RES (6, Reg8 D) }
          | 0xB3 -> { len = Uint16.of_int 2; instr = RES (6, Reg8 E) }
          | 0xB4 -> { len = Uint16.of_int 2; instr = RES (6, Reg8 H) }
          | 0xB5 -> { len = Uint16.of_int 2; instr = RES (6, Reg8 L) }
          | 0xB6 -> { len = Uint16.of_int 2; instr = RES (6, PtrR HL) }
          | 0xB7 -> { len = Uint16.of_int 2; instr = RES (6, Reg8 A) }
          | 0xB8 -> { len = Uint16.of_int 2; instr = RES (7, Reg8 B) }
          | 0xB9 -> { len = Uint16.of_int 2; instr = RES (7, Reg8 C) }
          | 0xBA -> { len = Uint16.of_int 2; instr = RES (7, Reg8 D) }
          | 0xBB -> { len = Uint16.of_int 2; instr = RES (7, Reg8 E) }
          | 0xBC -> { len = Uint16.of_int 2; instr = RES (7, Reg8 H) }
          | 0xBD -> { len = Uint16.of_int 2; instr = RES (7, Reg8 L) }
          | 0xBE -> { len = Uint16.of_int 2; instr = RES (7, PtrR HL) }
          | 0xBF -> { len = Uint16.of_int 2; instr = RES (7, Reg8 A) }
          | 0xC0 -> { len = Uint16.of_int 2; instr = SET (0, Reg8 B) }
          | 0xC1 -> { len = Uint16.of_int 2; instr = SET (0, Reg8 C) }
          | 0xC2 -> { len = Uint16.of_int 2; instr = SET (0, Reg8 D) }
          | 0xC3 -> { len = Uint16.of_int 2; instr = SET (0, Reg8 E) }
          | 0xC4 -> { len = Uint16.of_int 2; instr = SET (0, Reg8 H) }
          | 0xC5 -> { len = Uint16.of_int 2; instr = SET (0, Reg8 L) }
          | 0xC6 -> { len = Uint16.of_int 2; instr = SET (0, PtrR HL) }
          | 0xC7 -> { len = Uint16.of_int 2; instr = SET (0, Reg8 A) }
          | 0xC8 -> { len = Uint16.of_int 2; instr = SET (1, Reg8 B) }
          | 0xC9 -> { len = Uint16.of_int 2; instr = SET (1, Reg8 C) }
          | 0xCA -> { len = Uint16.of_int 2; instr = SET (1, Reg8 D) }
          | 0xCB -> { len = Uint16.of_int 2; instr = SET (1, Reg8 E) }
          | 0xCC -> { len = Uint16.of_int 2; instr = SET (1, Reg8 H) }
          | 0xCD -> { len = Uint16.of_int 2; instr = SET (1, Reg8 L) }
          | 0xCE -> { len = Uint16.of_int 2; instr = SET (1, PtrR HL) }
          | 0xCF -> { len = Uint16.of_int 2; instr = SET (1, Reg8 A) }
          | 0xD0 -> { len = Uint16.of_int 2; instr = SET (2, Reg8 B) }
          | 0xD1 -> { len = Uint16.of_int 2; instr = SET (2, Reg8 C) }
          | 0xD2 -> { len = Uint16.of_int 2; instr = SET (2, Reg8 D) }
          | 0xD3 -> { len = Uint16.of_int 2; instr = SET (2, Reg8 E) }
          | 0xD4 -> { len = Uint16.of_int 2; instr = SET (2, Reg8 H) }
          | 0xD5 -> { len = Uint16.of_int 2; instr = SET (2, Reg8 L) }
          | 0xD6 -> { len = Uint16.of_int 2; instr = SET (2, PtrR HL) }
          | 0xD7 -> { len = Uint16.of_int 2; instr = SET (2, Reg8 A) }
          | 0xD8 -> { len = Uint16.of_int 2; instr = SET (3, Reg8 B) }
          | 0xD9 -> { len = Uint16.of_int 2; instr = SET (3, Reg8 C) }
          | 0xDA -> { len = Uint16.of_int 2; instr = SET (3, Reg8 D) }
          | 0xDB -> { len = Uint16.of_int 2; instr = SET (3, Reg8 E) }
          | 0xDC -> { len = Uint16.of_int 2; instr = SET (3, Reg8 H) }
          | 0xDD -> { len = Uint16.of_int 2; instr = SET (3, Reg8 L) }
          | 0xDE -> { len = Uint16.of_int 2; instr = SET (3, PtrR HL) }
          | 0xDF -> { len = Uint16.of_int 2; instr = SET (3, Reg8 A) }
          | 0xE0 -> { len = Uint16.of_int 2; instr = SET (4, Reg8 B) }
          | 0xE1 -> { len = Uint16.of_int 2; instr = SET (4, Reg8 C) }
          | 0xE2 -> { len = Uint16.of_int 2; instr = SET (4, Reg8 D) }
          | 0xE3 -> { len = Uint16.of_int 2; instr = SET (4, Reg8 E) }
          | 0xE4 -> { len = Uint16.of_int 2; instr = SET (4, Reg8 H) }
          | 0xE5 -> { len = Uint16.of_int 2; instr = SET (4, Reg8 L) }
          | 0xE6 -> { len = Uint16.of_int 2; instr = SET (4, PtrR HL) }
          | 0xE7 -> { len = Uint16.of_int 2; instr = SET (4, Reg8 A) }
          | 0xE8 -> { len = Uint16.of_int 2; instr = SET (5, Reg8 B) }
          | 0xE9 -> { len = Uint16.of_int 2; instr = SET (5, Reg8 C) }
          | 0xEA -> { len = Uint16.of_int 2; instr = SET (5, Reg8 D) }
          | 0xEB -> { len = Uint16.of_int 2; instr = SET (5, Reg8 E) }
          | 0xEC -> { len = Uint16.of_int 2; instr = SET (5, Reg8 H) }
          | 0xED -> { len = Uint16.of_int 2; instr = SET (5, Reg8 L) }
          | 0xEE -> { len = Uint16.of_int 2; instr = SET (5, PtrR HL) }
          | 0xEF -> { len = Uint16.of_int 2; instr = SET (5, Reg8 A) }
          | 0xF0 -> { len = Uint16.of_int 2; instr = SET (6, Reg8 B) }
          | 0xF1 -> { len = Uint16.of_int 2; instr = SET (6, Reg8 C) }
          | 0xF2 -> { len = Uint16.of_int 2; instr = SET (6, Reg8 D) }
          | 0xF3 -> { len = Uint16.of_int 2; instr = SET (6, Reg8 E) }
          | 0xF4 -> { len = Uint16.of_int 2; instr = SET (6, Reg8 H) }
          | 0xF5 -> { len = Uint16.of_int 2; instr = SET (6, Reg8 L) }
          | 0xF6 -> { len = Uint16.of_int 2; instr = SET (6, PtrR HL) }
          | 0xF7 -> { len = Uint16.of_int 2; instr = SET (6, Reg8 A) }
          | 0xF8 -> { len = Uint16.of_int 2; instr = SET (7, Reg8 B) }
          | 0xF9 -> { len = Uint16.of_int 2; instr = SET (7, Reg8 C) }
          | 0xFA -> { len = Uint16.of_int 2; instr = SET (7, Reg8 D) }
          | 0xFB -> { len = Uint16.of_int 2; instr = SET (7, Reg8 E) }
          | 0xFC -> { len = Uint16.of_int 2; instr = SET (7, Reg8 H) }
          | 0xFD -> { len = Uint16.of_int 2; instr = SET (7, Reg8 L) }
          | 0xFE -> { len = Uint16.of_int 2; instr = SET (7, PtrR HL) }
          | 0xFF -> { len = Uint16.of_int 2; instr = SET (7, Reg8 A) }
          | _ -> failwith @@ Printf.sprintf "Unhandled prefixed opcode %#x" op)
      | 0xFF -> { len = Uint16.one; instr = RST (Uint16.of_int 0x38) }
      | _ -> failwith @@ Printf.sprintf "Unhandled opcode %#x" op
end
