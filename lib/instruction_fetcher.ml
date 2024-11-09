open Stdint
open Instruction

module Make (Mem : Addressable_intf.WordAddressable) = struct
  let fetch mem ~pc : Instruction_info.t =
      let op = Mem.read_byte mem pc |> Uint8.to_int in
      let read_byte_operand () = Mem.read_byte mem Uint16.(pc + one)
      and read_word_operand () = Mem.read_word mem Uint16.(pc + one) in
      match op with
      | 0x00 -> { len = Uint16.one; instr = NOP }
      | 0x01 ->
          let n = read_word_operand () in
          { len = Uint16.of_int 3; instr = LD16 (Reg16 BC, Imm16 n) }
      (* | 0x02 -> LD8 (Reg16 BC, Reg8 A) *)
      | 0x03 -> { len = Uint16.one; instr = INC16 (Reg16 BC) }
      | 0x04 -> { len = Uint16.one; instr = INC8 (Reg8 B) }
      | 0x05 -> { len = Uint16.one; instr = DEC8 (Reg8 B) }
      | 0x06 ->
          let n = read_byte_operand () in
          { len = Uint16.of_int 2; instr = LD8 (Reg8 B, Imm8 n) }
      | 0x07 -> { len = Uint16.one; instr = RLCA }
      | 0x09 -> { len = Uint16.one; instr = ADD16 (Reg16 HL, Reg16 BC) }
      (* | 0x0A -> LD8 (Reg8 A, Reg16 BC) *)
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
      (* | 0x12 -> LD8 (Reg16 DE, Reg8 A) *)
      | 0x13 -> { len = Uint16.one; instr = INC16 (Reg16 DE) }
      | 0x14 -> { len = Uint16.one; instr = INC8 (Reg8 D) }
      | 0x15 -> { len = Uint16.one; instr = DEC8 (Reg8 D) }
      | 0x16 ->
          let n = read_byte_operand () in
          { len = Uint16.of_int 2; instr = LD8 (Reg8 D, Imm8 n) }
      | 0x17 -> { len = Uint16.one; instr = RLA }
      (* | 0x18 -> JR (int e8) *)
      | 0x19 -> { len = Uint16.one; instr = ADD16 (Reg16 HL, Reg16 DE) }
      (* | 0x1A -> LD8 (Reg8 A, Reg16 DE) *)
      | 0x1B -> { len = Uint16.one; instr = DEC16 (Reg16 DE) }
      | 0x1C -> { len = Uint16.one; instr = INC8 (Reg8 E) }
      | 0x1D -> { len = Uint16.one; instr = DEC8 (Reg8 E) }
      | 0x1E ->
          let n = read_byte_operand () in
          { len = Uint16.of_int 2; instr = LD8 (Reg8 E, Imm8 n) }
      | 0x1F -> { len = Uint16.one; instr = RRA }
      (* | 0x20 -> JR (Reg16 NZ, Reg16 e8) *)
      | 0x21 ->
          let n = read_word_operand () in
          { len = Uint16.of_int 3; instr = LD16 (Reg16 HL, Imm16 n) }
      (* | 0x22 -> LD8 (Reg16 HL, Reg8 A) *)
      | 0x23 -> { len = Uint16.one; instr = INC16 (Reg16 HL) }
      | 0x24 -> { len = Uint16.one; instr = INC8 (Reg8 H) }
      | 0x25 -> { len = Uint16.one; instr = DEC8 (Reg8 H) }
      | 0x26 ->
          let n = read_byte_operand () in
          { len = Uint16.of_int 2; instr = LD8 (Reg8 H, Imm8 n) }
      | 0x27 -> { len = Uint16.one; instr = DAA }
      (* | 0x28 -> JR (Reg8 Z, Reg16 e8) *)
      | 0x29 -> { len = Uint16.one; instr = ADD16 (Reg16 HL, Reg16 HL) }
      (* | 0x2A -> LD8 (Reg8 A, Reg16 HL) *)
      | 0x2B -> { len = Uint16.one; instr = DEC16 (Reg16 HL) }
      | 0x2C -> { len = Uint16.one; instr = INC8 (Reg8 L) }
      | 0x2D -> { len = Uint16.one; instr = DEC8 (Reg8 L) }
      | 0x2E ->
          let n = read_byte_operand () in
          { len = Uint16.of_int 2; instr = LD8 (Reg8 L, Imm8 n) }
      | 0x2F -> { len = Uint16.one; instr = CPL }
      (* | 0x30 -> JR (Reg16 NC, Reg16 e8) *)
      (* | 0x31 -> *)
      (*     let n = read_word () in *)
      (*     LD16 (Reg16 SP, Imm16 n) *)
      (* | 0x32 -> LD8 (Reg16 HL, Reg8 A) *)
      (* | 0x33 -> INC16 (Reg16 SP) *)
      (* | 0x34 -> INC8 (Reg16 HL) *)
      (* | 0x35 -> DEC8 (Reg16 HL) *)
      (* | 0x36 -> *)
      (*     let n = read_byte () in *)
      (*     LD8 (Reg16 HL, Imm8 n) *)
      | 0x37 -> { len = Uint16.one; instr = SCF }
      (* | 0x38 -> JR (Reg8 C, Reg16 e8) *)
      (* | 0x39 -> ADD16 (Reg16 HL, Reg16 SP) *)
      (* | 0x3A -> LD8 (Reg8 A, Reg16 HL) *)
      (* | 0x3B -> DEC16 (Reg16 SP) *)
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
      (* | 0x46 -> LD8 (Reg8 B, Reg16 HL) *)
      | 0x47 -> { len = Uint16.one; instr = LD8 (Reg8 B, Reg8 A) }
      | 0x48 -> { len = Uint16.one; instr = LD8 (Reg8 C, Reg8 B) }
      | 0x49 -> { len = Uint16.one; instr = LD8 (Reg8 C, Reg8 C) }
      | 0x4A -> { len = Uint16.one; instr = LD8 (Reg8 C, Reg8 D) }
      | 0x4B -> { len = Uint16.one; instr = LD8 (Reg8 C, Reg8 E) }
      | 0x4C -> { len = Uint16.one; instr = LD8 (Reg8 C, Reg8 H) }
      | 0x4D -> { len = Uint16.one; instr = LD8 (Reg8 C, Reg8 L) }
      (* | 0x4E -> LD8 (Reg8 C, Reg16 HL) *)
      | 0x4F -> { len = Uint16.one; instr = LD8 (Reg8 C, Reg8 A) }
      | 0x50 -> { len = Uint16.one; instr = LD8 (Reg8 D, Reg8 B) }
      | 0x51 -> { len = Uint16.one; instr = LD8 (Reg8 D, Reg8 C) }
      | 0x52 -> { len = Uint16.one; instr = LD8 (Reg8 D, Reg8 D) }
      | 0x53 -> { len = Uint16.one; instr = LD8 (Reg8 D, Reg8 E) }
      | 0x54 -> { len = Uint16.one; instr = LD8 (Reg8 D, Reg8 H) }
      | 0x55 -> { len = Uint16.one; instr = LD8 (Reg8 D, Reg8 L) }
      (* | 0x56 -> LD8 (Reg8 D, Reg16 HL) *)
      | 0x57 -> { len = Uint16.one; instr = LD8 (Reg8 D, Reg8 A) }
      | 0x58 -> { len = Uint16.one; instr = LD8 (Reg8 E, Reg8 B) }
      | 0x59 -> { len = Uint16.one; instr = LD8 (Reg8 E, Reg8 C) }
      | 0x5A -> { len = Uint16.one; instr = LD8 (Reg8 E, Reg8 D) }
      | 0x5B -> { len = Uint16.one; instr = LD8 (Reg8 E, Reg8 E) }
      | 0x5C -> { len = Uint16.one; instr = LD8 (Reg8 E, Reg8 H) }
      | 0x5D -> { len = Uint16.one; instr = LD8 (Reg8 E, Reg8 L) }
      (* | 0x5E -> LD8 (Reg8 E, Reg16 HL) *)
      | 0x5F -> { len = Uint16.one; instr = LD8 (Reg8 E, Reg8 A) }
      | 0x60 -> { len = Uint16.one; instr = LD8 (Reg8 H, Reg8 B) }
      | 0x61 -> { len = Uint16.one; instr = LD8 (Reg8 H, Reg8 C) }
      | 0x62 -> { len = Uint16.one; instr = LD8 (Reg8 H, Reg8 D) }
      | 0x63 -> { len = Uint16.one; instr = LD8 (Reg8 H, Reg8 E) }
      | 0x64 -> { len = Uint16.one; instr = LD8 (Reg8 H, Reg8 H) }
      | 0x65 -> { len = Uint16.one; instr = LD8 (Reg8 H, Reg8 L) }
      (* | 0x66 -> LD8 (Reg8 H, Reg16 HL) *)
      | 0x67 -> { len = Uint16.one; instr = LD8 (Reg8 H, Reg8 A) }
      | 0x68 -> { len = Uint16.one; instr = LD8 (Reg8 L, Reg8 B) }
      | 0x69 -> { len = Uint16.one; instr = LD8 (Reg8 L, Reg8 C) }
      | 0x6A -> { len = Uint16.one; instr = LD8 (Reg8 L, Reg8 D) }
      | 0x6B -> { len = Uint16.one; instr = LD8 (Reg8 L, Reg8 E) }
      | 0x6C -> { len = Uint16.one; instr = LD8 (Reg8 L, Reg8 H) }
      | 0x6D -> { len = Uint16.one; instr = LD8 (Reg8 L, Reg8 L) }
      (* | 0x6E -> LD8 (Reg8 L, Reg16 HL) *)
      | 0x6F -> { len = Uint16.one; instr = LD8 (Reg8 L, Reg8 A) }
      (* | 0x70 -> LD8 (Reg16 HL, Reg8 B) *)
      (* | 0x71 -> LD8 (Reg16 HL, Reg8 C) *)
      (* | 0x72 -> LD8 (Reg16 HL, Reg8 D) *)
      (* | 0x73 -> LD8 (Reg16 HL, Reg8 E) *)
      (* | 0x74 -> LD8 (Reg16 HL, Reg8 H) *)
      (* | 0x75 -> LD8 (Reg16 HL, Reg8 L) *)
      | 0x76 -> { len = Uint16.one; instr = HALT }
      (* | 0x77 -> LD8 (Reg16 HL, Reg8 A) *)
      | 0x78 -> { len = Uint16.one; instr = LD8 (Reg8 A, Reg8 B) }
      | 0x79 -> { len = Uint16.one; instr = LD8 (Reg8 A, Reg8 C) }
      | 0x7A -> { len = Uint16.one; instr = LD8 (Reg8 A, Reg8 D) }
      | 0x7B -> { len = Uint16.one; instr = LD8 (Reg8 A, Reg8 E) }
      | 0x7C -> { len = Uint16.one; instr = LD8 (Reg8 A, Reg8 H) }
      | 0x7D -> { len = Uint16.one; instr = LD8 (Reg8 A, Reg8 L) }
      (* | 0x7E -> LD8 (Reg8 A, Reg16 HL) *)
      | 0x7F -> { len = Uint16.one; instr = LD8 (Reg8 A, Reg8 A) }
      | 0x80 -> { len = Uint16.one; instr = ADD8 (Reg8 A, Reg8 B) }
      | 0x81 -> { len = Uint16.one; instr = ADD8 (Reg8 A, Reg8 C) }
      | 0x82 -> { len = Uint16.one; instr = ADD8 (Reg8 A, Reg8 D) }
      | 0x83 -> { len = Uint16.one; instr = ADD8 (Reg8 A, Reg8 E) }
      | 0x84 -> { len = Uint16.one; instr = ADD8 (Reg8 A, Reg8 H) }
      | 0x85 -> { len = Uint16.one; instr = ADD8 (Reg8 A, Reg8 L) }
      (* | 0x86 -> ADD8 (Reg8 A, Reg16 HL) *)
      | 0x87 -> { len = Uint16.one; instr = ADD8 (Reg8 A, Reg8 A) }
      | 0x88 -> { len = Uint16.one; instr = ADC (Reg8 A, Reg8 B) }
      | 0x89 -> { len = Uint16.one; instr = ADC (Reg8 A, Reg8 C) }
      | 0x8A -> { len = Uint16.one; instr = ADC (Reg8 A, Reg8 D) }
      | 0x8B -> { len = Uint16.one; instr = ADC (Reg8 A, Reg8 E) }
      | 0x8C -> { len = Uint16.one; instr = ADC (Reg8 A, Reg8 H) }
      | 0x8D -> { len = Uint16.one; instr = ADC (Reg8 A, Reg8 L) }
      (* | 0x8E -> ADC (Reg8 A, Reg16 HL) *)
      | 0x8F -> { len = Uint16.one; instr = ADC (Reg8 A, Reg8 A) }
      | 0x90 -> { len = Uint16.one; instr = SUB (Reg8 A, Reg8 B) }
      | 0x91 -> { len = Uint16.one; instr = SUB (Reg8 A, Reg8 C) }
      | 0x92 -> { len = Uint16.one; instr = SUB (Reg8 A, Reg8 D) }
      | 0x93 -> { len = Uint16.one; instr = SUB (Reg8 A, Reg8 E) }
      | 0x94 -> { len = Uint16.one; instr = SUB (Reg8 A, Reg8 H) }
      | 0x95 -> { len = Uint16.one; instr = SUB (Reg8 A, Reg8 L) }
      (* | 0x96 -> SUB (Reg8 A, Reg16 HL) *)
      | 0x97 -> { len = Uint16.one; instr = SUB (Reg8 A, Reg8 A) }
      | 0x98 -> { len = Uint16.one; instr = SBC (Reg8 A, Reg8 B) }
      | 0x99 -> { len = Uint16.one; instr = SBC (Reg8 A, Reg8 C) }
      | 0x9A -> { len = Uint16.one; instr = SBC (Reg8 A, Reg8 D) }
      | 0x9B -> { len = Uint16.one; instr = SBC (Reg8 A, Reg8 E) }
      | 0x9C -> { len = Uint16.one; instr = SBC (Reg8 A, Reg8 H) }
      | 0x9D -> { len = Uint16.one; instr = SBC (Reg8 A, Reg8 L) }
      (* | 0x9E -> SBC (Reg8 A, Reg16 HL) *)
      | 0x9F -> { len = Uint16.one; instr = SBC (Reg8 A, Reg8 A) }
      | 0xA0 -> { len = Uint16.one; instr = AND (Reg8 A, Reg8 B) }
      | 0xA1 -> { len = Uint16.one; instr = AND (Reg8 A, Reg8 C) }
      | 0xA2 -> { len = Uint16.one; instr = AND (Reg8 A, Reg8 D) }
      | 0xA3 -> { len = Uint16.one; instr = AND (Reg8 A, Reg8 E) }
      | 0xA4 -> { len = Uint16.one; instr = AND (Reg8 A, Reg8 H) }
      | 0xA5 -> { len = Uint16.one; instr = AND (Reg8 A, Reg8 L) }
      (* | 0xA6 -> AND (Reg8 A, Reg16 HL) *)
      | 0xA7 -> { len = Uint16.one; instr = AND (Reg8 A, Reg8 A) }
      | 0xA8 -> { len = Uint16.one; instr = XOR (Reg8 A, Reg8 B) }
      | 0xA9 -> { len = Uint16.one; instr = XOR (Reg8 A, Reg8 C) }
      | 0xAA -> { len = Uint16.one; instr = XOR (Reg8 A, Reg8 D) }
      | 0xAB -> { len = Uint16.one; instr = XOR (Reg8 A, Reg8 E) }
      | 0xAC -> { len = Uint16.one; instr = XOR (Reg8 A, Reg8 H) }
      | 0xAD -> { len = Uint16.one; instr = XOR (Reg8 A, Reg8 L) }
      (* | 0xAE -> XOR (Reg8 A, Reg16 HL) *)
      | 0xAF -> { len = Uint16.one; instr = XOR (Reg8 A, Reg8 A) }
      | 0xB0 -> { len = Uint16.one; instr = OR (Reg8 A, Reg8 B) }
      | 0xB1 -> { len = Uint16.one; instr = OR (Reg8 A, Reg8 C) }
      | 0xB2 -> { len = Uint16.one; instr = OR (Reg8 A, Reg8 D) }
      | 0xB3 -> { len = Uint16.one; instr = OR (Reg8 A, Reg8 E) }
      | 0xB4 -> { len = Uint16.one; instr = OR (Reg8 A, Reg8 H) }
      | 0xB5 -> { len = Uint16.one; instr = OR (Reg8 A, Reg8 L) }
      (* | 0xB6 -> OR (Reg8 A, Reg16 HL) *)
      | 0xB7 -> { len = Uint16.one; instr = OR (Reg8 A, Reg8 A) }
      | 0xB8 -> { len = Uint16.one; instr = CP (Reg8 A, Reg8 B) }
      | 0xB9 -> { len = Uint16.one; instr = CP (Reg8 A, Reg8 C) }
      | 0xBA -> { len = Uint16.one; instr = CP (Reg8 A, Reg8 D) }
      | 0xBB -> { len = Uint16.one; instr = CP (Reg8 A, Reg8 E) }
      | 0xBC -> { len = Uint16.one; instr = CP (Reg8 A, Reg8 H) }
      | 0xBD -> { len = Uint16.one; instr = CP (Reg8 A, Reg8 L) }
      (* | 0xBE -> CP (Reg8 A, Reg16 HL) *)
      | 0xBF -> { len = Uint16.one; instr = CP (Reg8 A, Reg8 A) }
      (* | 0xC0 -> RET (Reg16 NZ) *)
      (* | 0xC1 -> POP (Reg16 BC) *)
      (* | 0xC5 -> PUSH (Reg16 BC) *)
      | 0xC6 ->
          let n = read_byte_operand () in
          { len = Uint16.of_int 2; instr = ADD8 (Reg8 A, Imm8 n) }
      (* | 0xC8 -> RET (Reg8 Z) *)
      | 0xC9 -> { len = Uint16.one; instr = RET }
      (* | 0xCB -> PREFIX *)
      | 0xCE ->
          let n = read_byte_operand () in
          { len = Uint16.of_int 2; instr = ADC (Reg8 A, Imm8 n) }
      (* | 0xD0 -> RET (Reg16 NC) *)
      (* | 0xD1 -> POP (Reg16 DE) *)
      | 0xD3 -> failwith "Illegal opcode 0xD3"
      (* | 0xD5 -> PUSH (Reg16 DE) *)
      | 0xD6 ->
          let n = read_byte_operand () in
          { len = Uint16.of_int 2; instr = SUB (Reg8 A, Imm8 n) }
      (* | 0xD8 -> RET (Reg8 C) *)
      | 0xD9 -> { len = Uint16.one; instr = RETI }
      | 0xDB -> failwith "Illegal opcode 0xDB"
      | 0xDD -> failwith "Illegal opcode 0xDD"
      | 0xDE ->
          let n = read_byte_operand () in
          { len = Uint16.of_int 2; instr = SBC (Reg8 A, Imm8 n) }
      (* | 0xE0 -> LD8H (Reg16 a8, Reg8 A) *)
      (* | 0xE1 -> POP (Reg16 HL) *)
      | 0xE2 -> { len = Uint16.one; instr = LD8 (Reg8 C, Reg8 A) }
      | 0xE3 -> failwith "Illegal opcode 0xE3"
      | 0xE4 -> failwith "Illegal opcode 0xE4"
      (* | 0xE5 -> PUSH (Reg16 HL) *)
      | 0xE6 ->
          let n = read_byte_operand () in
          { len = Uint16.of_int 2; instr = AND (Reg8 A, Imm8 n) }
      (* | 0xE8 -> ADD8 (Reg16 SP, Reg16 e8) *)
      (* | 0xE9 -> JP (Reg16 HL) *)
      | 0xEB -> failwith "Illegal opcode 0xEB"
      | 0xEC -> failwith "Illegal opcode 0xEC"
      | 0xED -> failwith "Illegal opcode 0xED"
      | 0xEE ->
          let n = read_byte_operand () in
          { len = Uint16.of_int 2; instr = XOR (Reg8 A, Imm8 n) }
      (* | 0xF0 -> LD8H (Reg8 A, Reg16 a8) *)
      (* | 0xF1 -> POP (Reg16 AF) *)
      | 0xF2 -> { len = Uint16.one; instr = LD8 (Reg8 A, Reg8 C) }
      | 0xF3 -> { len = Uint16.one; instr = DI }
      | 0xF4 -> failwith "Illegal opcode 0xF4"
      (* | 0xF5 -> PUSH (Reg16 AF) *)
      | 0xF6 ->
          let n = read_byte_operand () in
          { len = Uint16.of_int 2; instr = OR (Reg8 A, Imm8 n) }
      (* | 0xF8 -> LD16 (Reg16 HL, Reg16 SP, Reg16 e8) *)
      (* | 0xF9 -> LD16 (Reg16 SP, Reg16 HL) *)
      | 0xFB -> { len = Uint16.one; instr = EI }
      | 0xFC -> failwith "Illegal opcode 0xFC"
      | 0xFD -> failwith "Illegal opcode 0xFD"
      | 0xFE ->
          let n = read_byte_operand () in
          { len = Uint16.of_int 2; instr = CP (Reg8 A, Imm8 n) }
      | _ -> failwith @@ Printf.sprintf "Unhandled opcode %#x" op
end
