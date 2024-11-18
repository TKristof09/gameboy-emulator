open Uint
open Instruction

let one = Uint16.one
let two = Uint16.of_int 2
let three = Uint16.of_int 3

module Make (Mem : Addressable_intf.WordAddressable) = struct
  let fetch mem ~pc : Instruction_info.t =
      let op = Mem.read_byte mem pc |> Uint8.to_int in
      let read_byte_operand () = Mem.read_byte mem (Uint16.succ pc) in
      let read_signed_byte_operand () = read_byte_operand () |> Uint8.to_int |> Int8.of_int in
      let read_word_operand () = Mem.read_word mem (Uint16.succ pc) in
      match op with
      | 0x00 -> { len = one; instr = NOP; mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x01 ->
          let n = read_word_operand () in
          {
            len = three;
            instr = LD16 (Reg16 BC, Imm16 n);
            mcycles_branch = 3;
            mcycles_nobranch = 3;
          }
      | 0x02 ->
          { len = one; instr = LD8 (PtrR BC, Reg8 A); mcycles_branch = 2; mcycles_nobranch = 2 }
      | 0x03 -> { len = one; instr = INC16 (Reg16 BC); mcycles_branch = 2; mcycles_nobranch = 2 }
      | 0x04 -> { len = one; instr = INC8 (Reg8 B); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x05 -> { len = one; instr = DEC8 (Reg8 B); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x06 ->
          let n = read_byte_operand () in
          { len = two; instr = LD8 (Reg8 B, Imm8 n); mcycles_branch = 2; mcycles_nobranch = 2 }
      | 0x07 -> { len = one; instr = RLCA; mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x08 ->
          let n = read_word_operand () in
          { len = three; instr = LD16 (PtrImm16 n, SP); mcycles_branch = 5; mcycles_nobranch = 5 }
      | 0x09 ->
          {
            len = one;
            instr = ADD16 (Reg16 HL, Reg16 BC);
            mcycles_branch = 2;
            mcycles_nobranch = 2;
          }
      | 0x0A ->
          { len = one; instr = LD8 (Reg8 A, PtrR BC); mcycles_branch = 2; mcycles_nobranch = 2 }
      | 0x0B -> { len = one; instr = DEC16 (Reg16 BC); mcycles_branch = 2; mcycles_nobranch = 2 }
      | 0x0C -> { len = one; instr = INC8 (Reg8 C); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x0D -> { len = one; instr = DEC8 (Reg8 C); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x0E ->
          let n = read_byte_operand () in
          { len = two; instr = LD8 (Reg8 C, Imm8 n); mcycles_branch = 2; mcycles_nobranch = 2 }
      | 0x0F -> { len = one; instr = RRCA; mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x10 ->
          let _ = read_byte_operand () in
          (* TODO *)
          { len = two; instr = STOP; mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x11 ->
          let n = read_word_operand () in
          {
            len = three;
            instr = LD16 (Reg16 DE, Imm16 n);
            mcycles_branch = 3;
            mcycles_nobranch = 3;
          }
      | 0x12 ->
          { len = one; instr = LD8 (PtrR DE, Reg8 A); mcycles_branch = 2; mcycles_nobranch = 2 }
      | 0x13 -> { len = one; instr = INC16 (Reg16 DE); mcycles_branch = 2; mcycles_nobranch = 2 }
      | 0x14 -> { len = one; instr = INC8 (Reg8 D); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x15 -> { len = one; instr = DEC8 (Reg8 D); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x16 ->
          let n = read_byte_operand () in
          { len = two; instr = LD8 (Reg8 D, Imm8 n); mcycles_branch = 2; mcycles_nobranch = 2 }
      | 0x17 -> { len = one; instr = RLA; mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x18 ->
          let e = read_signed_byte_operand () in
          { len = two; instr = JR (None, e); mcycles_branch = 3; mcycles_nobranch = 3 }
      | 0x19 ->
          {
            len = one;
            instr = ADD16 (Reg16 HL, Reg16 DE);
            mcycles_branch = 2;
            mcycles_nobranch = 2;
          }
      | 0x1A ->
          { len = one; instr = LD8 (Reg8 A, PtrR DE); mcycles_branch = 2; mcycles_nobranch = 2 }
      | 0x1B -> { len = one; instr = DEC16 (Reg16 DE); mcycles_branch = 2; mcycles_nobranch = 2 }
      | 0x1C -> { len = one; instr = INC8 (Reg8 E); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x1D -> { len = one; instr = DEC8 (Reg8 E); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x1E ->
          let n = read_byte_operand () in
          { len = two; instr = LD8 (Reg8 E, Imm8 n); mcycles_branch = 2; mcycles_nobranch = 2 }
      | 0x1F -> { len = one; instr = RRA; mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x20 ->
          let e = read_signed_byte_operand () in
          { len = two; instr = JR (NZ, e); mcycles_branch = 3; mcycles_nobranch = 2 }
      | 0x21 ->
          let n = read_word_operand () in
          {
            len = three;
            instr = LD16 (Reg16 HL, Imm16 n);
            mcycles_branch = 3;
            mcycles_nobranch = 3;
          }
      | 0x22 -> { len = one; instr = LD8 (HL_i, Reg8 A); mcycles_branch = 2; mcycles_nobranch = 2 }
      | 0x23 -> { len = one; instr = INC16 (Reg16 HL); mcycles_branch = 2; mcycles_nobranch = 2 }
      | 0x24 -> { len = one; instr = INC8 (Reg8 H); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x25 -> { len = one; instr = DEC8 (Reg8 H); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x26 ->
          let n = read_byte_operand () in
          { len = two; instr = LD8 (Reg8 H, Imm8 n); mcycles_branch = 2; mcycles_nobranch = 2 }
      | 0x27 -> { len = one; instr = DAA; mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x28 ->
          let e = read_signed_byte_operand () in
          { len = two; instr = JR (Z, e); mcycles_branch = 3; mcycles_nobranch = 2 }
      | 0x29 ->
          {
            len = one;
            instr = ADD16 (Reg16 HL, Reg16 HL);
            mcycles_branch = 2;
            mcycles_nobranch = 2;
          }
      | 0x2A -> { len = one; instr = LD8 (Reg8 A, HL_i); mcycles_branch = 2; mcycles_nobranch = 2 }
      | 0x2B -> { len = one; instr = DEC16 (Reg16 HL); mcycles_branch = 2; mcycles_nobranch = 2 }
      | 0x2C -> { len = one; instr = INC8 (Reg8 L); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x2D -> { len = one; instr = DEC8 (Reg8 L); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x2E ->
          let n = read_byte_operand () in
          { len = two; instr = LD8 (Reg8 L, Imm8 n); mcycles_branch = 2; mcycles_nobranch = 2 }
      | 0x2F -> { len = one; instr = CPL; mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x30 ->
          let e = read_signed_byte_operand () in
          { len = two; instr = JR (NC, e); mcycles_branch = 3; mcycles_nobranch = 2 }
      | 0x31 ->
          let n = read_word_operand () in
          { len = three; instr = LD16 (SP, Imm16 n); mcycles_branch = 3; mcycles_nobranch = 3 }
      | 0x32 -> { len = one; instr = LD8 (HL_d, Reg8 A); mcycles_branch = 2; mcycles_nobranch = 2 }
      | 0x33 -> { len = one; instr = INC16 SP; mcycles_branch = 2; mcycles_nobranch = 2 }
      | 0x34 -> { len = one; instr = INC8 (PtrR HL); mcycles_branch = 3; mcycles_nobranch = 3 }
      | 0x35 -> { len = one; instr = DEC8 (PtrR HL); mcycles_branch = 3; mcycles_nobranch = 3 }
      | 0x36 ->
          let n = read_byte_operand () in
          { len = two; instr = LD8 (PtrR HL, Imm8 n); mcycles_branch = 3; mcycles_nobranch = 3 }
      | 0x37 -> { len = one; instr = SCF; mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x38 ->
          let e = read_signed_byte_operand () in
          { len = two; instr = JR (C, e); mcycles_branch = 3; mcycles_nobranch = 2 }
      | 0x39 ->
          { len = one; instr = ADD16 (Reg16 HL, SP); mcycles_branch = 2; mcycles_nobranch = 2 }
      | 0x3A -> { len = one; instr = LD8 (Reg8 A, HL_d); mcycles_branch = 2; mcycles_nobranch = 2 }
      | 0x3B -> { len = one; instr = DEC16 SP; mcycles_branch = 2; mcycles_nobranch = 2 }
      | 0x3C -> { len = one; instr = INC8 (Reg8 A); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x3D -> { len = one; instr = DEC8 (Reg8 A); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x3E ->
          let n = read_byte_operand () in
          { len = two; instr = LD8 (Reg8 A, Imm8 n); mcycles_branch = 2; mcycles_nobranch = 2 }
      | 0x3F -> { len = one; instr = CCF; mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x40 ->
          { len = one; instr = LD8 (Reg8 B, Reg8 B); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x41 ->
          { len = one; instr = LD8 (Reg8 B, Reg8 C); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x42 ->
          { len = one; instr = LD8 (Reg8 B, Reg8 D); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x43 ->
          { len = one; instr = LD8 (Reg8 B, Reg8 E); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x44 ->
          { len = one; instr = LD8 (Reg8 B, Reg8 H); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x45 ->
          { len = one; instr = LD8 (Reg8 B, Reg8 L); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x46 ->
          { len = one; instr = LD8 (Reg8 B, PtrR HL); mcycles_branch = 2; mcycles_nobranch = 2 }
      | 0x47 ->
          { len = one; instr = LD8 (Reg8 B, Reg8 A); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x48 ->
          { len = one; instr = LD8 (Reg8 C, Reg8 B); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x49 ->
          { len = one; instr = LD8 (Reg8 C, Reg8 C); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x4A ->
          { len = one; instr = LD8 (Reg8 C, Reg8 D); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x4B ->
          { len = one; instr = LD8 (Reg8 C, Reg8 E); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x4C ->
          { len = one; instr = LD8 (Reg8 C, Reg8 H); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x4D ->
          { len = one; instr = LD8 (Reg8 C, Reg8 L); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x4E ->
          { len = one; instr = LD8 (Reg8 C, PtrR HL); mcycles_branch = 2; mcycles_nobranch = 2 }
      | 0x4F ->
          { len = one; instr = LD8 (Reg8 C, Reg8 A); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x50 ->
          { len = one; instr = LD8 (Reg8 D, Reg8 B); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x51 ->
          { len = one; instr = LD8 (Reg8 D, Reg8 C); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x52 ->
          { len = one; instr = LD8 (Reg8 D, Reg8 D); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x53 ->
          { len = one; instr = LD8 (Reg8 D, Reg8 E); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x54 ->
          { len = one; instr = LD8 (Reg8 D, Reg8 H); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x55 ->
          { len = one; instr = LD8 (Reg8 D, Reg8 L); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x56 ->
          { len = one; instr = LD8 (Reg8 D, PtrR HL); mcycles_branch = 2; mcycles_nobranch = 2 }
      | 0x57 ->
          { len = one; instr = LD8 (Reg8 D, Reg8 A); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x58 ->
          { len = one; instr = LD8 (Reg8 E, Reg8 B); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x59 ->
          { len = one; instr = LD8 (Reg8 E, Reg8 C); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x5A ->
          { len = one; instr = LD8 (Reg8 E, Reg8 D); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x5B ->
          { len = one; instr = LD8 (Reg8 E, Reg8 E); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x5C ->
          { len = one; instr = LD8 (Reg8 E, Reg8 H); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x5D ->
          { len = one; instr = LD8 (Reg8 E, Reg8 L); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x5E ->
          { len = one; instr = LD8 (Reg8 E, PtrR HL); mcycles_branch = 2; mcycles_nobranch = 2 }
      | 0x5F ->
          { len = one; instr = LD8 (Reg8 E, Reg8 A); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x60 ->
          { len = one; instr = LD8 (Reg8 H, Reg8 B); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x61 ->
          { len = one; instr = LD8 (Reg8 H, Reg8 C); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x62 ->
          { len = one; instr = LD8 (Reg8 H, Reg8 D); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x63 ->
          { len = one; instr = LD8 (Reg8 H, Reg8 E); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x64 ->
          { len = one; instr = LD8 (Reg8 H, Reg8 H); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x65 ->
          { len = one; instr = LD8 (Reg8 H, Reg8 L); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x66 ->
          { len = one; instr = LD8 (Reg8 H, PtrR HL); mcycles_branch = 2; mcycles_nobranch = 2 }
      | 0x67 ->
          { len = one; instr = LD8 (Reg8 H, Reg8 A); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x68 ->
          { len = one; instr = LD8 (Reg8 L, Reg8 B); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x69 ->
          { len = one; instr = LD8 (Reg8 L, Reg8 C); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x6A ->
          { len = one; instr = LD8 (Reg8 L, Reg8 D); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x6B ->
          { len = one; instr = LD8 (Reg8 L, Reg8 E); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x6C ->
          { len = one; instr = LD8 (Reg8 L, Reg8 H); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x6D ->
          { len = one; instr = LD8 (Reg8 L, Reg8 L); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x6E ->
          { len = one; instr = LD8 (Reg8 L, PtrR HL); mcycles_branch = 2; mcycles_nobranch = 2 }
      | 0x6F ->
          { len = one; instr = LD8 (Reg8 L, Reg8 A); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x70 ->
          { len = one; instr = LD8 (PtrR HL, Reg8 B); mcycles_branch = 2; mcycles_nobranch = 2 }
      | 0x71 ->
          { len = one; instr = LD8 (PtrR HL, Reg8 C); mcycles_branch = 2; mcycles_nobranch = 2 }
      | 0x72 ->
          { len = one; instr = LD8 (PtrR HL, Reg8 D); mcycles_branch = 2; mcycles_nobranch = 2 }
      | 0x73 ->
          { len = one; instr = LD8 (PtrR HL, Reg8 E); mcycles_branch = 2; mcycles_nobranch = 2 }
      | 0x74 ->
          { len = one; instr = LD8 (PtrR HL, Reg8 H); mcycles_branch = 2; mcycles_nobranch = 2 }
      | 0x75 ->
          { len = one; instr = LD8 (PtrR HL, Reg8 L); mcycles_branch = 2; mcycles_nobranch = 2 }
      | 0x76 -> { len = one; instr = HALT; mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x77 ->
          { len = one; instr = LD8 (PtrR HL, Reg8 A); mcycles_branch = 2; mcycles_nobranch = 2 }
      | 0x78 ->
          { len = one; instr = LD8 (Reg8 A, Reg8 B); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x79 ->
          { len = one; instr = LD8 (Reg8 A, Reg8 C); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x7A ->
          { len = one; instr = LD8 (Reg8 A, Reg8 D); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x7B ->
          { len = one; instr = LD8 (Reg8 A, Reg8 E); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x7C ->
          { len = one; instr = LD8 (Reg8 A, Reg8 H); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x7D ->
          { len = one; instr = LD8 (Reg8 A, Reg8 L); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x7E ->
          { len = one; instr = LD8 (Reg8 A, PtrR HL); mcycles_branch = 2; mcycles_nobranch = 2 }
      | 0x7F ->
          { len = one; instr = LD8 (Reg8 A, Reg8 A); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x80 ->
          { len = one; instr = ADD8 (Reg8 A, Reg8 B); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x81 ->
          { len = one; instr = ADD8 (Reg8 A, Reg8 C); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x82 ->
          { len = one; instr = ADD8 (Reg8 A, Reg8 D); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x83 ->
          { len = one; instr = ADD8 (Reg8 A, Reg8 E); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x84 ->
          { len = one; instr = ADD8 (Reg8 A, Reg8 H); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x85 ->
          { len = one; instr = ADD8 (Reg8 A, Reg8 L); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x86 ->
          { len = one; instr = ADD8 (Reg8 A, PtrR HL); mcycles_branch = 2; mcycles_nobranch = 2 }
      | 0x87 ->
          { len = one; instr = ADD8 (Reg8 A, Reg8 A); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x88 ->
          { len = one; instr = ADC (Reg8 A, Reg8 B); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x89 ->
          { len = one; instr = ADC (Reg8 A, Reg8 C); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x8A ->
          { len = one; instr = ADC (Reg8 A, Reg8 D); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x8B ->
          { len = one; instr = ADC (Reg8 A, Reg8 E); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x8C ->
          { len = one; instr = ADC (Reg8 A, Reg8 H); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x8D ->
          { len = one; instr = ADC (Reg8 A, Reg8 L); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x8E ->
          { len = one; instr = ADC (Reg8 A, PtrR HL); mcycles_branch = 2; mcycles_nobranch = 2 }
      | 0x8F ->
          { len = one; instr = ADC (Reg8 A, Reg8 A); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x90 ->
          { len = one; instr = SUB (Reg8 A, Reg8 B); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x91 ->
          { len = one; instr = SUB (Reg8 A, Reg8 C); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x92 ->
          { len = one; instr = SUB (Reg8 A, Reg8 D); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x93 ->
          { len = one; instr = SUB (Reg8 A, Reg8 E); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x94 ->
          { len = one; instr = SUB (Reg8 A, Reg8 H); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x95 ->
          { len = one; instr = SUB (Reg8 A, Reg8 L); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x96 ->
          { len = one; instr = SUB (Reg8 A, PtrR HL); mcycles_branch = 2; mcycles_nobranch = 2 }
      | 0x97 ->
          { len = one; instr = SUB (Reg8 A, Reg8 A); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x98 ->
          { len = one; instr = SBC (Reg8 A, Reg8 B); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x99 ->
          { len = one; instr = SBC (Reg8 A, Reg8 C); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x9A ->
          { len = one; instr = SBC (Reg8 A, Reg8 D); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x9B ->
          { len = one; instr = SBC (Reg8 A, Reg8 E); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x9C ->
          { len = one; instr = SBC (Reg8 A, Reg8 H); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x9D ->
          { len = one; instr = SBC (Reg8 A, Reg8 L); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0x9E ->
          { len = one; instr = SBC (Reg8 A, PtrR HL); mcycles_branch = 2; mcycles_nobranch = 2 }
      | 0x9F ->
          { len = one; instr = SBC (Reg8 A, Reg8 A); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0xA0 ->
          { len = one; instr = AND (Reg8 A, Reg8 B); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0xA1 ->
          { len = one; instr = AND (Reg8 A, Reg8 C); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0xA2 ->
          { len = one; instr = AND (Reg8 A, Reg8 D); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0xA3 ->
          { len = one; instr = AND (Reg8 A, Reg8 E); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0xA4 ->
          { len = one; instr = AND (Reg8 A, Reg8 H); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0xA5 ->
          { len = one; instr = AND (Reg8 A, Reg8 L); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0xA6 ->
          { len = one; instr = AND (Reg8 A, PtrR HL); mcycles_branch = 2; mcycles_nobranch = 2 }
      | 0xA7 ->
          { len = one; instr = AND (Reg8 A, Reg8 A); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0xA8 ->
          { len = one; instr = XOR (Reg8 A, Reg8 B); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0xA9 ->
          { len = one; instr = XOR (Reg8 A, Reg8 C); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0xAA ->
          { len = one; instr = XOR (Reg8 A, Reg8 D); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0xAB ->
          { len = one; instr = XOR (Reg8 A, Reg8 E); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0xAC ->
          { len = one; instr = XOR (Reg8 A, Reg8 H); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0xAD ->
          { len = one; instr = XOR (Reg8 A, Reg8 L); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0xAE ->
          { len = one; instr = XOR (Reg8 A, PtrR HL); mcycles_branch = 2; mcycles_nobranch = 2 }
      | 0xAF ->
          { len = one; instr = XOR (Reg8 A, Reg8 A); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0xB0 -> { len = one; instr = OR (Reg8 A, Reg8 B); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0xB1 -> { len = one; instr = OR (Reg8 A, Reg8 C); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0xB2 -> { len = one; instr = OR (Reg8 A, Reg8 D); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0xB3 -> { len = one; instr = OR (Reg8 A, Reg8 E); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0xB4 -> { len = one; instr = OR (Reg8 A, Reg8 H); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0xB5 -> { len = one; instr = OR (Reg8 A, Reg8 L); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0xB6 ->
          { len = one; instr = OR (Reg8 A, PtrR HL); mcycles_branch = 2; mcycles_nobranch = 2 }
      | 0xB7 -> { len = one; instr = OR (Reg8 A, Reg8 A); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0xB8 -> { len = one; instr = CP (Reg8 A, Reg8 B); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0xB9 -> { len = one; instr = CP (Reg8 A, Reg8 C); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0xBA -> { len = one; instr = CP (Reg8 A, Reg8 D); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0xBB -> { len = one; instr = CP (Reg8 A, Reg8 E); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0xBC -> { len = one; instr = CP (Reg8 A, Reg8 H); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0xBD -> { len = one; instr = CP (Reg8 A, Reg8 L); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0xBE ->
          { len = one; instr = CP (Reg8 A, PtrR HL); mcycles_branch = 2; mcycles_nobranch = 2 }
      | 0xBF -> { len = one; instr = CP (Reg8 A, Reg8 A); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0xC0 -> { len = one; instr = RET NZ; mcycles_branch = 5; mcycles_nobranch = 2 }
      | 0xC1 -> { len = one; instr = POP (Reg16 BC); mcycles_branch = 3; mcycles_nobranch = 3 }
      | 0xC2 ->
          let n = read_word_operand () in
          { len = three; instr = JP (NZ, Imm16 n); mcycles_branch = 4; mcycles_nobranch = 3 }
      | 0xC3 ->
          let n = read_word_operand () in
          { len = three; instr = JP (None, Imm16 n); mcycles_branch = 4; mcycles_nobranch = 4 }
      | 0xC4 ->
          let n = read_word_operand () in
          { len = three; instr = CALL (NZ, n); mcycles_branch = 6; mcycles_nobranch = 3 }
      | 0xC5 -> { len = one; instr = PUSH (Reg16 BC); mcycles_branch = 4; mcycles_nobranch = 4 }
      | 0xC6 ->
          let n = read_byte_operand () in
          { len = two; instr = ADD8 (Reg8 A, Imm8 n); mcycles_branch = 2; mcycles_nobranch = 2 }
      | 0xC7 ->
          { len = one; instr = RST (Uint16.of_int 0x00); mcycles_branch = 4; mcycles_nobranch = 4 }
      | 0xC8 -> { len = one; instr = RET Z; mcycles_branch = 5; mcycles_nobranch = 2 }
      | 0xC9 -> { len = one; instr = RET None; mcycles_branch = 4; mcycles_nobranch = 4 }
      | 0xCA ->
          let n = read_word_operand () in
          { len = three; instr = JP (Z, Imm16 n); mcycles_branch = 4; mcycles_nobranch = 3 }
      (* 0xCB prefix is handled later *)
      | 0xCC ->
          let n = read_word_operand () in
          { len = three; instr = CALL (Z, n); mcycles_branch = 6; mcycles_nobranch = 3 }
      | 0xCD ->
          let n = read_word_operand () in
          { len = three; instr = CALL (None, n); mcycles_branch = 6; mcycles_nobranch = 6 }
      | 0xCE ->
          let n = read_byte_operand () in
          { len = two; instr = ADC (Reg8 A, Imm8 n); mcycles_branch = 2; mcycles_nobranch = 2 }
      | 0xCF ->
          { len = one; instr = RST (Uint16.of_int 0x08); mcycles_branch = 4; mcycles_nobranch = 4 }
      | 0xD0 -> { len = one; instr = RET NC; mcycles_branch = 5; mcycles_nobranch = 2 }
      | 0xD1 -> { len = one; instr = POP (Reg16 DE); mcycles_branch = 3; mcycles_nobranch = 3 }
      | 0xD2 ->
          let n = read_word_operand () in
          { len = three; instr = JP (NC, Imm16 n); mcycles_branch = 4; mcycles_nobranch = 3 }
      | 0xD3 -> failwith "Illegal opcode 0xD3"
      | 0xD4 ->
          let n = read_word_operand () in
          { len = three; instr = CALL (NC, n); mcycles_branch = 6; mcycles_nobranch = 3 }
      | 0xD5 -> { len = one; instr = PUSH (Reg16 DE); mcycles_branch = 4; mcycles_nobranch = 4 }
      | 0xD6 ->
          let n = read_byte_operand () in
          { len = two; instr = SUB (Reg8 A, Imm8 n); mcycles_branch = 2; mcycles_nobranch = 2 }
      | 0xD7 ->
          { len = one; instr = RST (Uint16.of_int 0x10); mcycles_branch = 4; mcycles_nobranch = 4 }
      | 0xD8 -> { len = one; instr = RET C; mcycles_branch = 5; mcycles_nobranch = 2 }
      | 0xD9 -> { len = one; instr = RETI; mcycles_branch = 4; mcycles_nobranch = 4 }
      | 0xDA ->
          let n = read_word_operand () in
          { len = three; instr = JP (C, Imm16 n); mcycles_branch = 4; mcycles_nobranch = 3 }
      | 0xDB -> failwith "Illegal opcode 0xDB"
      | 0xDC ->
          let n = read_word_operand () in
          { len = three; instr = CALL (C, n); mcycles_branch = 6; mcycles_nobranch = 3 }
      | 0xDD -> failwith "Illegal opcode 0xDD"
      | 0xDE ->
          let n = read_byte_operand () in
          { len = two; instr = SBC (Reg8 A, Imm8 n); mcycles_branch = 2; mcycles_nobranch = 2 }
      | 0xDF ->
          { len = one; instr = RST (Uint16.of_int 0x18); mcycles_branch = 4; mcycles_nobranch = 4 }
      | 0xE0 ->
          let n = read_byte_operand () in
          { len = two; instr = LD8 (Offset n, Reg8 A); mcycles_branch = 3; mcycles_nobranch = 3 }
      | 0xE1 -> { len = one; instr = POP (Reg16 HL); mcycles_branch = 3; mcycles_nobranch = 3 }
      | 0xE2 ->
          { len = one; instr = LD8 (C_offset, Reg8 A); mcycles_branch = 2; mcycles_nobranch = 2 }
      | 0xE3 -> failwith "Illegal opcode 0xE3"
      | 0xE4 -> failwith "Illegal opcode 0xE4"
      | 0xE5 -> { len = one; instr = PUSH (Reg16 HL); mcycles_branch = 4; mcycles_nobranch = 4 }
      | 0xE6 ->
          let n = read_byte_operand () in
          { len = two; instr = AND (Reg8 A, Imm8 n); mcycles_branch = 2; mcycles_nobranch = 2 }
      | 0xE7 ->
          { len = one; instr = RST (Uint16.of_int 0x20); mcycles_branch = 4; mcycles_nobranch = 4 }
      | 0xE8 ->
          let e = read_signed_byte_operand () in
          { len = two; instr = ADDSP e; mcycles_branch = 4; mcycles_nobranch = 4 }
      | 0xE9 -> { len = one; instr = JP (None, Reg16 HL); mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0xEA ->
          let n = read_word_operand () in
          { len = three; instr = LD8 (PtrImm n, Reg8 A); mcycles_branch = 4; mcycles_nobranch = 4 }
      | 0xEB -> failwith "Illegal opcode 0xEB"
      | 0xEC -> failwith "Illegal opcode 0xEC"
      | 0xED -> failwith "Illegal opcode 0xED"
      | 0xEE ->
          let n = read_byte_operand () in
          { len = two; instr = XOR (Reg8 A, Imm8 n); mcycles_branch = 2; mcycles_nobranch = 2 }
      | 0xEF ->
          { len = one; instr = RST (Uint16.of_int 0x28); mcycles_branch = 4; mcycles_nobranch = 4 }
      | 0xF0 ->
          let n = read_byte_operand () in
          { len = two; instr = LD8 (Reg8 A, Offset n); mcycles_branch = 3; mcycles_nobranch = 3 }
      | 0xF1 -> { len = one; instr = POP (Reg16 AF); mcycles_branch = 3; mcycles_nobranch = 3 }
      | 0xF2 ->
          { len = one; instr = LD8 (Reg8 A, C_offset); mcycles_branch = 2; mcycles_nobranch = 2 }
      | 0xF3 -> { len = one; instr = DI; mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0xF4 -> failwith "Illegal opcode 0xF4"
      | 0xF5 -> { len = one; instr = PUSH (Reg16 AF); mcycles_branch = 4; mcycles_nobranch = 4 }
      | 0xF6 ->
          let n = read_byte_operand () in
          { len = two; instr = OR (Reg8 A, Imm8 n); mcycles_branch = 2; mcycles_nobranch = 2 }
      | 0xF7 ->
          { len = one; instr = RST (Uint16.of_int 0x30); mcycles_branch = 4; mcycles_nobranch = 4 }
      | 0xF8 ->
          let e = read_signed_byte_operand () in
          {
            len = two;
            instr = LD16 (Reg16 HL, SP_offset e);
            mcycles_branch = 3;
            mcycles_nobranch = 3;
          }
      | 0xF9 -> { len = one; instr = LD16 (SP, Reg16 HL); mcycles_branch = 2; mcycles_nobranch = 2 }
      | 0xFA ->
          let n = read_word_operand () in
          { len = three; instr = LD8 (Reg8 A, PtrImm n); mcycles_branch = 4; mcycles_nobranch = 4 }
      | 0xFB -> { len = one; instr = EI; mcycles_branch = 1; mcycles_nobranch = 1 }
      | 0xFC -> failwith "Illegal opcode 0xFC"
      | 0xFD -> failwith "Illegal opcode 0xFD"
      | 0xFE ->
          let n = read_byte_operand () in
          { len = two; instr = CP (Reg8 A, Imm8 n); mcycles_branch = 2; mcycles_nobranch = 2 }
      | 0xFF ->
          { len = one; instr = RST (Uint16.of_int 0x38); mcycles_branch = 4; mcycles_nobranch = 4 }
      | 0xCB -> (
          let op = Mem.read_byte mem (Uint16.succ pc) |> Uint8.to_int in

          match op with
          | 0x00 -> { len = two; instr = RLC (Reg8 B); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x01 -> { len = two; instr = RLC (Reg8 C); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x02 -> { len = two; instr = RLC (Reg8 D); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x03 -> { len = two; instr = RLC (Reg8 E); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x04 -> { len = two; instr = RLC (Reg8 H); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x05 -> { len = two; instr = RLC (Reg8 L); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x06 -> { len = two; instr = RLC (PtrR HL); mcycles_branch = 4; mcycles_nobranch = 4 }
          | 0x07 -> { len = two; instr = RLC (Reg8 A); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x08 -> { len = two; instr = RRC (Reg8 B); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x09 -> { len = two; instr = RRC (Reg8 C); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x0A -> { len = two; instr = RRC (Reg8 D); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x0B -> { len = two; instr = RRC (Reg8 E); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x0C -> { len = two; instr = RRC (Reg8 H); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x0D -> { len = two; instr = RRC (Reg8 L); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x0E -> { len = two; instr = RRC (PtrR HL); mcycles_branch = 4; mcycles_nobranch = 4 }
          | 0x0F -> { len = two; instr = RRC (Reg8 A); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x10 -> { len = two; instr = RL (Reg8 B); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x11 -> { len = two; instr = RL (Reg8 C); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x12 -> { len = two; instr = RL (Reg8 D); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x13 -> { len = two; instr = RL (Reg8 E); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x14 -> { len = two; instr = RL (Reg8 H); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x15 -> { len = two; instr = RL (Reg8 L); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x16 -> { len = two; instr = RL (PtrR HL); mcycles_branch = 4; mcycles_nobranch = 4 }
          | 0x17 -> { len = two; instr = RL (Reg8 A); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x18 -> { len = two; instr = RR (Reg8 B); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x19 -> { len = two; instr = RR (Reg8 C); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x1A -> { len = two; instr = RR (Reg8 D); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x1B -> { len = two; instr = RR (Reg8 E); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x1C -> { len = two; instr = RR (Reg8 H); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x1D -> { len = two; instr = RR (Reg8 L); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x1E -> { len = two; instr = RR (PtrR HL); mcycles_branch = 4; mcycles_nobranch = 4 }
          | 0x1F -> { len = two; instr = RR (Reg8 A); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x20 -> { len = two; instr = SLA (Reg8 B); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x21 -> { len = two; instr = SLA (Reg8 C); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x22 -> { len = two; instr = SLA (Reg8 D); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x23 -> { len = two; instr = SLA (Reg8 E); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x24 -> { len = two; instr = SLA (Reg8 H); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x25 -> { len = two; instr = SLA (Reg8 L); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x26 -> { len = two; instr = SLA (PtrR HL); mcycles_branch = 4; mcycles_nobranch = 4 }
          | 0x27 -> { len = two; instr = SLA (Reg8 A); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x28 -> { len = two; instr = SRA (Reg8 B); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x29 -> { len = two; instr = SRA (Reg8 C); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x2A -> { len = two; instr = SRA (Reg8 D); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x2B -> { len = two; instr = SRA (Reg8 E); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x2C -> { len = two; instr = SRA (Reg8 H); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x2D -> { len = two; instr = SRA (Reg8 L); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x2E -> { len = two; instr = SRA (PtrR HL); mcycles_branch = 4; mcycles_nobranch = 4 }
          | 0x2F -> { len = two; instr = SRA (Reg8 A); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x30 -> { len = two; instr = SWAP (Reg8 B); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x31 -> { len = two; instr = SWAP (Reg8 C); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x32 -> { len = two; instr = SWAP (Reg8 D); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x33 -> { len = two; instr = SWAP (Reg8 E); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x34 -> { len = two; instr = SWAP (Reg8 H); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x35 -> { len = two; instr = SWAP (Reg8 L); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x36 -> { len = two; instr = SWAP (PtrR HL); mcycles_branch = 4; mcycles_nobranch = 4 }
          | 0x37 -> { len = two; instr = SWAP (Reg8 A); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x38 -> { len = two; instr = SRL (Reg8 B); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x39 -> { len = two; instr = SRL (Reg8 C); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x3A -> { len = two; instr = SRL (Reg8 D); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x3B -> { len = two; instr = SRL (Reg8 E); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x3C -> { len = two; instr = SRL (Reg8 H); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x3D -> { len = two; instr = SRL (Reg8 L); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x3E -> { len = two; instr = SRL (PtrR HL); mcycles_branch = 4; mcycles_nobranch = 4 }
          | 0x3F -> { len = two; instr = SRL (Reg8 A); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x40 -> { len = two; instr = BIT (0, Reg8 B); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x41 -> { len = two; instr = BIT (0, Reg8 C); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x42 -> { len = two; instr = BIT (0, Reg8 D); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x43 -> { len = two; instr = BIT (0, Reg8 E); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x44 -> { len = two; instr = BIT (0, Reg8 H); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x45 -> { len = two; instr = BIT (0, Reg8 L); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x46 ->
              { len = two; instr = BIT (0, PtrR HL); mcycles_branch = 3; mcycles_nobranch = 3 }
          | 0x47 -> { len = two; instr = BIT (0, Reg8 A); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x48 -> { len = two; instr = BIT (1, Reg8 B); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x49 -> { len = two; instr = BIT (1, Reg8 C); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x4A -> { len = two; instr = BIT (1, Reg8 D); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x4B -> { len = two; instr = BIT (1, Reg8 E); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x4C -> { len = two; instr = BIT (1, Reg8 H); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x4D -> { len = two; instr = BIT (1, Reg8 L); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x4E ->
              { len = two; instr = BIT (1, PtrR HL); mcycles_branch = 3; mcycles_nobranch = 3 }
          | 0x4F -> { len = two; instr = BIT (1, Reg8 A); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x50 -> { len = two; instr = BIT (2, Reg8 B); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x51 -> { len = two; instr = BIT (2, Reg8 C); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x52 -> { len = two; instr = BIT (2, Reg8 D); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x53 -> { len = two; instr = BIT (2, Reg8 E); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x54 -> { len = two; instr = BIT (2, Reg8 H); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x55 -> { len = two; instr = BIT (2, Reg8 L); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x56 ->
              { len = two; instr = BIT (2, PtrR HL); mcycles_branch = 3; mcycles_nobranch = 3 }
          | 0x57 -> { len = two; instr = BIT (2, Reg8 A); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x58 -> { len = two; instr = BIT (3, Reg8 B); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x59 -> { len = two; instr = BIT (3, Reg8 C); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x5A -> { len = two; instr = BIT (3, Reg8 D); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x5B -> { len = two; instr = BIT (3, Reg8 E); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x5C -> { len = two; instr = BIT (3, Reg8 H); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x5D -> { len = two; instr = BIT (3, Reg8 L); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x5E ->
              { len = two; instr = BIT (3, PtrR HL); mcycles_branch = 3; mcycles_nobranch = 3 }
          | 0x5F -> { len = two; instr = BIT (3, Reg8 A); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x60 -> { len = two; instr = BIT (4, Reg8 B); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x61 -> { len = two; instr = BIT (4, Reg8 C); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x62 -> { len = two; instr = BIT (4, Reg8 D); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x63 -> { len = two; instr = BIT (4, Reg8 E); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x64 -> { len = two; instr = BIT (4, Reg8 H); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x65 -> { len = two; instr = BIT (4, Reg8 L); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x66 ->
              { len = two; instr = BIT (4, PtrR HL); mcycles_branch = 3; mcycles_nobranch = 3 }
          | 0x67 -> { len = two; instr = BIT (4, Reg8 A); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x68 -> { len = two; instr = BIT (5, Reg8 B); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x69 -> { len = two; instr = BIT (5, Reg8 C); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x6A -> { len = two; instr = BIT (5, Reg8 D); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x6B -> { len = two; instr = BIT (5, Reg8 E); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x6C -> { len = two; instr = BIT (5, Reg8 H); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x6D -> { len = two; instr = BIT (5, Reg8 L); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x6E ->
              { len = two; instr = BIT (5, PtrR HL); mcycles_branch = 3; mcycles_nobranch = 3 }
          | 0x6F -> { len = two; instr = BIT (5, Reg8 A); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x70 -> { len = two; instr = BIT (6, Reg8 B); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x71 -> { len = two; instr = BIT (6, Reg8 C); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x72 -> { len = two; instr = BIT (6, Reg8 D); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x73 -> { len = two; instr = BIT (6, Reg8 E); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x74 -> { len = two; instr = BIT (6, Reg8 H); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x75 -> { len = two; instr = BIT (6, Reg8 L); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x76 ->
              { len = two; instr = BIT (6, PtrR HL); mcycles_branch = 3; mcycles_nobranch = 3 }
          | 0x77 -> { len = two; instr = BIT (6, Reg8 A); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x78 -> { len = two; instr = BIT (7, Reg8 B); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x79 -> { len = two; instr = BIT (7, Reg8 C); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x7A -> { len = two; instr = BIT (7, Reg8 D); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x7B -> { len = two; instr = BIT (7, Reg8 E); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x7C -> { len = two; instr = BIT (7, Reg8 H); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x7D -> { len = two; instr = BIT (7, Reg8 L); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x7E ->
              { len = two; instr = BIT (7, PtrR HL); mcycles_branch = 3; mcycles_nobranch = 3 }
          | 0x7F -> { len = two; instr = BIT (7, Reg8 A); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x80 -> { len = two; instr = RES (0, Reg8 B); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x81 -> { len = two; instr = RES (0, Reg8 C); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x82 -> { len = two; instr = RES (0, Reg8 D); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x83 -> { len = two; instr = RES (0, Reg8 E); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x84 -> { len = two; instr = RES (0, Reg8 H); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x85 -> { len = two; instr = RES (0, Reg8 L); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x86 ->
              { len = two; instr = RES (0, PtrR HL); mcycles_branch = 4; mcycles_nobranch = 4 }
          | 0x87 -> { len = two; instr = RES (0, Reg8 A); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x88 -> { len = two; instr = RES (1, Reg8 B); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x89 -> { len = two; instr = RES (1, Reg8 C); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x8A -> { len = two; instr = RES (1, Reg8 D); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x8B -> { len = two; instr = RES (1, Reg8 E); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x8C -> { len = two; instr = RES (1, Reg8 H); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x8D -> { len = two; instr = RES (1, Reg8 L); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x8E ->
              { len = two; instr = RES (1, PtrR HL); mcycles_branch = 4; mcycles_nobranch = 4 }
          | 0x8F -> { len = two; instr = RES (1, Reg8 A); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x90 -> { len = two; instr = RES (2, Reg8 B); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x91 -> { len = two; instr = RES (2, Reg8 C); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x92 -> { len = two; instr = RES (2, Reg8 D); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x93 -> { len = two; instr = RES (2, Reg8 E); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x94 -> { len = two; instr = RES (2, Reg8 H); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x95 -> { len = two; instr = RES (2, Reg8 L); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x96 ->
              { len = two; instr = RES (2, PtrR HL); mcycles_branch = 4; mcycles_nobranch = 4 }
          | 0x97 -> { len = two; instr = RES (2, Reg8 A); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x98 -> { len = two; instr = RES (3, Reg8 B); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x99 -> { len = two; instr = RES (3, Reg8 C); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x9A -> { len = two; instr = RES (3, Reg8 D); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x9B -> { len = two; instr = RES (3, Reg8 E); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x9C -> { len = two; instr = RES (3, Reg8 H); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x9D -> { len = two; instr = RES (3, Reg8 L); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0x9E ->
              { len = two; instr = RES (3, PtrR HL); mcycles_branch = 4; mcycles_nobranch = 4 }
          | 0x9F -> { len = two; instr = RES (3, Reg8 A); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0xA0 -> { len = two; instr = RES (4, Reg8 B); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0xA1 -> { len = two; instr = RES (4, Reg8 C); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0xA2 -> { len = two; instr = RES (4, Reg8 D); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0xA3 -> { len = two; instr = RES (4, Reg8 E); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0xA4 -> { len = two; instr = RES (4, Reg8 H); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0xA5 -> { len = two; instr = RES (4, Reg8 L); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0xA6 ->
              { len = two; instr = RES (4, PtrR HL); mcycles_branch = 4; mcycles_nobranch = 4 }
          | 0xA7 -> { len = two; instr = RES (4, Reg8 A); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0xA8 -> { len = two; instr = RES (5, Reg8 B); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0xA9 -> { len = two; instr = RES (5, Reg8 C); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0xAA -> { len = two; instr = RES (5, Reg8 D); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0xAB -> { len = two; instr = RES (5, Reg8 E); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0xAC -> { len = two; instr = RES (5, Reg8 H); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0xAD -> { len = two; instr = RES (5, Reg8 L); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0xAE ->
              { len = two; instr = RES (5, PtrR HL); mcycles_branch = 4; mcycles_nobranch = 4 }
          | 0xAF -> { len = two; instr = RES (5, Reg8 A); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0xB0 -> { len = two; instr = RES (6, Reg8 B); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0xB1 -> { len = two; instr = RES (6, Reg8 C); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0xB2 -> { len = two; instr = RES (6, Reg8 D); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0xB3 -> { len = two; instr = RES (6, Reg8 E); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0xB4 -> { len = two; instr = RES (6, Reg8 H); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0xB5 -> { len = two; instr = RES (6, Reg8 L); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0xB6 ->
              { len = two; instr = RES (6, PtrR HL); mcycles_branch = 4; mcycles_nobranch = 4 }
          | 0xB7 -> { len = two; instr = RES (6, Reg8 A); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0xB8 -> { len = two; instr = RES (7, Reg8 B); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0xB9 -> { len = two; instr = RES (7, Reg8 C); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0xBA -> { len = two; instr = RES (7, Reg8 D); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0xBB -> { len = two; instr = RES (7, Reg8 E); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0xBC -> { len = two; instr = RES (7, Reg8 H); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0xBD -> { len = two; instr = RES (7, Reg8 L); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0xBE ->
              { len = two; instr = RES (7, PtrR HL); mcycles_branch = 4; mcycles_nobranch = 4 }
          | 0xBF -> { len = two; instr = RES (7, Reg8 A); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0xC0 -> { len = two; instr = SET (0, Reg8 B); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0xC1 -> { len = two; instr = SET (0, Reg8 C); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0xC2 -> { len = two; instr = SET (0, Reg8 D); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0xC3 -> { len = two; instr = SET (0, Reg8 E); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0xC4 -> { len = two; instr = SET (0, Reg8 H); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0xC5 -> { len = two; instr = SET (0, Reg8 L); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0xC6 ->
              { len = two; instr = SET (0, PtrR HL); mcycles_branch = 4; mcycles_nobranch = 4 }
          | 0xC7 -> { len = two; instr = SET (0, Reg8 A); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0xC8 -> { len = two; instr = SET (1, Reg8 B); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0xC9 -> { len = two; instr = SET (1, Reg8 C); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0xCA -> { len = two; instr = SET (1, Reg8 D); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0xCB -> { len = two; instr = SET (1, Reg8 E); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0xCC -> { len = two; instr = SET (1, Reg8 H); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0xCD -> { len = two; instr = SET (1, Reg8 L); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0xCE ->
              { len = two; instr = SET (1, PtrR HL); mcycles_branch = 4; mcycles_nobranch = 4 }
          | 0xCF -> { len = two; instr = SET (1, Reg8 A); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0xD0 -> { len = two; instr = SET (2, Reg8 B); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0xD1 -> { len = two; instr = SET (2, Reg8 C); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0xD2 -> { len = two; instr = SET (2, Reg8 D); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0xD3 -> { len = two; instr = SET (2, Reg8 E); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0xD4 -> { len = two; instr = SET (2, Reg8 H); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0xD5 -> { len = two; instr = SET (2, Reg8 L); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0xD6 ->
              { len = two; instr = SET (2, PtrR HL); mcycles_branch = 4; mcycles_nobranch = 4 }
          | 0xD7 -> { len = two; instr = SET (2, Reg8 A); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0xD8 -> { len = two; instr = SET (3, Reg8 B); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0xD9 -> { len = two; instr = SET (3, Reg8 C); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0xDA -> { len = two; instr = SET (3, Reg8 D); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0xDB -> { len = two; instr = SET (3, Reg8 E); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0xDC -> { len = two; instr = SET (3, Reg8 H); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0xDD -> { len = two; instr = SET (3, Reg8 L); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0xDE ->
              { len = two; instr = SET (3, PtrR HL); mcycles_branch = 4; mcycles_nobranch = 4 }
          | 0xDF -> { len = two; instr = SET (3, Reg8 A); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0xE0 -> { len = two; instr = SET (4, Reg8 B); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0xE1 -> { len = two; instr = SET (4, Reg8 C); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0xE2 -> { len = two; instr = SET (4, Reg8 D); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0xE3 -> { len = two; instr = SET (4, Reg8 E); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0xE4 -> { len = two; instr = SET (4, Reg8 H); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0xE5 -> { len = two; instr = SET (4, Reg8 L); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0xE6 ->
              { len = two; instr = SET (4, PtrR HL); mcycles_branch = 4; mcycles_nobranch = 4 }
          | 0xE7 -> { len = two; instr = SET (4, Reg8 A); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0xE8 -> { len = two; instr = SET (5, Reg8 B); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0xE9 -> { len = two; instr = SET (5, Reg8 C); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0xEA -> { len = two; instr = SET (5, Reg8 D); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0xEB -> { len = two; instr = SET (5, Reg8 E); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0xEC -> { len = two; instr = SET (5, Reg8 H); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0xED -> { len = two; instr = SET (5, Reg8 L); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0xEE ->
              { len = two; instr = SET (5, PtrR HL); mcycles_branch = 4; mcycles_nobranch = 4 }
          | 0xEF -> { len = two; instr = SET (5, Reg8 A); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0xF0 -> { len = two; instr = SET (6, Reg8 B); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0xF1 -> { len = two; instr = SET (6, Reg8 C); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0xF2 -> { len = two; instr = SET (6, Reg8 D); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0xF3 -> { len = two; instr = SET (6, Reg8 E); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0xF4 -> { len = two; instr = SET (6, Reg8 H); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0xF5 -> { len = two; instr = SET (6, Reg8 L); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0xF6 ->
              { len = two; instr = SET (6, PtrR HL); mcycles_branch = 4; mcycles_nobranch = 4 }
          | 0xF7 -> { len = two; instr = SET (6, Reg8 A); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0xF8 -> { len = two; instr = SET (7, Reg8 B); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0xF9 -> { len = two; instr = SET (7, Reg8 C); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0xFA -> { len = two; instr = SET (7, Reg8 D); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0xFB -> { len = two; instr = SET (7, Reg8 E); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0xFC -> { len = two; instr = SET (7, Reg8 H); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0xFD -> { len = two; instr = SET (7, Reg8 L); mcycles_branch = 2; mcycles_nobranch = 2 }
          | 0xFE ->
              { len = two; instr = SET (7, PtrR HL); mcycles_branch = 4; mcycles_nobranch = 4 }
          | 0xFF -> { len = two; instr = SET (7, Reg8 A); mcycles_branch = 2; mcycles_nobranch = 2 }
          | _ -> failwith @@ Printf.sprintf "Unhandled prefixed opcode %#x" op)
      | _ -> failwith @@ Printf.sprintf "Unhandled opcode %#x" op
end
