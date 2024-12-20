include Gameboy
open Uint
module Mem = Mock_memory
module Fetcher = Instruction_fetcher.Make (Mem)

let%expect_test "test fetch add8" =
    let mem = Mem.of_list 1 [ 0x87 ] in
    let info = Fetcher.fetch mem ~pc:Uint16.zero in
    info.instr |> Instruction.show |> Printf.printf "Len: %s Instr: %s" (Uint16.to_string info.len);
    [%expect {| Len: 1 Instr: ADD8 A, A |}]

let%expect_test "test fetch later add8" =
    let mem = Mem.of_list 3 [ 0x01; 0x02; 0x87 ] in
    let info = Fetcher.fetch mem ~pc:(Uint16.of_int 2) in
    info.instr |> Instruction.show |> Printf.printf "Len: %s Instr: %s" (Uint16.to_string info.len);
    [%expect {| Len: 1 Instr: ADD8 A, A |}]

let%expect_test "test fetch add8 imm" =
    let mem = Mem.of_list 2 [ 0xC6; 0x45 ] in
    let info = Fetcher.fetch mem ~pc:(Uint16.of_int 0) in
    info.instr |> Instruction.show |> Printf.printf "Len: %s Instr: %s" (Uint16.to_string info.len);
    [%expect {| Len: 2 Instr: ADD8 A, 0x45 |}]

let%expect_test "test fetch ld16 imm16" =
    let mem = Mem.of_list 3 [ 0x01; 0x34; 0x12 ] in
    let info = Fetcher.fetch mem ~pc:(Uint16.of_int 0) in
    info.instr |> Instruction.show |> Printf.printf "Len: %s Instr: %s" (Uint16.to_string info.len);
    [%expect {| Len: 3 Instr: LD16 BC, 0x1234 |}]

let%expect_test "test fetch all" =
    let illegals = [ 0xD3; 0xDB; 0xDD; 0xE3; 0xE4; 0xEB; 0xEC; 0xED; 0xF4; 0xFC; 0xFD ] in
    for i = 0 to 0xFF do
      if i = 0xCB || List.mem i illegals then
        ()
      else
        let mem = Mem.of_list 3 [ i; 0x34; 0x12 ] in
        let info = Fetcher.fetch mem ~pc:Uint16.zero in
        Printf.printf "%#x Len: %s Instr: %s Branch: %d Nobranch: %d\n" i
          (Uint16.to_string info.len) (Instruction.show info.instr) info.mcycles_branch
          info.mcycles_nobranch
    done;
    for i = 0 to 0xFF do
      let mem = Mem.of_list 2 [ 0xCB; i ] in
      let info = Fetcher.fetch mem ~pc:Uint16.zero in
      Printf.printf "%#x Len: %s Instr: %s Branch: %d Nobranch: %d\n" i (Uint16.to_string info.len)
        (Instruction.show info.instr) info.mcycles_branch info.mcycles_nobranch
    done;
    [%expect
        {|
      0 Len: 1 Instr: NOP Branch: 1 Nobranch: 1
      0x1 Len: 3 Instr: LD16 BC, 0x1234 Branch: 3 Nobranch: 3
      0x2 Len: 1 Instr: LD8 [BC], A Branch: 2 Nobranch: 2
      0x3 Len: 1 Instr: INC16 BC Branch: 2 Nobranch: 2
      0x4 Len: 1 Instr: INC8 B Branch: 1 Nobranch: 1
      0x5 Len: 1 Instr: DEC8 B Branch: 1 Nobranch: 1
      0x6 Len: 2 Instr: LD8 B, 0x34 Branch: 2 Nobranch: 2
      0x7 Len: 1 Instr: RLCA Branch: 1 Nobranch: 1
      0x8 Len: 3 Instr: LD16 [0x1234], SP Branch: 5 Nobranch: 5
      0x9 Len: 1 Instr: ADD16 HL, BC Branch: 2 Nobranch: 2
      0xa Len: 1 Instr: LD8 A, [BC] Branch: 2 Nobranch: 2
      0xb Len: 1 Instr: DEC16 BC Branch: 2 Nobranch: 2
      0xc Len: 1 Instr: INC8 C Branch: 1 Nobranch: 1
      0xd Len: 1 Instr: DEC8 C Branch: 1 Nobranch: 1
      0xe Len: 2 Instr: LD8 C, 0x34 Branch: 2 Nobranch: 2
      0xf Len: 1 Instr: RRCA Branch: 1 Nobranch: 1
      0x10 Len: 2 Instr: STOP Branch: 1 Nobranch: 1
      0x11 Len: 3 Instr: LD16 DE, 0x1234 Branch: 3 Nobranch: 3
      0x12 Len: 1 Instr: LD8 [DE], A Branch: 2 Nobranch: 2
      0x13 Len: 1 Instr: INC16 DE Branch: 2 Nobranch: 2
      0x14 Len: 1 Instr: INC8 D Branch: 1 Nobranch: 1
      0x15 Len: 1 Instr: DEC8 D Branch: 1 Nobranch: 1
      0x16 Len: 2 Instr: LD8 D, 0x34 Branch: 2 Nobranch: 2
      0x17 Len: 1 Instr: RLA Branch: 1 Nobranch: 1
      0x18 Len: 2 Instr: JR None, 0x34 Branch: 3 Nobranch: 3
      0x19 Len: 1 Instr: ADD16 HL, DE Branch: 2 Nobranch: 2
      0x1a Len: 1 Instr: LD8 A, [DE] Branch: 2 Nobranch: 2
      0x1b Len: 1 Instr: DEC16 DE Branch: 2 Nobranch: 2
      0x1c Len: 1 Instr: INC8 E Branch: 1 Nobranch: 1
      0x1d Len: 1 Instr: DEC8 E Branch: 1 Nobranch: 1
      0x1e Len: 2 Instr: LD8 E, 0x34 Branch: 2 Nobranch: 2
      0x1f Len: 1 Instr: RRA Branch: 1 Nobranch: 1
      0x20 Len: 2 Instr: JR NZ, 0x34 Branch: 3 Nobranch: 2
      0x21 Len: 3 Instr: LD16 HL, 0x1234 Branch: 3 Nobranch: 3
      0x22 Len: 1 Instr: LD8 [HL+], A Branch: 2 Nobranch: 2
      0x23 Len: 1 Instr: INC16 HL Branch: 2 Nobranch: 2
      0x24 Len: 1 Instr: INC8 H Branch: 1 Nobranch: 1
      0x25 Len: 1 Instr: DEC8 H Branch: 1 Nobranch: 1
      0x26 Len: 2 Instr: LD8 H, 0x34 Branch: 2 Nobranch: 2
      0x27 Len: 1 Instr: DAA Branch: 1 Nobranch: 1
      0x28 Len: 2 Instr: JR Z, 0x34 Branch: 3 Nobranch: 2
      0x29 Len: 1 Instr: ADD16 HL, HL Branch: 2 Nobranch: 2
      0x2a Len: 1 Instr: LD8 A, [HL+] Branch: 2 Nobranch: 2
      0x2b Len: 1 Instr: DEC16 HL Branch: 2 Nobranch: 2
      0x2c Len: 1 Instr: INC8 L Branch: 1 Nobranch: 1
      0x2d Len: 1 Instr: DEC8 L Branch: 1 Nobranch: 1
      0x2e Len: 2 Instr: LD8 L, 0x34 Branch: 2 Nobranch: 2
      0x2f Len: 1 Instr: CPL Branch: 1 Nobranch: 1
      0x30 Len: 2 Instr: JR NC, 0x34 Branch: 3 Nobranch: 2
      0x31 Len: 3 Instr: LD16 SP, 0x1234 Branch: 3 Nobranch: 3
      0x32 Len: 1 Instr: LD8 [HL-], A Branch: 2 Nobranch: 2
      0x33 Len: 1 Instr: INC16 SP Branch: 2 Nobranch: 2
      0x34 Len: 1 Instr: INC8 [HL] Branch: 3 Nobranch: 3
      0x35 Len: 1 Instr: DEC8 [HL] Branch: 3 Nobranch: 3
      0x36 Len: 2 Instr: LD8 [HL], 0x34 Branch: 3 Nobranch: 3
      0x37 Len: 1 Instr: SCF Branch: 1 Nobranch: 1
      0x38 Len: 2 Instr: JR C, 0x34 Branch: 3 Nobranch: 2
      0x39 Len: 1 Instr: ADD16 HL, SP Branch: 2 Nobranch: 2
      0x3a Len: 1 Instr: LD8 A, [HL-] Branch: 2 Nobranch: 2
      0x3b Len: 1 Instr: DEC16 SP Branch: 2 Nobranch: 2
      0x3c Len: 1 Instr: INC8 A Branch: 1 Nobranch: 1
      0x3d Len: 1 Instr: DEC8 A Branch: 1 Nobranch: 1
      0x3e Len: 2 Instr: LD8 A, 0x34 Branch: 2 Nobranch: 2
      0x3f Len: 1 Instr: CCF Branch: 1 Nobranch: 1
      0x40 Len: 1 Instr: LD8 B, B Branch: 1 Nobranch: 1
      0x41 Len: 1 Instr: LD8 B, C Branch: 1 Nobranch: 1
      0x42 Len: 1 Instr: LD8 B, D Branch: 1 Nobranch: 1
      0x43 Len: 1 Instr: LD8 B, E Branch: 1 Nobranch: 1
      0x44 Len: 1 Instr: LD8 B, H Branch: 1 Nobranch: 1
      0x45 Len: 1 Instr: LD8 B, L Branch: 1 Nobranch: 1
      0x46 Len: 1 Instr: LD8 B, [HL] Branch: 2 Nobranch: 2
      0x47 Len: 1 Instr: LD8 B, A Branch: 1 Nobranch: 1
      0x48 Len: 1 Instr: LD8 C, B Branch: 1 Nobranch: 1
      0x49 Len: 1 Instr: LD8 C, C Branch: 1 Nobranch: 1
      0x4a Len: 1 Instr: LD8 C, D Branch: 1 Nobranch: 1
      0x4b Len: 1 Instr: LD8 C, E Branch: 1 Nobranch: 1
      0x4c Len: 1 Instr: LD8 C, H Branch: 1 Nobranch: 1
      0x4d Len: 1 Instr: LD8 C, L Branch: 1 Nobranch: 1
      0x4e Len: 1 Instr: LD8 C, [HL] Branch: 2 Nobranch: 2
      0x4f Len: 1 Instr: LD8 C, A Branch: 1 Nobranch: 1
      0x50 Len: 1 Instr: LD8 D, B Branch: 1 Nobranch: 1
      0x51 Len: 1 Instr: LD8 D, C Branch: 1 Nobranch: 1
      0x52 Len: 1 Instr: LD8 D, D Branch: 1 Nobranch: 1
      0x53 Len: 1 Instr: LD8 D, E Branch: 1 Nobranch: 1
      0x54 Len: 1 Instr: LD8 D, H Branch: 1 Nobranch: 1
      0x55 Len: 1 Instr: LD8 D, L Branch: 1 Nobranch: 1
      0x56 Len: 1 Instr: LD8 D, [HL] Branch: 2 Nobranch: 2
      0x57 Len: 1 Instr: LD8 D, A Branch: 1 Nobranch: 1
      0x58 Len: 1 Instr: LD8 E, B Branch: 1 Nobranch: 1
      0x59 Len: 1 Instr: LD8 E, C Branch: 1 Nobranch: 1
      0x5a Len: 1 Instr: LD8 E, D Branch: 1 Nobranch: 1
      0x5b Len: 1 Instr: LD8 E, E Branch: 1 Nobranch: 1
      0x5c Len: 1 Instr: LD8 E, H Branch: 1 Nobranch: 1
      0x5d Len: 1 Instr: LD8 E, L Branch: 1 Nobranch: 1
      0x5e Len: 1 Instr: LD8 E, [HL] Branch: 2 Nobranch: 2
      0x5f Len: 1 Instr: LD8 E, A Branch: 1 Nobranch: 1
      0x60 Len: 1 Instr: LD8 H, B Branch: 1 Nobranch: 1
      0x61 Len: 1 Instr: LD8 H, C Branch: 1 Nobranch: 1
      0x62 Len: 1 Instr: LD8 H, D Branch: 1 Nobranch: 1
      0x63 Len: 1 Instr: LD8 H, E Branch: 1 Nobranch: 1
      0x64 Len: 1 Instr: LD8 H, H Branch: 1 Nobranch: 1
      0x65 Len: 1 Instr: LD8 H, L Branch: 1 Nobranch: 1
      0x66 Len: 1 Instr: LD8 H, [HL] Branch: 2 Nobranch: 2
      0x67 Len: 1 Instr: LD8 H, A Branch: 1 Nobranch: 1
      0x68 Len: 1 Instr: LD8 L, B Branch: 1 Nobranch: 1
      0x69 Len: 1 Instr: LD8 L, C Branch: 1 Nobranch: 1
      0x6a Len: 1 Instr: LD8 L, D Branch: 1 Nobranch: 1
      0x6b Len: 1 Instr: LD8 L, E Branch: 1 Nobranch: 1
      0x6c Len: 1 Instr: LD8 L, H Branch: 1 Nobranch: 1
      0x6d Len: 1 Instr: LD8 L, L Branch: 1 Nobranch: 1
      0x6e Len: 1 Instr: LD8 L, [HL] Branch: 2 Nobranch: 2
      0x6f Len: 1 Instr: LD8 L, A Branch: 1 Nobranch: 1
      0x70 Len: 1 Instr: LD8 [HL], B Branch: 2 Nobranch: 2
      0x71 Len: 1 Instr: LD8 [HL], C Branch: 2 Nobranch: 2
      0x72 Len: 1 Instr: LD8 [HL], D Branch: 2 Nobranch: 2
      0x73 Len: 1 Instr: LD8 [HL], E Branch: 2 Nobranch: 2
      0x74 Len: 1 Instr: LD8 [HL], H Branch: 2 Nobranch: 2
      0x75 Len: 1 Instr: LD8 [HL], L Branch: 2 Nobranch: 2
      0x76 Len: 1 Instr: HALT Branch: 1 Nobranch: 1
      0x77 Len: 1 Instr: LD8 [HL], A Branch: 2 Nobranch: 2
      0x78 Len: 1 Instr: LD8 A, B Branch: 1 Nobranch: 1
      0x79 Len: 1 Instr: LD8 A, C Branch: 1 Nobranch: 1
      0x7a Len: 1 Instr: LD8 A, D Branch: 1 Nobranch: 1
      0x7b Len: 1 Instr: LD8 A, E Branch: 1 Nobranch: 1
      0x7c Len: 1 Instr: LD8 A, H Branch: 1 Nobranch: 1
      0x7d Len: 1 Instr: LD8 A, L Branch: 1 Nobranch: 1
      0x7e Len: 1 Instr: LD8 A, [HL] Branch: 2 Nobranch: 2
      0x7f Len: 1 Instr: LD8 A, A Branch: 1 Nobranch: 1
      0x80 Len: 1 Instr: ADD8 A, B Branch: 1 Nobranch: 1
      0x81 Len: 1 Instr: ADD8 A, C Branch: 1 Nobranch: 1
      0x82 Len: 1 Instr: ADD8 A, D Branch: 1 Nobranch: 1
      0x83 Len: 1 Instr: ADD8 A, E Branch: 1 Nobranch: 1
      0x84 Len: 1 Instr: ADD8 A, H Branch: 1 Nobranch: 1
      0x85 Len: 1 Instr: ADD8 A, L Branch: 1 Nobranch: 1
      0x86 Len: 1 Instr: ADD8 A, [HL] Branch: 2 Nobranch: 2
      0x87 Len: 1 Instr: ADD8 A, A Branch: 1 Nobranch: 1
      0x88 Len: 1 Instr: ADC  A, B Branch: 1 Nobranch: 1
      0x89 Len: 1 Instr: ADC  A, C Branch: 1 Nobranch: 1
      0x8a Len: 1 Instr: ADC  A, D Branch: 1 Nobranch: 1
      0x8b Len: 1 Instr: ADC  A, E Branch: 1 Nobranch: 1
      0x8c Len: 1 Instr: ADC  A, H Branch: 1 Nobranch: 1
      0x8d Len: 1 Instr: ADC  A, L Branch: 1 Nobranch: 1
      0x8e Len: 1 Instr: ADC  A, [HL] Branch: 2 Nobranch: 2
      0x8f Len: 1 Instr: ADC  A, A Branch: 1 Nobranch: 1
      0x90 Len: 1 Instr: SUB A, B Branch: 1 Nobranch: 1
      0x91 Len: 1 Instr: SUB A, C Branch: 1 Nobranch: 1
      0x92 Len: 1 Instr: SUB A, D Branch: 1 Nobranch: 1
      0x93 Len: 1 Instr: SUB A, E Branch: 1 Nobranch: 1
      0x94 Len: 1 Instr: SUB A, H Branch: 1 Nobranch: 1
      0x95 Len: 1 Instr: SUB A, L Branch: 1 Nobranch: 1
      0x96 Len: 1 Instr: SUB A, [HL] Branch: 2 Nobranch: 2
      0x97 Len: 1 Instr: SUB A, A Branch: 1 Nobranch: 1
      0x98 Len: 1 Instr: SBC A, B Branch: 1 Nobranch: 1
      0x99 Len: 1 Instr: SBC A, C Branch: 1 Nobranch: 1
      0x9a Len: 1 Instr: SBC A, D Branch: 1 Nobranch: 1
      0x9b Len: 1 Instr: SBC A, E Branch: 1 Nobranch: 1
      0x9c Len: 1 Instr: SBC A, H Branch: 1 Nobranch: 1
      0x9d Len: 1 Instr: SBC A, L Branch: 1 Nobranch: 1
      0x9e Len: 1 Instr: SBC A, [HL] Branch: 2 Nobranch: 2
      0x9f Len: 1 Instr: SBC A, A Branch: 1 Nobranch: 1
      0xa0 Len: 1 Instr: AND A, B Branch: 1 Nobranch: 1
      0xa1 Len: 1 Instr: AND A, C Branch: 1 Nobranch: 1
      0xa2 Len: 1 Instr: AND A, D Branch: 1 Nobranch: 1
      0xa3 Len: 1 Instr: AND A, E Branch: 1 Nobranch: 1
      0xa4 Len: 1 Instr: AND A, H Branch: 1 Nobranch: 1
      0xa5 Len: 1 Instr: AND A, L Branch: 1 Nobranch: 1
      0xa6 Len: 1 Instr: AND A, [HL] Branch: 2 Nobranch: 2
      0xa7 Len: 1 Instr: AND A, A Branch: 1 Nobranch: 1
      0xa8 Len: 1 Instr: XOR A, B Branch: 1 Nobranch: 1
      0xa9 Len: 1 Instr: XOR A, C Branch: 1 Nobranch: 1
      0xaa Len: 1 Instr: XOR A, D Branch: 1 Nobranch: 1
      0xab Len: 1 Instr: XOR A, E Branch: 1 Nobranch: 1
      0xac Len: 1 Instr: XOR A, H Branch: 1 Nobranch: 1
      0xad Len: 1 Instr: XOR A, L Branch: 1 Nobranch: 1
      0xae Len: 1 Instr: XOR A, [HL] Branch: 2 Nobranch: 2
      0xaf Len: 1 Instr: XOR A, A Branch: 1 Nobranch: 1
      0xb0 Len: 1 Instr: OR A, B Branch: 1 Nobranch: 1
      0xb1 Len: 1 Instr: OR A, C Branch: 1 Nobranch: 1
      0xb2 Len: 1 Instr: OR A, D Branch: 1 Nobranch: 1
      0xb3 Len: 1 Instr: OR A, E Branch: 1 Nobranch: 1
      0xb4 Len: 1 Instr: OR A, H Branch: 1 Nobranch: 1
      0xb5 Len: 1 Instr: OR A, L Branch: 1 Nobranch: 1
      0xb6 Len: 1 Instr: OR A, [HL] Branch: 2 Nobranch: 2
      0xb7 Len: 1 Instr: OR A, A Branch: 1 Nobranch: 1
      0xb8 Len: 1 Instr: CP A, B Branch: 1 Nobranch: 1
      0xb9 Len: 1 Instr: CP A, C Branch: 1 Nobranch: 1
      0xba Len: 1 Instr: CP A, D Branch: 1 Nobranch: 1
      0xbb Len: 1 Instr: CP A, E Branch: 1 Nobranch: 1
      0xbc Len: 1 Instr: CP A, H Branch: 1 Nobranch: 1
      0xbd Len: 1 Instr: CP A, L Branch: 1 Nobranch: 1
      0xbe Len: 1 Instr: CP A, [HL] Branch: 2 Nobranch: 2
      0xbf Len: 1 Instr: CP A, A Branch: 1 Nobranch: 1
      0xc0 Len: 1 Instr: RET NZ Branch: 5 Nobranch: 2
      0xc1 Len: 1 Instr: POP BC Branch: 3 Nobranch: 3
      0xc2 Len: 3 Instr: JP NZ, 0x1234 Branch: 4 Nobranch: 3
      0xc3 Len: 3 Instr: JP None, 0x1234 Branch: 4 Nobranch: 4
      0xc4 Len: 3 Instr: CALL NZ, 0x1234 Branch: 6 Nobranch: 3
      0xc5 Len: 1 Instr: PUSH BC Branch: 4 Nobranch: 4
      0xc6 Len: 2 Instr: ADD8 A, 0x34 Branch: 2 Nobranch: 2
      0xc7 Len: 1 Instr: RST 0x0 Branch: 4 Nobranch: 4
      0xc8 Len: 1 Instr: RET Z Branch: 5 Nobranch: 2
      0xc9 Len: 1 Instr: RET None Branch: 4 Nobranch: 4
      0xca Len: 3 Instr: JP Z, 0x1234 Branch: 4 Nobranch: 3
      0xcc Len: 3 Instr: CALL Z, 0x1234 Branch: 6 Nobranch: 3
      0xcd Len: 3 Instr: CALL None, 0x1234 Branch: 6 Nobranch: 6
      0xce Len: 2 Instr: ADC  A, 0x34 Branch: 2 Nobranch: 2
      0xcf Len: 1 Instr: RST 0x8 Branch: 4 Nobranch: 4
      0xd0 Len: 1 Instr: RET NC Branch: 5 Nobranch: 2
      0xd1 Len: 1 Instr: POP DE Branch: 3 Nobranch: 3
      0xd2 Len: 3 Instr: JP NC, 0x1234 Branch: 4 Nobranch: 3
      0xd4 Len: 3 Instr: CALL NC, 0x1234 Branch: 6 Nobranch: 3
      0xd5 Len: 1 Instr: PUSH DE Branch: 4 Nobranch: 4
      0xd6 Len: 2 Instr: SUB A, 0x34 Branch: 2 Nobranch: 2
      0xd7 Len: 1 Instr: RST 0x10 Branch: 4 Nobranch: 4
      0xd8 Len: 1 Instr: RET C Branch: 5 Nobranch: 2
      0xd9 Len: 1 Instr: RETI Branch: 4 Nobranch: 4
      0xda Len: 3 Instr: JP C, 0x1234 Branch: 4 Nobranch: 3
      0xdc Len: 3 Instr: CALL C, 0x1234 Branch: 6 Nobranch: 3
      0xde Len: 2 Instr: SBC A, 0x34 Branch: 2 Nobranch: 2
      0xdf Len: 1 Instr: RST 0x18 Branch: 4 Nobranch: 4
      0xe0 Len: 2 Instr: LD8 [0xFF00 + 0x34], A Branch: 3 Nobranch: 3
      0xe1 Len: 1 Instr: POP HL Branch: 3 Nobranch: 3
      0xe2 Len: 1 Instr: LD8 [0xFF00 + C], A Branch: 2 Nobranch: 2
      0xe5 Len: 1 Instr: PUSH HL Branch: 4 Nobranch: 4
      0xe6 Len: 2 Instr: AND A, 0x34 Branch: 2 Nobranch: 2
      0xe7 Len: 1 Instr: RST 0x20 Branch: 4 Nobranch: 4
      0xe8 Len: 2 Instr: ADDSP 52 Branch: 4 Nobranch: 4
      0xe9 Len: 1 Instr: JP None, HL Branch: 1 Nobranch: 1
      0xea Len: 3 Instr: LD8 [0x1234], A Branch: 4 Nobranch: 4
      0xee Len: 2 Instr: XOR A, 0x34 Branch: 2 Nobranch: 2
      0xef Len: 1 Instr: RST 0x28 Branch: 4 Nobranch: 4
      0xf0 Len: 2 Instr: LD8 A, [0xFF00 + 0x34] Branch: 3 Nobranch: 3
      0xf1 Len: 1 Instr: POP AF Branch: 3 Nobranch: 3
      0xf2 Len: 1 Instr: LD8 A, [0xFF00 + C] Branch: 2 Nobranch: 2
      0xf3 Len: 1 Instr: DI Branch: 1 Nobranch: 1
      0xf5 Len: 1 Instr: PUSH AF Branch: 4 Nobranch: 4
      0xf6 Len: 2 Instr: OR A, 0x34 Branch: 2 Nobranch: 2
      0xf7 Len: 1 Instr: RST 0x30 Branch: 4 Nobranch: 4
      0xf8 Len: 2 Instr: LD16 HL, [SP + 52] Branch: 3 Nobranch: 3
      0xf9 Len: 1 Instr: LD16 SP, HL Branch: 2 Nobranch: 2
      0xfa Len: 3 Instr: LD8 A, [0x1234] Branch: 4 Nobranch: 4
      0xfb Len: 1 Instr: EI Branch: 1 Nobranch: 1
      0xfe Len: 2 Instr: CP A, 0x34 Branch: 2 Nobranch: 2
      0xff Len: 1 Instr: RST 0x38 Branch: 4 Nobranch: 4
      0 Len: 2 Instr: RLC B Branch: 2 Nobranch: 2
      0x1 Len: 2 Instr: RLC C Branch: 2 Nobranch: 2
      0x2 Len: 2 Instr: RLC D Branch: 2 Nobranch: 2
      0x3 Len: 2 Instr: RLC E Branch: 2 Nobranch: 2
      0x4 Len: 2 Instr: RLC H Branch: 2 Nobranch: 2
      0x5 Len: 2 Instr: RLC L Branch: 2 Nobranch: 2
      0x6 Len: 2 Instr: RLC [HL] Branch: 4 Nobranch: 4
      0x7 Len: 2 Instr: RLC A Branch: 2 Nobranch: 2
      0x8 Len: 2 Instr: RRC B Branch: 2 Nobranch: 2
      0x9 Len: 2 Instr: RRC C Branch: 2 Nobranch: 2
      0xa Len: 2 Instr: RRC D Branch: 2 Nobranch: 2
      0xb Len: 2 Instr: RRC E Branch: 2 Nobranch: 2
      0xc Len: 2 Instr: RRC H Branch: 2 Nobranch: 2
      0xd Len: 2 Instr: RRC L Branch: 2 Nobranch: 2
      0xe Len: 2 Instr: RRC [HL] Branch: 4 Nobranch: 4
      0xf Len: 2 Instr: RRC A Branch: 2 Nobranch: 2
      0x10 Len: 2 Instr: RL B Branch: 2 Nobranch: 2
      0x11 Len: 2 Instr: RL C Branch: 2 Nobranch: 2
      0x12 Len: 2 Instr: RL D Branch: 2 Nobranch: 2
      0x13 Len: 2 Instr: RL E Branch: 2 Nobranch: 2
      0x14 Len: 2 Instr: RL H Branch: 2 Nobranch: 2
      0x15 Len: 2 Instr: RL L Branch: 2 Nobranch: 2
      0x16 Len: 2 Instr: RL [HL] Branch: 4 Nobranch: 4
      0x17 Len: 2 Instr: RL A Branch: 2 Nobranch: 2
      0x18 Len: 2 Instr: RR B Branch: 2 Nobranch: 2
      0x19 Len: 2 Instr: RR C Branch: 2 Nobranch: 2
      0x1a Len: 2 Instr: RR D Branch: 2 Nobranch: 2
      0x1b Len: 2 Instr: RR E Branch: 2 Nobranch: 2
      0x1c Len: 2 Instr: RR H Branch: 2 Nobranch: 2
      0x1d Len: 2 Instr: RR L Branch: 2 Nobranch: 2
      0x1e Len: 2 Instr: RR [HL] Branch: 4 Nobranch: 4
      0x1f Len: 2 Instr: RR A Branch: 2 Nobranch: 2
      0x20 Len: 2 Instr: SLA B Branch: 2 Nobranch: 2
      0x21 Len: 2 Instr: SLA C Branch: 2 Nobranch: 2
      0x22 Len: 2 Instr: SLA D Branch: 2 Nobranch: 2
      0x23 Len: 2 Instr: SLA E Branch: 2 Nobranch: 2
      0x24 Len: 2 Instr: SLA H Branch: 2 Nobranch: 2
      0x25 Len: 2 Instr: SLA L Branch: 2 Nobranch: 2
      0x26 Len: 2 Instr: SLA [HL] Branch: 4 Nobranch: 4
      0x27 Len: 2 Instr: SLA A Branch: 2 Nobranch: 2
      0x28 Len: 2 Instr: SRA B Branch: 2 Nobranch: 2
      0x29 Len: 2 Instr: SRA C Branch: 2 Nobranch: 2
      0x2a Len: 2 Instr: SRA D Branch: 2 Nobranch: 2
      0x2b Len: 2 Instr: SRA E Branch: 2 Nobranch: 2
      0x2c Len: 2 Instr: SRA H Branch: 2 Nobranch: 2
      0x2d Len: 2 Instr: SRA L Branch: 2 Nobranch: 2
      0x2e Len: 2 Instr: SRA [HL] Branch: 4 Nobranch: 4
      0x2f Len: 2 Instr: SRA A Branch: 2 Nobranch: 2
      0x30 Len: 2 Instr: SWAP B Branch: 2 Nobranch: 2
      0x31 Len: 2 Instr: SWAP C Branch: 2 Nobranch: 2
      0x32 Len: 2 Instr: SWAP D Branch: 2 Nobranch: 2
      0x33 Len: 2 Instr: SWAP E Branch: 2 Nobranch: 2
      0x34 Len: 2 Instr: SWAP H Branch: 2 Nobranch: 2
      0x35 Len: 2 Instr: SWAP L Branch: 2 Nobranch: 2
      0x36 Len: 2 Instr: SWAP [HL] Branch: 4 Nobranch: 4
      0x37 Len: 2 Instr: SWAP A Branch: 2 Nobranch: 2
      0x38 Len: 2 Instr: SRL B Branch: 2 Nobranch: 2
      0x39 Len: 2 Instr: SRL C Branch: 2 Nobranch: 2
      0x3a Len: 2 Instr: SRL D Branch: 2 Nobranch: 2
      0x3b Len: 2 Instr: SRL E Branch: 2 Nobranch: 2
      0x3c Len: 2 Instr: SRL H Branch: 2 Nobranch: 2
      0x3d Len: 2 Instr: SRL L Branch: 2 Nobranch: 2
      0x3e Len: 2 Instr: SRL [HL] Branch: 4 Nobranch: 4
      0x3f Len: 2 Instr: SRL A Branch: 2 Nobranch: 2
      0x40 Len: 2 Instr: BIT 0, B Branch: 2 Nobranch: 2
      0x41 Len: 2 Instr: BIT 0, C Branch: 2 Nobranch: 2
      0x42 Len: 2 Instr: BIT 0, D Branch: 2 Nobranch: 2
      0x43 Len: 2 Instr: BIT 0, E Branch: 2 Nobranch: 2
      0x44 Len: 2 Instr: BIT 0, H Branch: 2 Nobranch: 2
      0x45 Len: 2 Instr: BIT 0, L Branch: 2 Nobranch: 2
      0x46 Len: 2 Instr: BIT 0, [HL] Branch: 3 Nobranch: 3
      0x47 Len: 2 Instr: BIT 0, A Branch: 2 Nobranch: 2
      0x48 Len: 2 Instr: BIT 1, B Branch: 2 Nobranch: 2
      0x49 Len: 2 Instr: BIT 1, C Branch: 2 Nobranch: 2
      0x4a Len: 2 Instr: BIT 1, D Branch: 2 Nobranch: 2
      0x4b Len: 2 Instr: BIT 1, E Branch: 2 Nobranch: 2
      0x4c Len: 2 Instr: BIT 1, H Branch: 2 Nobranch: 2
      0x4d Len: 2 Instr: BIT 1, L Branch: 2 Nobranch: 2
      0x4e Len: 2 Instr: BIT 1, [HL] Branch: 3 Nobranch: 3
      0x4f Len: 2 Instr: BIT 1, A Branch: 2 Nobranch: 2
      0x50 Len: 2 Instr: BIT 2, B Branch: 2 Nobranch: 2
      0x51 Len: 2 Instr: BIT 2, C Branch: 2 Nobranch: 2
      0x52 Len: 2 Instr: BIT 2, D Branch: 2 Nobranch: 2
      0x53 Len: 2 Instr: BIT 2, E Branch: 2 Nobranch: 2
      0x54 Len: 2 Instr: BIT 2, H Branch: 2 Nobranch: 2
      0x55 Len: 2 Instr: BIT 2, L Branch: 2 Nobranch: 2
      0x56 Len: 2 Instr: BIT 2, [HL] Branch: 3 Nobranch: 3
      0x57 Len: 2 Instr: BIT 2, A Branch: 2 Nobranch: 2
      0x58 Len: 2 Instr: BIT 3, B Branch: 2 Nobranch: 2
      0x59 Len: 2 Instr: BIT 3, C Branch: 2 Nobranch: 2
      0x5a Len: 2 Instr: BIT 3, D Branch: 2 Nobranch: 2
      0x5b Len: 2 Instr: BIT 3, E Branch: 2 Nobranch: 2
      0x5c Len: 2 Instr: BIT 3, H Branch: 2 Nobranch: 2
      0x5d Len: 2 Instr: BIT 3, L Branch: 2 Nobranch: 2
      0x5e Len: 2 Instr: BIT 3, [HL] Branch: 3 Nobranch: 3
      0x5f Len: 2 Instr: BIT 3, A Branch: 2 Nobranch: 2
      0x60 Len: 2 Instr: BIT 4, B Branch: 2 Nobranch: 2
      0x61 Len: 2 Instr: BIT 4, C Branch: 2 Nobranch: 2
      0x62 Len: 2 Instr: BIT 4, D Branch: 2 Nobranch: 2
      0x63 Len: 2 Instr: BIT 4, E Branch: 2 Nobranch: 2
      0x64 Len: 2 Instr: BIT 4, H Branch: 2 Nobranch: 2
      0x65 Len: 2 Instr: BIT 4, L Branch: 2 Nobranch: 2
      0x66 Len: 2 Instr: BIT 4, [HL] Branch: 3 Nobranch: 3
      0x67 Len: 2 Instr: BIT 4, A Branch: 2 Nobranch: 2
      0x68 Len: 2 Instr: BIT 5, B Branch: 2 Nobranch: 2
      0x69 Len: 2 Instr: BIT 5, C Branch: 2 Nobranch: 2
      0x6a Len: 2 Instr: BIT 5, D Branch: 2 Nobranch: 2
      0x6b Len: 2 Instr: BIT 5, E Branch: 2 Nobranch: 2
      0x6c Len: 2 Instr: BIT 5, H Branch: 2 Nobranch: 2
      0x6d Len: 2 Instr: BIT 5, L Branch: 2 Nobranch: 2
      0x6e Len: 2 Instr: BIT 5, [HL] Branch: 3 Nobranch: 3
      0x6f Len: 2 Instr: BIT 5, A Branch: 2 Nobranch: 2
      0x70 Len: 2 Instr: BIT 6, B Branch: 2 Nobranch: 2
      0x71 Len: 2 Instr: BIT 6, C Branch: 2 Nobranch: 2
      0x72 Len: 2 Instr: BIT 6, D Branch: 2 Nobranch: 2
      0x73 Len: 2 Instr: BIT 6, E Branch: 2 Nobranch: 2
      0x74 Len: 2 Instr: BIT 6, H Branch: 2 Nobranch: 2
      0x75 Len: 2 Instr: BIT 6, L Branch: 2 Nobranch: 2
      0x76 Len: 2 Instr: BIT 6, [HL] Branch: 3 Nobranch: 3
      0x77 Len: 2 Instr: BIT 6, A Branch: 2 Nobranch: 2
      0x78 Len: 2 Instr: BIT 7, B Branch: 2 Nobranch: 2
      0x79 Len: 2 Instr: BIT 7, C Branch: 2 Nobranch: 2
      0x7a Len: 2 Instr: BIT 7, D Branch: 2 Nobranch: 2
      0x7b Len: 2 Instr: BIT 7, E Branch: 2 Nobranch: 2
      0x7c Len: 2 Instr: BIT 7, H Branch: 2 Nobranch: 2
      0x7d Len: 2 Instr: BIT 7, L Branch: 2 Nobranch: 2
      0x7e Len: 2 Instr: BIT 7, [HL] Branch: 3 Nobranch: 3
      0x7f Len: 2 Instr: BIT 7, A Branch: 2 Nobranch: 2
      0x80 Len: 2 Instr: RES 0, B Branch: 2 Nobranch: 2
      0x81 Len: 2 Instr: RES 0, C Branch: 2 Nobranch: 2
      0x82 Len: 2 Instr: RES 0, D Branch: 2 Nobranch: 2
      0x83 Len: 2 Instr: RES 0, E Branch: 2 Nobranch: 2
      0x84 Len: 2 Instr: RES 0, H Branch: 2 Nobranch: 2
      0x85 Len: 2 Instr: RES 0, L Branch: 2 Nobranch: 2
      0x86 Len: 2 Instr: RES 0, [HL] Branch: 4 Nobranch: 4
      0x87 Len: 2 Instr: RES 0, A Branch: 2 Nobranch: 2
      0x88 Len: 2 Instr: RES 1, B Branch: 2 Nobranch: 2
      0x89 Len: 2 Instr: RES 1, C Branch: 2 Nobranch: 2
      0x8a Len: 2 Instr: RES 1, D Branch: 2 Nobranch: 2
      0x8b Len: 2 Instr: RES 1, E Branch: 2 Nobranch: 2
      0x8c Len: 2 Instr: RES 1, H Branch: 2 Nobranch: 2
      0x8d Len: 2 Instr: RES 1, L Branch: 2 Nobranch: 2
      0x8e Len: 2 Instr: RES 1, [HL] Branch: 4 Nobranch: 4
      0x8f Len: 2 Instr: RES 1, A Branch: 2 Nobranch: 2
      0x90 Len: 2 Instr: RES 2, B Branch: 2 Nobranch: 2
      0x91 Len: 2 Instr: RES 2, C Branch: 2 Nobranch: 2
      0x92 Len: 2 Instr: RES 2, D Branch: 2 Nobranch: 2
      0x93 Len: 2 Instr: RES 2, E Branch: 2 Nobranch: 2
      0x94 Len: 2 Instr: RES 2, H Branch: 2 Nobranch: 2
      0x95 Len: 2 Instr: RES 2, L Branch: 2 Nobranch: 2
      0x96 Len: 2 Instr: RES 2, [HL] Branch: 4 Nobranch: 4
      0x97 Len: 2 Instr: RES 2, A Branch: 2 Nobranch: 2
      0x98 Len: 2 Instr: RES 3, B Branch: 2 Nobranch: 2
      0x99 Len: 2 Instr: RES 3, C Branch: 2 Nobranch: 2
      0x9a Len: 2 Instr: RES 3, D Branch: 2 Nobranch: 2
      0x9b Len: 2 Instr: RES 3, E Branch: 2 Nobranch: 2
      0x9c Len: 2 Instr: RES 3, H Branch: 2 Nobranch: 2
      0x9d Len: 2 Instr: RES 3, L Branch: 2 Nobranch: 2
      0x9e Len: 2 Instr: RES 3, [HL] Branch: 4 Nobranch: 4
      0x9f Len: 2 Instr: RES 3, A Branch: 2 Nobranch: 2
      0xa0 Len: 2 Instr: RES 4, B Branch: 2 Nobranch: 2
      0xa1 Len: 2 Instr: RES 4, C Branch: 2 Nobranch: 2
      0xa2 Len: 2 Instr: RES 4, D Branch: 2 Nobranch: 2
      0xa3 Len: 2 Instr: RES 4, E Branch: 2 Nobranch: 2
      0xa4 Len: 2 Instr: RES 4, H Branch: 2 Nobranch: 2
      0xa5 Len: 2 Instr: RES 4, L Branch: 2 Nobranch: 2
      0xa6 Len: 2 Instr: RES 4, [HL] Branch: 4 Nobranch: 4
      0xa7 Len: 2 Instr: RES 4, A Branch: 2 Nobranch: 2
      0xa8 Len: 2 Instr: RES 5, B Branch: 2 Nobranch: 2
      0xa9 Len: 2 Instr: RES 5, C Branch: 2 Nobranch: 2
      0xaa Len: 2 Instr: RES 5, D Branch: 2 Nobranch: 2
      0xab Len: 2 Instr: RES 5, E Branch: 2 Nobranch: 2
      0xac Len: 2 Instr: RES 5, H Branch: 2 Nobranch: 2
      0xad Len: 2 Instr: RES 5, L Branch: 2 Nobranch: 2
      0xae Len: 2 Instr: RES 5, [HL] Branch: 4 Nobranch: 4
      0xaf Len: 2 Instr: RES 5, A Branch: 2 Nobranch: 2
      0xb0 Len: 2 Instr: RES 6, B Branch: 2 Nobranch: 2
      0xb1 Len: 2 Instr: RES 6, C Branch: 2 Nobranch: 2
      0xb2 Len: 2 Instr: RES 6, D Branch: 2 Nobranch: 2
      0xb3 Len: 2 Instr: RES 6, E Branch: 2 Nobranch: 2
      0xb4 Len: 2 Instr: RES 6, H Branch: 2 Nobranch: 2
      0xb5 Len: 2 Instr: RES 6, L Branch: 2 Nobranch: 2
      0xb6 Len: 2 Instr: RES 6, [HL] Branch: 4 Nobranch: 4
      0xb7 Len: 2 Instr: RES 6, A Branch: 2 Nobranch: 2
      0xb8 Len: 2 Instr: RES 7, B Branch: 2 Nobranch: 2
      0xb9 Len: 2 Instr: RES 7, C Branch: 2 Nobranch: 2
      0xba Len: 2 Instr: RES 7, D Branch: 2 Nobranch: 2
      0xbb Len: 2 Instr: RES 7, E Branch: 2 Nobranch: 2
      0xbc Len: 2 Instr: RES 7, H Branch: 2 Nobranch: 2
      0xbd Len: 2 Instr: RES 7, L Branch: 2 Nobranch: 2
      0xbe Len: 2 Instr: RES 7, [HL] Branch: 4 Nobranch: 4
      0xbf Len: 2 Instr: RES 7, A Branch: 2 Nobranch: 2
      0xc0 Len: 2 Instr: SET 0, B Branch: 2 Nobranch: 2
      0xc1 Len: 2 Instr: SET 0, C Branch: 2 Nobranch: 2
      0xc2 Len: 2 Instr: SET 0, D Branch: 2 Nobranch: 2
      0xc3 Len: 2 Instr: SET 0, E Branch: 2 Nobranch: 2
      0xc4 Len: 2 Instr: SET 0, H Branch: 2 Nobranch: 2
      0xc5 Len: 2 Instr: SET 0, L Branch: 2 Nobranch: 2
      0xc6 Len: 2 Instr: SET 0, [HL] Branch: 4 Nobranch: 4
      0xc7 Len: 2 Instr: SET 0, A Branch: 2 Nobranch: 2
      0xc8 Len: 2 Instr: SET 1, B Branch: 2 Nobranch: 2
      0xc9 Len: 2 Instr: SET 1, C Branch: 2 Nobranch: 2
      0xca Len: 2 Instr: SET 1, D Branch: 2 Nobranch: 2
      0xcb Len: 2 Instr: SET 1, E Branch: 2 Nobranch: 2
      0xcc Len: 2 Instr: SET 1, H Branch: 2 Nobranch: 2
      0xcd Len: 2 Instr: SET 1, L Branch: 2 Nobranch: 2
      0xce Len: 2 Instr: SET 1, [HL] Branch: 4 Nobranch: 4
      0xcf Len: 2 Instr: SET 1, A Branch: 2 Nobranch: 2
      0xd0 Len: 2 Instr: SET 2, B Branch: 2 Nobranch: 2
      0xd1 Len: 2 Instr: SET 2, C Branch: 2 Nobranch: 2
      0xd2 Len: 2 Instr: SET 2, D Branch: 2 Nobranch: 2
      0xd3 Len: 2 Instr: SET 2, E Branch: 2 Nobranch: 2
      0xd4 Len: 2 Instr: SET 2, H Branch: 2 Nobranch: 2
      0xd5 Len: 2 Instr: SET 2, L Branch: 2 Nobranch: 2
      0xd6 Len: 2 Instr: SET 2, [HL] Branch: 4 Nobranch: 4
      0xd7 Len: 2 Instr: SET 2, A Branch: 2 Nobranch: 2
      0xd8 Len: 2 Instr: SET 3, B Branch: 2 Nobranch: 2
      0xd9 Len: 2 Instr: SET 3, C Branch: 2 Nobranch: 2
      0xda Len: 2 Instr: SET 3, D Branch: 2 Nobranch: 2
      0xdb Len: 2 Instr: SET 3, E Branch: 2 Nobranch: 2
      0xdc Len: 2 Instr: SET 3, H Branch: 2 Nobranch: 2
      0xdd Len: 2 Instr: SET 3, L Branch: 2 Nobranch: 2
      0xde Len: 2 Instr: SET 3, [HL] Branch: 4 Nobranch: 4
      0xdf Len: 2 Instr: SET 3, A Branch: 2 Nobranch: 2
      0xe0 Len: 2 Instr: SET 4, B Branch: 2 Nobranch: 2
      0xe1 Len: 2 Instr: SET 4, C Branch: 2 Nobranch: 2
      0xe2 Len: 2 Instr: SET 4, D Branch: 2 Nobranch: 2
      0xe3 Len: 2 Instr: SET 4, E Branch: 2 Nobranch: 2
      0xe4 Len: 2 Instr: SET 4, H Branch: 2 Nobranch: 2
      0xe5 Len: 2 Instr: SET 4, L Branch: 2 Nobranch: 2
      0xe6 Len: 2 Instr: SET 4, [HL] Branch: 4 Nobranch: 4
      0xe7 Len: 2 Instr: SET 4, A Branch: 2 Nobranch: 2
      0xe8 Len: 2 Instr: SET 5, B Branch: 2 Nobranch: 2
      0xe9 Len: 2 Instr: SET 5, C Branch: 2 Nobranch: 2
      0xea Len: 2 Instr: SET 5, D Branch: 2 Nobranch: 2
      0xeb Len: 2 Instr: SET 5, E Branch: 2 Nobranch: 2
      0xec Len: 2 Instr: SET 5, H Branch: 2 Nobranch: 2
      0xed Len: 2 Instr: SET 5, L Branch: 2 Nobranch: 2
      0xee Len: 2 Instr: SET 5, [HL] Branch: 4 Nobranch: 4
      0xef Len: 2 Instr: SET 5, A Branch: 2 Nobranch: 2
      0xf0 Len: 2 Instr: SET 6, B Branch: 2 Nobranch: 2
      0xf1 Len: 2 Instr: SET 6, C Branch: 2 Nobranch: 2
      0xf2 Len: 2 Instr: SET 6, D Branch: 2 Nobranch: 2
      0xf3 Len: 2 Instr: SET 6, E Branch: 2 Nobranch: 2
      0xf4 Len: 2 Instr: SET 6, H Branch: 2 Nobranch: 2
      0xf5 Len: 2 Instr: SET 6, L Branch: 2 Nobranch: 2
      0xf6 Len: 2 Instr: SET 6, [HL] Branch: 4 Nobranch: 4
      0xf7 Len: 2 Instr: SET 6, A Branch: 2 Nobranch: 2
      0xf8 Len: 2 Instr: SET 7, B Branch: 2 Nobranch: 2
      0xf9 Len: 2 Instr: SET 7, C Branch: 2 Nobranch: 2
      0xfa Len: 2 Instr: SET 7, D Branch: 2 Nobranch: 2
      0xfb Len: 2 Instr: SET 7, E Branch: 2 Nobranch: 2
      0xfc Len: 2 Instr: SET 7, H Branch: 2 Nobranch: 2
      0xfd Len: 2 Instr: SET 7, L Branch: 2 Nobranch: 2
      0xfe Len: 2 Instr: SET 7, [HL] Branch: 4 Nobranch: 4
      0xff Len: 2 Instr: SET 7, A Branch: 2 Nobranch: 2
      |}]
