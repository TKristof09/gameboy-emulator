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
        info.instr
        |> Instruction.show
        |> Printf.printf "%#x Len: %s Instr: %s\n" i (Uint16.to_string info.len)
    done;
    for i = 0 to 0xFF do
      let mem = Mem.of_list 2 [ 0xCB; i ] in
      let info = Fetcher.fetch mem ~pc:Uint16.zero in
      info.instr
      |> Instruction.show
      |> Printf.printf "%#x Len: %s Instr: %s\n" i (Uint16.to_string info.len)
    done;
    [%expect
        {|
0 Len: 1 Instr: NOP
0x1 Len: 3 Instr: LD16 BC, 0x1234
0x2 Len: 1 Instr: LD8 [BC], A
0x3 Len: 1 Instr: INC16 BC
0x4 Len: 1 Instr: INC8 B
0x5 Len: 1 Instr: DEC8 B
0x6 Len: 2 Instr: LD8 B, 0x34
0x7 Len: 1 Instr: RLCA
0x8 Len: 3 Instr: LD16 [0x1234], SP
0x9 Len: 1 Instr: ADD16 HL, BC
0xa Len: 1 Instr: LD8 A, [BC]
0xb Len: 1 Instr: DEC16 BC
0xc Len: 1 Instr: INC8 C
0xd Len: 1 Instr: DEC8 C
0xe Len: 2 Instr: LD8 C, 0x34
0xf Len: 1 Instr: RRCA
0x10 Len: 2 Instr: STOP
0x11 Len: 3 Instr: LD16 DE, 0x1234
0x12 Len: 1 Instr: LD8 [DE], A
0x13 Len: 1 Instr: INC16 DE
0x14 Len: 1 Instr: INC8 D
0x15 Len: 1 Instr: DEC8 D
0x16 Len: 2 Instr: LD8 D, 0x34
0x17 Len: 1 Instr: RLA
0x18 Len: 2 Instr: JR None, 0x34
0x19 Len: 1 Instr: ADD16 HL, DE
0x1a Len: 1 Instr: LD8 A, [DE]
0x1b Len: 1 Instr: DEC16 DE
0x1c Len: 1 Instr: INC8 E
0x1d Len: 1 Instr: DEC8 E
0x1e Len: 2 Instr: LD8 E, 0x34
0x1f Len: 1 Instr: RRA
0x20 Len: 2 Instr: JR NZ, 0x34
0x21 Len: 3 Instr: LD16 HL, 0x1234
0x22 Len: 1 Instr: LD8 [HL+], A
0x23 Len: 1 Instr: INC16 HL
0x24 Len: 1 Instr: INC8 H
0x25 Len: 1 Instr: DEC8 H
0x26 Len: 2 Instr: LD8 H, 0x34
0x27 Len: 1 Instr: DAA
0x28 Len: 2 Instr: JR Z, 0x34
0x29 Len: 1 Instr: ADD16 HL, HL
0x2a Len: 1 Instr: LD8 A, [HL]
0x2b Len: 1 Instr: DEC16 HL
0x2c Len: 1 Instr: INC8 L
0x2d Len: 1 Instr: DEC8 L
0x2e Len: 2 Instr: LD8 L, 0x34
0x2f Len: 1 Instr: CPL
0x30 Len: 2 Instr: JR NC, 0x34
0x31 Len: 3 Instr: LD16 SP, 0x1234
0x32 Len: 1 Instr: LD8 [HL-], A
0x33 Len: 1 Instr: INC16 SP
0x34 Len: 1 Instr: INC8 [HL]
0x35 Len: 1 Instr: DEC8 [HL]
0x36 Len: 2 Instr: LD8 [HL], 0x34
0x37 Len: 1 Instr: SCF
0x38 Len: 2 Instr: JR C, 0x34
0x39 Len: 1 Instr: ADD16 HL, SP
0x3a Len: 1 Instr: LD8 A, [HL-]
0x3b Len: 1 Instr: DEC16 SP
0x3c Len: 1 Instr: INC8 A
0x3d Len: 1 Instr: DEC8 A
0x3e Len: 2 Instr: LD8 A, 0x34
0x3f Len: 1 Instr: CCF
0x40 Len: 1 Instr: LD8 B, B
0x41 Len: 1 Instr: LD8 B, C
0x42 Len: 1 Instr: LD8 B, D
0x43 Len: 1 Instr: LD8 B, E
0x44 Len: 1 Instr: LD8 B, H
0x45 Len: 1 Instr: LD8 B, L
0x46 Len: 1 Instr: LD8 B, [HL]
0x47 Len: 1 Instr: LD8 B, A
0x48 Len: 1 Instr: LD8 C, B
0x49 Len: 1 Instr: LD8 C, C
0x4a Len: 1 Instr: LD8 C, D
0x4b Len: 1 Instr: LD8 C, E
0x4c Len: 1 Instr: LD8 C, H
0x4d Len: 1 Instr: LD8 C, L
0x4e Len: 1 Instr: LD8 C, [HL]
0x4f Len: 1 Instr: LD8 C, A
0x50 Len: 1 Instr: LD8 D, B
0x51 Len: 1 Instr: LD8 D, C
0x52 Len: 1 Instr: LD8 D, D
0x53 Len: 1 Instr: LD8 D, E
0x54 Len: 1 Instr: LD8 D, H
0x55 Len: 1 Instr: LD8 D, L
0x56 Len: 1 Instr: LD8 D, [HL]
0x57 Len: 1 Instr: LD8 D, A
0x58 Len: 1 Instr: LD8 E, B
0x59 Len: 1 Instr: LD8 E, C
0x5a Len: 1 Instr: LD8 E, D
0x5b Len: 1 Instr: LD8 E, E
0x5c Len: 1 Instr: LD8 E, H
0x5d Len: 1 Instr: LD8 E, L
0x5e Len: 1 Instr: LD8 E, [HL]
0x5f Len: 1 Instr: LD8 E, A
0x60 Len: 1 Instr: LD8 H, B
0x61 Len: 1 Instr: LD8 H, C
0x62 Len: 1 Instr: LD8 H, D
0x63 Len: 1 Instr: LD8 H, E
0x64 Len: 1 Instr: LD8 H, H
0x65 Len: 1 Instr: LD8 H, L
0x66 Len: 1 Instr: LD8 H, [HL]
0x67 Len: 1 Instr: LD8 H, A
0x68 Len: 1 Instr: LD8 L, B
0x69 Len: 1 Instr: LD8 L, C
0x6a Len: 1 Instr: LD8 L, D
0x6b Len: 1 Instr: LD8 L, E
0x6c Len: 1 Instr: LD8 L, H
0x6d Len: 1 Instr: LD8 L, L
0x6e Len: 1 Instr: LD8 L, [HL]
0x6f Len: 1 Instr: LD8 L, A
0x70 Len: 1 Instr: LD8 [HL], B
0x71 Len: 1 Instr: LD8 [HL], C
0x72 Len: 1 Instr: LD8 [HL], D
0x73 Len: 1 Instr: LD8 [HL], E
0x74 Len: 1 Instr: LD8 [HL], H
0x75 Len: 1 Instr: LD8 [HL], L
0x76 Len: 1 Instr: HALT
0x77 Len: 1 Instr: LD8 [HL], A
0x78 Len: 1 Instr: LD8 A, B
0x79 Len: 1 Instr: LD8 A, C
0x7a Len: 1 Instr: LD8 A, D
0x7b Len: 1 Instr: LD8 A, E
0x7c Len: 1 Instr: LD8 A, H
0x7d Len: 1 Instr: LD8 A, L
0x7e Len: 1 Instr: LD8 A, [HL]
0x7f Len: 1 Instr: LD8 A, A
0x80 Len: 1 Instr: ADD8 A, B
0x81 Len: 1 Instr: ADD8 A, C
0x82 Len: 1 Instr: ADD8 A, D
0x83 Len: 1 Instr: ADD8 A, E
0x84 Len: 1 Instr: ADD8 A, H
0x85 Len: 1 Instr: ADD8 A, L
0x86 Len: 1 Instr: ADD8 A, [HL]
0x87 Len: 1 Instr: ADD8 A, A
0x88 Len: 1 Instr: ADC  A, B
0x89 Len: 1 Instr: ADC  A, C
0x8a Len: 1 Instr: ADC  A, D
0x8b Len: 1 Instr: ADC  A, E
0x8c Len: 1 Instr: ADC  A, H
0x8d Len: 1 Instr: ADC  A, L
0x8e Len: 1 Instr: ADC  A, [HL]
0x8f Len: 1 Instr: ADC  A, A
0x90 Len: 1 Instr: SUB A, B
0x91 Len: 1 Instr: SUB A, C
0x92 Len: 1 Instr: SUB A, D
0x93 Len: 1 Instr: SUB A, E
0x94 Len: 1 Instr: SUB A, H
0x95 Len: 1 Instr: SUB A, L
0x96 Len: 1 Instr: SUB A, [HL]
0x97 Len: 1 Instr: SUB A, A
0x98 Len: 1 Instr: SBC A, B
0x99 Len: 1 Instr: SBC A, C
0x9a Len: 1 Instr: SBC A, D
0x9b Len: 1 Instr: SBC A, E
0x9c Len: 1 Instr: SBC A, H
0x9d Len: 1 Instr: SBC A, L
0x9e Len: 1 Instr: SBC A, [HL]
0x9f Len: 1 Instr: SBC A, A
0xa0 Len: 1 Instr: AND A, B
0xa1 Len: 1 Instr: AND A, C
0xa2 Len: 1 Instr: AND A, D
0xa3 Len: 1 Instr: AND A, E
0xa4 Len: 1 Instr: AND A, H
0xa5 Len: 1 Instr: AND A, L
0xa6 Len: 1 Instr: AND A, [HL]
0xa7 Len: 1 Instr: AND A, A
0xa8 Len: 1 Instr: XOR A, B
0xa9 Len: 1 Instr: XOR A, C
0xaa Len: 1 Instr: XOR A, D
0xab Len: 1 Instr: XOR A, E
0xac Len: 1 Instr: XOR A, H
0xad Len: 1 Instr: XOR A, L
0xae Len: 1 Instr: XOR A, [HL]
0xaf Len: 1 Instr: XOR A, A
0xb0 Len: 1 Instr: OR A, B
0xb1 Len: 1 Instr: OR A, C
0xb2 Len: 1 Instr: OR A, D
0xb3 Len: 1 Instr: OR A, E
0xb4 Len: 1 Instr: OR A, H
0xb5 Len: 1 Instr: OR A, L
0xb6 Len: 1 Instr: OR A, [HL]
0xb7 Len: 1 Instr: OR A, A
0xb8 Len: 1 Instr: CP A, B
0xb9 Len: 1 Instr: CP A, C
0xba Len: 1 Instr: CP A, D
0xbb Len: 1 Instr: CP A, E
0xbc Len: 1 Instr: CP A, H
0xbd Len: 1 Instr: CP A, L
0xbe Len: 1 Instr: CP A, [HL]
0xbf Len: 1 Instr: CP A, A
0xc0 Len: 1 Instr: RET NZ
0xc1 Len: 1 Instr: POP BC
0xc2 Len: 3 Instr: JP NZ, 0x1234
0xc3 Len: 3 Instr: JP None, 0x1234
0xc4 Len: 3 Instr: CALL NZ, 4660
0xc5 Len: 1 Instr: PUSH BC
0xc6 Len: 2 Instr: ADD8 A, 0x34
0xc7 Len: 1 Instr: RST 0x0
0xc8 Len: 1 Instr: RET Z
0xc9 Len: 1 Instr: RET None
0xca Len: 3 Instr: JP Z, 0x1234
0xcc Len: 3 Instr: CALL Z, 4660
0xcd Len: 3 Instr: CALL None, 4660
0xce Len: 2 Instr: ADC  A, 0x34
0xcf Len: 1 Instr: RST 0x8
0xd0 Len: 1 Instr: RET NC
0xd1 Len: 1 Instr: POP DE
0xd2 Len: 3 Instr: JP NC, 0x1234
0xd4 Len: 3 Instr: CALL NC, 4660
0xd5 Len: 1 Instr: PUSH DE
0xd6 Len: 2 Instr: SUB A, 0x34
0xd7 Len: 1 Instr: RST 0x10
0xd8 Len: 1 Instr: RET C
0xd9 Len: 1 Instr: RETI
0xda Len: 3 Instr: JP C, 0x1234
0xdc Len: 3 Instr: CALL C, 4660
0xde Len: 2 Instr: SBC A, 0x34
0xdf Len: 1 Instr: RST 0x18
0xe0 Len: 2 Instr: LD8 [0xFF00 + 0x34], A
0xe1 Len: 1 Instr: POP HL
0xe2 Len: 1 Instr: LD8 [0xFF00 + C], A
0xe5 Len: 1 Instr: PUSH HL
0xe6 Len: 2 Instr: AND A, 0x34
0xe7 Len: 1 Instr: RST 0x20
0xe8 Len: 2 Instr: ADDSP 52
0xe9 Len: 1 Instr: JP None, HL
0xea Len: 3 Instr: LD8 [0x1234], A
0xee Len: 2 Instr: XOR A, 0x34
0xef Len: 1 Instr: RST 0x28
0xf0 Len: 2 Instr: LD8 A, [0xFF00 + 0x34]
0xf1 Len: 1 Instr: POP AF
0xf2 Len: 1 Instr: LD8 A, [0xFF00 + C]
0xf3 Len: 1 Instr: DI
0xf5 Len: 1 Instr: PUSH AF
0xf6 Len: 2 Instr: OR A, 0x34
0xf7 Len: 1 Instr: RST 0x30
0xf8 Len: 2 Instr: LD16 HL, [SP + 52]
0xf9 Len: 1 Instr: LD16 SP, HL
0xfa Len: 3 Instr: LD8 A, [0x1234]
0xfb Len: 1 Instr: EI
0xfe Len: 2 Instr: CP A, 0x34
0xff Len: 1 Instr: RST 0x38
0 Len: 2 Instr: RLC B
0x1 Len: 2 Instr: RLC C
0x2 Len: 2 Instr: RLC D
0x3 Len: 2 Instr: RLC E
0x4 Len: 2 Instr: RLC H
0x5 Len: 2 Instr: RLC L
0x6 Len: 2 Instr: RLC [HL]
0x7 Len: 2 Instr: RLC A
0x8 Len: 2 Instr: RRC B
0x9 Len: 2 Instr: RRC C
0xa Len: 2 Instr: RRC D
0xb Len: 2 Instr: RRC E
0xc Len: 2 Instr: RRC H
0xd Len: 2 Instr: RRC L
0xe Len: 2 Instr: RRC [HL]
0xf Len: 2 Instr: RRC A
0x10 Len: 2 Instr: RL B
0x11 Len: 2 Instr: RL C
0x12 Len: 2 Instr: RL D
0x13 Len: 2 Instr: RL E
0x14 Len: 2 Instr: RL H
0x15 Len: 2 Instr: RL L
0x16 Len: 2 Instr: RL [HL]
0x17 Len: 2 Instr: RL A
0x18 Len: 2 Instr: RR B
0x19 Len: 2 Instr: RR C
0x1a Len: 2 Instr: RR D
0x1b Len: 2 Instr: RR E
0x1c Len: 2 Instr: RR H
0x1d Len: 2 Instr: RR L
0x1e Len: 2 Instr: RR [HL]
0x1f Len: 2 Instr: RR A
0x20 Len: 2 Instr: SLA B
0x21 Len: 2 Instr: SLA C
0x22 Len: 2 Instr: SLA D
0x23 Len: 2 Instr: SLA E
0x24 Len: 2 Instr: SLA H
0x25 Len: 2 Instr: SLA L
0x26 Len: 2 Instr: SLA [HL]
0x27 Len: 2 Instr: SLA A
0x28 Len: 2 Instr: SRA B
0x29 Len: 2 Instr: SRA C
0x2a Len: 2 Instr: SRA D
0x2b Len: 2 Instr: SRA E
0x2c Len: 2 Instr: SRA H
0x2d Len: 2 Instr: SRA L
0x2e Len: 2 Instr: SRA [HL]
0x2f Len: 2 Instr: SRA A
0x30 Len: 2 Instr: SWAP B
0x31 Len: 2 Instr: SWAP C
0x32 Len: 2 Instr: SWAP D
0x33 Len: 2 Instr: SWAP E
0x34 Len: 2 Instr: SWAP H
0x35 Len: 2 Instr: SWAP L
0x36 Len: 2 Instr: SWAP [HL]
0x37 Len: 2 Instr: SWAP A
0x38 Len: 2 Instr: SRL B
0x39 Len: 2 Instr: SRL C
0x3a Len: 2 Instr: SRL D
0x3b Len: 2 Instr: SRL E
0x3c Len: 2 Instr: SRL H
0x3d Len: 2 Instr: SRL L
0x3e Len: 2 Instr: SRL [HL]
0x3f Len: 2 Instr: SRL A
0x40 Len: 2 Instr: BIT 0, B
0x41 Len: 2 Instr: BIT 0, C
0x42 Len: 2 Instr: BIT 0, D
0x43 Len: 2 Instr: BIT 0, E
0x44 Len: 2 Instr: BIT 0, H
0x45 Len: 2 Instr: BIT 0, L
0x46 Len: 2 Instr: BIT 0, [HL]
0x47 Len: 2 Instr: BIT 0, A
0x48 Len: 2 Instr: BIT 1, B
0x49 Len: 2 Instr: BIT 1, C
0x4a Len: 2 Instr: BIT 1, D
0x4b Len: 2 Instr: BIT 1, E
0x4c Len: 2 Instr: BIT 1, H
0x4d Len: 2 Instr: BIT 1, L
0x4e Len: 2 Instr: BIT 1, [HL]
0x4f Len: 2 Instr: BIT 1, A
0x50 Len: 2 Instr: BIT 2, B
0x51 Len: 2 Instr: BIT 2, C
0x52 Len: 2 Instr: BIT 2, D
0x53 Len: 2 Instr: BIT 2, E
0x54 Len: 2 Instr: BIT 2, H
0x55 Len: 2 Instr: BIT 2, L
0x56 Len: 2 Instr: BIT 2, [HL]
0x57 Len: 2 Instr: BIT 2, A
0x58 Len: 2 Instr: BIT 3, B
0x59 Len: 2 Instr: BIT 3, C
0x5a Len: 2 Instr: BIT 3, D
0x5b Len: 2 Instr: BIT 3, E
0x5c Len: 2 Instr: BIT 3, H
0x5d Len: 2 Instr: BIT 3, L
0x5e Len: 2 Instr: BIT 3, [HL]
0x5f Len: 2 Instr: BIT 3, A
0x60 Len: 2 Instr: BIT 4, B
0x61 Len: 2 Instr: BIT 4, C
0x62 Len: 2 Instr: BIT 4, D
0x63 Len: 2 Instr: BIT 4, E
0x64 Len: 2 Instr: BIT 4, H
0x65 Len: 2 Instr: BIT 4, L
0x66 Len: 2 Instr: BIT 4, [HL]
0x67 Len: 2 Instr: BIT 4, A
0x68 Len: 2 Instr: BIT 5, B
0x69 Len: 2 Instr: BIT 5, C
0x6a Len: 2 Instr: BIT 5, D
0x6b Len: 2 Instr: BIT 5, E
0x6c Len: 2 Instr: BIT 5, H
0x6d Len: 2 Instr: BIT 5, L
0x6e Len: 2 Instr: BIT 5, [HL]
0x6f Len: 2 Instr: BIT 5, A
0x70 Len: 2 Instr: BIT 6, B
0x71 Len: 2 Instr: BIT 6, C
0x72 Len: 2 Instr: BIT 6, D
0x73 Len: 2 Instr: BIT 6, E
0x74 Len: 2 Instr: BIT 6, H
0x75 Len: 2 Instr: BIT 6, L
0x76 Len: 2 Instr: BIT 6, [HL]
0x77 Len: 2 Instr: BIT 6, A
0x78 Len: 2 Instr: BIT 7, B
0x79 Len: 2 Instr: BIT 7, C
0x7a Len: 2 Instr: BIT 7, D
0x7b Len: 2 Instr: BIT 7, E
0x7c Len: 2 Instr: BIT 7, H
0x7d Len: 2 Instr: BIT 7, L
0x7e Len: 2 Instr: BIT 7, [HL]
0x7f Len: 2 Instr: BIT 7, A
0x80 Len: 2 Instr: RES 0, B
0x81 Len: 2 Instr: RES 0, C
0x82 Len: 2 Instr: RES 0, D
0x83 Len: 2 Instr: RES 0, E
0x84 Len: 2 Instr: RES 0, H
0x85 Len: 2 Instr: RES 0, L
0x86 Len: 2 Instr: RES 0, [HL]
0x87 Len: 2 Instr: RES 0, A
0x88 Len: 2 Instr: RES 1, B
0x89 Len: 2 Instr: RES 1, C
0x8a Len: 2 Instr: RES 1, D
0x8b Len: 2 Instr: RES 1, E
0x8c Len: 2 Instr: RES 1, H
0x8d Len: 2 Instr: RES 1, L
0x8e Len: 2 Instr: RES 1, [HL]
0x8f Len: 2 Instr: RES 1, A
0x90 Len: 2 Instr: RES 2, B
0x91 Len: 2 Instr: RES 2, C
0x92 Len: 2 Instr: RES 2, D
0x93 Len: 2 Instr: RES 2, E
0x94 Len: 2 Instr: RES 2, H
0x95 Len: 2 Instr: RES 2, L
0x96 Len: 2 Instr: RES 2, [HL]
0x97 Len: 2 Instr: RES 2, A
0x98 Len: 2 Instr: RES 3, B
0x99 Len: 2 Instr: RES 3, C
0x9a Len: 2 Instr: RES 3, D
0x9b Len: 2 Instr: RES 3, E
0x9c Len: 2 Instr: RES 3, H
0x9d Len: 2 Instr: RES 3, L
0x9e Len: 2 Instr: RES 3, [HL]
0x9f Len: 2 Instr: RES 3, A
0xa0 Len: 2 Instr: RES 4, B
0xa1 Len: 2 Instr: RES 4, C
0xa2 Len: 2 Instr: RES 4, D
0xa3 Len: 2 Instr: RES 4, E
0xa4 Len: 2 Instr: RES 4, H
0xa5 Len: 2 Instr: RES 4, L
0xa6 Len: 2 Instr: RES 4, [HL]
0xa7 Len: 2 Instr: RES 4, A
0xa8 Len: 2 Instr: RES 5, B
0xa9 Len: 2 Instr: RES 5, C
0xaa Len: 2 Instr: RES 5, D
0xab Len: 2 Instr: RES 5, E
0xac Len: 2 Instr: RES 5, H
0xad Len: 2 Instr: RES 5, L
0xae Len: 2 Instr: RES 5, [HL]
0xaf Len: 2 Instr: RES 5, A
0xb0 Len: 2 Instr: RES 6, B
0xb1 Len: 2 Instr: RES 6, C
0xb2 Len: 2 Instr: RES 6, D
0xb3 Len: 2 Instr: RES 6, E
0xb4 Len: 2 Instr: RES 6, H
0xb5 Len: 2 Instr: RES 6, L
0xb6 Len: 2 Instr: RES 6, [HL]
0xb7 Len: 2 Instr: RES 6, A
0xb8 Len: 2 Instr: RES 7, B
0xb9 Len: 2 Instr: RES 7, C
0xba Len: 2 Instr: RES 7, D
0xbb Len: 2 Instr: RES 7, E
0xbc Len: 2 Instr: RES 7, H
0xbd Len: 2 Instr: RES 7, L
0xbe Len: 2 Instr: RES 7, [HL]
0xbf Len: 2 Instr: RES 7, A
0xc0 Len: 2 Instr: SET 0, B
0xc1 Len: 2 Instr: SET 0, C
0xc2 Len: 2 Instr: SET 0, D
0xc3 Len: 2 Instr: SET 0, E
0xc4 Len: 2 Instr: SET 0, H
0xc5 Len: 2 Instr: SET 0, L
0xc6 Len: 2 Instr: SET 0, [HL]
0xc7 Len: 2 Instr: SET 0, A
0xc8 Len: 2 Instr: SET 1, B
0xc9 Len: 2 Instr: SET 1, C
0xca Len: 2 Instr: SET 1, D
0xcb Len: 2 Instr: SET 1, E
0xcc Len: 2 Instr: SET 1, H
0xcd Len: 2 Instr: SET 1, L
0xce Len: 2 Instr: SET 1, [HL]
0xcf Len: 2 Instr: SET 1, A
0xd0 Len: 2 Instr: SET 2, B
0xd1 Len: 2 Instr: SET 2, C
0xd2 Len: 2 Instr: SET 2, D
0xd3 Len: 2 Instr: SET 2, E
0xd4 Len: 2 Instr: SET 2, H
0xd5 Len: 2 Instr: SET 2, L
0xd6 Len: 2 Instr: SET 2, [HL]
0xd7 Len: 2 Instr: SET 2, A
0xd8 Len: 2 Instr: SET 3, B
0xd9 Len: 2 Instr: SET 3, C
0xda Len: 2 Instr: SET 3, D
0xdb Len: 2 Instr: SET 3, E
0xdc Len: 2 Instr: SET 3, H
0xdd Len: 2 Instr: SET 3, L
0xde Len: 2 Instr: SET 3, [HL]
0xdf Len: 2 Instr: SET 3, A
0xe0 Len: 2 Instr: SET 4, B
0xe1 Len: 2 Instr: SET 4, C
0xe2 Len: 2 Instr: SET 4, D
0xe3 Len: 2 Instr: SET 4, E
0xe4 Len: 2 Instr: SET 4, H
0xe5 Len: 2 Instr: SET 4, L
0xe6 Len: 2 Instr: SET 4, [HL]
0xe7 Len: 2 Instr: SET 4, A
0xe8 Len: 2 Instr: SET 5, B
0xe9 Len: 2 Instr: SET 5, C
0xea Len: 2 Instr: SET 5, D
0xeb Len: 2 Instr: SET 5, E
0xec Len: 2 Instr: SET 5, H
0xed Len: 2 Instr: SET 5, L
0xee Len: 2 Instr: SET 5, [HL]
0xef Len: 2 Instr: SET 5, A
0xf0 Len: 2 Instr: SET 6, B
0xf1 Len: 2 Instr: SET 6, C
0xf2 Len: 2 Instr: SET 6, D
0xf3 Len: 2 Instr: SET 6, E
0xf4 Len: 2 Instr: SET 6, H
0xf5 Len: 2 Instr: SET 6, L
0xf6 Len: 2 Instr: SET 6, [HL]
0xf7 Len: 2 Instr: SET 6, A
0xf8 Len: 2 Instr: SET 7, B
0xf9 Len: 2 Instr: SET 7, C
0xfa Len: 2 Instr: SET 7, D
0xfb Len: 2 Instr: SET 7, E
0xfc Len: 2 Instr: SET 7, H
0xfd Len: 2 Instr: SET 7, L
0xfe Len: 2 Instr: SET 7, [HL]
0xff Len: 2 Instr: SET 7, A
      |}]
