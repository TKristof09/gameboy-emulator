include Gameboy
open Stdint
module Mem = Mock_memory
module Cpu = Cpu.Make (Mem)

let%expect_test "test cpu execute add8" =
    let mem = Mock_memory.of_list [] in
    let cpu = Cpu.create ~mem in
    Cpu.execute cpu (ADD8 (Reg8 A, Imm8 (Uint8.of_int 42))) Uint16.one;
    Cpu.execute cpu (ADD8 (Reg8 A, Imm8 (Uint8.of_int 27))) Uint16.one;
    Cpu.show cpu |> print_endline;
    [%expect {| SP:0x0 PC:0x2 REG:A: 0x45 B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0x20 H: 0x0 L: 0x0 |}]
(* half carry flag is set *)

let%expect_test "test cpu execute adc" =
    let mem = Mock_memory.of_list [] in
    let cpu = Cpu.create ~mem in
    Cpu.execute cpu (ADD8 (Reg8 A, Imm8 (Uint8.of_int 200))) (Uint16.of_int 2);
    Cpu.execute cpu (ADD8 (Reg8 A, Imm8 (Uint8.of_int 57))) (Uint16.of_int 2);
    Cpu.execute cpu (ADC (Reg8 A, Imm8 (Uint8.of_int 67))) (Uint16.of_int 2);

    Cpu.show cpu |> print_endline;
    [%expect {| SP:0x0 PC:0x6 REG:A: 0x45 B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0x0 H: 0x0 L: 0x0 |}]
(* half carry isn't set here *)

let%expect_test "test cpu step" =
    let mem = Mock_memory.of_list [ 0xC6; 21; 0x87 ] in
    (* ADD8 A 21; ADD8 A A *)
    let cpu = Cpu.create ~mem in
    Cpu.step cpu;
    Cpu.step cpu;
    Cpu.show cpu |> print_endline;
    [%expect {| SP:0x0 PC:0x3 REG:A: 0x2a B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0x0 H: 0x0 L: 0x0 |}]
