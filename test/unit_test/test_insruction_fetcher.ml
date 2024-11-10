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
