include Gameboy
open Uint
module Mem = Mock_memory
module Cpu = Cpu.Make (Mem)

let create_cpu ?(data = []) ?(size = 0) () =
    let mem = Mock_memory.of_list size data in
    (Cpu.create ~bus:mem, mem)

let%expect_test "test cpu execute add8" =
    let cpu, _ = create_cpu () in
    Cpu.execute cpu (ADD8 (Reg8 A, Imm8 (Uint8.of_int 42))) Uint16.one;
    Cpu.execute cpu (ADD8 (Reg8 A, Imm8 (Uint8.of_int 27))) Uint16.one;
    Cpu.show cpu |> print_endline;
    [%expect {| SP:0x0 PC:0x2 REG:A: 0x45 B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0x20 H: 0x0 L: 0x0 |}]
(* half carry flag is set *)

let%expect_test "test cpu execute adc" =
    let cpu, _ = create_cpu () in
    Cpu.execute cpu (ADD8 (Reg8 A, Imm8 (Uint8.of_int 200))) (Uint16.of_int 2);
    Cpu.execute cpu (ADD8 (Reg8 A, Imm8 (Uint8.of_int 57))) (Uint16.of_int 2);
    Cpu.execute cpu (ADC (Reg8 A, Imm8 (Uint8.of_int 67))) (Uint16.of_int 2);

    Cpu.show cpu |> print_endline;
    [%expect {| SP:0x0 PC:0x6 REG:A: 0x45 B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0x0 H: 0x0 L: 0x0 |}]
(* half carry isn't set here *)

let%expect_test "test cpu step" =
    (* ADD8 A 21; ADD8 A A *)
    let cpu, _ = create_cpu ~data:[ 0xC6; 21; 0x87 ] ~size:3 () in
    Cpu.step cpu;
    Cpu.step cpu;
    Cpu.show cpu |> print_endline;
    [%expect {| SP:0x0 PC:0x3 REG:A: 0x2a B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0x0 H: 0x0 L: 0x0 |}]

let%expect_test "LD8 A 2" =
    let cpu, _ = create_cpu ~data:[ 0x3E; 0x02 ] ~size:0xFFFF () in
    Cpu.step cpu;
    Cpu.show cpu |> print_endline;
    [%expect {| SP:0x0 PC:0x2 REG:A: 0x2 B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0x0 H: 0x0 L: 0x0 |}]

let%expect_test "LD16 HL 0x0102" =
    let cpu, _ = create_cpu ~data:[ 0x21; 0x02; 0x01 ] ~size:0xFFFF () in
    Cpu.step cpu;
    Cpu.show cpu |> print_endline;
    [%expect {| SP:0x0 PC:0x3 REG:A: 0x0 B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0x0 H: 0x1 L: 0x2 |}]

let%expect_test "LD8 A B" =
    (* LD B 2; LD A B *)
    let cpu, _ = create_cpu ~data:[ 0x06; 0x02; 0x78 ] ~size:0xFFFF () in
    Cpu.step cpu;
    Cpu.step cpu;
    Cpu.show cpu |> print_endline;
    [%expect {| SP:0x0 PC:0x3 REG:A: 0x2 B: 0x2 C: 0x0 D: 0x0 E: 0x0 F: 0x0 H: 0x0 L: 0x0 |}]

let%expect_test "LD8 A [HL]" =
    (* LD HL 0x10; LD A [HL] *)
    let cpu, mem = create_cpu ~data:[ 0x21; 0x10; 0x00; 0x7E ] ~size:0xFFFF () in
    Mem.write_byte mem ~addr:(Uint16.of_int 0x10) ~data:Uint8.one;
    Cpu.step cpu;
    Cpu.step cpu;
    Cpu.show cpu |> print_endline;
    [%expect {| SP:0x0 PC:0x4 REG:A: 0x1 B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0x0 H: 0x0 L: 0x10 |}]

let%expect_test "LD8 [HL] 2" =
    (* LD HL 0x10; LD [HL] 2 *)
    let cpu, mem = create_cpu ~data:[ 0x21; 0x10; 0x00; 0x36; 0x02 ] ~size:0xFFFF () in
    Cpu.step cpu;
    Cpu.step cpu;
    let res = Mem.read_byte mem (Uint16.of_int 0x10) in
    Printf.printf "%s\n" (Uint8.to_string_hex res);
    [%expect {| 0x2 |}]

let%expect_test "CALL 0x0042" =
    (* LD SP 0x0010; CALL None 0x0042 *)
    let cpu, _ = create_cpu ~data:[ 0x31; 0x10; 0x00; 0xCD; 0x42; 0x00 ] ~size:0xFFFF () in
    Cpu.step cpu;
    Cpu.step cpu;
    Cpu.show cpu |> print_endline;
    [%expect {| SP:0xe PC:0x42 REG:A: 0x0 B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0x0 H: 0x0 L: 0x0 |}]

let%expect_test "RET 0x0042" =
    (* LD SP 0x0010; CALL None 0x0042; RET  None*)
    let cpu, mem = create_cpu ~data:[ 0x31; 0x10; 0x00; 0xCD; 0x42; 0x00 ] ~size:0xFFFF () in
    Mem.write_byte mem ~addr:(Uint16.of_int 0x0042) ~data:(Uint8.of_int 0xC9);

    Cpu.step cpu;
    Cpu.step cpu;
    Cpu.step cpu;
    Cpu.show cpu |> print_endline;
    [%expect {| SP:0x10 PC:0x6 REG:A: 0x0 B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0x0 H: 0x0 L: 0x0 |}]

let%expect_test "JR None" =
    (* JR None 0x42 *)
    let cpu, _ = create_cpu ~data:[ 0x18; 0x42 ] ~size:0xFFFF () in

    Cpu.step cpu;
    Cpu.show cpu |> print_endline;
    [%expect {| SP:0x0 PC:0x44 REG:A: 0x0 B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0x0 H: 0x0 L: 0x0 |}]

let%expect_test "JR NZ" =
    (* JR NZ 0x42 *)
    let cpu, _ = create_cpu ~data:[ 0x20; 0x42 ] ~size:0xFFFF () in

    Cpu.step cpu;
    Cpu.show cpu |> print_endline;
    [%expect {| SP:0x0 PC:0x44 REG:A: 0x0 B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0x0 H: 0x0 L: 0x0 |}]

let%expect_test "JR Z" =
    (* ADD8 A A; JR Z 0x42 *)
    let cpu, _ = create_cpu ~data:[ 0x87; 0x28; 0x42 ] ~size:0xFFFF () in

    Cpu.step cpu;
    Cpu.step cpu;
    Cpu.show cpu |> print_endline;
    [%expect {| SP:0x0 PC:0x45 REG:A: 0x0 B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0x80 H: 0x0 L: 0x0 |}]

let%expect_test "JR NC" =
    (*  JR NC 0x42 *)
    let cpu, _ = create_cpu ~data:[ 0x30; 0x42 ] ~size:0xFFFF () in

    Cpu.step cpu;
    Cpu.show cpu |> print_endline;
    [%expect {| SP:0x0 PC:0x44 REG:A: 0x0 B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0x0 H: 0x0 L: 0x0 |}]

let%expect_test "JR C" =
    (* ADD8 A 255; ADD8 A 1; JR C 0x42 *)
    let cpu, _ = create_cpu ~data:[ 0xC6; 0xFF; 0xC6; 0x01; 0x28; 0x42 ] ~size:0xFFFF () in

    Cpu.step cpu;
    Cpu.step cpu;
    Cpu.step cpu;
    Cpu.show cpu |> print_endline;
    [%expect {| SP:0x0 PC:0x48 REG:A: 0x0 B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0xb0 H: 0x0 L: 0x0 |}]

let%expect_test "INC8" =
    (*  INC8 A *)
    let cpu, _ = create_cpu ~data:[ 0x3C ] ~size:0xFFFF () in

    Cpu.step cpu;
    Cpu.show cpu |> print_endline;
    [%expect {| SP:0x0 PC:0x1 REG:A: 0x1 B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0x0 H: 0x0 L: 0x0 |}]

let%expect_test "INC8 half carry" =
    (*  ADD8 A 0x0F; INC8 A *)
    let cpu, _ = create_cpu ~data:[ 0xC6; 0x0F; 0x3C ] ~size:0xFFFF () in

    Cpu.step cpu;
    Cpu.step cpu;
    Cpu.show cpu |> print_endline;
    [%expect {| SP:0x0 PC:0x3 REG:A: 0x10 B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0x20 H: 0x0 L: 0x0 |}]

let%expect_test "DEC8 " =
    (*  ADD8 A 0x02; DEC8 A *)
    let cpu, _ = create_cpu ~data:[ 0xC6; 0x02; 0x3D ] ~size:0xFFFF () in

    Cpu.step cpu;
    Cpu.step cpu;
    Cpu.show cpu |> print_endline;
    [%expect {| SP:0x0 PC:0x3 REG:A: 0x1 B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0x40 H: 0x0 L: 0x0 |}]

let%expect_test "DEC8 half carry" =
    (*  DEC8 A *)
    let cpu, _ = create_cpu ~data:[ 0x3D ] ~size:0xFFFF () in

    Cpu.step cpu;
    Cpu.show cpu |> print_endline;
    [%expect {| SP:0x0 PC:0x1 REG:A: 0xff B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0x60 H: 0x0 L: 0x0 |}]

let%expect_test "SUB no carry" =
    (*  ADD8 A 0x02; SUB A 1 *)
    let cpu, _ = create_cpu ~data:[ 0xC6; 0x02; 0xD6; 0x01 ] ~size:0xFFFF () in

    Cpu.step cpu;
    Cpu.step cpu;
    Cpu.show cpu |> print_endline;
    [%expect {| SP:0x0 PC:0x4 REG:A: 0x1 B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0x40 H: 0x0 L: 0x0 |}]

let%expect_test "SUB carry" =
    (*  ADD8 A 0x10; SUB A 0x20 *)
    let cpu, _ = create_cpu ~data:[ 0xC6; 0x10; 0xD6; 0x20 ] ~size:0xFFFF () in

    Cpu.step cpu;
    Cpu.step cpu;
    Cpu.show cpu |> print_endline;
    [%expect {| SP:0x0 PC:0x4 REG:A: 0xf0 B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0x50 H: 0x0 L: 0x0 |}]

let%expect_test "SUB zero" =
    (*  ADD8 A 0x10; SUB A 0x20 *)
    let cpu, _ = create_cpu ~data:[ 0xC6; 0x10; 0x97 ] ~size:0xFFFF () in

    Cpu.step cpu;
    Cpu.step cpu;
    Cpu.show cpu |> print_endline;
    [%expect {| SP:0x0 PC:0x3 REG:A: 0x0 B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0xc0 H: 0x0 L: 0x0 |}]

let%expect_test "SUB half carry" =
    (*  ADD8 A 0x10; SUB A 0x01 *)
    let cpu, _ = create_cpu ~data:[ 0xC6; 0x10; 0xD6; 0x01 ] ~size:0xFFFF () in

    Cpu.step cpu;
    Cpu.step cpu;
    Cpu.show cpu |> print_endline;
    [%expect {| SP:0x0 PC:0x4 REG:A: 0xf B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0x60 H: 0x0 L: 0x0 |}]

let%expect_test "BIT zero" =
    (* LD8 A 4; BIT A 1 *)
    let cpu, _ = create_cpu ~data:[ 0x3E; 0x04; 0xCB; 0x4F ] ~size:0xFFFF () in

    Cpu.step cpu;
    Cpu.step cpu;
    Cpu.show cpu |> print_endline;
    [%expect {| SP:0x0 PC:0x4 REG:A: 0x4 B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0xa0 H: 0x0 L: 0x0 |}]

let%expect_test "BIT non zero" =
    (* LD8 A 3; BIT A 1 *)
    let cpu, _ = create_cpu ~data:[ 0x3E; 0x03; 0xCB; 0x4F ] ~size:0xFFFF () in

    Cpu.step cpu;
    Cpu.step cpu;
    Cpu.show cpu |> print_endline;
    [%expect {| SP:0x0 PC:0x4 REG:A: 0x3 B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0x20 H: 0x0 L: 0x0 |}]

let%expect_test "PUSH 0x102" =
    (* LD SP 0x0010; LD16 BC 0x0102; PUSH BC *)
    let cpu, mem = create_cpu ~data:[ 0x31; 0x10; 0x00; 0x01; 0x02; 0x01; 0xC5 ] ~size:0xFFFF () in
    Cpu.step cpu;
    Cpu.step cpu;
    Cpu.step cpu;
    Cpu.show cpu |> print_endline;
    let res = Mem.read_word mem (Uint16.of_int 0x0e) in
    Printf.printf "%s\n" (Uint16.to_string_hex res);
    [%expect
        {|
      SP:0xe PC:0x7 REG:A: 0x0 B: 0x1 C: 0x2 D: 0x0 E: 0x0 F: 0x0 H: 0x0 L: 0x0
      0x102
      |}]

let%expect_test "POP 0x102" =
    (* LD SP 0x0010; LD16 BC 0x0102; PUSH BC; POP DE *)
    let cpu, _ =
        create_cpu ~data:[ 0x31; 0x10; 0x00; 0x01; 0x02; 0x01; 0xC5; 0xD1 ] ~size:0xFFFF ()
    in
    Cpu.step cpu;
    Cpu.step cpu;
    Cpu.step cpu;
    Cpu.step cpu;
    Cpu.show cpu |> print_endline;
    [%expect {| SP:0x10 PC:0x8 REG:A: 0x0 B: 0x1 C: 0x2 D: 0x1 E: 0x2 F: 0x0 H: 0x0 L: 0x0 |}]

let%expect_test "RL no carry" =
    (* LD8 A 4; RL A *)
    let cpu, _ = create_cpu ~data:[ 0x3E; 0x04; 0xCB; 0x17 ] ~size:0xFFFF () in

    Cpu.step cpu;
    Cpu.step cpu;
    Cpu.show cpu |> print_endline;
    [%expect {| SP:0x0 PC:0x4 REG:A: 0x8 B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0x0 H: 0x0 L: 0x0 |}]

let%expect_test "RL carry" =
    (* LD A 255; ADD A 1; LD8 B 0x84; RL B *)
    let cpu, _ =
        create_cpu ~data:[ 0x3E; 0xFF; 0xC6; 0x01; 0x06; 0x84; 0xCB; 0x10 ] ~size:0xFFFF ()
    in

    Cpu.step cpu;
    Cpu.step cpu;
    Cpu.step cpu;
    Cpu.step cpu;
    Cpu.show cpu |> print_endline;
    [%expect {| SP:0x0 PC:0x8 REG:A: 0x0 B: 0x9 C: 0x0 D: 0x0 E: 0x0 F: 0x10 H: 0x0 L: 0x0 |}]

let%expect_test "RLA no carry" =
    (* LD8 A 4; RLA *)
    let cpu, _ = create_cpu ~data:[ 0x3E; 0x04; 0x17 ] ~size:0xFFFF () in

    Cpu.step cpu;
    Cpu.step cpu;
    Cpu.show cpu |> print_endline;
    [%expect {| SP:0x0 PC:0x3 REG:A: 0x8 B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0x0 H: 0x0 L: 0x0 |}]

let%expect_test "RLA carry" =
    (* LD A 0xFF; ADD A 1; LD8 A 0x84; RLA *)
    let cpu, _ = create_cpu ~data:[ 0x3E; 0xFF; 0xC6; 0x02; 0x3E; 0x84; 0x17 ] ~size:0xFFFF () in

    Cpu.step cpu;
    Cpu.step cpu;
    Cpu.step cpu;
    Cpu.step cpu;
    Cpu.show cpu |> print_endline;
    [%expect {| SP:0x0 PC:0x7 REG:A: 0x9 B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0x10 H: 0x0 L: 0x0 |}]

let%expect_test "CP equal" =
    (*  LD A 2; LD B 2; CP A B *)
    let cpu, _ = create_cpu ~data:[ 0x3E; 0x02; 0x06; 0x02; 0xB8 ] ~size:0xFFFF () in

    Cpu.step cpu;
    Cpu.step cpu;
    Cpu.step cpu;
    Cpu.show cpu |> print_endline;
    [%expect {| SP:0x0 PC:0x5 REG:A: 0x2 B: 0x2 C: 0x0 D: 0x0 E: 0x0 F: 0xc0 H: 0x0 L: 0x0 |}]

let%expect_test "CP not equal" =
    (*  LD A 2; LD B 3; CP A B *)
    let cpu, _ = create_cpu ~data:[ 0x3E; 0x02; 0x06; 0x03; 0xB8 ] ~size:0xFFFF () in

    Cpu.step cpu;
    Cpu.step cpu;
    Cpu.step cpu;
    Cpu.show cpu |> print_endline;
    [%expect {| SP:0x0 PC:0x5 REG:A: 0x2 B: 0x3 C: 0x0 D: 0x0 E: 0x0 F: 0x70 H: 0x0 L: 0x0 |}]
