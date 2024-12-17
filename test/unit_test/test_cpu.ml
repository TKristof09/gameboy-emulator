include Gameboy
open Uint
module Mem = Mock_memory
module Cpu = Cpu.Make (Mem)

let create_cpu ?(data = []) ?(size = 0) () =
    let mem = Mock_memory.of_list size data in
    let interrupt_manager = Interrupt_manager.create () in
    (Cpu.create ~bus:mem ~interrupt_manager, mem, interrupt_manager)

let%expect_test "test cpu execute add8" =
    let cpu, _, _ = create_cpu () in
    let _ = Cpu.execute cpu (ADD8 (Reg8 A, Imm8 (Uint8.of_int 42))) Uint16.one 0 0 in
    let _ = Cpu.execute cpu (ADD8 (Reg8 A, Imm8 (Uint8.of_int 27))) Uint16.one 0 0 in
    Cpu.show cpu |> print_endline;
    [%expect
        {| HALT:false SP:0x0 PC:0x2 REG:A: 0x45 B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0x20 H: 0x0 L: 0x0 |}]
(* half carry flag is set *)

let%expect_test "test cpu execute adc" =
    let cpu, _, _ = create_cpu () in
    let _ = Cpu.execute cpu (ADD8 (Reg8 A, Imm8 (Uint8.of_int 200))) (Uint16.of_int 2) 0 0 in
    let _ = Cpu.execute cpu (ADD8 (Reg8 A, Imm8 (Uint8.of_int 57))) (Uint16.of_int 2) 0 0 in
    let _ = Cpu.execute cpu (ADC (Reg8 A, Imm8 (Uint8.of_int 67))) (Uint16.of_int 2) 0 0 in

    Cpu.show cpu |> print_endline;
    [%expect
        {| HALT:false SP:0x0 PC:0x6 REG:A: 0x45 B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0x0 H: 0x0 L: 0x0 |}]
(* half carry isn't set here *)

let%expect_test "test cpu step" =
    (* ADD8 A 21; ADD8 A A *)
    let cpu, _, _ = create_cpu ~data:[ 0xC6; 21; 0x87 ] ~size:3 () in
    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    Cpu.show cpu |> print_endline;
    [%expect
        {| HALT:false SP:0x0 PC:0x3 REG:A: 0x2a B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0x0 H: 0x0 L: 0x0 |}]

let%expect_test "LD8 [HL+] A" =
    (* LD A 2; LD [HL+] A *)
    let cpu, mem, _ = create_cpu ~data:[ 0x3E; 0x02; 0x22 ] ~size:0xFFFF () in
    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    Cpu.show cpu |> print_endline;
    print_endline (Uint8.to_string_hex (Mem.read_byte mem Uint16.zero));
    [%expect
        {|
      HALT:false SP:0x0 PC:0x3 REG:A: 0x2 B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0x0 H: 0x0 L: 0x1
      0x2
      |}]

let%expect_test "LD8 [HL-] A" =
    (* LD A 2; LD [HL-] A *)
    let cpu, mem, _ = create_cpu ~data:[ 0x3E; 0x02; 0x32 ] ~size:0xFFFF () in
    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    Cpu.show cpu |> print_endline;
    print_endline (Uint8.to_string_hex (Mem.read_byte mem Uint16.zero));
    [%expect
        {|
      HALT:false SP:0x0 PC:0x3 REG:A: 0x2 B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0x0 H: 0xff L: 0xff
      0x2
      |}]

let%expect_test "LD8 A 2" =
    let cpu, _, _ = create_cpu ~data:[ 0x3E; 0x02 ] ~size:0xFFFF () in
    let _ = Cpu.step cpu in
    Cpu.show cpu |> print_endline;
    [%expect
        {| HALT:false SP:0x0 PC:0x2 REG:A: 0x2 B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0x0 H: 0x0 L: 0x0 |}]

let%expect_test "LD16 HL 0x0102" =
    let cpu, _, _ = create_cpu ~data:[ 0x21; 0x02; 0x01 ] ~size:0xFFFF () in
    let _ = Cpu.step cpu in
    Cpu.show cpu |> print_endline;
    [%expect
        {| HALT:false SP:0x0 PC:0x3 REG:A: 0x0 B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0x0 H: 0x1 L: 0x2 |}]

let%expect_test "LD16 HL SP+0x01 no flags" =
    (* LD SP 0x0010; LD HL SP+0x01 *)
    let cpu, _, _ = create_cpu ~data:[ 0x31; 0x10; 0x00; 0xF8; 0x01 ] ~size:0xFFFF () in
    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    Cpu.show cpu |> print_endline;
    [%expect
        {| HALT:false SP:0x10 PC:0x5 REG:A: 0x0 B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0x0 H: 0x0 L: 0x11 |}]

let%expect_test "LD16 HL SP+0x01 carry+half carry" =
    (* LD SP 0x00FF; LD HL SP+0x01 *)
    let cpu, _, _ = create_cpu ~data:[ 0x31; 0xFF; 0x00; 0xF8; 0x01 ] ~size:0xFFFF () in
    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    Cpu.show cpu |> print_endline;
    [%expect
        {| HALT:false SP:0xff PC:0x5 REG:A: 0x0 B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0x30 H: 0x1 L: 0x0 |}]

let%expect_test "LD16 HL SP-0x01 carry+half carry" =
    (* this produces carry and half carry because for this instruction the flags are calculated as if it was an unsigned addition not a subtraction *)
    (* LD SP 0x00FF; LD HL SP-0x01 *)
    let cpu, _, _ = create_cpu ~data:[ 0x31; 0xFF; 0x00; 0xF8; -1 ] ~size:0xFFFF () in
    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    Cpu.show cpu |> print_endline;
    [%expect
        {| HALT:false SP:0xff PC:0x5 REG:A: 0x0 B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0x30 H: 0x0 L: 0xfe |}]

let%expect_test "LD16 HL SP-0x01 no flags" =
    (* LD SP 0x0100; LD HL SP-0x01 *)
    let cpu, _, _ = create_cpu ~data:[ 0x31; 0x00; 0x01; 0xF8; -1 ] ~size:0xFFFF () in
    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    Cpu.show cpu |> print_endline;
    [%expect
        {| HALT:false SP:0x100 PC:0x5 REG:A: 0x0 B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0x0 H: 0x0 L: 0xff |}]

let%expect_test "LD8 A B" =
    (* LD B 2; LD A B *)
    let cpu, _, _ = create_cpu ~data:[ 0x06; 0x02; 0x78 ] ~size:0xFFFF () in
    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    Cpu.show cpu |> print_endline;
    [%expect
        {| HALT:false SP:0x0 PC:0x3 REG:A: 0x2 B: 0x2 C: 0x0 D: 0x0 E: 0x0 F: 0x0 H: 0x0 L: 0x0 |}]

let%expect_test "LD8 A [HL]" =
    (* LD HL 0x10; LD A [HL] *)
    let cpu, mem, _ = create_cpu ~data:[ 0x21; 0x10; 0x00; 0x7E ] ~size:0xFFFF () in
    Mem.write_byte mem ~addr:(Uint16.of_int 0x10) ~data:Uint8.one;
    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    Cpu.show cpu |> print_endline;
    [%expect
        {| HALT:false SP:0x0 PC:0x4 REG:A: 0x1 B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0x0 H: 0x0 L: 0x10 |}]

let%expect_test "LD8 [HL] 2" =
    (* LD HL 0x10; LD [HL] 2 *)
    let cpu, mem, _ = create_cpu ~data:[ 0x21; 0x10; 0x00; 0x36; 0x02 ] ~size:0xFFFF () in
    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    let res = Mem.read_byte mem (Uint16.of_int 0x10) in
    Printf.printf "%s\n" (Uint8.to_string_hex res);
    [%expect {| 0x2 |}]

let%expect_test "CALL 0x0042" =
    (* LD SP 0x0010; CALL None 0x0042 *)
    let cpu, _, _ = create_cpu ~data:[ 0x31; 0x10; 0x00; 0xCD; 0x42; 0x00 ] ~size:0xFFFF () in
    let _ = Cpu.step cpu in
    let c, _, _ = Cpu.step cpu in
    Cpu.show cpu |> print_endline;
    Printf.printf "%d\n" c;
    [%expect
        {|
      HALT:false SP:0xe PC:0x42 REG:A: 0x0 B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0x0 H: 0x0 L: 0x0
      6
      |}]

let%expect_test "RET 0x0042" =
    (* LD SP 0x0010; CALL None 0x0042; RET  None*)
    let cpu, mem, _ = create_cpu ~data:[ 0x31; 0x10; 0x00; 0xCD; 0x42; 0x00 ] ~size:0xFFFF () in
    Mem.write_byte mem ~addr:(Uint16.of_int 0x0042) ~data:(Uint8.of_int 0xC9);

    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    Cpu.show cpu |> print_endline;
    [%expect
        {| HALT:false SP:0x10 PC:0x6 REG:A: 0x0 B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0x0 H: 0x0 L: 0x0 |}]

let%expect_test "JR None" =
    (* JR None 0x42 *)
    let cpu, _, _ = create_cpu ~data:[ 0x18; 0x42 ] ~size:0xFFFF () in

    let c, _, _ = Cpu.step cpu in
    Cpu.show cpu |> print_endline;
    Printf.printf "%d\n" c;
    [%expect
        {|
      HALT:false SP:0x0 PC:0x44 REG:A: 0x0 B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0x0 H: 0x0 L: 0x0
      3
      |}]

let%expect_test "JR NZ" =
    (* JR NZ 0x42 *)
    let cpu, _, _ = create_cpu ~data:[ 0x20; 0x42 ] ~size:0xFFFF () in

    let c, _, _ = Cpu.step cpu in
    Cpu.show cpu |> print_endline;
    Printf.printf "%d\n" c;
    [%expect
        {|
      HALT:false SP:0x0 PC:0x44 REG:A: 0x0 B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0x0 H: 0x0 L: 0x0
      3
      |}]

let%expect_test "JR Z" =
    (* ADD8 A A; JR Z 0x42 *)
    let cpu, _, _ = create_cpu ~data:[ 0x87; 0x28; 0x42 ] ~size:0xFFFF () in

    let _ = Cpu.step cpu in
    let c, _, _ = Cpu.step cpu in
    Cpu.show cpu |> print_endline;
    Printf.printf "%d\n" c;
    [%expect
        {|
      HALT:false SP:0x0 PC:0x45 REG:A: 0x0 B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0x80 H: 0x0 L: 0x0
      3
      |}]

let%expect_test "JR NC" =
    (*  JR NC 0x42 *)
    let cpu, _, _ = create_cpu ~data:[ 0x30; 0x42 ] ~size:0xFFFF () in

    let c, _, _ = Cpu.step cpu in
    Cpu.show cpu |> print_endline;
    Printf.printf "%d\n" c;
    [%expect
        {|
      HALT:false SP:0x0 PC:0x44 REG:A: 0x0 B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0x0 H: 0x0 L: 0x0
      3
      |}]

let%expect_test "JR C" =
    (* ADD8 A 255; ADD8 A 1; JR C 0x42 *)
    let cpu, _, _ = create_cpu ~data:[ 0xC6; 0xFF; 0xC6; 0x01; 0x28; 0x42 ] ~size:0xFFFF () in

    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    let c, _, _ = Cpu.step cpu in
    Cpu.show cpu |> print_endline;
    Printf.printf "%d\n" c;
    [%expect
        {|
      HALT:false SP:0x0 PC:0x48 REG:A: 0x0 B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0xb0 H: 0x0 L: 0x0
      3
      |}]

let%expect_test "JP None" =
    (* ADD8 A 255; ADD8 A 1; JP None 0x42 *)
    let cpu, _, _ = create_cpu ~data:[ 0xC6; 0xFF; 0xC6; 0x01; 0xC3; 0x42 ] ~size:0xFFFF () in

    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    let c, _, _ = Cpu.step cpu in
    Cpu.show cpu |> print_endline;
    Printf.printf "%d\n" c;
    [%expect
        {|
      HALT:false SP:0x0 PC:0x42 REG:A: 0x0 B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0xb0 H: 0x0 L: 0x0
      4
      |}]

let%expect_test "INC8" =
    (*  INC8 A *)
    let cpu, _, _ = create_cpu ~data:[ 0x3C ] ~size:0xFFFF () in

    let _ = Cpu.step cpu in
    Cpu.show cpu |> print_endline;
    [%expect
        {| HALT:false SP:0x0 PC:0x1 REG:A: 0x1 B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0x0 H: 0x0 L: 0x0 |}]

let%expect_test "INC8 half carry" =
    (*  ADD8 A 0x0F; INC8 A *)
    let cpu, _, _ = create_cpu ~data:[ 0xC6; 0x0F; 0x3C ] ~size:0xFFFF () in

    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    Cpu.show cpu |> print_endline;
    [%expect
        {| HALT:false SP:0x0 PC:0x3 REG:A: 0x10 B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0x20 H: 0x0 L: 0x0 |}]

let%expect_test "DEC8 " =
    (*  ADD8 A 0x02; DEC8 A *)
    let cpu, _, _ = create_cpu ~data:[ 0xC6; 0x02; 0x3D ] ~size:0xFFFF () in

    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    Cpu.show cpu |> print_endline;
    [%expect
        {| HALT:false SP:0x0 PC:0x3 REG:A: 0x1 B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0x40 H: 0x0 L: 0x0 |}]

let%expect_test "DEC8 half carry" =
    (*  DEC8 A *)
    let cpu, _, _ = create_cpu ~data:[ 0x3D ] ~size:0xFFFF () in

    let _ = Cpu.step cpu in
    Cpu.show cpu |> print_endline;
    [%expect
        {| HALT:false SP:0x0 PC:0x1 REG:A: 0xff B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0x60 H: 0x0 L: 0x0 |}]

let%expect_test "SUB no carry" =
    (*  ADD8 A 0x02; SUB A 1 *)
    let cpu, _, _ = create_cpu ~data:[ 0xC6; 0x02; 0xD6; 0x01 ] ~size:0xFFFF () in

    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    Cpu.show cpu |> print_endline;
    [%expect
        {| HALT:false SP:0x0 PC:0x4 REG:A: 0x1 B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0x40 H: 0x0 L: 0x0 |}]

let%expect_test "SUB carry" =
    (*  ADD8 A 0x10; SUB A 0x20 *)
    let cpu, _, _ = create_cpu ~data:[ 0xC6; 0x10; 0xD6; 0x20 ] ~size:0xFFFF () in

    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    Cpu.show cpu |> print_endline;
    [%expect
        {| HALT:false SP:0x0 PC:0x4 REG:A: 0xf0 B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0x50 H: 0x0 L: 0x0 |}]

let%expect_test "SUB zero" =
    (*  ADD8 A 0x10; SUB A 0x20 *)
    let cpu, _, _ = create_cpu ~data:[ 0xC6; 0x10; 0x97 ] ~size:0xFFFF () in

    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    Cpu.show cpu |> print_endline;
    [%expect
        {| HALT:false SP:0x0 PC:0x3 REG:A: 0x0 B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0xc0 H: 0x0 L: 0x0 |}]

let%expect_test "SUB half carry" =
    (*  ADD8 A 0x10; SUB A 0x01 *)
    let cpu, _, _ = create_cpu ~data:[ 0xC6; 0x10; 0xD6; 0x01 ] ~size:0xFFFF () in

    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    Cpu.show cpu |> print_endline;
    [%expect
        {| HALT:false SP:0x0 PC:0x4 REG:A: 0xf B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0x60 H: 0x0 L: 0x0 |}]

let%expect_test "SBC carry+half carry" =
    (*  ADD8 A 0x02; ADD8 A 0xFF; SUB A 1 *)
    let cpu, _, _ = create_cpu ~data:[ 0xC6; 0x02; 0xC6; 0xFF; 0xDE; 0x01 ] ~size:0xFFFF () in

    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    Cpu.show cpu |> print_endline;
    [%expect
        {| HALT:false SP:0x0 PC:0x6 REG:A: 0xff B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0x70 H: 0x0 L: 0x0 |}]

let%expect_test "BIT zero" =
    (* LD8 A 4; BIT A 1 *)
    let cpu, _, _ = create_cpu ~data:[ 0x3E; 0x04; 0xCB; 0x4F ] ~size:0xFFFF () in

    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    Cpu.show cpu |> print_endline;
    [%expect
        {| HALT:false SP:0x0 PC:0x4 REG:A: 0x4 B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0xa0 H: 0x0 L: 0x0 |}]

let%expect_test "BIT non zero" =
    (* LD8 A 3; BIT A 1 *)
    let cpu, _, _ = create_cpu ~data:[ 0x3E; 0x03; 0xCB; 0x4F ] ~size:0xFFFF () in

    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    Cpu.show cpu |> print_endline;
    [%expect
        {| HALT:false SP:0x0 PC:0x4 REG:A: 0x3 B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0x20 H: 0x0 L: 0x0 |}]

let%expect_test "PUSH 0x102" =
    (* LD SP 0x0010; LD16 BC 0x0102; PUSH BC *)
    let cpu, mem, _ =
        create_cpu ~data:[ 0x31; 0x10; 0x00; 0x01; 0x02; 0x01; 0xC5 ] ~size:0xFFFF ()
    in
    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    Cpu.show cpu |> print_endline;
    let res = Mem.read_word mem (Uint16.of_int 0x0e) in
    Printf.printf "%s\n" (Uint16.to_string_hex res);
    [%expect
        {|
      HALT:false SP:0xe PC:0x7 REG:A: 0x0 B: 0x1 C: 0x2 D: 0x0 E: 0x0 F: 0x0 H: 0x0 L: 0x0
      0x102
      |}]

let%expect_test "POP 0x102" =
    (* LD SP 0x0010; LD16 BC 0x0102; PUSH BC; POP DE *)
    let cpu, _, _ =
        create_cpu ~data:[ 0x31; 0x10; 0x00; 0x01; 0x02; 0x01; 0xC5; 0xD1 ] ~size:0xFFFF ()
    in
    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    Cpu.show cpu |> print_endline;
    [%expect
        {| HALT:false SP:0x10 PC:0x8 REG:A: 0x0 B: 0x1 C: 0x2 D: 0x1 E: 0x2 F: 0x0 H: 0x0 L: 0x0 |}]

let%expect_test "CP equal" =
    (*  LD A 2; LD B 2; CP A B *)
    let cpu, _, _ = create_cpu ~data:[ 0x3E; 0x02; 0x06; 0x02; 0xB8 ] ~size:0xFFFF () in

    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    Cpu.show cpu |> print_endline;
    [%expect
        {| HALT:false SP:0x0 PC:0x5 REG:A: 0x2 B: 0x2 C: 0x0 D: 0x0 E: 0x0 F: 0xc0 H: 0x0 L: 0x0 |}]

let%expect_test "CP not equal" =
    (*  LD A 2; LD B 3; CP A B *)
    let cpu, _, _ = create_cpu ~data:[ 0x3E; 0x02; 0x06; 0x03; 0xB8 ] ~size:0xFFFF () in

    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    Cpu.show cpu |> print_endline;
    [%expect
        {| HALT:false SP:0x0 PC:0x5 REG:A: 0x2 B: 0x3 C: 0x0 D: 0x0 E: 0x0 F: 0x70 H: 0x0 L: 0x0 |}]

let%expect_test "AND" =
    (*  LD A 5; AND A 3 *)
    let cpu, _, _ = create_cpu ~data:[ 0x3E; 0x05; 0xE6; 0x03 ] ~size:0xFFFF () in
    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    Cpu.show cpu |> print_endline;
    [%expect
        {| HALT:false SP:0x0 PC:0x4 REG:A: 0x1 B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0x20 H: 0x0 L: 0x0 |}]

let%expect_test "OR" =
    (*  LD A 5; OR A 3 *)
    let cpu, _, _ = create_cpu ~data:[ 0x3E; 0x05; 0xF6; 0x03 ] ~size:0xFFFF () in
    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    Cpu.show cpu |> print_endline;
    [%expect
        {| HALT:false SP:0x0 PC:0x4 REG:A: 0x7 B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0x0 H: 0x0 L: 0x0 |}]

let%expect_test "SCF" =
    (*  SCF *)
    let cpu, _, _ = create_cpu ~data:[ 0x37 ] ~size:0xFFFF () in
    let _ = Cpu.step cpu in
    Cpu.show cpu |> print_endline;
    [%expect
        {| HALT:false SP:0x0 PC:0x1 REG:A: 0x0 B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0x10 H: 0x0 L: 0x0 |}]

let%expect_test "ADD16 half carry" =
    (*  LD HL 0xF00; LD BC 0x100; ADD16 HL BC *)
    let cpu, _, _ = create_cpu ~data:[ 0x21; 0x00; 0x0F; 0x01; 0x00; 0x01; 0x09 ] ~size:0xFFFF () in
    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    Cpu.show cpu |> print_endline;
    [%expect
        {| HALT:false SP:0x0 PC:0x7 REG:A: 0x0 B: 0x1 C: 0x0 D: 0x0 E: 0x0 F: 0x20 H: 0x10 L: 0x0 |}]

let%expect_test "ADD16 carry" =
    (*  LD HL 0xFF00; LD BC 0x101; ADD16 HL BC *)
    let cpu, _, _ = create_cpu ~data:[ 0x21; 0x00; 0xFF; 0x01; 0x01; 0x01; 0x09 ] ~size:0xFFFF () in
    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    Cpu.show cpu |> print_endline;
    [%expect
        {| HALT:false SP:0x0 PC:0x7 REG:A: 0x0 B: 0x1 C: 0x1 D: 0x0 E: 0x0 F: 0x30 H: 0x0 L: 0x1 |}]

let%expect_test "ADD16 no carry" =
    (*  LD HL 0x100; LD BC 0x102; ADD16 HL BC *)
    let cpu, _, _ = create_cpu ~data:[ 0x21; 0x00; 0x01; 0x01; 0x02; 0x01; 0x09 ] ~size:0xFFFF () in
    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    Cpu.show cpu |> print_endline;
    [%expect
        {| HALT:false SP:0x0 PC:0x7 REG:A: 0x0 B: 0x1 C: 0x2 D: 0x0 E: 0x0 F: 0x0 H: 0x2 L: 0x2 |}]

let%expect_test "CPL" =
    (*  LD A 5; CPL *)
    let cpu, _, _ = create_cpu ~data:[ 0x3E; 0x05; 0x2F ] ~size:0xFFFF () in
    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    Cpu.show cpu |> print_endline;
    [%expect
        {| HALT:false SP:0x0 PC:0x3 REG:A: 0xfa B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0x60 H: 0x0 L: 0x0 |}]

let%expect_test "SWAP" =
    (*  LD A 0b11001010; SWAP 5 *)
    let cpu, _, _ = create_cpu ~data:[ 0x3E; 0b11001010; 0xCB; 0x37 ] ~size:0xFFFF () in
    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    Cpu.show cpu |> print_endline;
    [%expect
        {| HALT:false SP:0x0 PC:0x4 REG:A: 0xac B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0x0 H: 0x0 L: 0x0 |}]

let%expect_test "RST 0x38" =
    (* LD SP 0x0010; RST 0x38; RET  None*)
    let cpu, mem, _ = create_cpu ~data:[ 0x31; 0x10; 0x00; 0xFF ] ~size:0xFFFF () in
    Mem.write_byte mem ~addr:(Uint16.of_int 0x0038) ~data:(Uint8.of_int 0xC9);

    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    Cpu.show cpu |> print_endline;
    let _ = Cpu.step cpu in
    Cpu.show cpu |> print_endline;
    [%expect
        {|
      HALT:false SP:0xe PC:0x38 REG:A: 0x0 B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0x0 H: 0x0 L: 0x0
      HALT:false SP:0x10 PC:0x4 REG:A: 0x0 B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0x0 H: 0x0 L: 0x0
      |}]

let%expect_test "RES 3 A" =
    (*  LD A 0b11001010; RES 3 A *)
    let cpu, _, _ = create_cpu ~data:[ 0x3E; 0b11001010; 0xCB; 0x9F ] ~size:0xFFFF () in
    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    Cpu.show cpu |> print_endline;
    [%expect
        {| HALT:false SP:0x0 PC:0x4 REG:A: 0xc2 B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0x0 H: 0x0 L: 0x0 |}]

let%expect_test "SLA A" =
    (*  LD A 0b10000010; SLA A *)
    let cpu, _, _ = create_cpu ~data:[ 0x3E; 0b10000010; 0xCB; 0x27 ] ~size:0xFFFF () in
    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    Cpu.show cpu |> print_endline;
    [%expect
        {| HALT:false SP:0x0 PC:0x4 REG:A: 0x4 B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0x10 H: 0x0 L: 0x0 |}]

let%expect_test "SRA A" =
    (*  LD A 0b10000010; SRA A *)
    let cpu, _, _ = create_cpu ~data:[ 0x3E; 0b10000010; 0xCB; 0x2F ] ~size:0xFFFF () in
    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    Cpu.show cpu |> print_endline;
    [%expect
        {| HALT:false SP:0x0 PC:0x4 REG:A: 0xc1 B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0x0 H: 0x0 L: 0x0 |}]

let%expect_test "SRL A" =
    (*  LD A 0b10000011; SRL A *)
    let cpu, _, _ = create_cpu ~data:[ 0x3E; 0b10000011; 0xCB; 0x3F ] ~size:0xFFFF () in
    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    Cpu.show cpu |> print_endline;
    [%expect
        {| HALT:false SP:0x0 PC:0x4 REG:A: 0x41 B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0x10 H: 0x0 L: 0x0 |}]

let%expect_test "RL no carry" =
    (* LD8 A 4; RL A *)
    let cpu, _, _ = create_cpu ~data:[ 0x3E; 0x04; 0xCB; 0x17 ] ~size:0xFFFF () in

    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    Cpu.show cpu |> print_endline;
    [%expect
        {| HALT:false SP:0x0 PC:0x4 REG:A: 0x8 B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0x0 H: 0x0 L: 0x0 |}]

let%expect_test "RL carry" =
    (* LD A 255; ADD A 1; LD8 B 0x84; RL B *)
    let cpu, _, _ =
        create_cpu ~data:[ 0x3E; 0xFF; 0xC6; 0x01; 0x06; 0x84; 0xCB; 0x10 ] ~size:0xFFFF ()
    in

    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    Cpu.show cpu |> print_endline;
    [%expect
        {| HALT:false SP:0x0 PC:0x8 REG:A: 0x0 B: 0x9 C: 0x0 D: 0x0 E: 0x0 F: 0x10 H: 0x0 L: 0x0 |}]

let%expect_test "RLA no carry" =
    (* LD8 A 4; RLA *)
    let cpu, _, _ = create_cpu ~data:[ 0x3E; 0x04; 0x17 ] ~size:0xFFFF () in

    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    Cpu.show cpu |> print_endline;
    [%expect
        {| HALT:false SP:0x0 PC:0x3 REG:A: 0x8 B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0x0 H: 0x0 L: 0x0 |}]

let%expect_test "RLA carry" =
    (* LD A 0xFF; ADD A 1; LD8 A 0x84; RLA *)
    let cpu, _, _ = create_cpu ~data:[ 0x3E; 0xFF; 0xC6; 0x02; 0x3E; 0x84; 0x17 ] ~size:0xFFFF () in

    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    Cpu.show cpu |> print_endline;
    [%expect
        {| HALT:false SP:0x0 PC:0x7 REG:A: 0x9 B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0x10 H: 0x0 L: 0x0 |}]

let%expect_test "RLC carry" =
    (* LD8 B 0b11010001; RLC B *)
    let cpu, _, _ = create_cpu ~data:[ 0x06; 0b11010001; 0xCB; 0x00 ] ~size:0xFFFF () in

    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    Cpu.show cpu |> print_endline;
    [%expect
        {| HALT:false SP:0x0 PC:0x4 REG:A: 0x0 B: 0xa3 C: 0x0 D: 0x0 E: 0x0 F: 0x10 H: 0x0 L: 0x0 |}]

let%expect_test "RLC no carry" =
    (* LD8 B 0b01010001; RLC B *)
    let cpu, _, _ = create_cpu ~data:[ 0x06; 0b01010001; 0xCB; 0x00 ] ~size:0xFFFF () in

    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    Cpu.show cpu |> print_endline;
    [%expect
        {| HALT:false SP:0x0 PC:0x4 REG:A: 0x0 B: 0xa2 C: 0x0 D: 0x0 E: 0x0 F: 0x0 H: 0x0 L: 0x0 |}]

let%expect_test "RLCA carry" =
    (* LD8 A 0b11010001; RLC B *)
    let cpu, _, _ = create_cpu ~data:[ 0x3E; 0b11010001; 0x07 ] ~size:0xFFFF () in

    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    Cpu.show cpu |> print_endline;
    [%expect
        {| HALT:false SP:0x0 PC:0x3 REG:A: 0xa3 B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0x10 H: 0x0 L: 0x0 |}]

let%expect_test "RLCA no carry" =
    (* LD8 A 0b01010001; RLC B *)
    let cpu, _, _ = create_cpu ~data:[ 0x3E; 0b01010001; 0x07 ] ~size:0xFFFF () in

    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    Cpu.show cpu |> print_endline;
    [%expect
        {| HALT:false SP:0x0 PC:0x3 REG:A: 0xa2 B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0x0 H: 0x0 L: 0x0 |}]

let%expect_test "RR no carry" =
    (* LD8 A 4; RR A *)
    let cpu, _, _ = create_cpu ~data:[ 0x3E; 0x04; 0xCB; 0x1F ] ~size:0xFFFF () in

    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    Cpu.show cpu |> print_endline;
    [%expect
        {| HALT:false SP:0x0 PC:0x4 REG:A: 0x2 B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0x0 H: 0x0 L: 0x0 |}]

let%expect_test "RR carry" =
    (* LD A 255; ADD A 1; LD8 B 0b10000001; RR B *)
    let cpu, _, _ =
        create_cpu ~data:[ 0x3E; 0xFF; 0xC6; 0x01; 0x06; 0b10000001; 0xCB; 0x18 ] ~size:0xFFFF ()
    in

    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    Cpu.show cpu |> print_endline;
    [%expect
        {| HALT:false SP:0x0 PC:0x8 REG:A: 0x0 B: 0xc0 C: 0x0 D: 0x0 E: 0x0 F: 0x10 H: 0x0 L: 0x0 |}]

let%expect_test "RRA no carry" =
    (* LD8 A 4; RRA *)
    let cpu, _, _ = create_cpu ~data:[ 0x3E; 0x04; 0x1F ] ~size:0xFFFF () in

    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    Cpu.show cpu |> print_endline;
    [%expect
        {| HALT:false SP:0x0 PC:0x3 REG:A: 0x2 B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0x0 H: 0x0 L: 0x0 |}]

let%expect_test "RRA carry" =
    (* LD A 0xFF; ADD A 2; LD8 A 0b10000001; RRA *)
    let cpu, _, _ =
        create_cpu ~data:[ 0x3E; 0xFF; 0xC6; 0x02; 0x3E; 0b10000001; 0x1F ] ~size:0xFFFF ()
    in

    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    Cpu.show cpu |> print_endline;
    [%expect
        {| HALT:false SP:0x0 PC:0x7 REG:A: 0xc0 B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0x10 H: 0x0 L: 0x0 |}]

let%expect_test "RRC carry" =
    (* LD8 B 0b11010001; RRC B *)
    let cpu, _, _ = create_cpu ~data:[ 0x06; 0b11010001; 0xCB; 0x08 ] ~size:0xFFFF () in

    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    Cpu.show cpu |> print_endline;
    [%expect
        {| HALT:false SP:0x0 PC:0x4 REG:A: 0x0 B: 0xe8 C: 0x0 D: 0x0 E: 0x0 F: 0x10 H: 0x0 L: 0x0 |}]

let%expect_test "RRC no carry" =
    (* LD8 B 0b01010010; RRC B *)
    let cpu, _, _ = create_cpu ~data:[ 0x06; 0b01010010; 0xCB; 0x08 ] ~size:0xFFFF () in

    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    Cpu.show cpu |> print_endline;
    [%expect
        {| HALT:false SP:0x0 PC:0x4 REG:A: 0x0 B: 0x29 C: 0x0 D: 0x0 E: 0x0 F: 0x0 H: 0x0 L: 0x0 |}]

let%expect_test "RRCA carry" =
    (* LD8 A 0b11010001; RRC B *)
    let cpu, _, _ = create_cpu ~data:[ 0x3E; 0b11010001; 0x0F ] ~size:0xFFFF () in

    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    Cpu.show cpu |> print_endline;
    [%expect
        {| HALT:false SP:0x0 PC:0x3 REG:A: 0xe8 B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0x10 H: 0x0 L: 0x0 |}]

let%expect_test "RRCA no carry" =
    (* LD8 A 0b01010010; RRC B *)
    let cpu, _, _ = create_cpu ~data:[ 0x3E; 0b01010010; 0x0F ] ~size:0xFFFF () in

    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    Cpu.show cpu |> print_endline;
    [%expect
        {| HALT:false SP:0x0 PC:0x3 REG:A: 0x29 B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0x0 H: 0x0 L: 0x0 |}]

let%expect_test "DAA carry" =
    (* LD A 0x90; ADD A 0x80; DAA *)
    let cpu, _, _ = create_cpu ~data:[ 0x3E; 0x90; 0xC6; 0x80; 0x27 ] ~size:0xFFFF () in

    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    Cpu.show cpu |> print_endline;
    [%expect
        {| HALT:false SP:0x0 PC:0x6 REG:A: 0x70 B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0x10 H: 0x0 L: 0x0 |}]

let%expect_test "DAA" =
    (* LD A 0x54; ADD A 0x28; DAA *)
    let cpu, _, _ = create_cpu ~data:[ 0x3E; 0x54; 0xC6; 0x28; 0x27 ] ~size:0xFFFF () in

    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    Cpu.show cpu |> print_endline;
    [%expect
        {| HALT:false SP:0x0 PC:0x6 REG:A: 0x82 B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0x0 H: 0x0 L: 0x0 |}]

let%expect_test "DAA sub" =
    (* LD A 0x20; SUB A 0x13; DAA *)
    let cpu, _, _ = create_cpu ~data:[ 0x3E; 0x20; 0xD6; 0x13; 0x27 ] ~size:0xFFFF () in

    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    Cpu.show cpu |> print_endline;
    [%expect
        {| HALT:false SP:0x0 PC:0x6 REG:A: 0x7 B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0x40 H: 0x0 L: 0x0 |}]

let%expect_test "DAA sub carry" =
    (* LD A 0x5; SUB A 0x21; DAA *)
    let cpu, _, _ = create_cpu ~data:[ 0x3E; 0x5; 0xD6; 0x21; 0x27 ] ~size:0xFFFF () in

    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    Cpu.show cpu |> print_endline;
    [%expect
        {| HALT:false SP:0x0 PC:0x6 REG:A: 0x84 B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0x50 H: 0x0 L: 0x0 |}]

let%expect_test "ADDSP half carry" =
    (* LD SP 0x000F; ADDSP 0x1 *)
    let cpu, _, _ = create_cpu ~data:[ 0x31; 0x0F; 0x00; 0xE8; 0x1 ] ~size:0xFFFF () in
    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    Cpu.show cpu |> print_endline;
    [%expect
        {| HALT:false SP:0x10 PC:0x5 REG:A: 0x0 B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0x20 H: 0x0 L: 0x0 |}]

let%expect_test "ADDSP carry" =
    (* LD SP 0x00F0; ADDSP 0x10 *)
    let cpu, _, _ = create_cpu ~data:[ 0x31; 0xF0; 0x00; 0xE8; 0x10 ] ~size:0xFFFF () in
    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    Cpu.show cpu |> print_endline;
    [%expect
        {| HALT:false SP:0x100 PC:0x5 REG:A: 0x0 B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0x10 H: 0x0 L: 0x0 |}]

let%expect_test "ADDSP negative" =
    (* LD SP 0x00F0; ADDSP -1 *)
    let cpu, _, _ = create_cpu ~data:[ 0x31; 0xF0; 0x00; 0xE8; -1 ] ~size:0xFFFF () in
    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    Cpu.show cpu |> print_endline;
    [%expect
        {| HALT:false SP:0xef PC:0x5 REG:A: 0x0 B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0x10 H: 0x0 L: 0x0 |}]

let%expect_test "halt" =
    (* HALT *)
    let cpu, _, _ = create_cpu ~data:[ 0x76 ] ~size:0xFFFF () in
    let _ = Cpu.step cpu in
    Cpu.show cpu |> print_endline;
    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    Cpu.show cpu |> print_endline;
    [%expect
        {|
      HALT:true SP:0x0 PC:0x1 REG:A: 0x0 B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0x0 H: 0x0 L: 0x0
      HALT:true SP:0x0 PC:0x1 REG:A: 0x0 B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0x0 H: 0x0 L: 0x0
      |}]

let%expect_test "halt wakeup IME on" =
    (* LD SP 0x000F; HALT *)
    let cpu, _, im = create_cpu ~data:[ 0x31; 0x0F; 0x00; 0x76 ] ~size:0xFFFF () in
    Interrupt_manager.set_master_enable im true;
    Interrupt_manager.write_byte im ~addr:(Uint16.of_int 0xFFFF) ~data:Uint8.max_int;
    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    Cpu.show cpu |> print_endline;
    Interrupt_manager.request_interrupt im Timer;
    let _ = Cpu.step cpu in
    Cpu.show cpu |> print_endline;
    [%expect
        {|
      HALT:true SP:0xf PC:0x4 REG:A: 0x0 B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0x0 H: 0x0 L: 0x0
      INFO: Handling interrupt Interrupt_manager.Timer
      HALT:false SP:0xd PC:0x50 REG:A: 0x0 B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0x0 H: 0x0 L: 0x0
      |}]

let%expect_test "halt wakeup IME off" =
    (* LD SP 0x000F; HALT ; LD A 0x42 *)
    let cpu, _, im = create_cpu ~data:[ 0x31; 0x0F; 0x00; 0x76; 0x3E; 0x42 ] ~size:0xFFFF () in
    Interrupt_manager.set_master_enable im false;
    Interrupt_manager.write_byte im ~addr:(Uint16.of_int 0xFFFF) ~data:Uint8.max_int;
    let _ = Cpu.step cpu in
    let _ = Cpu.step cpu in
    Cpu.show cpu |> print_endline;
    Interrupt_manager.request_interrupt im Timer;
    let _ = Cpu.step cpu in
    Cpu.show cpu |> print_endline;
    [%expect
        {|
      HALT:true SP:0xf PC:0x4 REG:A: 0x0 B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0x0 H: 0x0 L: 0x0
      HALT:false SP:0xf PC:0x6 REG:A: 0x42 B: 0x0 C: 0x0 D: 0x0 E: 0x0 F: 0x0 H: 0x0 L: 0x0
      |}]
