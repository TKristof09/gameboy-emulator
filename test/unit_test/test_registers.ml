include Gameboy
open Stdint

let%expect_test "test write and read single 8bit register" =
    let t = Registers.create () in
    Registers.write_r8 t A (0xAB |> Uint8.of_int);
    let v = Registers.read_r8 t A in
    print_endline @@ Uint8.to_string_hex v;
    [%expect {| 0xab |}]

let%expect_test "test write and read single 16bit register" =
    let t = Registers.create () in
    Registers.write_r16 t AF (0xABCD |> Uint16.of_int);
    let v = Registers.read_r16 t AF in
    print_endline @@ Uint16.to_string_hex v;
    [%expect {| 0xabcd |}]

let%expect_test "test write a pair of 8bit registers and read the corresponding 16bit pair" =
    let t = Registers.create () in
    Registers.write_r8 t A (0xAB |> Uint8.of_int);
    Registers.write_r8 t F (0xCD |> Uint8.of_int);
    let v = Registers.read_r16 t AF in
    print_endline @@ Uint16.to_string_hex v;
    [%expect {| 0xabcd |}]

let%expect_test "test write a 16bit register and read the higher 8 bits" =
    let t = Registers.create () in
    Registers.write_r16 t AF (0xABCD |> Uint16.of_int);
    let v = Registers.read_r8 t A in
    print_endline @@ Uint8.to_string_hex v;
    [%expect {| 0xab |}]

let%expect_test "test set, unset and read flag" =
    let t = Registers.create () in
    Registers.set_flag t Carry;
    Registers.set_flag t Half_carry;
    Registers.set_flag t Zero;
    Registers.unset_flag t Half_carry;
    let carry = Registers.read_flag t Carry in
    let half_carry = Registers.read_flag t Half_carry in
    let sub = Registers.read_flag t Sub in
    let zero = Registers.read_flag t Zero in
    Printf.printf "%b %b %b %b" carry half_carry sub zero;
    [%expect {|true false false true|}]

let%expect_test "test set multiple flags" =
    let t = Registers.create () in
    Registers.set_flag t Carry;
    Registers.set_flags t ~c:false ~h:false ~s:false ~z:true ();
    let carry = Registers.read_flag t Carry in
    let half_carry = Registers.read_flag t Half_carry in
    let sub = Registers.read_flag t Sub in
    let zero = Registers.read_flag t Zero in
    Printf.printf "%b %b %b %b" carry half_carry sub zero;
    [%expect {|false false false true|}]
