include Gameboy
open Uint

let%expect_test "Create" =
    let interrupt_manager = Interrupt_manager.create () in
    let joypad = Joypad.create interrupt_manager in
    print_endline @@ Joypad.show joypad;
    [%expect
        {|
      { Joypad.interrupt_manager = <opaque>; selection = `None; up = false;
        down = false; left = false; right = false; a = false; b = false;
        start = false; select = false }
      |}]

let%expect_test "Press A" =
    let interrupt_manager = Interrupt_manager.create () in
    let joypad = Joypad.create interrupt_manager in
    Joypad.press joypad A;
    Joypad.write_byte joypad ~addr:Uint16.zero ~data:(Uint8.of_int 0b0010000);
    print_endline @@ Joypad.show joypad;
    print_endline @@ Uint8.to_string_bin (Joypad.read_byte joypad Uint16.zero);
    [%expect
        {|
      { Joypad.interrupt_manager = <opaque>; selection = `Buttons; up = false;
        down = false; left = false; right = false; a = true; b = false;
        start = false; select = false }
      0b11110
      |}]

let%expect_test "Press+release A" =
    let interrupt_manager = Interrupt_manager.create () in
    let joypad = Joypad.create interrupt_manager in
    Joypad.press joypad A;
    Joypad.release joypad A;
    Joypad.write_byte joypad ~addr:Uint16.zero ~data:(Uint8.of_int 0b0010000);
    print_endline @@ Joypad.show joypad;
    print_endline @@ Uint8.to_string_bin (Joypad.read_byte joypad Uint16.zero);
    [%expect
        {|
      { Joypad.interrupt_manager = <opaque>; selection = `Buttons; up = false;
        down = false; left = false; right = false; a = false; b = false;
        start = false; select = false }
      0b11111
      |}]

let%expect_test "Press B" =
    let interrupt_manager = Interrupt_manager.create () in
    let joypad = Joypad.create interrupt_manager in
    Joypad.press joypad B;
    Joypad.write_byte joypad ~addr:Uint16.zero ~data:(Uint8.of_int 0b0010000);
    print_endline @@ Joypad.show joypad;
    print_endline @@ Uint8.to_string_bin (Joypad.read_byte joypad Uint16.zero);
    [%expect
        {|
      { Joypad.interrupt_manager = <opaque>; selection = `Buttons; up = false;
        down = false; left = false; right = false; a = false; b = true;
        start = false; select = false }
      0b11101
      |}]

let%expect_test "Press Start" =
    let interrupt_manager = Interrupt_manager.create () in
    let joypad = Joypad.create interrupt_manager in
    Joypad.press joypad Start;
    Joypad.write_byte joypad ~addr:Uint16.zero ~data:(Uint8.of_int 0b0010000);
    print_endline @@ Joypad.show joypad;
    print_endline @@ Uint8.to_string_bin (Joypad.read_byte joypad Uint16.zero);
    [%expect
        {|
      { Joypad.interrupt_manager = <opaque>; selection = `Buttons; up = false;
        down = false; left = false; right = false; a = false; b = false;
        start = true; select = false }
      0b10111
      |}]

let%expect_test "Press Select" =
    let interrupt_manager = Interrupt_manager.create () in
    let joypad = Joypad.create interrupt_manager in
    Joypad.press joypad Select;
    Joypad.write_byte joypad ~addr:Uint16.zero ~data:(Uint8.of_int 0b0010000);
    print_endline @@ Joypad.show joypad;
    print_endline @@ Uint8.to_string_bin (Joypad.read_byte joypad Uint16.zero);
    [%expect
        {|
      { Joypad.interrupt_manager = <opaque>; selection = `Buttons; up = false;
        down = false; left = false; right = false; a = false; b = false;
        start = false; select = true }
      0b11011
      |}]

let%expect_test "Press Down" =
    let interrupt_manager = Interrupt_manager.create () in
    let joypad = Joypad.create interrupt_manager in
    Joypad.press joypad Down;
    Joypad.write_byte joypad ~addr:Uint16.zero ~data:(Uint8.of_int 0b00100000);
    print_endline @@ Joypad.show joypad;
    print_endline @@ Uint8.to_string_bin (Joypad.read_byte joypad Uint16.zero);
    [%expect
        {|
      { Joypad.interrupt_manager = <opaque>; selection = `DPad; up = false;
        down = true; left = false; right = false; a = false; b = false;
        start = false; select = false }
      0b100111
      |}]

let%expect_test "Press Up" =
    let interrupt_manager = Interrupt_manager.create () in
    let joypad = Joypad.create interrupt_manager in
    Joypad.press joypad Up;
    Joypad.write_byte joypad ~addr:Uint16.zero ~data:(Uint8.of_int 0b00100000);
    print_endline @@ Joypad.show joypad;
    print_endline @@ Uint8.to_string_bin (Joypad.read_byte joypad Uint16.zero);
    [%expect
        {|
      { Joypad.interrupt_manager = <opaque>; selection = `DPad; up = true;
        down = false; left = false; right = false; a = false; b = false;
        start = false; select = false }
      0b101011
      |}]

let%expect_test "Press Left" =
    let interrupt_manager = Interrupt_manager.create () in
    let joypad = Joypad.create interrupt_manager in
    Joypad.press joypad Left;
    Joypad.write_byte joypad ~addr:Uint16.zero ~data:(Uint8.of_int 0b00100000);
    print_endline @@ Joypad.show joypad;
    print_endline @@ Uint8.to_string_bin (Joypad.read_byte joypad Uint16.zero);
    [%expect
        {|
      { Joypad.interrupt_manager = <opaque>; selection = `DPad; up = false;
        down = false; left = true; right = false; a = false; b = false;
        start = false; select = false }
      0b101101
      |}]

let%expect_test "Press Right" =
    let interrupt_manager = Interrupt_manager.create () in
    let joypad = Joypad.create interrupt_manager in
    Joypad.press joypad Right;
    Joypad.write_byte joypad ~addr:Uint16.zero ~data:(Uint8.of_int 0b00100000);
    print_endline @@ Joypad.show joypad;
    print_endline @@ Uint8.to_string_bin (Joypad.read_byte joypad Uint16.zero);
    [%expect
        {|
      { Joypad.interrupt_manager = <opaque>; selection = `DPad; up = false;
        down = false; left = false; right = true; a = false; b = false;
        start = false; select = false }
      0b101110
      |}]

let%expect_test "Press Left+A - Dpad" =
    let interrupt_manager = Interrupt_manager.create () in
    let joypad = Joypad.create interrupt_manager in
    Joypad.press joypad Left;
    Joypad.press joypad A;
    Joypad.write_byte joypad ~addr:Uint16.zero ~data:(Uint8.of_int 0b00100000);
    print_endline @@ Joypad.show joypad;
    print_endline @@ Uint8.to_string_bin (Joypad.read_byte joypad Uint16.zero);
    [%expect
        {|
      { Joypad.interrupt_manager = <opaque>; selection = `DPad; up = false;
        down = false; left = true; right = false; a = true; b = false;
        start = false; select = false }
      0b101101
      |}]

let%expect_test "Press Left+A - Buttons" =
    let interrupt_manager = Interrupt_manager.create () in
    let joypad = Joypad.create interrupt_manager in
    Joypad.press joypad Left;
    Joypad.press joypad A;
    Joypad.write_byte joypad ~addr:Uint16.zero ~data:(Uint8.of_int 0b0010000);
    print_endline @@ Joypad.show joypad;
    print_endline @@ Uint8.to_string_bin (Joypad.read_byte joypad Uint16.zero);
    [%expect
        {|
      { Joypad.interrupt_manager = <opaque>; selection = `Buttons; up = false;
        down = false; left = true; right = false; a = true; b = false;
        start = false; select = false }
      0b11110
      |}]

let%expect_test "DPad + Buttons" =
    let interrupt_manager = Interrupt_manager.create () in
    let joypad = Joypad.create interrupt_manager in
    Joypad.press joypad Left;
    Joypad.press joypad A;
    Joypad.write_byte joypad ~addr:Uint16.zero ~data:(Uint8.of_int 0b00110000);
    print_endline @@ Joypad.show joypad;
    print_endline @@ Uint8.to_string_bin (Joypad.read_byte joypad Uint16.zero);
    [%expect
        {|
      { Joypad.interrupt_manager = <opaque>; selection = `None; up = false;
        down = false; left = true; right = false; a = true; b = false;
        start = false; select = false }
      0b111111
      |}]
