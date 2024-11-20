open Gameboy
open Uint

let%expect_test "test" =
    let oam = Oam.create () in
    (* y *)
    Oam.write_byte oam ~addr:(Uint16.of_int 0xFE00) ~data:(Uint8.of_int 42);
    (* x *)
    Oam.write_byte oam ~addr:(Uint16.of_int 0xFE01) ~data:(Uint8.of_int 7);
    (* tile index *)
    Oam.write_byte oam ~addr:(Uint16.of_int 0xFE02) ~data:(Uint8.of_int 1);
    (* attributes : low prio, x_flip, palette 1*)
    Oam.write_byte oam ~addr:(Uint16.of_int 0xFE03) ~data:(Uint8.of_int 0b10110000);
    print_endline (Oam.show_obj (Oam.get_objects oam).(0));
    print_endline (Uint8.to_string_bin (Oam.read_byte oam (Uint16.of_int 0xFE00)));
    print_endline (Uint8.to_string_bin (Oam.read_byte oam (Uint16.of_int 0xFE01)));
    print_endline (Uint8.to_string_bin (Oam.read_byte oam (Uint16.of_int 0xFE02)));
    print_endline (Uint8.to_string_bin (Oam.read_byte oam (Uint16.of_int 0xFE03)));
    [%expect
        {|
      { Oam.x = 255; y = 26; tile_index = 1; priority = `BG_WIN_prio;
        y_flip = false; x_flip = true; palette = `OBP1 }
      0b101010
      0b111
      0b1
      0b10110000
      |}]
