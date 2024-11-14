open Gameboy
open Uint

let%expect_test "test create" =
    let p = Palette.create ~addr:(Uint16.of_int 0xFF47) in
    print_endline (Color.show (Palette.get_color p Color_id.ID_0));
    print_endline (Color.show (Palette.get_color p Color_id.ID_1));
    print_endline (Color.show (Palette.get_color p Color_id.ID_2));
    print_endline (Color.show (Palette.get_color p Color_id.ID_3));
    print_endline (Uint8.to_string_bin (Palette.read_byte p (Uint16.of_int 0xFF47)));
    [%expect
        {|
      Color.White
      Color.Light_grey
      Color.Dark_grey
      Color.Black
      0b11100100
      |}]

let%expect_test "test write/read" =
    let p = Palette.create ~addr:(Uint16.of_int 0xFF47) in
    Palette.write_byte p ~addr:(Uint16.of_int 0xFF47) ~data:(Uint8.of_int 0b11010010);
    print_endline (Color.show (Palette.get_color p Color_id.ID_0));
    print_endline (Color.show (Palette.get_color p Color_id.ID_1));
    print_endline (Color.show (Palette.get_color p Color_id.ID_2));
    print_endline (Color.show (Palette.get_color p Color_id.ID_3));
    print_endline (Uint8.to_string_bin (Palette.read_byte p (Uint16.of_int 0xFF47)));
    [%expect
        {|
      Color.Dark_grey
      Color.White
      Color.Light_grey
      Color.Black
      0b11010010
      |}]
