open Gameboy
open Uint

let%expect_test "test first row mode 8000" =
    let data = Tile_data.create () in
    Tile_data.write_byte data ~addr:(Uint16.of_int 0x8010) ~data:(Uint8.of_int 0b01010000);
    Tile_data.write_byte data ~addr:(Uint16.of_int 0x8011) ~data:(Uint8.of_int 0b00110000);
    let p = Palette.create ~addr:(Uint16.of_int 0xFF47) in
    print_endline (Color.show (Tile_data.get_pixel data Mode_8000 Uint8.one ~x:0 ~y:0 p));
    print_endline (Color.show (Tile_data.get_pixel data Mode_8000 Uint8.one ~x:1 ~y:0 p));
    print_endline (Color.show (Tile_data.get_pixel data Mode_8000 Uint8.one ~x:2 ~y:0 p));
    print_endline (Color.show (Tile_data.get_pixel data Mode_8000 Uint8.one ~x:3 ~y:0 p));
    print_endline (Uint8.to_string_bin (Tile_data.read_byte data (Uint16.of_int 0x8011)));
    [%expect
        {|
      Color.White
      Color.Light_grey
      Color.Dark_grey
      Color.Black
      0b110000
      |}]

let%expect_test "test later row mode 8000" =
    let data = Tile_data.create () in
    Tile_data.write_byte data ~addr:(Uint16.of_int 0x8012) ~data:(Uint8.of_int 0b01010000);
    Tile_data.write_byte data ~addr:(Uint16.of_int 0x8013) ~data:(Uint8.of_int 0b00110000);
    let p = Palette.create ~addr:(Uint16.of_int 0xFF47) in
    print_endline (Color.show (Tile_data.get_pixel data Mode_8000 Uint8.one ~x:0 ~y:1 p));
    print_endline (Color.show (Tile_data.get_pixel data Mode_8000 Uint8.one ~x:1 ~y:1 p));
    print_endline (Color.show (Tile_data.get_pixel data Mode_8000 Uint8.one ~x:2 ~y:1 p));
    print_endline (Color.show (Tile_data.get_pixel data Mode_8000 Uint8.one ~x:3 ~y:1 p));
    print_endline (Uint8.to_string_bin (Tile_data.read_byte data (Uint16.of_int 0x8012)));
    [%expect
        {|
      Color.White
      Color.Light_grey
      Color.Dark_grey
      Color.Black
      0b1010000
      |}]

let%expect_test "test mode 8800 negative" =
    let data = Tile_data.create () in
    Tile_data.write_byte data ~addr:(Uint16.of_int 0x8810) ~data:(Uint8.of_int 0b01010000);
    Tile_data.write_byte data ~addr:(Uint16.of_int 0x8811) ~data:(Uint8.of_int 0b00110000);
    let p = Palette.create ~addr:(Uint16.of_int 0xFF47) in
    let id = Uint8.of_int (-127) in
    print_endline (Color.show (Tile_data.get_pixel data Mode_8800 id ~x:0 ~y:0 p));
    print_endline (Color.show (Tile_data.get_pixel data Mode_8800 id ~x:1 ~y:0 p));
    print_endline (Color.show (Tile_data.get_pixel data Mode_8800 id ~x:2 ~y:0 p));
    print_endline (Color.show (Tile_data.get_pixel data Mode_8800 id ~x:3 ~y:0 p));
    print_endline (Uint8.to_string_bin (Tile_data.read_byte data (Uint16.of_int 0x8810)));
    [%expect
        {|
      Color.White
      Color.Light_grey
      Color.Dark_grey
      Color.Black
      0b1010000
      |}]

let%expect_test "test mode 8800 positive" =
    let data = Tile_data.create () in
    Tile_data.write_byte data ~addr:(Uint16.of_int 0x97F0) ~data:(Uint8.of_int 0b01010000);
    Tile_data.write_byte data ~addr:(Uint16.of_int 0x97F1) ~data:(Uint8.of_int 0b00110000);
    let p = Palette.create ~addr:(Uint16.of_int 0xFF47) in
    let id = Uint8.of_int 127 in
    print_endline (Color.show (Tile_data.get_pixel data Mode_8800 id ~x:0 ~y:0 p));
    print_endline (Color.show (Tile_data.get_pixel data Mode_8800 id ~x:1 ~y:0 p));
    print_endline (Color.show (Tile_data.get_pixel data Mode_8800 id ~x:2 ~y:0 p));
    print_endline (Color.show (Tile_data.get_pixel data Mode_8800 id ~x:3 ~y:0 p));
    print_endline (Uint8.to_string_bin (Tile_data.read_byte data (Uint16.of_int 0x97F1)));
    [%expect
        {|
      Color.White
      Color.Light_grey
      Color.Dark_grey
      Color.Black
      0b110000
      |}]

let%expect_test "test 16 pixel tall" =
    let data = Tile_data.create () in
    Tile_data.write_byte data ~addr:(Uint16.of_int 0x8012) ~data:(Uint8.of_int 0b01010000);
    Tile_data.write_byte data ~addr:(Uint16.of_int 0x8013) ~data:(Uint8.of_int 0b00110000);
    let p = Palette.create ~addr:(Uint16.of_int 0xFF47) in
    print_endline (Color.show (Tile_data.get_pixel data Mode_8000 Uint8.zero ~x:0 ~y:9 p));
    print_endline (Color.show (Tile_data.get_pixel data Mode_8000 Uint8.zero ~x:1 ~y:9 p));
    print_endline (Color.show (Tile_data.get_pixel data Mode_8000 Uint8.zero ~x:2 ~y:9 p));
    print_endline (Color.show (Tile_data.get_pixel data Mode_8000 Uint8.zero ~x:3 ~y:9 p));
    print_endline (Uint8.to_string_bin (Tile_data.read_byte data (Uint16.of_int 0x8013)));
    [%expect
        {|
      Color.White
      Color.Light_grey
      Color.Dark_grey
      Color.Black
      0b110000
      |}]
