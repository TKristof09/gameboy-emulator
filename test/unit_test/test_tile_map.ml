open Gameboy
open Uint

let%expect_test "test map0" =
    let m = Tile_map.create () in
    Tile_map.write_byte m ~addr:(Uint16.of_int 0x9800) ~data:(Uint8.of_int 127);
    print_endline (Uint8.to_string (Tile_map.get_tile_index m ~x:0 ~y:0 `Map_0));
    print_endline (Uint8.to_string (Tile_map.read_byte m (Uint16.of_int 0x9800)));
    [%expect {|
      127
      127
      |}]

let%expect_test "test map1" =
    let m = Tile_map.create () in
    Tile_map.write_byte m ~addr:(Uint16.of_int 0x9C30) ~data:(Uint8.of_int 127);
    print_endline (Uint8.to_string (Tile_map.get_tile_index m ~x:(16 * 8) ~y:(1 * 8) `Map_1));
    print_endline (Uint8.to_string (Tile_map.read_byte m (Uint16.of_int 0x9C30)));
    [%expect {|
      127
      127
      |}]
