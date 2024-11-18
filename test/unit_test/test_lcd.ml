open Gameboy
open Uint

let%expect_test "create" =
    let lcd = Lcd.create () in
    print_endline (Lcd.show lcd);
    [%expect
        {|
      { Lcd.control =
        { Lcd.bg_win_enable = true; obj_enable = true; obj_size = `OBJ_size_8x8;
          bg_tile_map = Tile_map.Map_0; bg_win_tile_data = Tile_data.Mode_8000;
          win_enable = true; win_tile_map = Tile_map.Map_0; lcd_ppu_enable = false
          };
        stat =
        { Lcd.ppu_mode = `OAM_search; ly_eq_lyc_flag = false;
          mode_0_interupt_enable = false; mode_1_interupt_enable = false;
          mode_2_interupt_enable = false; ly_eq_lyc_interupt_enable = false;
          ly = 0; lyc = 0; scx = 0; scy = 0; winx = 0; winy = 0 }
        }
      |}]

let%expect_test "write control" =
    let lcd = Lcd.create () in
    Lcd.write_byte lcd ~addr:(Uint16.of_int 0xFF40) ~data:(Uint8.of_int 0b10011101);
    print_endline (Lcd.show lcd);
    print_endline (Uint8.to_string_bin (Lcd.read_byte lcd (Uint16.of_int 0xFF40)));
    [%expect
        {|
      { Lcd.control =
        { Lcd.bg_win_enable = true; obj_enable = false; obj_size = `OBJ_size_8x16;
          bg_tile_map = Tile_map.Map_1; bg_win_tile_data = Tile_data.Mode_8000;
          win_enable = false; win_tile_map = Tile_map.Map_0; lcd_ppu_enable = true
          };
        stat =
        { Lcd.ppu_mode = `OAM_search; ly_eq_lyc_flag = false;
          mode_0_interupt_enable = false; mode_1_interupt_enable = false;
          mode_2_interupt_enable = false; ly_eq_lyc_interupt_enable = false;
          ly = 0; lyc = 0; scx = 0; scy = 0; winx = 0; winy = 0 }
        }
      0b10011101
      |}]

let%expect_test "write stat" =
    let lcd = Lcd.create () in
    Lcd.write_byte lcd ~addr:(Uint16.of_int 0xFF41) ~data:(Uint8.of_int 0b00011101);
    print_endline (Lcd.show lcd);
    print_endline (Uint8.to_string_bin (Lcd.read_byte lcd (Uint16.of_int 0xFF41)));
    [%expect
        {|
      { Lcd.control =
        { Lcd.bg_win_enable = true; obj_enable = true; obj_size = `OBJ_size_8x8;
          bg_tile_map = Tile_map.Map_0; bg_win_tile_data = Tile_data.Mode_8000;
          win_enable = true; win_tile_map = Tile_map.Map_0; lcd_ppu_enable = false
          };
        stat =
        { Lcd.ppu_mode = `OAM_search; ly_eq_lyc_flag = false;
          mode_0_interupt_enable = true; mode_1_interupt_enable = true;
          mode_2_interupt_enable = false; ly_eq_lyc_interupt_enable = false;
          ly = 0; lyc = 0; scx = 0; scy = 0; winx = 0; winy = 0 }
        }
      0b11010
      |}]

let%expect_test "incr ly and write lcy" =
    let lcd = Lcd.create () in
    Lcd.incr_ly lcd;
    Lcd.write_byte lcd ~addr:(Uint16.of_int 0xFF45) ~data:(Uint8.of_int 67);
    print_endline (Lcd.show lcd);
    print_endline (Uint8.to_string (Lcd.read_byte lcd (Uint16.of_int 0xFF44)));
    print_endline (Uint8.to_string (Lcd.read_byte lcd (Uint16.of_int 0xFF45)));
    [%expect
        {|
      { Lcd.control =
        { Lcd.bg_win_enable = true; obj_enable = true; obj_size = `OBJ_size_8x8;
          bg_tile_map = Tile_map.Map_0; bg_win_tile_data = Tile_data.Mode_8000;
          win_enable = true; win_tile_map = Tile_map.Map_0; lcd_ppu_enable = false
          };
        stat =
        { Lcd.ppu_mode = `OAM_search; ly_eq_lyc_flag = false;
          mode_0_interupt_enable = false; mode_1_interupt_enable = false;
          mode_2_interupt_enable = false; ly_eq_lyc_interupt_enable = false;
          ly = 1; lyc = 67; scx = 0; scy = 0; winx = 0; winy = 0 }
        }
      1
      67
      |}]

let%expect_test "write scx and scy" =
    let lcd = Lcd.create () in
    Lcd.write_byte lcd ~addr:(Uint16.of_int 0xFF43) ~data:(Uint8.of_int 66);
    Lcd.write_byte lcd ~addr:(Uint16.of_int 0xFF42) ~data:(Uint8.of_int 67);
    print_endline (Lcd.show lcd);
    print_endline (Uint8.to_string (Lcd.read_byte lcd (Uint16.of_int 0xFF43)));
    print_endline (Uint8.to_string (Lcd.read_byte lcd (Uint16.of_int 0xFF42)));
    [%expect
        {|
      { Lcd.control =
        { Lcd.bg_win_enable = true; obj_enable = true; obj_size = `OBJ_size_8x8;
          bg_tile_map = Tile_map.Map_0; bg_win_tile_data = Tile_data.Mode_8000;
          win_enable = true; win_tile_map = Tile_map.Map_0; lcd_ppu_enable = false
          };
        stat =
        { Lcd.ppu_mode = `OAM_search; ly_eq_lyc_flag = false;
          mode_0_interupt_enable = false; mode_1_interupt_enable = false;
          mode_2_interupt_enable = false; ly_eq_lyc_interupt_enable = false;
          ly = 0; lyc = 0; scx = 66; scy = 67; winx = 0; winy = 0 }
        }
      66
      67
      |}]

let%expect_test "write winx and winy" =
    let lcd = Lcd.create () in
    Lcd.write_byte lcd ~addr:(Uint16.of_int 0xFF4B) ~data:(Uint8.of_int 66);
    Lcd.write_byte lcd ~addr:(Uint16.of_int 0xFF4A) ~data:(Uint8.of_int 67);
    print_endline (Lcd.show lcd);
    print_endline (Uint8.to_string (Lcd.read_byte lcd (Uint16.of_int 0xFF4B)));
    print_endline (Uint8.to_string (Lcd.read_byte lcd (Uint16.of_int 0xFF4A)));
    [%expect
        {|
      { Lcd.control =
        { Lcd.bg_win_enable = true; obj_enable = true; obj_size = `OBJ_size_8x8;
          bg_tile_map = Tile_map.Map_0; bg_win_tile_data = Tile_data.Mode_8000;
          win_enable = true; win_tile_map = Tile_map.Map_0; lcd_ppu_enable = false
          };
        stat =
        { Lcd.ppu_mode = `OAM_search; ly_eq_lyc_flag = false;
          mode_0_interupt_enable = false; mode_1_interupt_enable = false;
          mode_2_interupt_enable = false; ly_eq_lyc_interupt_enable = false;
          ly = 0; lyc = 0; scx = 0; scy = 0; winx = 66; winy = 67 }
        }
      66
      67
      |}]
