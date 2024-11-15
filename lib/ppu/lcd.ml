open Uint

type control = {
    mutable bg_win_enable : bool;
    mutable obj_enable : bool;
    mutable obj_size : [ `OBJ_size_8x8 | `OBJ_size_8x16 ];
    mutable bg_tile_map : [ `Map_0 | `Map_1 ];
    mutable bg_win_tile_data : [ `Mode_8000 | `Mode_8800 ];
    mutable win_enable : bool;
    mutable win_tile_map : [ `Map_0 | `Map_1 ];
    mutable lcd_ppu_enable : bool;
  }
[@@deriving show]

type stat = {
    mutable ppu_mode : [ `Mode_0 | `Mode_1 | `Mode_2 | `Mode_3 ];
    mutable ly_eq_lyc_flag : bool;
    mutable mode_0_interupt_enable : bool;
    mutable mode_1_interupt_enable : bool;
    mutable mode_2_interupt_enable : bool;
    mutable ly_eq_lyc_interupt_enable : bool;
    mutable ly : uint8;
    mutable lyc : uint8;
    mutable scx : uint8;
    mutable scy : uint8;
    mutable winx : uint8;
    mutable winy : uint8;
  }
[@@deriving show]

type t = {
    control : control;
    stat : stat;
  }
[@@deriving show]

let create () =
    {
      control =
        {
          bg_win_enable = true;
          obj_enable = true;
          obj_size = `OBJ_size_8x8;
          bg_tile_map = `Map_0;
          bg_win_tile_data = `Mode_8000;
          win_enable = true;
          win_tile_map = `Map_0;
          lcd_ppu_enable = true;
        };
      stat =
        {
          ppu_mode = `Mode_0;
          ly_eq_lyc_flag = false;
          mode_0_interupt_enable = false;
          mode_1_interupt_enable = false;
          mode_2_interupt_enable = false;
          ly_eq_lyc_interupt_enable = false;
          ly = Uint8.zero;
          lyc = Uint8.zero;
          scx = Uint8.zero;
          scy = Uint8.zero;
          winx = Uint8.zero;
          winy = Uint8.zero;
        };
    }

let set_mode t mode = t.stat.ppu_mode <- mode
let get_mode t = t.stat.ppu_mode
let incr_ly t = t.stat.ly <- Uint8.add t.stat.ly Uint8.one
let reset_ly t = t.stat.ly <- Uint8.zero
let set_ly_eq_lyc t b = t.stat.ly_eq_lyc_flag <- b
let get_scroll t = (t.stat.scx, t.stat.scy)
let get_win t = (t.stat.winx, t.stat.winy)

let read_control control =
    Bool.to_int control.bg_win_enable
    lor (Bool.to_int control.obj_enable lsl 1)
    lor (match control.obj_size with
        | `OBJ_size_8x8 -> 0
        | `OBJ_size_8x16 -> 1 lsl 2)
    lor (match control.bg_tile_map with
        | `Map_0 -> 0
        | `Map_1 -> 1 lsl 3)
    lor (match control.bg_win_tile_data with
        | `Mode_8800 -> 0
        | `Mode_8000 -> 1 lsl 4)
    lor (Bool.to_int control.win_enable lsl 5)
    lor (match control.win_tile_map with
        | `Map_0 -> 0
        | `Map_1 -> 1 lsl 6)
    lor (Bool.to_int control.lcd_ppu_enable lsl 7)
    |> Uint8.of_int

let read_stat stat =
    (match stat.ppu_mode with
    | `Mode_0 -> 0
    | `Mode_1 -> 1
    | `Mode_2 -> 2
    | `Mode_3 -> 3)
    lor (Bool.to_int stat.ly_eq_lyc_flag lsl 2)
    lor (Bool.to_int stat.mode_0_interupt_enable lsl 3)
    lor (Bool.to_int stat.mode_1_interupt_enable lsl 4)
    lor (Bool.to_int stat.mode_2_interupt_enable lsl 5)
    lor (Bool.to_int stat.ly_eq_lyc_interupt_enable lsl 6)
    |> Uint8.of_int

let write_control control data =
    let data = Uint8.to_int data in
    control.bg_win_enable <- data land 0b00000001 <> 0;
    control.obj_enable <- data land 0b00000010 <> 0;
    control.obj_size <- (if data land 0b00000100 <> 0 then `OBJ_size_8x16 else `OBJ_size_8x8);
    control.bg_tile_map <- (if data land 0b00001000 <> 0 then `Map_1 else `Map_0);
    control.bg_win_tile_data <- (if data land 0b00010000 <> 0 then `Mode_8000 else `Mode_8800);
    control.win_enable <- data land 0b00100000 <> 0;
    control.win_tile_map <- (if data land 0b01000000 <> 0 then `Map_1 else `Map_0);
    control.lcd_ppu_enable <- data land 0b10000000 <> 0

let write_stat stat data =
    let data = Uint8.to_int data in
    stat.ppu_mode <-
      (match data land 0b00000011 with
      | 0 -> `Mode_0
      | 1 -> `Mode_1
      | 2 -> `Mode_2
      | 3 -> `Mode_3
      | _ -> failwith "Invalid mode");
    stat.ly_eq_lyc_flag <- data land 0b00000100 <> 0;
    stat.mode_0_interupt_enable <- data land 0b00001000 <> 0;
    stat.mode_1_interupt_enable <- data land 0b00010000 <> 0;
    stat.mode_2_interupt_enable <- data land 0b00100000 <> 0;
    stat.ly_eq_lyc_interupt_enable <- data land 0b01000000 <> 0

let read_byte t addr =
    match Uint16.to_int addr with
    | addr_int when addr_int = 0xFF40 -> read_control t.control
    | addr_int when addr_int = 0xFF41 -> read_stat t.stat
    | addr_int when addr_int = 0xFF42 -> t.stat.scy
    | addr_int when addr_int = 0xFF43 -> t.stat.scx
    | addr_int when addr_int = 0xFF44 -> t.stat.ly
    | addr_int when addr_int = 0xFF45 -> t.stat.lyc
    (* 0xFF46 is for DMA, 0xFF47-0xFF49 are palettes *)
    | addr_int when addr_int = 0xFF4A -> t.stat.winy
    | addr_int when addr_int = 0xFF4B -> t.stat.winx
    | _ -> failwith "Invalid lcd read address"

let write_byte t ~addr ~data =
    match Uint16.to_int addr with
    | addr_int when addr_int = 0xFF40 -> write_control t.control data
    | addr_int when addr_int = 0xFF41 -> write_stat t.stat data
    | addr_int when addr_int = 0xFF42 -> t.stat.scy <- data
    | addr_int when addr_int = 0xFF43 -> t.stat.scx <- data
    | addr_int when addr_int = 0xFF44 -> t.stat.ly <- data
    | addr_int when addr_int = 0xFF45 -> t.stat.lyc <- data
    (* 0xFF46 is for DMA, 0xFF47-0xFF49 are palettes *)
    | addr_int when addr_int = 0xFF4A -> t.stat.winy <- data
    | addr_int when addr_int = 0xFF4B -> t.stat.winx <- data
    | _ -> failwith "Invalid lcd write address"
