open Uint

type control = {
    mutable bg_win_enable : bool;
    mutable obj_enable : bool;
    mutable obj_size : [ `OBJ_size_8x8 | `OBJ_size_8x16 ];
    mutable bg_tile_map : Tile_map.map;
    mutable bg_win_tile_data : Tile_data.access_mode;
    mutable win_enable : bool;
    mutable win_tile_map : Tile_map.map;
    mutable lcd_ppu_enable : bool;
  }
[@@deriving show]

type stat = {
    mutable ppu_mode : [ `HBlank | `VBlank | `OAM_search | `Drawing ];
    mutable ly_eq_lyc_flag : bool;
    mutable mode_0_interupt_enable : bool;
    mutable mode_1_interupt_enable : bool;
    mutable mode_2_interupt_enable : bool;
    mutable ly_eq_lyc_interupt_enable : bool;
    mutable ly : int;
    mutable lyc : int;
    mutable scx : uint8;
    mutable scy : uint8;
    mutable winx : uint8;
    mutable winy : uint8;
  }
[@@deriving show]

type t = {
    control : control;
    stat : stat;
    interrupt_manager : Interrupt_manager.t; [@opaque]
  }
[@@deriving show]

let create interrupt_manager =
    {
      control =
        {
          bg_win_enable = true;
          obj_enable = true;
          obj_size = `OBJ_size_8x8;
          bg_tile_map = Map_0;
          bg_win_tile_data = Mode_8000;
          win_enable = true;
          win_tile_map = Map_0;
          lcd_ppu_enable = false;
        };
      stat =
        {
          ppu_mode = `OAM_search;
          ly_eq_lyc_flag = false;
          mode_0_interupt_enable = false;
          mode_1_interupt_enable = false;
          mode_2_interupt_enable = false;
          ly_eq_lyc_interupt_enable = false;
          ly = 0;
          lyc = 0;
          scx = Uint8.zero;
          scy = Uint8.zero;
          winx = Uint8.zero;
          winy = Uint8.zero;
        };
      interrupt_manager;
    }

let set_mode t mode =
    t.stat.ppu_mode <- mode;
    match mode with
    | `HBlank ->
        if t.stat.mode_0_interupt_enable then
          Interrupt_manager.request_interrupt t.interrupt_manager LCD
    | `VBlank ->
        Interrupt_manager.request_interrupt t.interrupt_manager VBlank;
        if t.stat.mode_1_interupt_enable then
          Interrupt_manager.request_interrupt t.interrupt_manager LCD
    | `OAM_search ->
        if t.stat.mode_2_interupt_enable then
          Interrupt_manager.request_interrupt t.interrupt_manager LCD
    | `Drawing -> ()

let get_mode t = t.stat.ppu_mode

let show_mode m =
    match m with
    | `HBlank -> "HBlank"
    | `VBlank -> "VBlank"
    | `OAM_search -> "OAM Search"
    | `Drawing -> "Drawing"

let incr_ly t =
    t.stat.ly <- t.stat.ly + 1;
    let is_eq = t.stat.ly = t.stat.lyc in
    t.stat.ly_eq_lyc_flag <- is_eq;
    if is_eq && t.stat.ly_eq_lyc_interupt_enable then
      Interrupt_manager.request_interrupt t.interrupt_manager LCD

let reset_ly t =
    t.stat.ly <- 0;
    let is_eq = t.stat.ly = t.stat.lyc in
    t.stat.ly_eq_lyc_flag <- is_eq;
    if is_eq && t.stat.ly_eq_lyc_interupt_enable then
      Interrupt_manager.request_interrupt t.interrupt_manager LCD

let get_ly t = t.stat.ly
let set_ly_eq_lyc t b = t.stat.ly_eq_lyc_flag <- b
let get_scroll t = (Uint8.to_int t.stat.scx, Uint8.to_int t.stat.scy)
let get_win_pos t = (Uint8.to_int t.stat.winx, Uint8.to_int t.stat.winy)
let ppu_is_enabled t = t.control.lcd_ppu_enable
let get_bg_tile_map t = t.control.bg_tile_map
let get_bg_win_access_mode t = t.control.bg_win_tile_data
let is_bg_win_enable t = t.control.bg_win_enable
let is_win_enable t = t.control.win_enable
let is_obj_enable t = t.control.obj_enable
let get_win_tile_map t = t.control.win_tile_map
let get_obj_height t = if t.control.obj_size = `OBJ_size_8x8 then 8 else 16

let read_control control =
    Bool.to_int control.bg_win_enable
    lor (Bool.to_int control.obj_enable lsl 1)
    lor (match control.obj_size with
        | `OBJ_size_8x8 -> 0
        | `OBJ_size_8x16 -> 1 lsl 2)
    lor (match control.bg_tile_map with
        | Map_0 -> 0
        | Map_1 -> 1 lsl 3)
    lor (match control.bg_win_tile_data with
        | Mode_8800 -> 0
        | Mode_8000 -> 1 lsl 4)
    lor (Bool.to_int control.win_enable lsl 5)
    lor (match control.win_tile_map with
        | Map_0 -> 0
        | Map_1 -> 1 lsl 6)
    lor (Bool.to_int control.lcd_ppu_enable lsl 7)
    |> Uint8.of_int

let read_stat stat =
    (match stat.ppu_mode with
    | `HBlank -> 0
    | `VBlank -> 1
    | `OAM_search -> 2
    | `Drawing -> 3)
    lor (Bool.to_int stat.ly_eq_lyc_flag lsl 2)
    lor (Bool.to_int stat.mode_0_interupt_enable lsl 3)
    lor (Bool.to_int stat.mode_1_interupt_enable lsl 4)
    lor (Bool.to_int stat.mode_2_interupt_enable lsl 5)
    lor (Bool.to_int stat.ly_eq_lyc_interupt_enable lsl 6)
    lor 0b1000000
    |> Uint8.of_int

let write_control control data =
    let data = Uint8.to_int data in
    control.bg_win_enable <- data land 0b00000001 <> 0;
    control.obj_enable <- data land 0b00000010 <> 0;
    control.obj_size <- (if data land 0b00000100 <> 0 then `OBJ_size_8x16 else `OBJ_size_8x8);
    control.bg_tile_map <- (if data land 0b00001000 <> 0 then Map_1 else Map_0);
    control.bg_win_tile_data <- (if data land 0b00010000 <> 0 then Mode_8000 else Mode_8800);
    control.win_enable <- data land 0b00100000 <> 0;
    control.win_tile_map <- (if data land 0b01000000 <> 0 then Map_1 else Map_0);
    control.lcd_ppu_enable <- data land 0b10000000 <> 0

let write_stat stat data =
    let data = Uint8.to_int data in
    (* read only *)
    (* stat.ppu_mode <- *)
    (*   (match data land 0b00000011 with *)
    (*   | 0 -> `HBlank *)
    (*   | 1 -> `VBlank *)
    (*   | 2 -> `OAM_search *)
    (*   | 3 -> `Drawing *)
    (*   | _ -> failwith "Invalid mode"); *)
    (* read only *)
    (* stat.ly_eq_lyc_flag <- data land 0b00000100 <> 0; *)
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
    | addr_int when addr_int = 0xFF44 -> Uint8.of_int t.stat.ly
    | addr_int when addr_int = 0xFF45 -> Uint8.of_int t.stat.lyc
    (* 0xFF46 is for DMA, 0xFF47-0xFF49 are palettes *)
    | addr_int when addr_int = 0xFF4A -> t.stat.winy
    | addr_int when addr_int = 0xFF4B -> t.stat.winx
    | _ -> failwith "Invalid lcd read address"

let write_byte t ~addr ~data =
    match Uint16.to_int addr with
    | addr_int when addr_int = 0xFF40 ->
        let old = t.control.lcd_ppu_enable in
        write_control t.control data;
        if old && not t.control.lcd_ppu_enable then (
          (* It is forbidden by nintendo to disable lcd outside of VBlank so I just check it because it would mean something wrong with my emulator *)
          if not (t.stat.ppu_mode = `VBlank) then (
            Tsdl.Sdl.log "%s\n" (show_mode t.stat.ppu_mode);
            assert false);
          (* when lcd is turned off mode is set to HBlank and LY to 0 *)
          t.stat.ppu_mode <- `HBlank;
          t.stat.ly <- 0)
    | addr_int when addr_int = 0xFF41 -> write_stat t.stat data
    | addr_int when addr_int = 0xFF42 -> t.stat.scy <- data
    | addr_int when addr_int = 0xFF43 -> t.stat.scx <- data
    | addr_int when addr_int = 0xFF44 -> failwith "LY is read only"
    | addr_int when addr_int = 0xFF45 -> t.stat.lyc <- Uint8.to_int data
    (* 0xFF46 is for DMA, 0xFF47-0xFF49 are palettes *)
    | addr_int when addr_int = 0xFF4A -> t.stat.winy <- data
    | addr_int when addr_int = 0xFF4B -> t.stat.winx <- data
    | _ -> failwith "Invalid lcd write address"
