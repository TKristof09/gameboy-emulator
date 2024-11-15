open Uint

type t = {
    bg_palette : Palette.t;
    obj_palette_0 : Palette.t;
    obj_palette_1 : Palette.t;
    tiles : Tile_data.t;
    tile_map : Tile_map.t;
    oam : Oam.t;
    lcd : Lcd.t;
    mutable mcycles_in_current_mode : int;
  }

let create () =
    {
      bg_palette = Palette.create ~addr:(Uint16.of_int 0xFF47);
      obj_palette_0 = Palette.create ~addr:(Uint16.of_int 0xFF48);
      obj_palette_1 = Palette.create ~addr:(Uint16.of_int 0xFF49);
      tiles = Tile_data.create ();
      tile_map = Tile_map.create ();
      oam = Oam.create ();
      lcd = Lcd.create ();
      mcycles_in_current_mode = 0;
    }

let accepts_address addr =
    match Uint16.to_int addr with
    | addr_int when 0x8000 <= addr_int && addr_int <= 0x97FF -> true
    | addr_int when 0x9800 <= addr_int && addr_int <= 0x9FFF -> true
    | addr_int when 0xFE00 <= addr_int && addr_int <= 0xFE9F -> true
    | addr_int when addr_int = 0xFF47 -> true
    | addr_int when addr_int = 0xFF48 -> true
    | addr_int when addr_int = 0xFF49 -> true
    (* this is safe for the lcd since the palette addresses are already handled *)
    | addr_int when 0xFF40 <= addr_int && addr_int <= 0xFF4B -> true
    | _ -> false

let read_byte t addr =
    match Uint16.to_int addr with
    | addr_int when 0x8000 <= addr_int && addr_int <= 0x97FF -> Tile_data.read_byte t.tiles addr
    | addr_int when 0x9800 <= addr_int && addr_int <= 0x9FFF -> Tile_map.read_byte t.tile_map addr
    | addr_int when 0xFE00 <= addr_int && addr_int <= 0xFE9F -> Oam.read_byte t.oam addr
    | addr_int when addr_int = 0xFF47 -> Palette.read_byte t.bg_palette addr
    | addr_int when addr_int = 0xFF48 -> Palette.read_byte t.obj_palette_0 addr
    | addr_int when addr_int = 0xFF49 -> Palette.read_byte t.obj_palette_1 addr
    (* this is safe for the lcd since the palette addresses are already handled *)
    | addr_int when 0xFF40 <= addr_int && addr_int <= 0xFF4B -> Lcd.read_byte t.lcd addr
    | _ -> failwith "Invalid gpu mem read address"

let write_byte t ~addr ~data =
    match Uint16.to_int addr with
    | addr_int when 0x8000 <= addr_int && addr_int <= 0x97FF ->
        Tile_data.write_byte t.tiles ~addr ~data
    | addr_int when 0x9800 <= addr_int && addr_int <= 0x9FFF ->
        Tile_map.write_byte t.tile_map ~addr ~data
    | addr_int when 0xFE00 <= addr_int && addr_int <= 0xFE9F -> Oam.write_byte t.oam ~addr ~data
    | addr_int when addr_int = 0xFF47 -> Palette.write_byte t.bg_palette ~addr ~data
    | addr_int when addr_int = 0xFF48 -> Palette.write_byte t.obj_palette_0 ~addr ~data
    | addr_int when addr_int = 0xFF49 -> Palette.write_byte t.obj_palette_1 ~addr ~data
    (* this is safe for the lcd since the palette addresses are already handled *)
    | addr_int when 0xFF40 <= addr_int && addr_int <= 0xFF4B -> Lcd.write_byte t.lcd ~addr ~data
    | _ -> failwith "Invalid gpu mem write address"

let execute t ~mcycles =
    t.mcycles_in_current_mode <- t.mcycles_in_current_mode + mcycles;
    let mode0_cycles = 51
    and mode1_cycles = 1140
    and mode2_cycles = 20
    and mode3_cycles = 43 in
    if Lcd.ppu_is_enabled t.lcd then
      match Lcd.get_mode t.lcd with
      | `Mode_0 ->
          if t.mcycles_in_current_mode >= mode0_cycles then (
            t.mcycles_in_current_mode <- t.mcycles_in_current_mode mod mode0_cycles;
            Lcd.incr_ly t.lcd;
            if Lcd.get_ly t.lcd = 144 then
              Lcd.set_mode t.lcd `Mode_1
            else
              Lcd.set_mode t.lcd `Mode_2)
      | `Mode_2 ->
          if t.mcycles_in_current_mode >= mode2_cycles then (
            t.mcycles_in_current_mode <- t.mcycles_in_current_mode mod mode2_cycles;
            Lcd.set_mode t.lcd `Mode_3)
      | `Mode_3 ->
          if t.mcycles_in_current_mode >= mode3_cycles then (
            t.mcycles_in_current_mode <- t.mcycles_in_current_mode mod mode3_cycles;
            Lcd.set_mode t.lcd `Mode_0)
      | `Mode_1 ->
          if t.mcycles_in_current_mode >= mode1_cycles then (
            t.mcycles_in_current_mode <- t.mcycles_in_current_mode mod mode1_cycles;
            Lcd.reset_ly t.lcd;
            Lcd.set_mode t.lcd `Mode_2)
