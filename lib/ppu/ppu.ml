open Uint
open StdLabels

type t = {
    bg_palette : Palette.t;
    obj_palette_0 : Palette.t;
    obj_palette_1 : Palette.t;
    tiles : Tile_data.t;
    tile_map : Tile_map.t;
    oam : Oam.t;
    lcd : Lcd.t;
    mutable mcycles_in_current_mode : int;
    framebuffer : Color.t array array;
  }

type frame_progress =
    | In_progress
    | Finished of Color.t array array

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
      framebuffer = Array.make_matrix ~dimx:160 ~dimy:144 Color.White;
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

let render_bg_window t ly =
    let scx, scy = Lcd.get_scroll t.lcd in
    let bg_tile_map = Lcd.get_bg_tile_map t.lcd in
    let access_mode = Lcd.get_bg_win_access_mode t.lcd in
    let y = (Uint8.to_int scy + ly) mod 256 in
    let tile_y = y mod 8 in
    let lx = ref 0 in
    while !lx < 160 do
      let x = (!lx + Uint8.to_int scx) mod 256 in
      let tile_x = x mod 8 in
      let tile_index = Tile_map.get_tile_index t.tile_map ~x ~y bg_tile_map in
      let color =
          Tile_data.get_pixel t.tiles access_mode tile_index ~x:tile_x ~y:tile_y t.bg_palette
      in
      t.framebuffer.(!lx).(ly) <- color;
      incr lx
    done

let execute t ~mcycles =
    t.mcycles_in_current_mode <- t.mcycles_in_current_mode + mcycles;
    let hblank_cycles = 51
    and vblank_cycles = 1140
    and oam_search_cycles = 20
    and drawing_cycles = 43 in
    if Lcd.ppu_is_enabled t.lcd then
      match Lcd.get_mode t.lcd with
      | `HBlank ->
          if t.mcycles_in_current_mode >= hblank_cycles then (
            t.mcycles_in_current_mode <- t.mcycles_in_current_mode mod hblank_cycles;
            Lcd.incr_ly t.lcd;
            if Lcd.get_ly t.lcd = 144 then
              Lcd.set_mode t.lcd `VBlank
            else
              Lcd.set_mode t.lcd `OAM_search);
          In_progress
      | `OAM_search ->
          if t.mcycles_in_current_mode >= oam_search_cycles then (
            t.mcycles_in_current_mode <- t.mcycles_in_current_mode mod oam_search_cycles;
            Lcd.set_mode t.lcd `Drawing);
          In_progress
      | `Drawing ->
          if t.mcycles_in_current_mode >= drawing_cycles then (
            t.mcycles_in_current_mode <- t.mcycles_in_current_mode mod drawing_cycles;
            (if Lcd.is_bg_win_enable t.lcd then
               let ly = Lcd.get_ly t.lcd in
               render_bg_window t ly);
            Lcd.set_mode t.lcd `HBlank);
          In_progress
      | `VBlank ->
          if t.mcycles_in_current_mode >= vblank_cycles then (
            t.mcycles_in_current_mode <- t.mcycles_in_current_mode mod vblank_cycles;
            Lcd.reset_ly t.lcd;
            Lcd.set_mode t.lcd `OAM_search;
            Finished t.framebuffer)
          else
            In_progress
    else
      In_progress
