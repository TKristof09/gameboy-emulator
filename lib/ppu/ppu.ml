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
    interrupt_manager : Interrupt_manager.t;
    mutable mcycles_in_current_mode : int;
    framebuffer : Color.t array array;
  }

type frame_progress =
    | In_progress
    | Finished of Color.t array array

let create interrupt_manager =
    {
      bg_palette = Palette.create ~addr:(Uint16.of_int 0xFF47);
      obj_palette_0 = Palette.create ~addr:(Uint16.of_int 0xFF48);
      obj_palette_1 = Palette.create ~addr:(Uint16.of_int 0xFF49);
      tiles = Tile_data.create ();
      tile_map = Tile_map.create ();
      oam = Oam.create ();
      lcd = Lcd.create ();
      interrupt_manager;
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
    | addr_int when addr_int = 0xFF46 -> false (* dma address is handled in the bus *)
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
    let render_bg () =
        let scx, scy = Lcd.get_scroll t.lcd in
        let bg_tile_map = Lcd.get_bg_tile_map t.lcd in
        let access_mode = Lcd.get_bg_win_access_mode t.lcd in
        let y = (scy + ly) mod 256 in
        let tile_y = y mod 8 in
        Seq.init 160 (fun i -> i)
        |> Seq.iter (fun lx ->
               let x = (lx + scx) mod 256 in
               let tile_x = x mod 8 in
               let tile_index = Tile_map.get_tile_index t.tile_map ~x ~y bg_tile_map in
               let color =
                   Tile_data.get_pixel t.tiles access_mode tile_index ~x:tile_x ~y:tile_y
                     t.bg_palette
               in
               t.framebuffer.(lx).(ly) <- color)
    and render_win () =
        let wx, wy = Lcd.get_win_pos t.lcd in
        if wy <= ly && ly <= wy + 256 && wx <= 160 then
          let win_tile_map = Lcd.get_win_tile_map t.lcd in
          let access_mode = Lcd.get_bg_win_access_mode t.lcd in
          let y = ly - wy in
          let tile_y = y mod 8 in
          Seq.init (160 - wx) (fun i -> i)
          |> Seq.iter (fun x ->
                 let tile_x = x mod 8 in
                 let tile_index = Tile_map.get_tile_index t.tile_map ~x ~y win_tile_map in
                 let color =
                     Tile_data.get_pixel t.tiles access_mode tile_index ~x:tile_x ~y:tile_y
                       t.bg_palette
                 in
                 t.framebuffer.(x + wx).(ly) <- color)
    in
    render_bg ();
    if Lcd.is_win_enable t.lcd then render_win ()

let render_objects t ly =
    let open Core in
    let obj_height = Lcd.get_obj_height t.lcd in
    Oam.get_objects t.oam
    |> Array.filter ~f:(fun (o : Oam.obj) ->
           let y = Uint8.to_int o.y in
           ly >= y && ly - y <= obj_height - 1)
    |> Array.iteri ~f:(fun i (o : Oam.obj) ->
           if i >= 10 then
             ()
           else
             let y = ly - Uint8.to_int o.y in
             let palette =
                 match o.palette with
                 | `OBP0 -> t.obj_palette_0
                 | `OBP1 -> t.obj_palette_1
             in
             Seq.init 8 (fun x -> x)
             |> Seq.iter (fun x ->
                    let lx = x + Uint8.to_int o.x in
                    if lx > 0 && lx <= 160 then
                      let color_id =
                          Tile_data.get_pixel_color_id t.tiles Mode_8000 o.tile_index
                            ~x:(if o.x_flip then 7 - x else x)
                            ~y:(if o.y_flip then obj_height - y - 1 else y)
                      in
                      match color_id with
                      | ID_0 -> ()
                      | _ -> (
                          match (o.priority, t.framebuffer.(lx).(ly)) with
                          | `OBJ_prio, _
                          | _, White ->
                              t.framebuffer.(lx).(ly) <- Palette.get_color palette color_id
                          | _ -> ())))

let execute t ~mcycles =
    t.mcycles_in_current_mode <- t.mcycles_in_current_mode + mcycles;
    let hblank_cycles = 51
    and vblank_one_line_cycles = 114
    and oam_search_cycles = 20
    and drawing_cycles = 43 in
    if Lcd.ppu_is_enabled t.lcd then
      match Lcd.get_mode t.lcd with
      | `HBlank ->
          if t.mcycles_in_current_mode >= hblank_cycles then (
            t.mcycles_in_current_mode <- t.mcycles_in_current_mode mod hblank_cycles;
            Lcd.incr_ly t.lcd;
            if Lcd.get_ly t.lcd = 144 then (
              Lcd.set_mode t.lcd `VBlank;
              Interrupt_manager.request_interrupt t.interrupt_manager VBlank)
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
            let ly = Lcd.get_ly t.lcd in
            if Lcd.is_bg_win_enable t.lcd then render_bg_window t ly;
            if Lcd.is_obj_enable t.lcd then render_objects t ly;
            Lcd.set_mode t.lcd `HBlank);
          In_progress
      | `VBlank ->
          if t.mcycles_in_current_mode >= vblank_one_line_cycles then (
            t.mcycles_in_current_mode <- t.mcycles_in_current_mode mod vblank_one_line_cycles;
            if Lcd.get_ly t.lcd < 154 then (
              Lcd.incr_ly t.lcd;
              In_progress)
            else (
              Lcd.reset_ly t.lcd;
              Lcd.set_mode t.lcd `OAM_search;
              Finished t.framebuffer))
          else
            In_progress
    else
      In_progress
