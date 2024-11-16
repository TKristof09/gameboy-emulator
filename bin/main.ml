open Gameboy
open Gameboy.Uint
module Cpu = Cpu.Make (Bus)
open Core
open Tsdl

let orig_width, orig_height = (160, 144)
let scale = 4
let scaled_width, scaled_height = (scale * orig_width, scale * orig_height)
let target_frame_time = Time_ns.Span.of_int_ms 16

let sdl_check = function
    | Error (`Msg e) ->
        Sdl.log "%s" e;
        exit 1
    | Ok x -> x

let render_framebuffer renderer texture fb =
    let copy_framebuffer fb pixels =
        for y = 0 to orig_height - 1 do
          for x = 0 to orig_width - 1 do
            let index = (y * orig_width) + x in
            match fb.(x).(y) with
            | Color.White -> pixels.{index} <- 0xE5FBF4l
            | Color.Light_grey -> pixels.{index} <- 0x97AEB8l
            | Color.Dark_grey -> pixels.{index} <- 0x61687Dl
            | Color.Black -> pixels.{index} <- 0x221E31l
          done
        done
    in
    Sdl.lock_texture texture None Bigarray.int32
    |> sdl_check
    |> fst (* lock_texture returns pixels,pitch *)
    |> copy_framebuffer fb;
    Sdl.unlock_texture texture;
    Sdl.render_copy renderer texture |> sdl_check;
    Sdl.render_present renderer

let main cpu ppu texture renderer =
    let start_time = ref (Time_ns.now ()) in
    while Cpu.get_pc cpu <> 0x100 do
      let c = Cpu.step cpu in
      match Ppu.execute ppu ~mcycles:c with
      | In_progress -> ()
      | Finished framebuffer ->
          render_framebuffer renderer texture framebuffer;
          let duration = Time_ns.diff (Time_ns.now ()) !start_time in
          let open Time_ns.Span in
          if duration < target_frame_time then
            Int32.of_int_exn (to_int_ms (target_frame_time - duration)) |> Sdl.delay;
          start_time := Time_ns.now ()
    done

let () =
    let open Core in
    let boot_rom =
        In_channel.read_all "./roms/dmg_boot.bin" |> Bytes.of_string |> Cartridge.create
    in
    let cartridge = In_channel.read_all "./roms/cavern.gb" |> Bytes.of_string |> Cartridge.create in
    let ppu = Ppu.create () in
    let wram = Ram.create ~start_addr:(Uint16.of_int 0xC000) ~end_addr:(Uint16.of_int 0xDFFF) in
    let hram = Ram.create ~start_addr:(Uint16.of_int 0xFF80) ~end_addr:(Uint16.of_int 0xFFFE) in
    let bus = Bus.create ~ppu ~wram ~hram ~boot_rom ~cartridge in
    let cpu = Cpu.create ~bus in

    Sdl.init Sdl.Init.(video + events) |> sdl_check;
    let win, renderer =
        Sdl.create_window_and_renderer ~w:scaled_width ~h:scaled_height Sdl.Window.windowed
        |> sdl_check
    in
    let texture =
        Sdl.create_texture renderer Sdl.Pixel.format_rgb888 Sdl.Texture.access_streaming
          ~w:orig_width ~h:orig_height
        |> sdl_check
    in

    main cpu ppu texture renderer;

    Sdl.destroy_window win;
    Sdl.destroy_renderer renderer;
    Sdl.destroy_texture texture;
    Sdl.quit ()
