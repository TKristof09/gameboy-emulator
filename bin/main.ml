open Gameboy
module Cpu = Cpu.Make (Bus)
open Core
open Tsdl
open Tsdl_ttf

let orig_width, orig_height = (160, 144)
let scale = 4
let scaled_width, scaled_height = (scale * orig_width, scale * orig_height)
(* let target_frame_time = Time_ns.Span.of_int_ms 16 *)

let sdl_check = function
    | Error (`Msg e) ->
        Sdl.log "%s" e;
        exit 1
    | Ok x -> x

let font =
    Ttf.init () |> sdl_check;
    Ttf.open_font "resources/DejaVuSans.ttf" 13 |> sdl_check

let render_text renderer text ~x ~y =
    let color = Sdl.Color.create ~r:0 ~g:0 ~b:0 ~a:255 in
    let surface = Ttf.render_text_solid font text color |> sdl_check in
    let text_texture = Sdl.create_texture_from_surface renderer surface |> sdl_check in
    let _, _, (w, h) = Sdl.query_texture text_texture |> sdl_check in
    Sdl.set_texture_blend_mode text_texture Sdl.Blend.mode_none |> sdl_check;
    let text_rect = Sdl.Rect.create ~x ~y ~w ~h in
    (* Sdl.render_fill_rect renderer (Some text_rect) |> sdl_check; *)
    Sdl.render_copy renderer ~dst:text_rect text_texture |> sdl_check;
    Sdl.free_surface surface

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
    Sdl.render_copy renderer texture |> sdl_check

let main cpu ppu joypad timer texture renderer =
    let pause = ref false in
    let step = ref false in
    let bp = ref None in
    let pause_emu () =
        render_text renderer "--PAUSED--" ~x:0 ~y:10;
        Sdl.render_present renderer;
        pause := true
    in
    let handle_events () =
        let ev = Sdl.Event.create () in
        while Sdl.poll_event (Some ev) do
          let ev_type = Sdl.Event.(get ev typ) in
          match Sdl.Event.enum ev_type with
          | `Key_down -> (
              let scancode = Sdl.Event.(get ev keyboard_scancode) |> Sdl.Scancode.enum in
              match scancode with
              | `P ->
                  pause := not !pause;
                  if !pause then pause_emu ()
              | `Comma ->
                  render_text renderer (Printf.sprintf "%s" (Cpu.show cpu)) ~x:0 ~y:60;
                  Sdl.render_present renderer
              | `Period ->
                  step := true;
                  render_text renderer (Printf.sprintf "%s" (Cpu.show cpu)) ~x:0 ~y:60;
                  Sdl.render_present renderer
              | `B -> (
                  pause_emu ();
                  Sdl.log "Enter breakpoint\n";
                  Out_channel.(flush stdout);
                  let line = In_channel.(input_line_exn stdin) in
                  bp := Scanf.sscanf_opt line " 0x%x" (fun x -> x);
                  match !bp with
                  | None -> Sdl.log "Breakpoint cleared\n"
                  | Some x -> Sdl.log "Set bp to 0x%X\n" x)
              | `D -> Joypad.press joypad Up
              | `T -> Joypad.press joypad Down
              | `R -> Joypad.press joypad Left
              | `S -> Joypad.press joypad Right
              | `H -> Joypad.press joypad A
              | `A -> Joypad.press joypad B
              | `E -> Joypad.press joypad Start
              | `I -> Joypad.press joypad Select
              | `Escape -> exit 0
              | _ -> ())
          | `Key_up -> (
              let scancode = Sdl.Event.(get ev keyboard_scancode) |> Sdl.Scancode.enum in
              match scancode with
              | `D -> Joypad.release joypad Up
              | `T -> Joypad.release joypad Down
              | `R -> Joypad.release joypad Left
              | `S -> Joypad.release joypad Right
              | `H -> Joypad.release joypad A
              | `A -> Joypad.release joypad B
              | `E -> Joypad.release joypad Start
              | `I -> Joypad.release joypad Select
              | _ -> ())
          | `Mouse_wheel ->
              step := true;
              render_text renderer (Printf.sprintf "%s" (Cpu.show cpu)) ~x:0 ~y:60;
              Sdl.render_present renderer
          | `Quit ->
              print_endline "Quitting...";
              exit 0
          | _ -> ()
        done
    in
    let start_time = ref (Time_ns.now ()) in
    let i = ref 0 in
    let fps = ref 0 in
    while true do
      while !pause && not !step do
        handle_events ();
        render_text renderer "--PAUSED--" ~x:0 ~y:10;
        Out_channel.flush Out_channel.stdout;
        Sdl.delay 16l (* 16ms *)
      done;
      (* handle_events (); *)
      let c, instr, pc = Cpu.step cpu in
      (* if pc = 0x16e then pause_emu (); *)
      step := false;
      (match instr with
      (* | LD8 (Reg8 D, Imm8 v) when Uint.Uint8.to_int v = 0x42 -> pause_emu () *)
      (* | RET Z -> pause_emu () *)
      (* | LD8 (Reg8 B, Reg8 B) -> pause_emu () *)
      | _ -> ());
      (match !bp with
      | None -> ()
      | Some x -> if pc = x then pause_emu ());
      (* Printf.printf "PC: %#x  - %s: %s\n" pc (Instruction.show instr) (Cpu.show cpu); *)
      Timer.run timer ~mcycles:c;
      (* Sdl.log "%s\n" (Timer.show timer); *)
      (* Sdl.log "----------------------\n"; *)
      match Ppu.execute ppu ~mcycles:c with
      | In_progress -> step := false
      | Off -> handle_events ()
      | Finished framebuffer ->
          handle_events ();
          render_framebuffer renderer texture framebuffer;
          (* render_text renderer *)
          (*   (Printf.sprintf "PC: %#x  - %s" pc (Instruction.show instr)) *)
          (*   ~x:0 ~y:20; *)
          (* let open Time_ns.Span in *)
          (* let duration = Time_ns.diff (Time_ns.now ()) !start_time in *)
          (* if duration < target_frame_time then *)
          (*   Int32.of_int_exn (to_int_ms (target_frame_time - duration)) |> Sdl.delay; *)
          let open Int in
          fps := !fps + Time_ns.Span.to_int_ms (Time_ns.diff (Time_ns.now ()) !start_time);
          incr i;

          render_text renderer
            (Printf.sprintf "FPS: %.0f" (Int.to_float !i *. 1000.0 /. Int.to_float !fps))
            (* (Printf.sprintf "Frametime: %d ms" (!fps / !i)) *)
            ~x:0 ~y:40;

          if !i = 10 then (
            i := 0;
            fps := 0);
          start_time := Time_ns.now ();
          Sdl.render_present renderer;
          step := false
    done

let () =
    let open Core in
    let boot_rom = In_channel.read_all "./roms/dmg_boot.bin" in
    let boot_rom = Bigstringaf.of_string ~off:0 ~len:(String.length boot_rom) boot_rom in
    let cartridge =
        (* In_channel.read_all "./test/resources/mooneye/emulator-only/mbc1/bits_bank1.gb" *)
        (* In_channel.read_all "./test/resources/mooneye/emulator-only/mbc1/bits_bank2.gb" *)
        (* In_channel.read_all "./test/resources/mooneye/emulator-only/mbc1/bits_mode.gb" *)
        (* In_channel.read_all "./test/resources/mooneye/emulator-only/mbc1/bits_ramg.gb" *)
        (* In_channel.read_all "./test/resources/mooneye/emulator-only/mbc1/multicart_rom_8Mb.gb" *)
        (* In_channel.read_all "./test/resources/mooneye/emulator-only/mbc1/ram_64kb.gb" *)
        (* In_channel.read_all "./test/resources/mooneye/emulator-only/mbc1/ram_256kb.gb" *)
        (* In_channel.read_all "./test/resources/mooneye/emulator-only/mbc1/rom_1Mb.gb" *)
        (* In_channel.read_all "./test/resources/mooneye/emulator-only/mbc1/rom_2Mb.gb" *)
        (* In_channel.read_all "./test/resources/mooneye/emulator-only/mbc1/rom_4Mb.gb" *)
        (* In_channel.read_all "./test/resources/mooneye/emulator-only/mbc1/rom_8Mb.gb" *)
        (* In_channel.read_all "./test/resources/mooneye/emulator-only/mbc1/rom_16Mb.gb" *)
        (* In_channel.read_all "./test/resources/mooneye/emulator-only/mbc1/rom_512kb.gb" *)
        (* In_channel.read_all "roms/SuperMarioLand.gb" *)
        (* In_channel.read_all "./test/resources/mbc3test.gb" *)
        In_channel.read_all "./roms/PokemonRed.gb"
    in
    let cartridge =
        Bigstringaf.of_string ~off:0 ~len:(String.length cartridge) cartridge |> Cartridge.create
    in
    Tsdl.Sdl.log "%s\n" (Cartridge.show cartridge);
    let open Gameboy.Uint in
    let wram = Ram.create ~start_addr:(Uint16.of_int 0xC000) ~end_addr:(Uint16.of_int 0xDFFF) in
    let hram = Ram.create ~start_addr:(Uint16.of_int 0xFF80) ~end_addr:(Uint16.of_int 0xFFFE) in
    let interrupt_manager = Interrupt_manager.create () in
    let ppu = Ppu.create interrupt_manager in
    let joypad = Joypad.create interrupt_manager in
    let timer = Timer.create interrupt_manager in
    let bus = Bus.create ~ppu ~wram ~hram ~boot_rom ~cartridge ~interrupt_manager ~joypad ~timer in
    let cpu = Cpu.create ~bus ~interrupt_manager in

    (*
    TODO: PPU state isn't set upcorrectly when skipping bootrom
    Mode needs to be VBLank and perhaps other stuff, need to check
    *)
    Cpu.skip_boot_rom cpu;
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

    main cpu ppu joypad timer texture renderer;

    Sdl.destroy_window win;
    Sdl.destroy_renderer renderer;
    Sdl.destroy_texture texture;
    Sdl.quit ()
