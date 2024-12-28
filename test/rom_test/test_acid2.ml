open Gameboy
module Cpu = Cpu.Make (Bus)
open Core

let orig_width, orig_height = (160, 144)
let image = Array.make_matrix ~dimx:orig_width ~dimy:orig_height 0xFF

let render_framebuffer fb =
    for y = 0 to orig_height - 1 do
      for x = 0 to orig_width - 1 do
        let color =
            (* for some reason Imagelib rescales the color values instead of keeping the hex values, so these are the pixel values in the image instead of the ones given in the acid2 readme *)
            match fb.(x).(y) with
            | Color.White -> 3
            | Color.Light_grey -> 2
            | Color.Dark_grey -> 1
            | Color.Black -> 0
        in
        image.(x).(y) <- color
      done
    done

let compare_screenshot () =
    let reference = ImageLib_unix.openfile "../resources/dmg-acid2-reference.png" in
    let get_reference x y = Image.read_grey reference x y Fun.id in
    for y = 0 to orig_height - 1 do
      for x = 0 to orig_width - 1 do
        let expected = get_reference x y in
        if image.(x).(y) <> expected then
          failwith @@ Printf.sprintf "%d,%d: Expected: %d -- Got: %d" x y expected image.(x).(y)
      done
    done

let%test_unit "Test acid2" =
    let open Core in
    let boot_rom = In_channel.read_all "../../roms/dmg_boot.bin" in
    let boot_rom = Bigstringaf.of_string ~off:0 ~len:(String.length boot_rom) boot_rom in
    let cartridge = In_channel.read_all "../resources/dmg-acid2.gb" in
    let cartridge =
        Bigstringaf.of_string ~off:0 ~len:(String.length cartridge) cartridge |> Cartridge.create
    in
    let open Gameboy.Uint in
    let wram = Ram.create ~start_addr:(Uint16.of_int 0xC000) ~end_addr:(Uint16.of_int 0xDFFF) in
    let hram = Ram.create ~start_addr:(Uint16.of_int 0xFF80) ~end_addr:(Uint16.of_int 0xFFFE) in
    let interrupt_manager = Interrupt_manager.create () in
    let ppu = Ppu.create interrupt_manager in
    let joypad = Joypad.create interrupt_manager in
    let timer = Timer.create interrupt_manager in
    let bus = Bus.create ~ppu ~wram ~hram ~boot_rom ~cartridge ~interrupt_manager ~joypad ~timer in
    let cpu = Cpu.create ~bus ~interrupt_manager in

    let finished = ref false in
    while not !finished do
      let c, instr, _ = Cpu.step cpu in
      (match instr with
      | LD8 (Reg8 B, Reg8 B) ->
          compare_screenshot ();
          finished := true
      | _ -> ());
      Timer.run timer ~mcycles:c;
      match Ppu.execute ppu ~mcycles:c with
      | In_progress -> ()
      | Off -> ()
      | Finished framebuffer -> render_framebuffer framebuffer
    done
