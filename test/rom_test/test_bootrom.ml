open Gameboy
open Gameboy.Uint
open Core
module Cpu = Cpu.Make (Bus)

let%expect_test "test loading bootrom" =
    let boot_rom = In_channel.read_all "../../roms/dmg_boot.bin" in
    let boot_rom =
        Bigstringaf.of_string ~off:0 ~len:(String.length boot_rom) boot_rom |> Cartridge.create
    in
    let cartridge = In_channel.read_all "../../roms/tetris.gb" in
    let cartridge =
        Bigstringaf.of_string ~off:0 ~len:(String.length cartridge) cartridge |> Cartridge.create
    in
    let wram = Ram.create ~start_addr:(Uint16.of_int 0xC000) ~end_addr:(Uint16.of_int 0xDFFF) in
    let hram = Ram.create ~start_addr:(Uint16.of_int 0xFF80) ~end_addr:(Uint16.of_int 0xFFFE) in
    let interrupt_manager = Interrupt_manager.create () in
    let ppu = Ppu.create interrupt_manager in
    let joypad = Joypad.create interrupt_manager in
    let timer = Timer.create interrupt_manager in
    let bus = Bus.create ~ppu ~wram ~hram ~boot_rom ~cartridge ~interrupt_manager ~joypad ~timer in
    let cpu = Cpu.create ~bus ~interrupt_manager in
    while fst @@ Cpu.get_pc cpu <> 0x100 do
      let c = Cpu.step cpu in
      let _ = Ppu.execute ppu ~mcycles:c in
      ()
    done;
    Cpu.show cpu |> print_endline;
    print_endline (Uint8.to_string (Bus.read_byte bus (Uint16.of_int 0xFF50)));
    (* We don't have these registers yet, all of these apart from 0xFF50 are sound related *)
    [%expect
        {|
      INFO: Unhandled memory location write 0xff26
      INFO: Unhandled memory location write 0xff11
      INFO: Unhandled memory location write 0xff12
      INFO: Unhandled memory location write 0xff25
      INFO: Unhandled memory location write 0xff24
      INFO: Unhandled memory location write 0xff13
      INFO: Unhandled memory location write 0xff14
      INFO: Unhandled memory location write 0xff13
      INFO: Unhandled memory location write 0xff14
      SP:0xfffe PC:0x100 REG:A: 0x1 B: 0x0 C: 0x13 D: 0x0 E: 0xd8 F: 0xb0 H: 0x1 L: 0x4d
      1
      |}]
