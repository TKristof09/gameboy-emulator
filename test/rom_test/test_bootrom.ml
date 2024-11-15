open Gameboy
open Gameboy.Uint
open Core
module Cpu = Cpu.Make (Bus)

let%expect_test "test loading bootrom" =
    let boot_rom =
        In_channel.read_all "../../roms/dmg_boot.bin" |> Bytes.of_string |> Cartridge.create
    in
    let cartridge =
        In_channel.read_all "../../roms/cavern.gb" |> Bytes.of_string |> Cartridge.create
    in
    let ppu = Ppu.create () in
    let wram = Ram.create ~start_addr:(Uint16.of_int 0xC000) ~end_addr:(Uint16.of_int 0xDFFF) in
    let hram = Ram.create ~start_addr:(Uint16.of_int 0xFF80) ~end_addr:(Uint16.of_int 0xFFFE) in
    let vram = Ram.create ~start_addr:(Uint16.of_int 0x8000) ~end_addr:(Uint16.of_int 0x9FFF) in
    let bus = Bus.create ~ppu ~wram ~hram ~vram ~boot_rom ~cartridge in
    let cpu = Cpu.create ~bus in
    while Cpu.get_pc cpu <> 0x100 do
      let c = Cpu.step cpu in
      Ppu.execute ppu ~mcycles:c
    done;
    Cpu.show cpu |> print_endline;
    (* We don't have these registers yet, all of these apart from 0xFF50 are sound related *)
    [%expect
        {|
      Unhandled memory location write 0xff26
      Unhandled memory location write 0xff11
      Unhandled memory location write 0xff12
      Unhandled memory location write 0xff25
      Unhandled memory location write 0xff24
      Unhandled memory location write 0xff13
      Unhandled memory location write 0xff14
      Unhandled memory location write 0xff13
      Unhandled memory location write 0xff14
      Unhandled memory location write 0xff50
      SP:0xfffe PC:0x100 REG:A: 0x1 B: 0x0 C: 0x13 D: 0x0 E: 0xd8 F: 0xb0 H: 0x1 L: 0x4d
      |}]
