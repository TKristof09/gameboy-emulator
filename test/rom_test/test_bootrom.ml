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
    let wram = Ram.create ~start_addr:(Uint16.of_int 0xC000) ~end_addr:(Uint16.of_int 0xDFFF) in
    let hram = Ram.create ~start_addr:(Uint16.of_int 0xFF80) ~end_addr:(Uint16.of_int 0xFFFE) in
    let vram = Ram.create ~start_addr:(Uint16.of_int 0x8000) ~end_addr:(Uint16.of_int 0x9FFF) in
    let bus = Bus.create ~wram ~hram ~vram ~boot_rom ~cartridge in
    let cpu = Cpu.create ~bus in
    let i = ref 0 in
    while !i < 100000 do
      Cpu.step cpu;
      incr i
    done;
    Cpu.show cpu |> print_endline;
    [%expect {||}]
