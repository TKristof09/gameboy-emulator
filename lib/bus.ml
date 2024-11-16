open Uint

type t = {
    ppu : Ppu.t;
    wram : Ram.t;
    hram : Ram.t;
    boot_rom : Cartridge.t;
    cartridge : Cartridge.t;
  }

let create ~ppu ~wram ~hram ~boot_rom ~cartridge = { ppu; wram; hram; boot_rom; cartridge }

let read_byte t addr =
    let read_boot_rom = true in
    (*  TODO: this flag has to be read from 0xFF50 *)
    match Uint16.to_int addr with
    | addr_int when read_boot_rom && addr_int <= 0x00FF -> Cartridge.read_byte t.boot_rom addr
    | addr_int when 0x0000 <= addr_int && addr_int <= 0x7FFF -> Cartridge.read_byte t.cartridge addr
    | addr_int when 0xC000 <= addr_int && addr_int <= 0xDFFF -> Ram.read_byte t.wram addr
    | addr_int when 0xFF80 <= addr_int && addr_int <= 0xFFFE -> Ram.read_byte t.hram addr
    | _ when Ppu.accepts_address addr -> Ppu.read_byte t.ppu addr
    | _ ->
        failwith @@ Printf.sprintf "Unhandled memory location read %s" (Uint16.to_string_hex addr)

let write_byte t ~addr ~data =
    match Uint16.to_int addr with
    | addr_int when 0x0000 <= addr_int && addr_int <= 0x7FFF ->
        Cartridge.write_byte t.cartridge ~addr ~data
    | addr_int when 0xC000 <= addr_int && addr_int <= 0xDFFF -> Ram.write_byte t.wram ~addr ~data
    | addr_int when 0xFF80 <= addr_int && addr_int <= 0xFFFE -> Ram.write_byte t.hram ~addr ~data
    | _ when Ppu.accepts_address addr -> Ppu.write_byte t.ppu ~addr ~data
    | _ -> Printf.printf "Unhandled memory location write %s\n" (Uint16.to_string_hex addr)

let read_word t addr =
    let lo = read_byte t addr in
    let hi = read_byte t (Uint16.succ addr) in
    uint16_of_uint8s ~low:lo ~high:hi

let write_word t ~addr ~data =
    let lo, hi = uint8s_of_uint16 data in
    write_byte t ~addr ~data:lo;
    write_byte t ~addr:(Uint16.succ addr) ~data:hi
