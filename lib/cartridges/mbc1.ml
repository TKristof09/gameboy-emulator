open Uint

type t = {
    rom_bytes : Bigstringaf.t;
    ram_bytes : Bigstringaf.t;
    mutable rom_bank_num : int;
    num_rom_banks : int;
    mutable ram_enabled : bool;
    mutable ram_bank_num : int;
    num_ram_banks : int;
  }

let create bytes (header : Cartridge_header.t) =
    let num_rom_banks = header.rom_size in
    let num_ram_banks = header.ram_size in
    {
      rom_bytes = bytes;
      ram_bytes = Bigstringaf.create (num_ram_banks * 0x2000);
      rom_bank_num = 0;
      num_rom_banks;
      ram_enabled = false;
      ram_bank_num = 0;
      num_ram_banks;
    }

let read_byte t addr =
    let addr_int = Uint16.to_int addr in
    match addr_int with
    | _ when 0x0000 <= addr_int && addr_int <= 0x3FFF ->
        Bigstringaf.unsafe_get t.rom_bytes addr_int |> uint8_of_char
    | _ when 0x4000 <= addr_int && addr_int <= 0x7FFF ->
        let offset = t.rom_bank_num * 0x4000 in
        Bigstringaf.unsafe_get t.rom_bytes (offset + addr_int - 0x4000) |> uint8_of_char
    | _ -> failwith "Unimplemented"

let mask_rom_bank_num t num =
    num
    land
    match t.num_rom_banks with
    | 2 -> 0b1
    | 4 -> 0b11
    | 8 -> 0b111
    | 16 -> 0b1111
    | 32 -> 0b11111
    | _ -> failwith @@ Printf.sprintf "Unsupported size %d" t.num_rom_banks

let write_byte t ~addr ~data =
    let data = Uint8.to_int data in
    match Uint16.to_int addr with
    | addr_int when 0x2000 <= addr_int && addr_int <= 0x3FFF ->
        let bank_num = mask_rom_bank_num t data in
        t.rom_bank_num <- (if bank_num = 0 then 1 else bank_num)
    | _ -> Tsdl.Sdl.log "Writing to MBC1 not implemented yet: %s\n" (Uint16.to_string_hex addr)
