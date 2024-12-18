open Uint

(* https://hacktix.github.io/GBEDG/mbcs/mbc1/ *)

type t = {
    rom_bytes : Bigstringaf.t; [@opaque]
    ram_bytes : Bigstringaf.t; [@opaque]
    mutable rom_bank_num : int;
    num_rom_banks : int;
    mutable ram_enabled : bool;
    mutable ram_bank_num : int;
    num_ram_banks : int;
    mutable bank_mode : [ `Simple | `Advanced ];
  }
[@@deriving show]

let create bytes (header : Cartridge_header.t) =
    let num_rom_banks = header.rom_size in
    let num_ram_banks = header.ram_size in
    {
      rom_bytes = bytes;
      ram_bytes = Bigstringaf.create (num_ram_banks * 0x2000);
      rom_bank_num = 1;
      num_rom_banks;
      ram_enabled = false;
      ram_bank_num = 0;
      num_ram_banks;
      bank_mode = `Simple;
    }

let mask_rom_bank_num t num =
    num
    land
    match t.num_rom_banks with
    | 2 -> 0b1
    | 4 -> 0b11
    | 8 -> 0b111
    | 16 -> 0b1111
    | 32
    | 64
    | 128 ->
        0b11111
    | _ -> failwith @@ Printf.sprintf "Unsupported size %d" t.num_rom_banks

let get_rom_0_bank_num t =
    match t.bank_mode with
    | `Simple -> 0
    | `Advanced ->
        let n = t.num_rom_banks in
        if n <= 32 then
          0
        else if n <= 64 then
          (t.ram_bank_num land 0b1) lsl 5
        else
          (t.ram_bank_num land 0b11) lsl 5

let get_high_bank_num t =
    let n = t.num_rom_banks in
    if n <= 32 then
      t.rom_bank_num land mask_rom_bank_num t t.rom_bank_num
    else if n <= 64 then
      let extra = (t.ram_bank_num land 0b1) lsl 5 in
      t.rom_bank_num land mask_rom_bank_num t t.rom_bank_num lor extra
    else
      let extra = (t.ram_bank_num land 0b11) lsl 5 in
      t.rom_bank_num land mask_rom_bank_num t t.rom_bank_num lor extra

let get_ram_address t addr =
    match (t.bank_mode, t.num_ram_banks) with
    | `Simple, _
    | `Advanced, 1 ->
        addr - (0xA000 mod 0x2000)
    | `Advanced, 4 -> (0x2000 * t.ram_bank_num) + (addr - 0xA000)
    | _ -> failwith "Invalid RAM configuration"

let read_byte t addr =
    let addr_int = Uint16.to_int addr in
    match addr_int with
    | _ when 0x0000 <= addr_int && addr_int <= 0x3FFF ->
        let rom_0_addr = (0x4000 * get_rom_0_bank_num t) + addr_int in
        Bigstringaf.unsafe_get t.rom_bytes rom_0_addr |> uint8_of_char
    | _ when 0x4000 <= addr_int && addr_int <= 0x7FFF ->
        let bank_num = get_high_bank_num t in
        let offset = bank_num * 0x4000 in
        Bigstringaf.unsafe_get t.rom_bytes (offset + addr_int - 0x4000) |> uint8_of_char
    | addr_int when 0xA000 <= addr_int && addr_int <= 0xBFFF ->
        if t.ram_enabled && t.num_ram_banks > 0 then
          let ram_addr = get_ram_address t addr_int in
          Bigstringaf.unsafe_get t.ram_bytes ram_addr |> uint8_of_char
        else
          Uint8.max_int
    | _ -> failwith "Unimplemented"

let write_byte t ~addr ~data =
    let data = Uint8.to_int data in
    match Uint16.to_int addr with
    | addr_int when 0x0000 <= addr_int && addr_int <= 0x1FFF ->
        let enable = data land 0x0F = 0xA in
        t.ram_enabled <- enable
    | addr_int when 0x2000 <= addr_int && addr_int <= 0x3FFF ->
        let bank_num = mask_rom_bank_num t data in
        t.rom_bank_num <- (if data = 0 then 1 else bank_num)
    | addr_int when 0x4000 <= addr_int && addr_int <= 0x5FFF -> t.ram_bank_num <- data land 0b11
    | addr_int when 0x6000 <= addr_int && addr_int <= 0x7FFF ->
        t.bank_mode <- (if data land 0b1 = 0 then `Simple else `Advanced)
    | addr_int when 0xA000 <= addr_int && addr_int <= 0xBFFF ->
        if t.ram_enabled && t.num_ram_banks > 0 then
          let ram_addr = get_ram_address t addr_int in
          Bigstringaf.unsafe_set t.ram_bytes ram_addr (Char.unsafe_chr data)
    | _ -> Tsdl.Sdl.log "Writing to MBC1 not implemented yet: %s\n" (Uint16.to_string_hex addr)
