open Uint

(* https://hacktix.github.io/GBEDG/mbcs/mbc3/ *)
type rtc_reg =
    | S
    | M
    | H
    | DL
    | DH
[@@deriving show]

type t = {
    rom_bytes : Bigstringaf.t; [@opaque]
    ram_bytes : Bigstringaf.t; [@opaque]
    num_ram_banks : int;
    num_rom_banks : int;
    mutable mapped_rom_bank : int;
    mutable misc_enabled : bool;
    mutable mapped_misc_bank : [ `RAM of int | `RTC of rtc_reg ];
    mutable timer_enabled : bool;
  }
[@@deriving show]

let try_load_ram (header : Cartridge_header.t) =
    if not header.has_battery then
      Bigstringaf.create (header.ram_size * 0x2000)
    else
      try
        let filename = Printf.sprintf "./ram_files/%s.bin" header.title in
        let bytes = Core.In_channel.read_all filename in
        Bigstringaf.of_string ~off:0 ~len:(String.length bytes) bytes
      with
      | _ -> Bigstringaf.create (header.ram_size * 0x2000)

let create bytes (header : Cartridge_header.t) =
    let num_rom_banks = header.rom_size in
    let num_ram_banks = header.ram_size in
    {
      rom_bytes = bytes;
      ram_bytes = try_load_ram header;
      num_rom_banks;
      num_ram_banks;
      mapped_rom_bank = 1;
      misc_enabled = false;
      mapped_misc_bank = `RAM 0;
      timer_enabled = false;
      (* TODO check initial value *)
    }

let get_ram_address t addr =
    match t.mapped_misc_bank with
    | `RAM n -> (0x2000 * n) + (addr - 0xA000)
    | `RTC _ -> failwith "RAM requested when RTC is mapped"

let num_to_rtc_reg num =
    match num with
    | 0x08 -> S
    | 0x09 -> M
    | 0x0A -> H
    | 0x0B -> DL
    | 0x0C -> DH
    | _ -> assert false

let read_byte t addr =
    let addr_int = Uint16.to_int addr in
    match addr_int with
    | _ when 0x0000 <= addr_int && addr_int <= 0x3FFF ->
        Bigstringaf.get t.rom_bytes addr_int |> uint8_of_char
    | _ when 0x4000 <= addr_int && addr_int <= 0x7FFF ->
        let bank_num = t.mapped_rom_bank in
        let addr_int_ = (0x4000 * bank_num) + (addr_int - 0x4000) in
        Bigstringaf.get t.rom_bytes addr_int_ |> uint8_of_char
    | addr_int when 0xA000 <= addr_int && addr_int <= 0xBFFF ->
        if t.misc_enabled then
          match t.mapped_misc_bank with
          | `RAM _ ->
              if t.num_ram_banks > 0 then
                let ram_addr = get_ram_address t addr_int in
                Bigstringaf.get t.ram_bytes ram_addr |> uint8_of_char
              else
                Uint8.max_int
          | `RTC _ -> failwith "RTC read not implemented yet"
        else
          Uint8.max_int
    | _ -> failwith "Unimplemented"

let write_byte t ~addr ~data =
    let data = Uint8.to_int data in
    match Uint16.to_int addr with
    | addr_int when 0x0000 <= addr_int && addr_int <= 0x1FFF ->
        let enable = data land 0x0F = 0xA in
        t.misc_enabled <- enable
    | addr_int when 0x2000 <= addr_int && addr_int <= 0x3FFF ->
        let bank_num = data land 0b111_1111 in
        t.mapped_rom_bank <- (if bank_num = 0 then 1 else bank_num)
    | addr_int when 0x4000 <= addr_int && addr_int <= 0x5FFF ->
        if data <= 0x03 then
          t.mapped_misc_bank <- `RAM (data land 0b11)
        else if 0x08 <= data && data <= 0x0C then
          t.mapped_misc_bank <- `RTC (num_to_rtc_reg data)
    | addr_int when 0x6000 <= addr_int && addr_int <= 0x7FFF ->
        (* TODO *)
        (* t.latched_value <- t.current_time *)
        (* failwith "RTC latch not implemented yet" *)
        ()
    | addr_int when 0xA000 <= addr_int && addr_int <= 0xBFFF -> (
        if t.misc_enabled then
          match t.mapped_misc_bank with
          | `RAM _ ->
              if t.num_ram_banks > 0 then
                let ram_addr = get_ram_address t addr_int in
                Bigstringaf.set t.ram_bytes ram_addr (Char.chr data)
          | `RTC _ -> failwith "RTC write not implemented yet")
    | _ ->
        Tsdl.Sdl.log "Writing to MBC3 not fully implemented yet: %s\n" (Uint16.to_string_hex addr)

let accepts_address addr_int =
    (0x0000 <= addr_int && addr_int <= 0x7FFF) || (0xA000 <= addr_int && addr_int <= 0xBFFF)

let save_ram t filename =
    let out_channel = Out_channel.open_bin filename in
    Out_channel.output_string out_channel (Bigstringaf.to_string t.ram_bytes)
