open Uint

type t = { bytes : Bigstringaf.t }

let create bytes _ = { bytes }

let read_byte t addr =
    let addr_int = Uint16.to_int addr in
    Bigstringaf.unsafe_get t.bytes addr_int |> uint8_of_char

let write_byte _t ~addr ~data:_ =
    Tsdl.Sdl.log "Can't write to cartridge ROM: %s\n" (Uint16.to_string_hex addr)

let accepts_address addr_int = 0x0000 <= addr_int && addr_int <= 0x7FFF
let save_ram _ _ = ()
