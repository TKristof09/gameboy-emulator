open Uint

type t = { bytes : Bytes.t }

let create bytes = { bytes }

let read_byte t addr =
    let addr_int = Uint16.to_int addr in
    Uint8.of_bytes_little_endian t.bytes addr_int

let write_byte _t ~addr:_ ~data:_ = failwith "Can't write to cartridge ROM"
