open Uint

type t = {
    bytes : Bytes.t;
    start_addr : uint16;
  }

let create ~start_addr ~end_addr =
    let size = Uint16.(end_addr - start_addr + one) in
    let bytes = Bytes.create (Uint16.to_int size) in
    { bytes; start_addr }

let read_byte t addr =
    let offset = Uint16.(addr - t.start_addr) in
    Uint8.of_bytes_little_endian t.bytes (Uint16.to_int offset)

let write_byte t ~addr ~data =
    let offset = Uint16.(addr - t.start_addr) in
    Uint8.to_bytes_little_endian data t.bytes (Uint16.to_int offset)
