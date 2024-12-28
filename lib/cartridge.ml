type t = {
    header : Cartridge_header.t;
    impl : (module Cartridge_intf.Instance); [@opaque]
  }
[@@deriving show]

let create bytes =
    let header = Cartridge_header.parse_header (Bigstringaf.sub bytes ~off:0 ~len:0x14F) in
    let impl =
        match header.cartridge_type with
        | ROM_ONLY -> Cartridge_intf.build_instance (module Rom_only) bytes header
        | MBC1 -> Cartridge_intf.build_instance (module Mbc1) bytes header
        | MBC3 -> Cartridge_intf.build_instance (module Mbc3) bytes header
    in
    { header; impl }

let read_byte t addr =
    let module M = (val t.impl : Cartridge_intf.Instance) in
    M.Cartridge_type.read_byte M.this addr

let write_byte t ~addr ~data =
    let module M = (val t.impl : Cartridge_intf.Instance) in
    M.Cartridge_type.write_byte M.this ~addr ~data

let accepts_address t addr_int =
    let module M = (val t.impl : Cartridge_intf.Instance) in
    M.Cartridge_type.accepts_address addr_int
