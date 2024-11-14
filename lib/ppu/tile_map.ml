open Uint

type t = {
    map0 : uint8 array array;
    map1 : uint8 array array;
  }

let create () =
    { map0 = Array.make_matrix 32 32 Uint8.zero; map1 = Array.make_matrix 32 32 Uint8.zero }

let get_tile_index t ~x ~y map_id =
    match map_id with
    | `Map_0 -> t.map0.(y / 8).(x / 8)
    | `Map_1 -> t.map1.(y / 8).(x / 8)

let read_map m offset =
    let y = offset / 32
    and x = offset mod 32 in
    m.(y).(x)

let write_map m ~offset ~data =
    let y = offset / 32
    and x = offset mod 32 in
    m.(y).(x) <- data

let read_byte t addr =
    match Uint16.to_int addr with
    | addr_int when 0x9800 <= addr_int && addr_int <= 0x9BFF -> read_map t.map0 (addr_int - 0x9800)
    | addr_int when 0x9C00 <= addr_int && addr_int <= 0x9FFF -> read_map t.map1 (addr_int - 0x9C00)
    | _ -> failwith "Invalid tile map read address"

let write_byte t ~addr ~data =
    match Uint16.to_int addr with
    | addr_int when 0x9800 <= addr_int && addr_int <= 0x9BFF ->
        write_map t.map0 ~offset:(addr_int - 0x9800) ~data
    | addr_int when 0x9C00 <= addr_int && addr_int <= 0x9FFF ->
        write_map t.map1 ~offset:(addr_int - 0x9C00) ~data
    | _ -> failwith "Invalid tile map read address"
