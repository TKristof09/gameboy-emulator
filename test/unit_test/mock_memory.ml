open Stdint
open Core

type t = uint8 array

let uint16_of_uint8s low high =
    let high = Uint16.shift_left (Uint16.of_uint8 high) 8
    and low = Uint16.of_uint8 low in
    Uint16.logor high low

let uint8s_of_uint16 value =
    let high = Uint16.to_uint8 (Uint16.shift_right_logical value 8)
    and low = Uint16.to_uint8 value in
    (high, low)

let of_list l = List.map l ~f:Uint8.of_int |> Array.of_list

let read_byte t addr =
    let addr = Uint16.to_int addr in
    t.(addr)

let read_word t addr =
    let addr = Uint16.to_int addr in
    uint16_of_uint8s t.(addr) t.(addr + 1)

let write_byte t ~addr ~data =
    let addr = Uint16.to_int addr in
    t.(addr) <- data

let write_word t ~addr ~data =
    let addr = Uint16.to_int addr in
    let high, low = uint8s_of_uint16 data in
    t.(addr) <- high;
    t.(addr + 1) <- low
