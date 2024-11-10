open Gameboy.Uint
open Core

type t = uint8 array

let of_list size l =
    let arr = Array.create ~len:size Uint8.zero in
    List.iteri l ~f:(fun i x -> arr.(i) <- Uint8.of_int x);
    arr

let read_byte t addr =
    let addr = Uint16.to_int addr in
    t.(addr)

let read_word t addr =
    let addr = Uint16.to_int addr in
    uint16_of_uint8s ~low:t.(addr) ~high:t.(addr + 1)

let write_byte t ~addr ~data =
    let addr = Uint16.to_int addr in
    t.(addr) <- data

let write_word t ~addr ~data =
    let addr = Uint16.to_int addr in
    let low, high = uint8s_of_uint16 data in
    t.(addr + 1) <- high;
    t.(addr) <- low
