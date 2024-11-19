include Stdint

let uint8_of_char c = Char.code c |> Uint8.of_int

let uint16_of_uint8s ~low ~high =
    let high = Uint16.shift_left (Uint16.of_uint8 high) 8
    and low = Uint16.of_uint8 low in
    Uint16.logor high low

let uint8s_of_uint16 value =
    let high = Uint16.to_uint8 (Uint16.shift_right_logical value 8)
    and low = Uint16.to_uint8 value in
    (low, high)

let pp_uint8 fmt x = Format.fprintf fmt "@[%s@]" (Uint8.to_string x)
let show_uint8 = Uint8.to_string
let pp_uint16 fmt x = Format.fprintf fmt "@[%s@]" (Uint16.to_string x)
let show_uint16 = Uint16.to_string

let byte_to_bits b =
    let open Uint8 in
    [|
      logand b (of_int 0b10000000) <> zero;
      logand b (of_int 0b01000000) <> zero;
      logand b (of_int 0b00100000) <> zero;
      logand b (of_int 0b00010000) <> zero;
      logand b (of_int 0b00001000) <> zero;
      logand b (of_int 0b00000100) <> zero;
      logand b (of_int 0b00000010) <> zero;
      logand b (of_int 0b00000001) <> zero;
    |]

(** Onder [|b7, b6, b5, ... b0|] *)
let bits_to_byte b =
    let open Core in
    Array.foldi b ~init:Uint8.zero ~f:(fun i acc bit ->
        Uint8.add acc (Uint8.of_int (Bool.to_int bit lsl (7 - i))))
