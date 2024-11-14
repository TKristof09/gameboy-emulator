type t =
    | Black
    | White
    | Light_grey
    | Dark_grey
[@@deriving show]

let color_to_bits t =
    match t with
    | White -> 0b00
    | Light_grey -> 0b01
    | Dark_grey -> 0b10
    | Black -> 0b11

let bits_to_color b =
    match b with
    | 0b00 -> White
    | 0b01 -> Light_grey
    | 0b10 -> Dark_grey
    | 0b11 -> Black
    | _ -> failwith "Invalid color bits"
