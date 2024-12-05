type cartridge_type =
    | ROM_ONLY
    | MBC1
[@@deriving show]

type t = {
    title : string;
    cartridge_type : cartridge_type;
    rom_size : int;
    ram_size : int;
  }
[@@deriving show]

let parse_header bytes =
    let open Core in
    let ctype =
        match Bigstringaf.unsafe_get bytes 0x0147 |> Char.to_int with
        | 0x00 -> ROM_ONLY
        | 0x01 -> MBC1
        | x -> failwith @@ Printf.sprintf "Unsupported ROM type %d" x
    in
    let rom_size = 2 lsl (Bigstringaf.unsafe_get bytes 0x0148 |> Char.to_int) in
    let ram_size =
        match Bigstringaf.unsafe_get bytes 0x0149 |> Char.to_int with
        | 0 -> 0
        | 1 -> failwith "Unused"
        | 2 -> 1
        | 3 -> 4
        | 4 -> 16
        | 5 -> 8
        | _ -> failwith "Invalid ram size"
    in
    { title = ""; cartridge_type = ctype; rom_size; ram_size }
