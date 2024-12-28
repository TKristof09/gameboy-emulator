type cartridge_type =
    | ROM_ONLY
    | MBC1
    | MBC3
[@@deriving show]

type t = {
    title : string;
    cartridge_type : cartridge_type;
    rom_size : int;
    ram_size : int;
    has_battery : bool;
  }
[@@deriving show]

let parse_header bytes =
    let open Core in
    let title =
        Bigstringaf.substring bytes ~off:0x134 ~len:16
        |> String.strip ~drop:(fun c -> Char.to_int c = 0)
    in
    let ctype =
        match Bigstringaf.get bytes 0x0147 |> Char.to_int with
        | 0x00 -> ROM_ONLY
        | 0x01
        | 0x02
        | 0x03 ->
            MBC1
        (* | 0x0F *)
        (* | 0x10 *)
        | 0x11
        | 0x12
        | 0x13 ->
            MBC3
        | x -> failwith @@ Printf.sprintf "Unsupported ROM type %d" x
    in
    let rom_size = 2 lsl (Bigstringaf.get bytes 0x0148 |> Char.to_int) in
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
    let has_battery =
        match Bigstringaf.get bytes 0x0147 |> Char.to_int with
        | 0x03
        | 0x06
        | 0x09
        | 0x0D
        | 0x0F
        | 0x10
        | 0x13
        | 0x1B
        | 0x1E
        | 0x22
        | 0xFF ->
            true
        | _ -> false
    in
    { title; cartridge_type = ctype; rom_size; ram_size; has_battery }
