open Uint

type tile = Color_id.t array array
type t = { tiles : tile array }

type access_mode =
    | Mode_8000
    | Mode_8800
[@@deriving show]

let data_addr = Uint16.of_int 0x8000

let create () =
    let open Core in
    { tiles = Array.init 384 ~f:(fun _ -> Array.make_matrix ~dimx:8 ~dimy:8 Color_id.ID_0) }

let get_pixel t access_mode tile_id ~x ~y pal =
    let tile_id = if y >= 8 then Uint8.(tile_id + one) else tile_id in
    let y = if y >= 8 then y - 8 else y in
    match access_mode with
    | Mode_8000 -> Palette.get_color pal t.tiles.(Uint8.to_int tile_id).(y).(x)
    | Mode_8800 ->
        let tile_id = tile_id |> Int8.of_uint8 |> Int8.to_int in
        Palette.get_color pal t.tiles.(tile_id + 256).(y).(x)

(* a byte corresponds to a row of color ids' low or hi bits *)
let read_byte t addr =
    let offset = Uint16.(addr - data_addr) |> Uint16.to_int in
    let index = offset / 16 in
    let y = offset mod 16 / 2 in
    let is_lo = offset mod 2 = 0 in
    let open Core in
    t.tiles.(index).(y)
    |> Array.map ~f:(fun id ->
           let hi, lo = Color_id.id_to_bits id in
           if is_lo then lo else hi)
    |> Uint.bits_to_byte

let write_byte t ~addr ~data =
    let bits = Uint.byte_to_bits data in
    let offset = Uint16.(addr - data_addr) |> Uint16.to_int in
    let index = offset / 16 in
    let y = offset mod 16 / 2 in
    let is_lo = offset mod 2 = 0 in
    (* Printf.printf "Offset %d, index %d, y %d, low %B, data %s\n" offset index y is_lo (Uint8.to_string_bin data); *)
    (* Array.iteri (fun i b -> Printf.printf "(%d, %B), " i b) bits; *)
    (* print_newline (); *)
    let open Core in
    t.tiles.(index).(y) <-
      Array.mapi bits ~f:(fun i b ->
          if is_lo then
            Color_id.set_bit_lo t.tiles.(index).(y).(i) b
          else
            Color_id.set_bit_hi t.tiles.(index).(y).(i) b)
