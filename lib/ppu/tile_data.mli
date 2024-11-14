open Uint

type t

val create : unit -> t
(** x and y are the coordinates inside the tile **)
val get_pixel : t -> [`Mode_8000 | `Mode_8800] -> uint8 -> int * int -> Palette.t -> Color.t


include Addressable_intf.ByteAddressable with type t := t
