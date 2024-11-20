open Uint

type t

type access_mode =
    | Mode_8000
    | Mode_8800
[@@deriving show]

val create : unit -> t

val get_pixel : t -> access_mode -> uint8 -> x:int -> y:int -> Palette.t -> Color.t
(** 
    [x] and [y] are the coordinates inside the tile *)

val get_pixel_color_id : t -> access_mode -> Stdint.Uint8.t -> x:int -> y:int -> Color_id.t
(** 
    [x] and [y] are the coordinates inside the tile *)

include Addressable_intf.ByteAddressable with type t := t
