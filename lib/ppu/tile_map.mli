open Uint

type t

val create : unit -> t

val get_tile_index : t -> x:int -> y:int -> [ `Map_0 | `Map_1 ] -> uint8
(** x and y are the actual pixel coordinates on screen, not the coordinates inside a single tile map **)

include Addressable_intf.ByteAddressable with type t := t
