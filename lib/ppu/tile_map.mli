open Uint

type t

type map =
    | Map_0
    | Map_1
    [@@deriving show]

val create : unit -> t

val get_tile_index : t -> x:int -> y:int -> map -> uint8
(** x and y are the actual pixel coordinates on screen, not the coordinates inside a single tile map **)

include Addressable_intf.ByteAddressable with type t := t
