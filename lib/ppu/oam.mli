open Uint

type t

type obj = {
    mutable x : uint8;
    mutable y : uint8;
    mutable tile_index : uint8;
    mutable priority : [ `OBJ_prio | `BG_WIN_prio ];
    mutable y_flip : bool;
    mutable x_flip : bool;
    mutable palette : [ `OBP0 | `OBP1 ];
  }
[@@deriving show]

val create : unit -> t
val get_objects : t -> obj array

include Addressable_intf.ByteAddressable with type t := t
