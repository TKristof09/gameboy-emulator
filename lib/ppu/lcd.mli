open Uint

type t

val create : unit -> t
val show : t -> string
val set_mode : t -> [ `Mode_0 | `Mode_1 | `Mode_2 | `Mode_3 ] -> unit
val get_mode : t -> [ `Mode_0 | `Mode_1 | `Mode_2 | `Mode_3 ]
val incr_ly : t -> unit
val reset_ly : t -> unit
val get_ly : t -> int
val set_ly_eq_lyc : t -> bool -> unit
val get_scroll : t -> uint8 * uint8
val get_win : t -> uint8 * uint8
val ppu_is_enabled : t -> bool

include Addressable_intf.ByteAddressable with type t := t
