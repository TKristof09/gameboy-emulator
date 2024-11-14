open Uint

type t

val create : addr:uint16 -> t
val get_color : t -> Color_id.t -> Color.t

include Addressable_intf.ByteAddressable with type t := t
