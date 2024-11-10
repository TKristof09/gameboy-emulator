open Uint

type t

val create : start_addr:uint16 -> end_addr:uint16 -> t

include Addressable_intf.ByteAddressable with type t := t
