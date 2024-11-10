type t

val create : bytes -> t

include Addressable_intf.ByteAddressable with type t := t
