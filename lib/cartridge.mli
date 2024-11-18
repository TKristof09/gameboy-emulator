type t

val create : Bigstringaf.t -> t

include Addressable_intf.ByteAddressable with type t := t
