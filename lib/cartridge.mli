type t

val create : Bigstringaf.t -> t
val show : t -> string

include Addressable_intf.ByteAddressable with type t := t
