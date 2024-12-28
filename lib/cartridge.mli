type t

val create : Bigstringaf.t -> t
val save_ram: t -> unit
val show : t -> string
val accepts_address : t -> int -> bool

include Addressable_intf.ByteAddressable with type t := t
