type t

val create : unit -> t
val accepts_address : Uint.Uint16.t -> bool
val execute : t -> mcycles:int -> unit

include Addressable_intf.ByteAddressable with type t := t
