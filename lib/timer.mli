type t

val create : Interrupt_manager.t -> t
val run : t -> mcycles:int -> unit
val show: t -> string

include Addressable_intf.ByteAddressable with type t := t
