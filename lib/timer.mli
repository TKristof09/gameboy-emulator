type t

val create : Interrupt_manager.t -> t
val run : t -> mcycles:int -> unit

include Addressable_intf.ByteAddressable with type t := t
