type t

type frame_progress =
    | In_progress
    | Finished of Color.t array array
    | Off

val create : Interrupt_manager.t -> t
val accepts_address : Uint.Uint16.t -> bool
val execute : t -> mcycles:int -> frame_progress

include Addressable_intf.ByteAddressable with type t := t
