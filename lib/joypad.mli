type t

type key =
    | Up
    | Down
    | Left
    | Right
    | A
    | B
    | Start
    | Select
[@@deriving show]

val create : Interrupt_manager.t -> t
val press : t -> key -> unit
val release : t -> key -> unit
val show : t -> string

include Addressable_intf.ByteAddressable with type t := t
