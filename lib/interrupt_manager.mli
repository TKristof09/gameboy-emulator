type t

type interrupt_type =
    | VBlank
    | LCD
    | Timer
    | Serial
    | Joypad
[@@deriving show]

val create : unit -> t
val request_interrupt : t -> interrupt_type -> unit
val acknowledge_interrupt : t -> interrupt_type -> unit
val is_enabled : t -> interrupt_type -> bool
val set_master_enable : t -> bool -> unit
val is_master_enabled : t -> bool
val get_pending : t -> interrupt_type option

include Addressable_intf.ByteAddressable with type t := t
