open Uint

module Make (Bus : Addressable_intf.WordAddressable) : sig
  type t

  val create : bus:Bus.t -> interrupt_manager:Interrupt_manager.t -> t
  val skip_boot_rom : t -> unit
  val execute : t -> Instruction.t -> uint16 -> int -> int -> int
  val step : t -> int * Instruction.t * int
  val show : t -> string

  (* just for testing *)
  val get_pc : t -> int * Instruction.t
end
