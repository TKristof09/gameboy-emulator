open Uint

module Make (Bus : Addressable_intf.WordAddressable) : sig
  type t

  val create : bus:Bus.t -> t
  val execute : t -> Instruction.t -> uint16 -> unit
  val step : t -> unit
  val show : t -> string
end
