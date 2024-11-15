open Uint

module Make (Bus : Addressable_intf.WordAddressable) : sig
  type t

  val create : bus:Bus.t -> t
  val execute : t -> Instruction.t -> uint16 -> int -> int -> int
  val step : t -> int
  val show : t -> string
end
