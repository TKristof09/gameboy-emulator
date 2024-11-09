open Stdint

module Make (Mem : Addressable_intf.WordAddressable) : sig
  type t

  val create : mem:Mem.t -> t
  val execute : t -> Instruction.t -> uint16 -> unit
  val step : t -> unit
  val show : t -> string
end
