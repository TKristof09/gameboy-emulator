open Stdint

module Make (Mem : Addressable_intf.WordAddressable) : sig
  val fetch : Mem.t -> pc:uint16 -> Instruction_info.t
end
