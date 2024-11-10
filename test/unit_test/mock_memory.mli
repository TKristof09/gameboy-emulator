open Gameboy

type t

val of_list : int -> int list -> t

include Addressable_intf.WordAddressable with type t := t
