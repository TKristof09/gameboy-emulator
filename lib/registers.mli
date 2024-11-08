open Stdint

type t

type r8 =
    | A
    | B
    | C
    | D
    | E
    | F
    | H
    | L

type r16 =
    | AF
    | BC
    | DE
    | HL

type flags =
    | Carry
    | Half_carry
    | Sub
    | Zero

val create : unit -> t
val read_r8 : t -> r8 -> uint8
val write_r8 : t -> r8 -> uint8 -> unit
val read_r16 : t -> r16 -> uint16
val write_r16 : t -> r16 -> uint16 -> unit
val set_flag : t -> flags -> unit
val unset_flag : t -> flags -> unit
val read_flag : t -> flags -> bool
