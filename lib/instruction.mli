(* https://rgbds.gbdev.io/docs/v0.8.0/gbz80.7 *)

open Uint

type _ arg =
    | Imm8 : uint8 -> uint8 arg
    | Imm16 : uint16 -> uint16 arg
    | Reg8 : Registers.r8 -> uint8 arg
    | Reg16 : Registers.r16 -> uint16 arg
    | SP : uint16 arg
    | HL_i : uint8 arg
    | HL_d : uint8 arg
    | PtrR : Registers.r16 -> uint8 arg
    | PtrImm : uint16 -> uint8 arg
    | PtrImm16 : uint16 -> uint16 arg
    | Offset : uint8 -> uint8 arg
    | C_offset : uint8 arg
    | SP_offset : int8 -> uint16 arg

type condition =
    | None
    | NZ
    | NC
    | Z
    | C

type t =
    (* 8-bit Arithmetic and Logic Instructions *)
    | ADC of uint8 arg * uint8 arg
    | ADD8 of uint8 arg * uint8 arg
    | AND of uint8 arg * uint8 arg
    | CP of uint8 arg * uint8 arg
    | DEC8 of uint8 arg
    | INC8 of uint8 arg
    | OR of uint8 arg * uint8 arg
    | SBC of uint8 arg * uint8 arg
    | SUB of uint8 arg * uint8 arg
    | XOR of uint8 arg * uint8 arg
    (* 16-bit Arithmetic Instructions *)
    | ADD16 of uint16 arg * uint16 arg
    | DEC16 of uint16 arg
    | INC16 of uint16 arg
    (* Bit Operations Instructions *)
    | BIT of int * uint8 arg
    | RES of int * uint8 arg
    | SET of int * uint8 arg
    | SWAP of uint8 arg
    (* Bit Shift Instructions *)
    | RL of uint8 arg
    | RLA
    | RLC of uint8 arg
    | RLCA
    | RR of uint8 arg
    | RRA
    | RRC of uint8 arg
    | RRCA
    | SLA of uint8 arg
    | SRA of uint8 arg
    | SRL of uint8 arg
    (* Load Instructions *)
    | LD8 of uint8 arg * uint8 arg
    | LD16 of uint16 arg * uint16 arg
    (* Jumps and Subroutines *)
    | CALL of condition * uint16
    | JP of condition * uint16 arg
    | JR of condition * int8
    | RET of condition
    | RETI
    | RST of uint16
    (* Stack Operations Instructions *)
    | ADDSP of int8
    (* | DEC *)
    (* | INC *)
    (* | LD *)
    | POP of uint16 arg
    | PUSH of uint16 arg
    (* Miscellaneous Instructions *)
    | CCF
    | CPL
    | DAA
    | DI
    | EI
    | HALT
    | NOP
    | SCF
    | STOP

val show : t -> string
