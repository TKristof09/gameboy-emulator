(* https://rgbds.gbdev.io/docs/v0.8.0/gbz80.7 *)

open Stdint

type _ arg =
    | Imm8 : uint8 -> uint8 arg
    | Imm16 : uint16 -> uint16 arg
    | Reg8 : Registers.r8 -> uint8 arg
    | Reg16 : Registers.r16 -> uint16 arg

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
    | BIT
    | RES
    | SET
    | SWAP of uint8 arg
    (* Bit Shift Instructions *)
    | RL
    | RLA
    | RLC
    | RLCA
    | RR
    | RRA
    | RRC
    | RRCA
    | SLA
    | SRA
    | SRL
    (* Load Instructions *)
    | LD8 of uint8 arg * uint8 arg
    | LD16 of uint16 arg * uint16 arg
    | LDH
    (* Jumps and Subroutines *)
    | CALL
    | JP
    | JR
    | RET
    | RETI
    | RST
    (* Stack Operations Instructions *)
    (* | ADD *)
    (* | DEC *)
    (* | INC *)
    (* | LD *)
    | POP
    | PUSH
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
