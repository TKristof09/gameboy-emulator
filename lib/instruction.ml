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

let show instr =
    let show_arg : type a. a arg -> string = function
        | Imm8 n -> Uint8.to_string_hex n
        | Imm16 n -> Uint16.to_string_hex n
        | Reg8 reg -> Registers.show_r8 reg
        | Reg16 reg -> Registers.show_r16 reg
    in

    match instr with
    | ADC (x, y) -> Printf.sprintf "ADC  %s, %s" (show_arg x) (show_arg y)
    | ADD8 (x, y) -> Printf.sprintf "ADD %s, %s" (show_arg x) (show_arg y)
    | AND (x, y) -> Printf.sprintf "AND %s, %s" (show_arg x) (show_arg y)
    | CP (x, y) -> Printf.sprintf "CP %s, %s" (show_arg x) (show_arg y)
    | DEC8 x -> Printf.sprintf "DEC %s" (show_arg x)
    | INC8 x -> Printf.sprintf "INC %s" (show_arg x)
    | OR (x, y) -> Printf.sprintf "OR %s, %s" (show_arg x) (show_arg y)
    | SBC (x, y) -> Printf.sprintf "SBC %s, %s" (show_arg x) (show_arg y)
    | SUB (x, y) -> Printf.sprintf "SUB %s, %s" (show_arg x) (show_arg y)
    | XOR (x, y) -> Printf.sprintf "XOR %s, %s" (show_arg x) (show_arg y)
    | ADD16 (x, y) -> Printf.sprintf "ADD %s, %s" (show_arg x) (show_arg y)
    | DEC16 x -> Printf.sprintf "DEC %s" (show_arg x)
    | INC16 x -> Printf.sprintf "INC %s" (show_arg x)
    | SWAP x -> Printf.sprintf "SWAP %s" (show_arg x)
    | RL -> "RL"
    | RLA -> "RLA"
    | RLC -> "RLC"
    | RLCA -> "RLCA"
    | RR -> "RR"
    | RRA -> "RRA"
    | RRC -> "RRC"
    | RRCA -> "RRCA"
    | SLA -> "SLA"
    | SRA -> "SRA"
    | SRL -> "SRL"
    | LD8 (x, y) -> Printf.sprintf "LD %s, %s" (show_arg x) (show_arg y)
    | LD16 (x, y) -> Printf.sprintf "LD %s, %s" (show_arg x) (show_arg y)
    | LDH -> "LDH"
    | CALL -> "CALL"
    | JP -> "JP"
    | JR -> "JR"
    | RET -> "RET"
    | RETI -> "RETI"
    | RST -> "RST"
    | POP -> "POP"
    | PUSH -> "PUSH"
    | CCF -> "CCF"
    | CPL -> "CPL"
    | DAA -> "DAA"
    | DI -> "DI"
    | EI -> "EI"
    | HALT -> "HALT"
    | NOP -> "NOP"
    | SCF -> "SCF"
    | STOP -> "STOP"
    | _ -> failwith "Unimplemented instruction"
