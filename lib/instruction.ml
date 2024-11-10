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
    | Offset : uint8 -> uint8 arg
    | C_offset : uint8 arg

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
    | JP
    | JR of condition * int8
    | RET of condition
    | RETI
    | RST
    (* Stack Operations Instructions *)
    (* | ADD *)
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

let show instr =
    let show_arg : type a. a arg -> string = function
        | Imm8 n -> Uint8.to_string_hex n
        | Imm16 n -> Uint16.to_string_hex n
        | Reg8 reg -> Registers.show_r8 reg
        | Reg16 reg -> Registers.show_r16 reg
        | SP -> "SP"
        | HL_i -> "[HL+]"
        | HL_d -> "[HL-]"
        | PtrR reg -> Printf.sprintf "[%s]" (Registers.show_r16 reg)
        | PtrImm n -> Printf.sprintf "[%s]" (Uint16.to_string_hex n)
        | Offset n -> Printf.sprintf "[0xFF00 + %s]" (Uint8.to_string_hex n)
        | C_offset -> Printf.sprintf "[0xFF00 + C]"
    in
    let show_condition cond =
        match cond with
        | None -> "None"
        | NZ -> "NZ"
        | NC -> "NC"
        | Z -> "Z"
        | C -> "C"
    in

    match instr with
    | ADC (x, y) -> Printf.sprintf "ADC  %s, %s" (show_arg x) (show_arg y)
    | ADD8 (x, y) -> Printf.sprintf "ADD8 %s, %s" (show_arg x) (show_arg y)
    | AND (x, y) -> Printf.sprintf "AND %s, %s" (show_arg x) (show_arg y)
    | CP (x, y) -> Printf.sprintf "CP %s, %s" (show_arg x) (show_arg y)
    | DEC8 x -> Printf.sprintf "DEC8 %s" (show_arg x)
    | INC8 x -> Printf.sprintf "INC8 %s" (show_arg x)
    | OR (x, y) -> Printf.sprintf "OR %s, %s" (show_arg x) (show_arg y)
    | SBC (x, y) -> Printf.sprintf "SBC %s, %s" (show_arg x) (show_arg y)
    | SUB (x, y) -> Printf.sprintf "SUB %s, %s" (show_arg x) (show_arg y)
    | XOR (x, y) -> Printf.sprintf "XOR %s, %s" (show_arg x) (show_arg y)
    | ADD16 (x, y) -> Printf.sprintf "ADD16 %s, %s" (show_arg x) (show_arg y)
    | DEC16 x -> Printf.sprintf "DEC16 %s" (show_arg x)
    | INC16 x -> Printf.sprintf "INC16 %s" (show_arg x)
    | SWAP x -> Printf.sprintf "SWAP %s" (show_arg x)
    | RL x -> Printf.sprintf "RL %s" (show_arg x)
    | RLA -> "RLA"
    | RLC x -> Printf.sprintf "RLC %s" (show_arg x)
    | RLCA -> "RLCA"
    | RR x -> Printf.sprintf "RR %s" (show_arg x)
    | RRA -> "RRA"
    | RRC x -> Printf.sprintf "RRC %s" (show_arg x)
    | RRCA -> "RRCA"
    | SLA x -> Printf.sprintf "SLA %s" (show_arg x)
    | SRA x -> Printf.sprintf "SRA %s" (show_arg x)
    | SRL x -> Printf.sprintf "SRL %s" (show_arg x)
    | LD8 (x, y) -> Printf.sprintf "LD8 %s, %s" (show_arg x) (show_arg y)
    | LD16 (x, y) -> Printf.sprintf "LD16 %s, %s" (show_arg x) (show_arg y)
    | BIT (n, x) -> Printf.sprintf "BIT %d, %s" n (show_arg x)
    | RES (n, x) -> Printf.sprintf "RES %d, %s" n (show_arg x)
    | SET (n, x) -> Printf.sprintf "SET %d, %s" n (show_arg x)
    | CALL (c, x) -> Printf.sprintf "CALL %s, %s" (show_condition c) (Uint16.to_string x)
    | JP -> "JP"
    | JR (c, x) -> Printf.sprintf "JR %s, %s" (show_condition c) (Int8.to_string_hex x)
    | RET c -> Printf.sprintf "RET %s" (show_condition c)
    | RETI -> "RETI"
    | RST -> "RST"
    | POP x -> Printf.sprintf "POP %s" (show_arg x)
    | PUSH x -> Printf.sprintf "PUSH %s" (show_arg x)
    | CCF -> "CCF"
    | CPL -> "CPL"
    | DAA -> "DAA"
    | DI -> "DI"
    | EI -> "EI"
    | HALT -> "HALT"
    | NOP -> "NOP"
    | SCF -> "SCF"
    | STOP -> "STOP"
