open Stdint

type t = {
    len : uint16;
    instr : Instruction.t;
  }
