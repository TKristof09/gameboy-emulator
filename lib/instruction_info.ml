open Uint

type t = {
    len : uint16;
    instr : Instruction.t;
    mcycles_branch : int;
    mcycles_nobranch : int;
  }
