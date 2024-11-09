open Stdint
open Instruction

module Make (Mem : Addressable_intf.WordAddressable) = struct
  module Instruction_fetcher = Instruction_fetcher.Make (Mem)

  type t = {
      registers : Registers.t;
      mutable pc : uint16;
      mutable sp : uint16;
      memory : Mem.t;
    }

  let create ~mem =
      { registers = Registers.create (); pc = Uint16.zero; sp = Uint16.zero; memory = mem }

  type advance_pc =
      | Nexti
      | Jump of uint16

  let execute cpu instr instr_len =
      let read_arg : type a. a arg -> a =
         fun arg ->
          match arg with
          | Imm8 n -> n
          | Imm16 n -> n
          | Reg8 r -> Registers.read_r8 cpu.registers r
          | Reg16 r -> Registers.read_r16 cpu.registers r
      and write_arg : type a. a arg -> a -> unit =
         fun arg v ->
          match arg with
          | Reg8 r -> Registers.write_r8 cpu.registers r v
          | Reg16 r -> Registers.write_r16 cpu.registers r v
          | _ -> failwith "Can't write to immediate arg"
      in
      let advance_pc =
          match instr with
          | ADD8 (x, y) ->
              let open Uint8 in
              let vx, vy = (read_arg x, read_arg y) in
              let res = vx + vy in
              Registers.set_flags cpu.registers ~z:(res = zero)
                ~c:(vx > Uint8.max_int - vy)
                ~h:(logand vx (of_int 0xF) + logand vy (of_int 0xF) > of_int 0xF)
                ~s:false ();
              write_arg x res;
              Nexti
          | ADC (x, y) ->
              let open Uint8 in
              let c = if Registers.read_flag cpu.registers Carry then one else zero in
              let vx, vy = (read_arg x, read_arg y) in
              let res = vx + vy + c in
              Registers.set_flags cpu.registers ~z:(res = zero)
                ~c:(vx > Uint8.max_int - vy - c)
                ~h:(logand vx (of_int 0xF) + logand vy (of_int 0xF) + c > of_int 0xF)
                ~s:false ();
              write_arg x res;
              Nexti
          | _ ->
              failwith (Printf.sprintf "%s:%s" "Unimplemented instruction" (Instruction.show instr))
      in

      match advance_pc with
      | Nexti -> cpu.pc <- Uint16.(cpu.pc + instr_len)
      | Jump _ -> failwith "Jump isn't supported yet"

  let step cpu =
      let info = Instruction_fetcher.fetch cpu.memory ~pc:cpu.pc in
      execute cpu info.instr info.len

  let show cpu =
      Printf.sprintf "SP:%s PC:%s REG:%s" (Uint16.to_string_hex cpu.sp)
        (Uint16.to_string_hex cpu.pc) (Registers.show cpu.registers)
end
