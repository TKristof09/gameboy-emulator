open Uint
open Instruction

module Make (Bus : Addressable_intf.WordAddressable) = struct
  module Instruction_fetcher = Instruction_fetcher.Make (Bus)

  type t = {
      registers : Registers.t;
      mutable pc : uint16;
      mutable sp : uint16;
      bus : Bus.t;
    }

  let show cpu =
      (* TODO: just to shut up the compiler *)
      Printf.sprintf "SP:%s PC:%s REG:%s" (Uint16.to_string_hex cpu.sp)
        (Uint16.to_string_hex cpu.pc) (Registers.show cpu.registers)

  let create ~bus = { registers = Registers.create (); pc = Uint16.zero; sp = Uint16.zero; bus }

  type advance_pc =
      | Nexti
      | Jump of uint16

  let execute cpu instr instr_len =
      cpu.pc <- Uint16.(cpu.pc + instr_len);

      let read_arg : type a. a arg -> a =
         fun arg ->
          match arg with
          | Imm8 n -> n
          | Imm16 n -> n
          | Reg8 r -> Registers.read_r8 cpu.registers r
          | Reg16 r -> Registers.read_r16 cpu.registers r
          | SP -> cpu.sp
          | HL_i ->
              let addr = Registers.read_r16 cpu.registers HL in
              let res = Bus.read_byte cpu.bus addr in
              Registers.write_r16 cpu.registers HL (Uint16.succ addr);
              res
          | HL_d ->
              let addr = Registers.read_r16 cpu.registers HL in
              let res = Bus.read_byte cpu.bus addr in
              Registers.write_r16 cpu.registers HL (Uint16.pred addr);
              res
          | PtrR reg ->
              let addr = Registers.read_r16 cpu.registers reg in
              Bus.read_byte cpu.bus addr
          | PtrImm addr -> Bus.read_byte cpu.bus addr
          | PtrImm16 addr -> Bus.read_word cpu.bus addr
          | Offset n ->
              let addr = Uint16.(of_int 0xFF00 + of_uint8 n) in
              Bus.read_byte cpu.bus addr
          | C_offset ->
              let n = Registers.read_r8 cpu.registers C in
              let addr = Uint16.(of_int 0xFF00 + of_uint8 n) in
              Bus.read_byte cpu.bus addr
          | SP_offset e -> Uint16.of_int (Uint16.to_int cpu.sp + Int8.to_int e)
      and write_arg : type a. a arg -> a -> unit =
         fun arg v ->
          match arg with
          | Reg8 r -> Registers.write_r8 cpu.registers r v
          | Reg16 r -> Registers.write_r16 cpu.registers r v
          | SP -> cpu.sp <- v
          | HL_i ->
              let addr = Registers.read_r16 cpu.registers HL in
              Bus.write_byte cpu.bus ~addr ~data:v;
              Registers.write_r16 cpu.registers HL (Uint16.succ addr)
          | HL_d ->
              let addr = Registers.read_r16 cpu.registers HL in
              Bus.write_byte cpu.bus ~addr ~data:v;
              Registers.write_r16 cpu.registers HL (Uint16.pred addr)
          | PtrR reg ->
              let addr = Registers.read_r16 cpu.registers reg in
              Bus.write_byte cpu.bus ~addr ~data:v
          | PtrImm addr -> Bus.write_byte cpu.bus ~addr ~data:v
          | PtrImm16 addr -> Bus.write_word cpu.bus ~addr ~data:v
          | Offset n ->
              let addr = Uint16.(of_int 0xFF00 + of_uint8 n) in
              Bus.write_byte cpu.bus ~addr ~data:v
          | C_offset ->
              let n = Registers.read_r8 cpu.registers C in
              let addr = Uint16.(of_int 0xFF00 + of_uint8 n) in
              Bus.write_byte cpu.bus ~addr ~data:v
          | Imm8 _
          | Imm16 _
          | SP_offset _ ->
              failwith "Can't write to immediate arg or SP offset"
      and check_condition cond =
          match cond with
          | Z -> Registers.read_flag cpu.registers Zero
          | NZ -> not (Registers.read_flag cpu.registers Zero)
          | C -> Registers.read_flag cpu.registers Carry
          | NC -> not (Registers.read_flag cpu.registers Carry)
          | None -> true
      and push_stack data =
          let open Uint16 in
          cpu.sp <- cpu.sp - of_int 2;
          Bus.write_word cpu.bus ~addr:cpu.sp ~data
      and pop_stack () =
          let res = Bus.read_word cpu.bus cpu.sp in
          cpu.sp <- Uint16.(cpu.sp + of_int 2);
          res
      in
      let advance_pc =
          match instr with
          | NOP -> Nexti
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
          | SUB (x, y) ->
              let open Uint8 in
              let vx, vy = (read_arg x, read_arg y) in
              let res = vx - vy in
              Registers.set_flags cpu.registers ~z:(res = zero) ~c:(vx < vy)
                ~h:(logand vx (of_int 0xF) < logand vy (of_int 0xF))
                ~s:true ();
              write_arg x res;
              Nexti
          | LD8 (x, y) ->
              let vy = read_arg y in
              write_arg x vy;
              Nexti
          | LD16 (x, y) ->
              let vy = read_arg y in
              write_arg x vy;
              Nexti
          | XOR (x, y) ->
              let open Uint8 in
              let vx, vy = (read_arg x, read_arg y) in
              let res = Uint8.logxor vx vy in
              Registers.set_flags cpu.registers ~z:(res = zero) ~s:false ~c:false ~h:false ();
              write_arg x res;
              Nexti
          | BIT (n, y) ->
              let open Uint8 in
              let v = read_arg y in
              Registers.set_flags cpu.registers
                ~z:(logand v (shift_left one n) = zero)
                ~s:false ~h:true ();
              Nexti
          | JR (c, x) ->
              if check_condition c then
                let addr = Uint16.to_int cpu.pc + Int8.to_int x |> Uint16.of_int in
                Jump addr
              else
                Nexti
          | INC8 x ->
              let open Uint8 in
              let vx = read_arg x in
              let res = succ vx in
              write_arg x res;
              Registers.set_flags cpu.registers ~z:(res = zero) ~s:false
                ~h:(logand vx (of_int 0x0F) = of_int 0x0F)
                ();
              Nexti
          | INC16 x ->
              let open Uint16 in
              let vx = read_arg x in
              let res = succ vx in
              write_arg x res;
              Nexti
          | DEC8 x ->
              let open Uint8 in
              let vx = read_arg x in
              let res = pred vx in
              Registers.set_flags cpu.registers ~z:(res = zero) ~s:true
                ~h:(logand vx (of_int 0x0F) = zero)
                ();
              write_arg x res;
              Nexti
          | DEC16 x ->
              let open Uint16 in
              let vx = read_arg x in
              let res = pred vx in
              write_arg x res;
              Nexti
          | CALL (c, x) ->
              if check_condition c then (
                push_stack cpu.pc;
                Jump x)
              else
                Nexti
          | RET c ->
              if check_condition c then
                let addr = pop_stack () in
                Jump addr
              else
                Nexti
          | PUSH x ->
              let data = read_arg x in
              push_stack data;
              Nexti
          | POP x ->
              let v = pop_stack () in
              write_arg x v;
              Nexti
          | RL x ->
              let open Uint8 in
              let vx = read_arg x in
              let c = if Registers.read_flag cpu.registers Carry then one else zero in
              let res = logor (shift_left vx 1) c in
              write_arg x res;
              (* check if bit 7 was set in x *)
              let new_c = logand vx (of_int 0x80) <> zero in
              Registers.set_flags cpu.registers ~z:(res = zero) ~s:false ~h:false ~c:new_c ();
              Nexti
          | RLA ->
              let open Uint8 in
              let a = Registers.read_r8 cpu.registers A in
              let c = if Registers.read_flag cpu.registers Carry then one else zero in
              let res = logor (shift_left a 1) c in
              Registers.write_r8 cpu.registers A res;
              (* check if bit 7 was set in a *)
              let new_c = logand a (of_int 0x80) <> zero in
              Registers.set_flags cpu.registers ~z:false ~s:false ~h:false ~c:new_c ();
              Nexti
          | CP (x, y) ->
              let open Uint8 in
              let vx, vy = (read_arg x, read_arg y) in
              let res = vx - vy in
              Registers.set_flags cpu.registers ~z:(res = zero) ~c:(vx < vy)
                ~h:(logand vx (of_int 0xF) < logand vy (of_int 0xF))
                ~s:true ();
              Nexti
          | _ ->
              failwith (Printf.sprintf "%s:%s" "Unimplemented instruction" (Instruction.show instr))
      in

      match advance_pc with
      | Nexti -> ()
      | Jump i -> cpu.pc <- i

  let step cpu =
      let info = Instruction_fetcher.fetch cpu.bus ~pc:cpu.pc in
      (* Printf.printf "PC: %s\n" (Uint16.to_string_hex cpu.pc); *)
      execute cpu info.instr info.len
end
