open Uint
open Instruction

module Make (Bus : Addressable_intf.WordAddressable) = struct
  module Instruction_fetcher = Instruction_fetcher.Make (Bus)

  type t = {
      registers : Registers.t;
      mutable pc : uint16;
      mutable sp : uint16;
      bus : Bus.t;
      interrupt_manager : Interrupt_manager.t;
      mutable enable_interrupt_nexti : bool;
      mutable is_halted : bool;
    }

  let show cpu =
      (* TODO: just to shut up the compiler *)
      Printf.sprintf "HALT:%b SP:%s PC:%s REG:%s" cpu.is_halted (Uint16.to_string_hex cpu.sp)
        (Uint16.to_string_hex cpu.pc) (Registers.show cpu.registers)

  let create ~bus ~interrupt_manager =
      {
        registers = Registers.create ();
        pc = Uint16.zero;
        sp = Uint16.zero;
        bus;
        interrupt_manager;
        enable_interrupt_nexti = false;
        is_halted = false;
      }

  let skip_boot_rom cpu =
      let open Uint8 in
      Registers.write_r8 cpu.registers A (of_int 0x01);
      Registers.write_r8 cpu.registers F (of_int 0xB0);
      Registers.write_r8 cpu.registers B (of_int 0x00);
      Registers.write_r8 cpu.registers C (of_int 0x13);
      Registers.write_r8 cpu.registers D (of_int 0x00);
      Registers.write_r8 cpu.registers E (of_int 0xD8);
      Registers.write_r8 cpu.registers H (of_int 0x01);
      Registers.write_r8 cpu.registers L (of_int 0x4D);
      cpu.sp <- Uint16.of_int 0xFFFE;
      cpu.pc <- Uint16.of_int 0x0100;
      Bus.write_byte cpu.bus ~addr:(Uint16.of_int 0xFF50) ~data:Uint8.one

  type advance_pc =
      | Nexti
      | Jump of uint16

  let two = Uint16.of_int 2

  let push_stack cpu data =
      cpu.sp <- Uint16.(cpu.sp - two);
      Bus.write_word cpu.bus ~addr:cpu.sp ~data

  let pop_stack cpu =
      let res = Bus.read_word cpu.bus cpu.sp in
      cpu.sp <- Uint16.(cpu.sp + two);
      res

  let execute cpu instr instr_len mcycles_branch mcycles_nobranch =
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
          | SP_offset e ->
              let sp_int = Uint16.to_int cpu.sp in
              let e_int = Int8.to_int e in
              Registers.set_flags cpu.registers ~z:false ~s:false
                ~h:((sp_int land 0xF) + (e_int land 0xF) > 0xF)
                ~c:((sp_int land 0xFF) + (e_int land 0xFF) > 0xFF)
                ();
              Uint16.of_int (sp_int + e_int)
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
          | ADD16 (x, y) ->
              let open Uint16 in
              let vx, vy = (read_arg x, read_arg y) in
              let res = vx + vy in
              Registers.set_flags cpu.registers
                ~c:(vx > Uint16.max_int - vy)
                ~h:(logand vx (of_int 0xFFF) + logand vy (of_int 0xFFF) > of_int 0xFFF)
                ~s:false ();
              write_arg x res;
              Nexti
          | ADC (x, y) ->
              let c = if Registers.read_flag cpu.registers Carry then Uint8.one else Uint8.zero in
              let vx, vy = (read_arg x, read_arg y) in
              let res = Uint8.(vx + vy + c) in
              let vx, vy, c = Uint8.(to_int vx, to_int vy, to_int c) in
              Registers.set_flags cpu.registers ~z:(res = Uint8.zero)
                ~c:(vx + vy + c > 0xFF)
                ~h:((vx land 0xF) + (vy land 0xF) + c > 0xF)
                ~s:false ();
              write_arg x res;
              Nexti
          | ADDSP n ->
              let x = Uint16.to_int cpu.sp in
              let y = Int8.to_int n in
              cpu.sp <- Uint16.of_int (x + y);
              Registers.set_flags cpu.registers ~z:false ~s:false
                ~c:((x land 0xFF) + (y land 0xFF) > 0xFF)
                ~h:((x land 0xF) + (y land 0xF) > 0xF)
                ();
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
          | SBC (x, y) ->
              let vx, vy = (read_arg x, read_arg y) in
              let c = if Registers.read_flag cpu.registers Carry then Uint8.one else Uint8.zero in
              let res = Uint8.(vx - vy - c) in
              let vx, vy, c = Uint8.(to_int vx, to_int vy, to_int c) in
              Registers.set_flags cpu.registers ~z:(res = Uint8.zero)
                ~c:(vx < vy + c)
                ~h:(vx land 0xF < (vy land 0xF) + c)
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
          | AND (x, y) ->
              let open Uint8 in
              let vx, vy = (read_arg x, read_arg y) in
              let res = logand vx vy in
              write_arg x res;
              Registers.set_flags cpu.registers ~c:false ~h:true ~s:false ~z:(res = zero) ();
              Nexti
          | OR (x, y) ->
              let open Uint8 in
              let vx, vy = (read_arg x, read_arg y) in
              let res = logor vx vy in
              write_arg x res;
              Registers.set_flags cpu.registers ~c:false ~h:false ~s:false ~z:(res = zero) ();
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
          | JP (c, x) ->
              if check_condition c then
                let addr = read_arg x in
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
                push_stack cpu cpu.pc;
                Jump x)
              else
                Nexti
          | RET c ->
              if check_condition c then
                let addr = pop_stack cpu in
                Jump addr
              else
                Nexti
          | PUSH x ->
              let data = read_arg x in
              push_stack cpu data;
              Nexti
          | POP x ->
              let v = pop_stack cpu in
              write_arg x v;
              Nexti
          | CP (x, y) ->
              let open Uint8 in
              let vx, vy = (read_arg x, read_arg y) in
              let res = vx - vy in
              Registers.set_flags cpu.registers ~z:(res = zero) ~c:(vx < vy)
                ~h:(logand vx (of_int 0xF) < logand vy (of_int 0xF))
                ~s:true ();
              Nexti
          | DI ->
              Interrupt_manager.set_master_enable cpu.interrupt_manager false;
              cpu.enable_interrupt_nexti <- false;
              Nexti
          | EI ->
              cpu.enable_interrupt_nexti <- true;
              Nexti
          | RETI ->
              Interrupt_manager.set_master_enable cpu.interrupt_manager true;
              let addr = pop_stack cpu in
              Jump addr
          | SCF ->
              Registers.set_flags cpu.registers ~c:true ~h:false ~s:false ();
              Nexti
          | CCF ->
              let c = Registers.read_flag cpu.registers Carry in
              Registers.set_flags cpu.registers ~c:(not c) ~s:false ~h:false ();
              Nexti
          | CPL ->
              let a = Registers.read_r8 cpu.registers A in
              Registers.write_r8 cpu.registers A (Uint8.lognot a);
              Registers.set_flags cpu.registers ~s:true ~h:true ();
              Nexti
          | SWAP x ->
              let open Uint8 in
              let vx = read_arg x in
              let lo = logand (of_int 0b00001111) vx in
              let hi = logand (of_int 0b11110000) vx in
              let res = shift_left lo 4 |> logor (shift_right_logical hi 4) in
              write_arg x res;
              Registers.set_flags cpu.registers ~z:(res = zero) ~s:false ~h:false ~c:false ();
              Nexti
          | RST x ->
              push_stack cpu cpu.pc;
              Jump x
          | RES (n, x) ->
              let vx = read_arg x in
              write_arg x Uint8.(logand (lognot @@ shift_left one n) vx);
              Nexti
          | SET (n, x) ->
              let vx = read_arg x in
              write_arg x Uint8.(logor (shift_left one n) vx);
              Nexti
          | SLA x ->
              let open Uint8 in
              let vx = read_arg x in
              let c = logand vx (of_int 0b10000000) <> zero in
              let res = shift_left vx 1 in
              write_arg x res;
              Registers.set_flags cpu.registers ~z:(res = zero) ~s:false ~h:false ~c ();
              Nexti
          | SRA x ->
              let open Uint8 in
              let vx = read_arg x in
              let c = logand vx one <> zero in
              let hi_bit = logand vx (of_int 0b10000000) in
              (* for some reason bit 7 remains unchanged *)
              let res = logor (shift_right_logical vx 1) hi_bit in
              write_arg x res;
              Registers.set_flags cpu.registers ~z:(res = zero) ~s:false ~h:false ~c ();
              Nexti
          | SRL x ->
              let open Uint8 in
              let vx = read_arg x in
              let c = logand vx one <> zero in
              let res = shift_right_logical vx 1 in
              write_arg x res;
              Registers.set_flags cpu.registers ~z:(res = zero) ~s:false ~h:false ~c ();
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
          | RLC x ->
              let open Uint8 in
              let vx = read_arg x in
              let c = if logand vx (of_int 0b10000000) <> zero then one else zero in
              let res = logor (shift_left vx 1) c in
              write_arg x res;
              Registers.set_flags cpu.registers ~z:(res = zero) ~s:false ~h:false ~c:(c <> zero) ();
              Nexti
          | RLCA ->
              let open Uint8 in
              let a = Registers.read_r8 cpu.registers A in
              let c = if logand a (of_int 0b10000000) <> zero then one else zero in
              let res = logor (shift_left a 1) c in
              Registers.write_r8 cpu.registers A res;
              Registers.set_flags cpu.registers ~z:false ~s:false ~h:false ~c:(c <> zero) ();
              Nexti
          | RR x ->
              let open Uint8 in
              let vx = read_arg x in
              let c = Registers.read_flag cpu.registers Carry in
              let hi_bit = if c then of_int 0b10000000 else zero in
              let res = logor (shift_right_logical vx 1) hi_bit in
              write_arg x res;
              (* check if bit 0 was set in x *)
              let new_c = logand vx one <> zero in
              Registers.set_flags cpu.registers ~z:(res = zero) ~s:false ~h:false ~c:new_c ();
              Nexti
          | RRA ->
              let open Uint8 in
              let a = Registers.read_r8 cpu.registers A in
              let c = Registers.read_flag cpu.registers Carry in
              let hi_bit = if c then of_int 0b10000000 else zero in
              let res = logor (shift_right_logical a 1) hi_bit in
              Registers.write_r8 cpu.registers A res;
              (* check if bit 0 was set in x *)
              let new_c = logand a one <> zero in
              Registers.set_flags cpu.registers ~z:false ~s:false ~h:false ~c:new_c ();
              Nexti
          | RRC x ->
              let open Uint8 in
              let vx = read_arg x in
              let lo_bit = logand vx one in
              let res = logor (shift_right_logical vx 1) (shift_left lo_bit 7) in
              write_arg x res;
              let new_c = lo_bit <> zero in
              Registers.set_flags cpu.registers ~z:(res = zero) ~s:false ~h:false ~c:new_c ();
              Nexti
          | RRCA ->
              let open Uint8 in
              let a = Registers.read_r8 cpu.registers A in
              let lo_bit = logand a one in
              let res = logor (shift_right_logical a 1) (shift_left lo_bit 7) in
              Registers.write_r8 cpu.registers A res;
              let new_c = lo_bit <> zero in
              Registers.set_flags cpu.registers ~z:false ~s:false ~h:false ~c:new_c ();
              Nexti
          | DAA ->
              let open Uint8 in
              let a = Registers.read_r8 cpu.registers A in
              let hc = Registers.read_flag cpu.registers Half_carry in
              let c = Registers.read_flag cpu.registers Carry in
              let s = Registers.read_flag cpu.registers Sub in
              let a_int = to_int a in
              let offset = if ((not s) && a_int land 0x0F > 0x09) || hc then 0x06 else 0x00 in
              let should_carry = ((not s) && a_int > 0x99) || c in
              let offset = if should_carry then offset lor 0x60 else offset in
              let res =
                  if s then
                    a - of_int offset
                  else
                    a + of_int offset
              in
              Registers.set_flags cpu.registers ~z:(res = zero) ~h:false ~c:should_carry ();
              Registers.write_r8 cpu.registers A res;
              Nexti
          | HALT ->
              cpu.is_halted <- true;
              Nexti
          | _ ->
              failwith (Printf.sprintf "%s:%s" "Unimplemented instruction" (Instruction.show instr))
      in

      match advance_pc with
      | Nexti -> mcycles_nobranch
      | Jump i ->
          cpu.pc <- i;
          mcycles_branch

  let execute_instruction cpu (instr_info : Instruction_info.t) =
      if cpu.enable_interrupt_nexti then (
        Interrupt_manager.set_master_enable cpu.interrupt_manager true;
        cpu.enable_interrupt_nexti <- false);
      execute cpu instr_info.instr instr_info.len instr_info.mcycles_branch
        instr_info.mcycles_nobranch

  let handle_interrupt cpu (int : Interrupt_manager.interrupt_type) =
      (* Tsdl.Sdl.log "Handling interrupt %s\n" (Interrupt_manager.show_interrupt_type int); *)
      Interrupt_manager.set_master_enable cpu.interrupt_manager false;
      Interrupt_manager.acknowledge_interrupt cpu.interrupt_manager int;
      push_stack cpu cpu.pc;
      (match int with
      | VBlank -> cpu.pc <- Uint16.of_int 0x40
      | LCD -> cpu.pc <- Uint16.of_int 0x48
      | Timer -> cpu.pc <- Uint16.of_int 0x50
      | Serial -> cpu.pc <- Uint16.of_int 0x58
      | Joypad -> cpu.pc <- Uint16.of_int 0x60);
      5

  let step cpu =
      let info = Instruction_fetcher.fetch cpu.bus ~pc:cpu.pc in

      (* Tsdl.Sdl.log "-----------------"; *)
      (* Tsdl.Sdl.log "%s\n" (show cpu); *)
      (* Tsdl.Sdl.log "PC: %s -- %s\n" (Uint16.to_string_hex cpu.pc) (Instruction.show info.instr); *)
      (* Tsdl.Sdl.log "-----------------"; *)
      let instr = info.instr in
      match Interrupt_manager.get_pending cpu.interrupt_manager with
      | None when not cpu.is_halted -> (execute_instruction cpu info, instr, Uint16.to_int cpu.pc)
      | None (* when cpu.is_halted *) -> (1, instr, Uint16.to_int cpu.pc)
      | Some int ->
          cpu.is_halted <- false;
          if Interrupt_manager.is_master_enabled cpu.interrupt_manager then
            (handle_interrupt cpu int, instr, Uint16.to_int cpu.pc)
          else
            (execute_instruction cpu info, instr, Uint16.to_int cpu.pc)

  let get_pc cpu = (Uint16.to_int cpu.pc, (Instruction_fetcher.fetch cpu.bus ~pc:cpu.pc).instr)
end
