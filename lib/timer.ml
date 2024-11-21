open Uint

type frequency =
    | F_256
    | F_4
    | F_16
    | F_64

type t = {
    interrupt_manager : Interrupt_manager.t;
    mutable m_cycle_counter : int;
    mutable div : int;
    mutable tima_value : int;
    mutable tima_enabled : bool;
    mutable tima_freq : frequency;
    mutable tma : int;
  }

let create interrupt_manager =
    {
      interrupt_manager;
      m_cycle_counter = 0;
      div = 0;
      tima_value = 0;
      tima_enabled = false;
      tima_freq = F_256;
      tma = 0;
    }

let run t ~mcycles =
    let new_counter = t.m_cycle_counter + mcycles in
    t.div <- t.div + ((new_counter / 64) - (t.m_cycle_counter / 64));
    (if t.tima_enabled then
       let freq =
           match t.tima_freq with
           | F_256 -> 256
           | F_4 -> 4
           | F_16 -> 16
           | F_64 -> 64
       in
       let new_tima = t.tima_value + ((new_counter / freq) - (t.m_cycle_counter / freq)) in
       if new_tima > 0xFF then (
         t.tima_value <- t.tma;
         Interrupt_manager.request_interrupt t.interrupt_manager Timer));
    (* 256 is divisible by all frequencies so we don't care about going higher *)
    t.m_cycle_counter <- new_counter mod 256

let freq_to_int = function
    | F_256 -> 0
    | F_4 -> 1
    | F_16 -> 2
    | F_64 -> 3

let freq_of_int = function
    | 0 -> F_256
    | 1 -> F_4
    | 2 -> F_16
    | 3 -> F_64
    | _ -> failwith "Invalid frequency"

let read_byte t addr =
    match Uint16.to_int addr with
    | 0xFF04 -> Uint8.of_int t.div
    | 0xFF05 -> Uint8.of_int t.tima_value
    | 0xFF06 -> Uint8.of_int t.tma
    | 0xFF07 -> Uint8.of_int ((Bool.to_int t.tima_enabled lsl 2) lor freq_to_int t.tima_freq)
    | _ -> failwith "Invalid timer read address"

let write_byte t ~addr ~data =
    match Uint16.to_int addr with
    | 0xFF04 -> t.div <- 0
    | 0xFF05 -> t.tima_value <- Uint8.to_int data
    | 0xFF06 -> t.tma <- Uint8.to_int data
    | 0xFF07 ->
        let open Uint8 in
        t.tima_enabled <- logand data (of_int 0b100) <> zero;
        t.tima_freq <- freq_of_int (to_int data land 0b11)
    | _ -> failwith "Invalid timer read address"
