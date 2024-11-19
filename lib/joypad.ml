open Uint

type key =
    | Up
    | Down
    | Left
    | Right
    | A
    | B
    | Start
    | Select
[@@deriving show]

type t = {
    interrupt_manager : Interrupt_manager.t; [@opaque]
    mutable selection : [ `None | `DPad | `Buttons ];
    mutable up : bool;
    mutable down : bool;
    mutable left : bool;
    mutable right : bool;
    mutable a : bool;
    mutable b : bool;
    mutable start : bool;
    mutable select : bool;
  }
[@@deriving show]

let create interrupt_manager =
    {
      interrupt_manager;
      selection = `None;
      up = false;
      down = false;
      left = false;
      right = false;
      a = false;
      b = false;
      start = false;
      select = false;
    }

let read_byte t _addr =
    (* Tsdl.Sdl.log "Read joypad %s" (show t); *)
    [|
      false;
      false;
      not (t.selection = `Buttons);
      not (t.selection = `DPad);
      (* for some reason bit = 0 means pressed for the gameboy so invert the boolean *)
      not (if t.selection = `DPad then t.down else t.start);
      not (if t.selection = `DPad then t.up else t.select);
      not (if t.selection = `DPad then t.left else t.b);
      not (if t.selection = `DPad then t.right else t.a);
    |]
    |> Uint.bits_to_byte
    (* if selection is None then lower nibble reads 0xF - all buttons released *)
    |> Uint8.logor (if t.selection = `None then Uint8.of_int 0xF else Uint8.zero)

let write_byte t ~addr:_ ~data =
    (* Tsdl.Sdl.log "Write to joypad %s" (Uint8.to_string_hex data); *)
    let bits = Uint.byte_to_bits data in
    match (bits.(2), bits.(3)) with
    | true, false -> t.selection <- `DPad
    | false, true -> t.selection <- `Buttons
    | _ -> t.selection <- `None

let press t key =
    (* Tsdl.Sdl.log "Pressed %s\n" (show_key key); *)
    Interrupt_manager.request_interrupt t.interrupt_manager Joypad;
    match key with
    | Up -> t.up <- true
    | Down -> t.down <- true
    | Left -> t.left <- true
    | Right -> t.right <- true
    | A -> t.a <- true
    | B -> t.b <- true
    | Start -> t.start <- true
    | Select -> t.select <- true

let release t key =
    (* Tsdl.Sdl.log "Released %s\n" (show_key key); *)
    match key with
    | Up -> t.up <- false
    | Down -> t.down <- false
    | Left -> t.left <- false
    | Right -> t.right <- false
    | A -> t.a <- false
    | B -> t.b <- false
    | Start -> t.start <- false
    | Select -> t.select <- false
