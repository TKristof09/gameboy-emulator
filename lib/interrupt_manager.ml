open Uint

type interrupt_type =
    | VBlank
    | LCD
    | Timer
    | Serial
    | Joypad
[@@deriving show]

type t = {
    mutable master_enabled : bool;
    mutable vblank_enabled : bool;
    mutable lcd_enabled : bool;
    mutable timer_enabled : bool;
    mutable serial_enabled : bool;
    mutable joypad_enabled : bool;
    mutable vblank_requested : bool;
    mutable lcd_requested : bool;
    mutable timer_requested : bool;
    mutable serial_requested : bool;
    mutable joypad_requested : bool;
  }

let create () =
    {
      master_enabled = false;
      vblank_enabled = false;
      lcd_enabled = false;
      timer_enabled = false;
      serial_enabled = false;
      joypad_enabled = false;
      vblank_requested = false;
      lcd_requested = false;
      timer_requested = false;
      serial_requested = false;
      joypad_requested = false;
    }

let request_interrupt t interrupt_type =
    match interrupt_type with
    | VBlank -> t.vblank_requested <- true
    | LCD -> t.lcd_requested <- true
    | Timer -> t.timer_requested <- true
    | Serial -> t.serial_requested <- true
    | Joypad -> t.joypad_requested <- true

let acknowledge_interrupt t interrupt_type =
    match interrupt_type with
    | VBlank -> t.vblank_requested <- false
    | LCD -> t.lcd_requested <- false
    | Timer -> t.timer_requested <- false
    | Serial -> t.serial_requested <- false
    | Joypad -> t.joypad_requested <- false

let is_enabled t interrupt_type =
    match interrupt_type with
    | VBlank -> t.master_enabled && t.vblank_enabled
    | LCD -> t.master_enabled && t.lcd_enabled
    | Timer -> t.master_enabled && t.timer_enabled
    | Serial -> t.master_enabled && t.serial_enabled
    | Joypad -> t.master_enabled && t.joypad_enabled

let set_master_enable t b = t.master_enabled <- b
let is_master_enabled t = t.master_enabled

let get_pending t =
    if t.vblank_enabled && t.vblank_requested then
      Some VBlank
    else if t.lcd_enabled && t.lcd_requested then
      Some LCD
    else if t.timer_enabled && t.timer_requested then
      Some Timer
    else if t.serial_enabled && t.serial_requested then
      Some Serial
    else if t.joypad_enabled && t.joypad_requested then
      Some Joypad
    else
      None
(* let open Core in *)
(* [ VBlank; LCD; Timer; Serial; Joypad ] *)
(* |> List.filter ~f:(fun type_ -> is_enabled t type_ && is_requested t type_) *)
(* |> List.hd *)

let read_flags t =
    Bool.to_int t.vblank_requested
    lor (Bool.to_int t.lcd_requested lsl 1)
    lor (Bool.to_int t.timer_requested lsl 2)
    lor (Bool.to_int t.serial_requested lsl 3)
    lor (Bool.to_int t.joypad_requested lsl 4)
    |> Uint8.of_int

let read_enables t =
    Bool.to_int t.vblank_enabled
    lor (Bool.to_int t.lcd_enabled lsl 1)
    lor (Bool.to_int t.timer_enabled lsl 2)
    lor (Bool.to_int t.serial_enabled lsl 3)
    lor (Bool.to_int t.joypad_enabled lsl 4)
    |> Uint8.of_int

let read_byte t addr =
    match Uint16.to_int addr with
    | 0xFF0F -> read_flags t
    | 0xFFFF -> read_enables t
    | _ -> failwith "Invalid interrupt read address"

let write_flags t data =
    let data = Uint8.to_int data in
    t.vblank_requested <- data land 0b00000001 <> 0;
    t.lcd_requested <- data land 0b00000010 <> 0;
    t.timer_requested <- data land 0b00000100 <> 0;
    t.serial_requested <- data land 0b00001000 <> 0;
    t.joypad_requested <- data land 0b00010000 <> 0

let write_enables t data =
    let data = Uint8.to_int data in
    t.vblank_enabled <- data land 0b00000001 <> 0;
    t.lcd_enabled <- data land 0b00000010 <> 0;
    t.timer_enabled <- data land 0b00000100 <> 0;
    t.serial_enabled <- data land 0b00001000 <> 0;
    t.joypad_enabled <- data land 0b00010000 <> 0

let write_byte t ~addr ~data =
    match Uint16.to_int addr with
    | 0xFF0F -> write_flags t data
    | 0xFFFF -> write_enables t data
    | _ -> failwith "Invalid interrupt write address"
