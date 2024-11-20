open Uint

type obj = {
    mutable x : uint8;
    mutable y : uint8;
    mutable tile_index : uint8;
    mutable priority : [ `OBJ_prio | `BG_WIN_prio ];
    mutable y_flip : bool;
    mutable x_flip : bool;
    mutable palette : [ `OBP0 | `OBP1 ];
  }
[@@deriving show]

type t = { mem : obj array }

let create () =
    {
      mem =
        Array.init 40 (fun _ ->
            Uint8.
              {
                x = zero;
                y = zero;
                tile_index = zero;
                priority = `OBJ_prio;
                y_flip = false;
                x_flip = false;
                palette = `OBP0;
              });
    }

let get_objects t = t.mem

let attribs_to_byte obj =
    (Bool.to_int (obj.priority = `BG_WIN_prio) lsl 7)
    lor (Bool.to_int obj.y_flip lsl 6)
    lor (Bool.to_int obj.x_flip lsl 5)
    lor (Bool.to_int (obj.palette = `OBP1) lsl 4)
    |> Uint8.of_int

let read_byte t addr =
    let addr = Uint16.to_int addr in
    let offset = addr - 0xFE00 in
    let obj_id = offset / 4 in
    let byte_num = offset mod 4 in
    let open Uint8 in
    match byte_num with
    | 0 -> t.mem.(obj_id).y + of_int 16
    | 1 -> t.mem.(obj_id).x + of_int 8
    | 2 -> t.mem.(obj_id).tile_index
    | 3 -> attribs_to_byte t.mem.(obj_id)
    | _ -> failwith "The rules of arithmetics have been broken"

let write_byte t ~addr ~data =
    let addr = Uint16.to_int addr in
    let offset = addr - 0xFE00 in
    let obj_id = offset / 4 in
    let byte_num = offset mod 4 in
    let open Uint8 in
    match byte_num with
    | 0 -> t.mem.(obj_id).y <- data - of_int 16
    | 1 -> t.mem.(obj_id).x <- data - of_int 8
    | 2 -> t.mem.(obj_id).tile_index <- data
    | 3 ->
        t.mem.(obj_id).priority <-
          (if logand data (of_int 0b10000000) <> zero then `BG_WIN_prio else `OBJ_prio);
        t.mem.(obj_id).y_flip <- logand data (of_int 0b01000000) <> zero;
        t.mem.(obj_id).x_flip <- logand data (of_int 0b00100000) <> zero;
        if logand data (of_int 0b00010000) <> zero then
          t.mem.(obj_id).palette <- `OBP1
        else
          t.mem.(obj_id).palette <- `OBP0
    | _ -> failwith "The rules of arithmetics have been broken"
