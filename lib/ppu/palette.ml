open Uint

type t = {
    addr : uint16;
    mutable color_0 : Color.t;
    mutable color_1 : Color.t;
    mutable color_2 : Color.t;
    mutable color_3 : Color.t;
  }

let create ~addr =
    {
      addr;
      color_0 = Color.White;
      color_1 = Color.Light_grey;
      color_2 = Color.Dark_grey;
      color_3 = Color.Black;
    }

let get_color t id =
    match id with
    | Color_id.ID_0 -> t.color_0
    | Color_id.ID_1 -> t.color_1
    | Color_id.ID_2 -> t.color_2
    | Color_id.ID_3 -> t.color_3

let read_byte t addr =
    if addr <> t.addr then
      failwith "Invalid address for palette"
    else
      Color.color_to_bits t.color_0
      lor (Color.color_to_bits t.color_1 lsl 2)
      lor (Color.color_to_bits t.color_2 lsl 4)
      lor (Color.color_to_bits t.color_3 lsl 6)
      |> Uint8.of_int

let write_byte t ~addr ~data =
    if addr <> t.addr then
      failwith "Invalid address for palette"
    else
      let data = Uint8.to_int data in
      t.color_0 <- Color.bits_to_color (data land 0b00000011);
      t.color_1 <- Color.bits_to_color ((data land 0b00001100) lsr 2);
      t.color_2 <- Color.bits_to_color ((data land 0b00110000) lsr 4);
      t.color_3 <- Color.bits_to_color ((data land 0b11000000) lsr 6)
