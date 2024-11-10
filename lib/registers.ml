open Uint

type t = {
    mutable a : Uint8.t;
    mutable b : Uint8.t;
    mutable c : Uint8.t;
    mutable d : Uint8.t;
    mutable e : Uint8.t;
    mutable f : Uint8.t;
    mutable h : Uint8.t;
    mutable l : Uint8.t;
  }

type r8 =
    | A
    | B
    | C
    | D
    | E
    | F
    | H
    | L

type r16 =
    | AF
    | BC
    | DE
    | HL

type flags =
    | Carry
    | Half_carry
    | Sub
    | Zero

let create () =
    {
      a = Uint8.zero;
      b = Uint8.zero;
      c = Uint8.zero;
      d = Uint8.zero;
      e = Uint8.zero;
      f = Uint8.zero;
      h = Uint8.zero;
      l = Uint8.zero;
    }

let read_r8 t reg =
    match reg with
    | A -> t.a
    | B -> t.b
    | C -> t.c
    | D -> t.d
    | E -> t.e
    | F -> t.f
    | H -> t.h
    | L -> t.l

let read_r16 t reg =
    match reg with
    | AF -> uint16_of_uint8s ~low:t.f ~high:t.a
    | BC -> uint16_of_uint8s ~low:t.c ~high:t.b
    | DE -> uint16_of_uint8s ~low:t.e ~high:t.d
    | HL -> uint16_of_uint8s ~low:t.l ~high:t.h

let write_r8 t reg value =
    match reg with
    | A -> t.a <- value
    | B -> t.b <- value
    | C -> t.c <- value
    | D -> t.d <- value
    | E -> t.e <- value
    | F -> t.f <- value
    | H -> t.h <- value
    | L -> t.l <- value

let write_r16 t reg value =
    match reg with
    | AF ->
        let f, a = uint8s_of_uint16 value in
        t.a <- a;
        t.f <- f
    | BC ->
        let c, b = uint8s_of_uint16 value in
        t.b <- b;
        t.c <- c
    | DE ->
        let e, d = uint8s_of_uint16 value in
        t.d <- d;
        t.e <- e
    | HL ->
        let l, h = uint8s_of_uint16 value in
        t.h <- h;
        t.l <- l

let set_flag t flag =
    match flag with
    | Carry -> t.f <- Uint8.(logor t.f (of_int 0x10))
    | Half_carry -> t.f <- Uint8.(logor t.f (of_int 0x20))
    | Sub -> t.f <- Uint8.(logor t.f (of_int 0x40))
    | Zero -> t.f <- Uint8.(logor t.f (of_int 0x80))

let unset_flag t flag =
    match flag with
    | Carry -> t.f <- Uint8.(logand t.f (of_int 0b11101111))
    | Half_carry -> t.f <- Uint8.(logand t.f (of_int 0b11011111))
    | Sub -> t.f <- Uint8.(logand t.f (of_int 0b10111111))
    | Zero -> t.f <- Uint8.(logand t.f (of_int 0b01111111))

let read_flag t flag =
    match flag with
    | Carry -> Uint8.(logand t.f (of_int 0x10)) <> Uint8.zero
    | Half_carry -> Uint8.(logand t.f (of_int 0x20)) <> Uint8.zero
    | Sub -> Uint8.(logand t.f (of_int 0x40)) <> Uint8.zero
    | Zero -> Uint8.(logand t.f (of_int 0x80)) <> Uint8.zero

let set_flags t ?(c = read_flag t Carry) ?(h = read_flag t Half_carry) ?(s = read_flag t Sub)
    ?(z = read_flag t Zero) () =
    if c then set_flag t Carry else unset_flag t Carry;
    if h then set_flag t Half_carry else unset_flag t Half_carry;
    if s then set_flag t Sub else unset_flag t Sub;
    if z then set_flag t Zero else unset_flag t Zero

let show_r8 r =
    match r with
    | A -> "A"
    | B -> "B"
    | C -> "C"
    | D -> "D"
    | E -> "E"
    | F -> "F"
    | H -> "H"
    | L -> "L"

let show_r16 r =
    match r with
    | AF -> "AF"
    | BC -> "BC"
    | DE -> "DE"
    | HL -> "HL"

let show t =
    Printf.sprintf "A: %s B: %s C: %s D: %s E: %s F: %s H: %s L: %s" (Uint8.to_string_hex t.a)
      (Uint8.to_string_hex t.b) (Uint8.to_string_hex t.c) (Uint8.to_string_hex t.d)
      (Uint8.to_string_hex t.e) (Uint8.to_string_hex t.f) (Uint8.to_string_hex t.h)
      (Uint8.to_string_hex t.l)
