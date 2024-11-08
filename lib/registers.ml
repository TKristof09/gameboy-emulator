open Stdint

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

let uint16_of_uint8s high low =
    let high = Uint16.shift_left (Uint16.of_uint8 high) 8
    and low = Uint16.of_uint8 low in
    Uint16.logor high low

let uint8s_of_uint16 value =
    let high = Uint16.to_uint8 (Uint16.shift_right_logical value 8)
    and low = Uint16.to_uint8 value in
    (high, low)

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
    | AF -> uint16_of_uint8s t.a t.f
    | BC -> uint16_of_uint8s t.b t.c
    | DE -> uint16_of_uint8s t.d t.e
    | HL -> uint16_of_uint8s t.h t.l

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
        let a, f = uint8s_of_uint16 value in
        t.a <- a;
        t.f <- f
    | BC ->
        let b, c = uint8s_of_uint16 value in
        t.b <- b;
        t.c <- c
    | DE ->
        let d, e = uint8s_of_uint16 value in
        t.d <- d;
        t.e <- e
    | HL ->
        let h, l = uint8s_of_uint16 value in
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
