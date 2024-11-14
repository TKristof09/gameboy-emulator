type t =
    | ID_0
    | ID_1
    | ID_2
    | ID_3

(* returns (hi,lo) *)
let id_to_bits t =
    match t with
    | ID_0 -> (false, false)
    | ID_1 -> (false, true)
    | ID_2 -> (true, false)
    | ID_3 -> (true, true)

let bits_to_id ~lo ~hi =
    match (lo, hi) with
    | false, false -> ID_0
    | false, true -> ID_1
    | true, false -> ID_2
    | true, true -> ID_3

let set_bit_hi t hi =
    match t with
    | ID_0 -> if hi then ID_2 else ID_0
    | ID_1 -> if hi then ID_3 else ID_1
    | ID_2 -> if hi then ID_2 else ID_0
    | ID_3 -> if hi then ID_3 else ID_1

let set_bit_lo t lo =
    match t with
    | ID_0 -> if lo then ID_1 else ID_0
    | ID_1 -> if lo then ID_1 else ID_0
    | ID_2 -> if lo then ID_3 else ID_2
    | ID_3 -> if lo then ID_3 else ID_2
