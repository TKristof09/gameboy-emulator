include Stdint

let uint16_of_uint8s ~low ~high =
    let high = Uint16.shift_left (Uint16.of_uint8 high) 8
    and low = Uint16.of_uint8 low in
    Uint16.logor high low

let uint8s_of_uint16 value =
    let high = Uint16.to_uint8 (Uint16.shift_right_logical value 8)
    and low = Uint16.to_uint8 value in
    (low, high)
