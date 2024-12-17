open Gameboy
open Uint

let create () =
    let interrupt_manager = Interrupt_manager.create () in
    (* enable all interrupts *)
    Interrupt_manager.set_master_enable interrupt_manager true;
    Interrupt_manager.write_byte interrupt_manager ~addr:(Uint16.of_int 0xFFFF) ~data:Uint8.max_int;
    (Timer.create interrupt_manager, interrupt_manager)

let print_before_after timer im cycles =
    Timer.run timer ~mcycles:(cycles - 1);
    Timer.show timer |> print_endline;
    Timer.run timer ~mcycles:1;
    Timer.show timer |> print_endline;
    match Interrupt_manager.get_pending im with
    | Some interrupt ->
        Printf.printf "Intterrupt requested: %s\n" (Interrupt_manager.show_interrupt_type interrupt)
    | None -> ()

let%expect_test "create" =
    let timer, _ = create () in
    Timer.show timer |> print_endline;
    [%expect
        {|
      { Timer.interrupt_manager = <opaque>; m_cycle_counter = 0; div = 0;
        tima_value = 0; tima_enabled = false; tima_freq = Timer.F_256; tma = 0 }
      |}]

let%expect_test "div increment" =
    let timer, im = create () in
    print_before_after timer im 64;
    [%expect
        {|
      { Timer.interrupt_manager = <opaque>; m_cycle_counter = 63; div = 0;
        tima_value = 0; tima_enabled = false; tima_freq = Timer.F_256; tma = 0 }
      { Timer.interrupt_manager = <opaque>; m_cycle_counter = 64; div = 1;
        tima_value = 0; tima_enabled = false; tima_freq = Timer.F_256; tma = 0 }
      |}]

let%expect_test "tima increment f256" =
    let timer, im = create () in
    Timer.write_byte timer ~addr:(Uint16.of_int 0xFF07) ~data:(Uint8.of_int 0b100);
    print_before_after timer im 256;
    [%expect
        {|
      { Timer.interrupt_manager = <opaque>; m_cycle_counter = 255; div = 3;
        tima_value = 0; tima_enabled = true; tima_freq = Timer.F_256; tma = 0 }
      { Timer.interrupt_manager = <opaque>; m_cycle_counter = 0; div = 4;
        tima_value = 1; tima_enabled = true; tima_freq = Timer.F_256; tma = 0 }
      |}]

let%expect_test "tima increment f4" =
    let timer, im = create () in
    Timer.write_byte timer ~addr:(Uint16.of_int 0xFF07) ~data:(Uint8.of_int 0b101);
    print_before_after timer im 4;
    [%expect
        {|
      { Timer.interrupt_manager = <opaque>; m_cycle_counter = 3; div = 0;
        tima_value = 0; tima_enabled = true; tima_freq = Timer.F_4; tma = 0 }
      { Timer.interrupt_manager = <opaque>; m_cycle_counter = 4; div = 0;
        tima_value = 1; tima_enabled = true; tima_freq = Timer.F_4; tma = 0 }
      |}]

let%expect_test "tima increment f16" =
    let timer, im = create () in
    Timer.write_byte timer ~addr:(Uint16.of_int 0xFF07) ~data:(Uint8.of_int 0b110);
    print_before_after timer im 16;
    [%expect
        {|
      { Timer.interrupt_manager = <opaque>; m_cycle_counter = 15; div = 0;
        tima_value = 0; tima_enabled = true; tima_freq = Timer.F_16; tma = 0 }
      { Timer.interrupt_manager = <opaque>; m_cycle_counter = 16; div = 0;
        tima_value = 1; tima_enabled = true; tima_freq = Timer.F_16; tma = 0 }
      |}]

let%expect_test "tima increment f64" =
    let timer, im = create () in
    Timer.write_byte timer ~addr:(Uint16.of_int 0xFF07) ~data:(Uint8.of_int 0b111);
    print_before_after timer im 64;
    [%expect
        {|
      { Timer.interrupt_manager = <opaque>; m_cycle_counter = 63; div = 0;
        tima_value = 0; tima_enabled = true; tima_freq = Timer.F_64; tma = 0 }
      { Timer.interrupt_manager = <opaque>; m_cycle_counter = 64; div = 1;
        tima_value = 1; tima_enabled = true; tima_freq = Timer.F_64; tma = 0 }
      |}]

let%expect_test "tima reset + interrupt" =
    let timer, im = create () in
    Timer.write_byte timer ~addr:(Uint16.of_int 0xFF07) ~data:(Uint8.of_int 0b101);
    (* set TMA to 42 *)
    Timer.write_byte timer ~addr:(Uint16.of_int 0xFF06) ~data:(Uint8.of_int 42);
    print_before_after timer im (4 * 256);
    [%expect
        {|
      { Timer.interrupt_manager = <opaque>; m_cycle_counter = 255; div = 15;
        tima_value = 255; tima_enabled = true; tima_freq = Timer.F_4; tma = 42 }
      { Timer.interrupt_manager = <opaque>; m_cycle_counter = 0; div = 16;
        tima_value = 42; tima_enabled = true; tima_freq = Timer.F_4; tma = 42 }
      Intterrupt requested: Interrupt_manager.Timer
      |}]

let%expect_test "div reset on write" =
    let timer, im = create () in
    print_before_after timer im 64;
    Timer.write_byte timer ~addr:(Uint16.of_int 0xFF04) ~data:(Uint8.of_int 42);
    Timer.show timer |> print_endline;
    [%expect
        {|
      { Timer.interrupt_manager = <opaque>; m_cycle_counter = 63; div = 0;
        tima_value = 0; tima_enabled = false; tima_freq = Timer.F_256; tma = 0 }
      { Timer.interrupt_manager = <opaque>; m_cycle_counter = 64; div = 1;
        tima_value = 0; tima_enabled = false; tima_freq = Timer.F_256; tma = 0 }
      { Timer.interrupt_manager = <opaque>; m_cycle_counter = 0; div = 0;
        tima_value = 0; tima_enabled = false; tima_freq = Timer.F_256; tma = 0 }
      |}]
