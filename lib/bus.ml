open Uint

type t = {
    ppu : Ppu.t;
    wram : Ram.t;
    hram : Ram.t;
    boot_rom : Cartridge.t;
    cartridge : Cartridge.t;
    interrupt_manager : Interrupt_manager.t;
    joypad : Joypad.t;
    timer : Timer.t;
    mutable is_boot_rom_disabled : bool;
    mutable dma : uint8;
  }

let create ~ppu ~wram ~hram ~boot_rom ~cartridge ~interrupt_manager ~joypad ~timer =
    {
      ppu;
      wram;
      hram;
      boot_rom;
      cartridge;
      interrupt_manager;
      joypad;
      timer;
      is_boot_rom_disabled = false;
      dma = Uint8.zero;
    }

let read_byte t addr =
    match Uint16.to_int addr with
    | addr_int when (not t.is_boot_rom_disabled) && 0x0000 <= addr_int && addr_int <= 0x00FF ->
        Cartridge.read_byte t.boot_rom addr
    | addr_int when 0x0000 <= addr_int && addr_int <= 0x7FFF -> Cartridge.read_byte t.cartridge addr
    | addr_int when 0xC000 <= addr_int && addr_int <= 0xDFFF -> Ram.read_byte t.wram addr
    | addr_int when 0xFF80 <= addr_int && addr_int <= 0xFFFE -> Ram.read_byte t.hram addr
    | _ when Ppu.accepts_address addr -> Ppu.read_byte t.ppu addr
    | 0xFF50 -> Uint8.of_int (Bool.to_int t.is_boot_rom_disabled)
    | 0xFF0F
    | 0xFFFF ->
        Interrupt_manager.read_byte t.interrupt_manager addr
    | 0xFF46 -> t.dma
    (* illegal addresses return 0 according to some sources and FF according to others so idk *)
    | addr_int when 0xFEA0 <= addr_int && addr_int <= 0xFEFF -> Uint8.of_int 0x00
    | 0xFF00 -> Joypad.read_byte t.joypad addr
    | addr_int when 0xFF04 <= addr_int && addr_int <= 0xFF07 -> Timer.read_byte t.timer addr
    | 0xFF4D -> Uint8.max_int (* speed switch, gcb only functionality *)
    | _ ->
        failwith @@ Printf.sprintf "Unhandled memory location read %s" (Uint16.to_string_hex addr)

let write_byte t ~addr ~data =
    match Uint16.to_int addr with
    | addr_int when 0x0000 <= addr_int && addr_int <= 0x7FFF ->
        Cartridge.write_byte t.cartridge ~addr ~data
    | addr_int when 0xC000 <= addr_int && addr_int <= 0xDFFF -> Ram.write_byte t.wram ~addr ~data
    | addr_int when 0xFF80 <= addr_int && addr_int <= 0xFFFE -> Ram.write_byte t.hram ~addr ~data
    | _ when Ppu.accepts_address addr -> Ppu.write_byte t.ppu ~addr ~data
    | 0xFF50 -> t.is_boot_rom_disabled <- Uint8.(data <> zero)
    | 0xFF0F
    | 0xFFFF ->
        Interrupt_manager.write_byte t.interrupt_manager ~addr ~data
    | 0xFF46 ->
        t.dma <- data;
        let source = Uint16.(shift_left (of_uint8 data) 8) in
        Seq.init 0x9F (fun i -> Uint16.(of_int i))
        |> Seq.iter (fun offset ->
               let data = read_byte t Uint16.(source + offset) in
               Ppu.write_byte t.ppu ~addr:Uint16.(of_int 0xFE00 + offset) ~data)
    | addr_int when 0xFEA0 <= addr_int && addr_int <= 0xFEFF ->
        (* This just does nothing but the program can "write" to it, (tetris does this, apparently is a bug but is legal) *)
        ()
    | 0xFF00 -> Joypad.write_byte t.joypad ~addr ~data
    | addr_int when 0xFF04 <= addr_int && addr_int <= 0xFF07 -> Timer.write_byte t.timer ~addr ~data
    | _ -> Tsdl.Sdl.log "Unhandled memory location write %s\n" (Uint16.to_string_hex addr)

let read_word t addr =
    let lo = read_byte t addr in
    let hi = read_byte t (Uint16.succ addr) in
    uint16_of_uint8s ~low:lo ~high:hi

let write_word t ~addr ~data =
    let lo, hi = uint8s_of_uint16 data in
    write_byte t ~addr ~data:lo;
    write_byte t ~addr:(Uint16.succ addr) ~data:hi
