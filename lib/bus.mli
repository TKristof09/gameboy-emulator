type t

val create :
  ppu:Ppu.t ->
  wram:Ram.t ->
  hram:Ram.t ->
  boot_rom:Bigstringaf.t ->
  cartridge:Cartridge.t ->
  interrupt_manager:Interrupt_manager.t ->
  joypad:Joypad.t ->
  timer:Timer.t ->
  t

include Addressable_intf.WordAddressable with type t := t
