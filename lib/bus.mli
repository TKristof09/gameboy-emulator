type t

val create :
  ppu:Ppu.t ->
  wram:Ram.t ->
  hram:Ram.t ->
  boot_rom:Cartridge.t ->
  cartridge:Cartridge.t ->
  interrupt_manager:Interrupt_manager.t ->
  t

include Addressable_intf.WordAddressable with type t := t
