type t

val create : Interrupt_manager.t -> t
val show : t -> string
val show_mode : [ `HBlank | `VBlank | `OAM_search | `Drawing ] -> string
val set_mode : t -> [ `HBlank | `VBlank | `OAM_search | `Drawing ] -> unit
val get_mode : t -> [ `HBlank | `VBlank | `OAM_search | `Drawing ]
val incr_ly : t -> unit
val reset_ly : t -> unit
val get_ly : t -> int
val set_ly_eq_lyc : t -> bool -> unit
val get_scroll : t -> int * int
val get_win_pos : t -> int * int
val ppu_is_enabled : t -> bool
val get_bg_tile_map : t -> Tile_map.map
val get_bg_win_access_mode : t -> Tile_data.access_mode
val is_bg_win_enable : t -> bool
val is_win_enable : t -> bool
val is_obj_enable : t -> bool
val get_win_tile_map : t -> Tile_map.map
val get_obj_height : t -> int

include Addressable_intf.ByteAddressable with type t := t
