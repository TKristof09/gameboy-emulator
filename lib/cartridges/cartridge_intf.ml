module type S = sig
  type t

  val create : Bigstringaf.t -> Cartridge_header.t -> t

  include Addressable_intf.ByteAddressable with type t := t
end

module type Instance = sig
  module Cartridge_type : S

  val this : Cartridge_type.t
end

let build_instance (type a) (module C : S with type t = a) bytes header =
    (module struct
      module Cartridge_type = C

      let this = C.create bytes header
    end : Instance)
