module Base64 = BatBase64
module BitSet = BatBitSet
module Dllist = BatDllist
module DynArray = BatDynArray
module Enum = BatEnum
module ExtArray = struct
  module Array = struct include Array include BatArray end
end
module ExtHashtbl = struct
  module Hashtbl = BatHashtbl
end
module ExtList = struct
  module List = struct include List include BatList end
end
module ExtString = struct
  module String = BatString
end
module Global = BatGlobal
module IO = BatIO
module OptParse = BatOptParse
module Option = BatOption
module PMap = BatMap
module RefList = BatRefList
module Std = BatPervasives
module UChar = BatUChar
module UTF8 = BatUTF8
(* module Unzip = NOT AVAILABLE *)
