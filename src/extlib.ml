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
  module String = struct include String include BatString end
end
module Global = BatGlobal
module IO = BatIO
module OptParse = BatOptParse
module Option = BatOption
module PMap = BatMap
module RefList = BatRefList
module Std = BatPervasives
module UChar = Ulib.UChar
module UTF8 = Ulib.UTF8
(* module Unzip = NOT AVAILABLE *)
