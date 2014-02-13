(* open this to extend all Foo with BatFoo *)

module Legacy = struct
  include Pervasives
  module Arg = Arg
  module Array = Array
  module ArrayLabels = ArrayLabels
  module Buffer = Buffer
  module Callback = Callback
  module Char = Char
  module Complex = Complex
  module Digest = Digest
  module Filename = Filename
  module Format = Format
  module Gc = Gc
  module Genlex = Genlex
  module Hashtbl = Hashtbl
  module Int32 = Int32
  module Int64 = Int64
  module Lazy = Lazy
  module Lexing = Lexing
  module List = List
  module ListLabels = ListLabels
  module Map = Map
  module Marshal = Marshal
  module MoreLabels = MoreLabels
  module Nativeint = Nativeint
  module Oo = Oo
  module Parsing = Parsing
  module Printexc = Printexc
  module Printf = Printf
  module Queue = Queue
  module Random = Random
  module Scanf = Scanf
  module Set = Set
  module Sort = Sort
  module Stack = Stack
  module StdLabels = StdLabels
  module Stream = Stream
  module String = String
  module StringLabels = StringLabels
  module Sys = Sys
  module Weak = Weak
  module Unix = Unix
  module Num = Num
  module Big_int = Big_int
  module Bigarray = Bigarray
  module Str = Str
end

(* stdlib modules *)
(* Arg *)
module Array = BatArray
(* ArrayLabels *)
module Buffer = BatBuffer
(* Callback *)
module Char = BatChar
module Complex = BatComplex
(* Filename *)
module Format = BatFormat
module Gc = BatGc
module Hashtbl = BatHashtbl
module Int32 = BatInt32
module Int64 = BatInt64
(* Lazy *)
module List = BatList
(* ListLabels *)
module Map = BatMap
module Marshal = BatMarshal
(* MoreLabels *)
module Nativeint = BatNativeint
module Oo = BatOo
(* Parsing *)
module Printexc = BatPrintexc
module Printf = BatPrintf (* UNTESTED FOR BACKWARDS COMPATIBILITY *)
module Queue = BatQueue
module Random = BatRandom
module Set = BatSet
(* Sort - Deprecated *)
module Stack = BatStack
module Stream = BatStream
module String = BatString
(* StringLabels *)
module Sys = BatSys
(* Weak *)

(*module Str = struct include Str include BatStr end*)

module Bigarray = BatBigarray

module Global = BatGlobal
module Option = BatOption
(* REMOVED, Extlib only module OptParse = BatOptParse *)
module Ref = BatRef
(*module Std = REMOVED - use BatPervasives *)

(* Batteries specific modules *)
module Heap = BatHeap
module Result = BatResult
module Return = BatReturn
module Tuple = BatTuple
module Tuple2 = BatTuple.Tuple2
module Tuple3 = BatTuple.Tuple3
module Tuple4 = BatTuple.Tuple4
module Tuple5 = BatTuple.Tuple5
module Concurrent = BatConcurrent

(* Batteries Specific *)
module Interfaces = BatInterfaces
module Number = BatNumber
module Float = BatFloat
module Int = BatInt
module Bool = BatBool
module Unit = BatUnit
(*module Int63 = BatInt63*)

(* Modules in-progress, API stability not guaranteed *)
module Incubator = struct
  module Bounded = BatBounded
end

(* Pervasives last *)
include Pervasives
include BatPervasives
