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
end

(* stdlib modules *)
(* Arg *)
module Array = struct include Array include BatArray end
(* ArrayLabels *)
module Buffer = BatBuffer
(* Callback *)
module Char = BatChar
module Complex = BatComplex
module Digest = BatDigest
(* Filename *)
module Format = struct include Format include BatFormat end
module Gc = BatGc
module Genlex = struct include Genlex include BatGenlex end
module Hashtbl = BatHashtbl
module Int32 = BatInt32
module Int64 = BatInt64
(* Lazy *)
module Lexing = struct include Lexing include BatLexing end
module List = BatList
(* ListLabels *)
module Map = BatMap
module Marshal = struct include Marshal include BatMarshal end
(* MoreLabels *)
module Nativeint = BatNativeint
module Oo = struct include Oo include BatOo end
(* Parsing *)
module Printexc = struct include Printexc include BatPrintexc end
module Printf = struct include Printf include BatPrintf end
module Queue = struct include Queue include BatQueue end
module Random = BatRandom
module Scanf = BatScanf
module Set = BatSet
(* Sort - Deprecated *)
module Stack = struct include Stack include BatStack end
module Stream = struct include Stream include BatStream end
module String = struct include String include BatString end
(* StringLabels *)
module Sys = struct include Sys include BatSys end
(* Weak *)

module Unix = struct include Unix include BatUnix end

(*module Str = struct include Str include BatStr end*)

module Big_int = struct include Big_int include BatBig_int end

module Bigarray = BatBigarray

module Stdlib_verifications = struct
  (* This module asserts that all the BatFoo modules are actually
extensions of stdlib modules, and that no functionality is lost *)
  module Array_t = (Array : module type of Legacy.Array)
  module Buffer_t =
    (Buffer: sig
	 include module type of Legacy.Buffer
	 val add_channel : t -> BatInnerIO.input -> int -> unit
	 val output_buffer : t -> string BatInnerIO.output
       end)
  module Char_t = (Char: module type of Legacy.Char)
  module Complex_t = (Complex : module type of Legacy.Complex)
  module Digest =
    (Digest: sig
	 include module type of Legacy.Digest
	 val channel : BatIO.input -> int -> Digest.t
	 val output : 'a BatIO.output -> t -> unit
	 val input : BatIO.input -> Digest.t
       end)
(*  module Format = (Format: module type of Legacy.Format)*)
  module Gc =
    (Gc: sig
	 include module type of Legacy.Gc
	 val print_stat : 'a BatInnerIO.output -> unit
       end)
  module Genlex = (Genlex : module type of Legacy.Genlex)
(*  module Hashtbl = (Hashtbl: module type of Legacy.Hashtbl)*)
  module Int32 = (Int32: module type of Legacy.Int32)
  module Int64 = (Int64: module type of Legacy.Int64)
  module Lexing =
    (Lexing: sig
	 include module type of Legacy.Lexing
	 val from_channel : BatIO.input -> Lexing.lexbuf
       end)
  module List = (List: module type of Legacy.List)
(*  module Map = (Map : module type of Legacy.Map)*)
  module Marshal =
    (Marshal: sig
	 include module type of Legacy.Marshal
	 val to_channel : _ BatIO.output -> 'b -> extern_flags list -> unit
	 val from_channel : BatIO.input -> 'a
       end)
  module Nativeint = (Nativeint: module type of Legacy.Nativeint)
  module Oo = (Oo : module type of Legacy.Oo)
  module Printexc =
    (Printexc: sig
	 include module type of Legacy.Printexc
	 val print : 'a BatInnerIO.output -> exn -> unit
	 val print_backtrace : 'a BatInnerIO.output -> unit
       end)
(*  module Printf = (Printf: module type of Legacy.Printf)*)
  module Queue = (Queue: module type of Legacy.Queue)
  module Random = (Random: module type of Legacy.Random)
(*  module Scanf = (Scanf : module type of Legacy.Scanf)*)
(*  module Set = (Set: module type of Legacy.Set)*)
  module Stack = (Stack : module type of Legacy.Stack)
  module Stream = (Stream : module type of Legacy.Stream)
  module String = (String : module type of Legacy.String)
  module Sys = (Sys : module type of Legacy.Sys)
  module Unix =
    (Unix: sig
	 include module type of Legacy.Unix
	 val in_channel_of_descr : Unix.file_descr -> BatInnerIO.input
	 val out_channel_of_descr : Unix.file_descr -> unit BatInnerIO.output
	 val descr_of_in_channel : BatInnerIO.input -> Unix.file_descr
	 val descr_of_out_channel : unit BatInnerIO.output -> Unix.file_descr
	 val open_process_in :
	   ?autoclose:bool -> ?cleanup:bool -> string -> BatInnerIO.input
	 val open_process_out :
	   ?cleanup:bool -> string -> unit BatInnerIO.output
	 val open_process :
	   ?autoclose:bool ->
	   ?cleanup:bool ->
	   string -> BatInnerIO.input * unit BatInnerIO.output
	 val open_process_full :
	   ?autoclose:bool ->
	   ?cleanup:bool ->
	   string ->
	   string array ->
	   BatInnerIO.input * unit BatInnerIO.output * BatInnerIO.input
	 val close_process_in : BatInnerIO.input -> Unix.process_status
	 val close_process_out :
	   unit BatInnerIO.output -> Unix.process_status
	 val close_process :
	   BatInnerIO.input * unit BatInnerIO.output -> Unix.process_status
	 val close_process_full :
	   BatInnerIO.input * unit BatInnerIO.output * BatInnerIO.input ->
	   Unix.process_status
	 val open_connection :
	   ?autoclose:bool ->
	   Unix.sockaddr -> BatInnerIO.input * unit BatInnerIO.output
	 val shutdown_connection : BatInnerIO.input -> unit
	 val establish_server :
	   ?autoclose:bool ->
	   ?cleanup:bool ->
	   (BatInnerIO.input -> unit BatInnerIO.output -> unit) ->
	   Unix.sockaddr -> unit
       end)
  module Big_int = (Big_int : module type of Legacy.Big_int)
  module Bigarray = (Bigarray : module type of Legacy.Bigarray)
end


(* Extlib modules not replacing stdlib *)
module Base64 = BatBase64
module BitSet = BatBitSet
module Bit_set = BatBitSet
module Dllist = BatDllist
module DynArray = BatDynArray
module Enum = BatEnum
module File = BatFile
module Global = BatGlobal
module IO = BatIO
module LazyList = BatLazyList
module MultiPMap = BatMultiPMap
module Option = BatOption
(* REMOVED, Extlib only module OptParse = BatOptParse *)
module RefList = BatRefList
module Ref = BatRef
(*module Std = REMOVED - use BatPervasives *)

(* Batteries specific modules *)
module Cache = BatCache
module CharParser = BatCharParser
module Deque = BatDeque
module Hashcons = BatHashcons
module Heap = BatHeap
module FingerTree = BatFingerTree
module Logger = BatLogger
module MultiMap = BatMultiMap
module ParserCo = BatParserCo
module PathGen = BatPathGen
module Print = BatPrint
module Result = BatResult
module Return = BatReturn
module Seq = BatSeq
module Tuple = BatTuple
module Tuple2 = BatTuple.Tuple2
module Tuple3 = BatTuple.Tuple3
module Tuple4 = BatTuple.Tuple4
module Tuple5 = BatTuple.Tuple5
module ValuePrinter = BatValuePrinter
module Vect = BatVect
module ISet = BatISet
module IMap = BatIMap
module Splay = BatSplay
module Uref = BatUref
module Text = Ulib.Text
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
  module Log = BatLog
  module Substring = BatSubstring
end

(* Pervasives last *)
include Pervasives
include BatPervasives
