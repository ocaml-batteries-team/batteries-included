open Batteries

module Stdlib_verifications = struct
  (* This module asserts that all the BatFoo modules are actually
     extensions of stdlib modules, and that no functionality is lost. *)
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
  (* FAILS BECAUSE OF Stack.Empty not being present because
     module Stack = (Stack : module type of Legacy.Stack)
  *)
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
