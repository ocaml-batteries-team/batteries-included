open Batteries

module Stdlib_verifications = struct
  (* This module asserts that all the BatFoo modules are actually
     extensions of stdlib modules, and that no functionality is lost. *)
  module Array_t =
    (Array : sig
       include module type of Legacy.Array
       val shuffle : ?state:Random.State.t -> 'a array -> unit
     end)
  module Buffer_t =
    (Buffer: sig
       include module type of Legacy.Buffer
       val add_channel : t -> BatInnerIO.input -> int -> unit
       val output_buffer : t -> string BatInnerIO.output
     end)
  module Bytes = (Bytes : module type of Legacy.Bytes)
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
  (* module Hashtbl = (Hashtbl: module type of Legacy.Hashtbl) *)
  module Hashtbl =
  struct
    module Make = (Hashtbl.Make : module type of Legacy.Hashtbl.Make)
  end
  module Int32 = (Int32: module type of Legacy.Int32)
  module Int64 = (Int64: module type of Legacy.Int64)
  module Lexing =
    (Lexing: sig
       include module type of Legacy.Lexing
       val from_channel : BatIO.input -> Lexing.lexbuf
     end)
  module List =
    (List: sig
       include module type of Legacy.List
       val find_map : ('a -> 'b option) -> 'a list -> 'b
     end)
##V>=4.7####V<4.14## module Seq = (Seq : module type of Legacy.Seq)
##V>=4.14## module Seq : sig
##V>=4.14##   include module type of Legacy.Seq
##V>=4.14## end = struct
##V>=4.14##   include BatSeq
##V>=4.14##   let equal = equal_stdlib
##V>=4.14## end
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
##V>=4.8## module Result = (Result: module type of Legacy.Result)
  (*  module Scanf = (Scanf : module type of Legacy.Scanf)*)
  (* FAILS BECAUSE OF Stack.Empty not being present because
     module Stack = (Stack : module type of Legacy.Stack)
  *)
  module Random = (Random: module type of Legacy.Random)
  module Stream = (Stream : module type of Legacy.Stream)
  module String : sig
    include module type of Legacy.String
  end = struct
    include BatString
    [@@@warning "-32"] (* unused-value-declaration *)
    let starts_with = starts_with_stdlib
    let ends_with = ends_with_stdlib
    let exists = exists_stdlib
  end
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
  (* FIXME: This does not pass for some reason:
  module Bigarray = (Bigarray : module type of Legacy.Bigarray)*)

  (* test compatibility of BatMap.S with Legacy.Map.S *)
  let sort_map (type s) (module Map : Legacy.Map.S with type key = s) l =
      Map.bindings (List.fold_right (fun x m -> Map.add x x m) l Map.empty)
  module IntMap = struct
     include BatMap.Int
     let update = update_stdlib
  end
  let _ = assert ([1,1;2,2;3,3;] = (sort_map (module IntMap) [3; 1; 2;]))
  (* test compat of BatSplay.S with Legacy.Map.S *)
  module IntSplayMap = struct
     include BatSplay.Map (BatInt)
     let update = update_stdlib
  end
  let _ = assert ([1,1;2,2;3,3;] = (sort_map (module IntSplayMap) [3; 1; 2;]))

  (* test compatibility of BatSet.S with Legacy.Set.S *)
  let sort (type s) (module Set : Legacy.Set.S with type elt = s) l =
      Set.elements (List.fold_right Set.add l Set.empty)
  module IntSet = struct
      include BatSet.Int
  end
  let _ = assert ([1;2;3] = (sort (module IntSet) [3; 1; 2;]))
end
