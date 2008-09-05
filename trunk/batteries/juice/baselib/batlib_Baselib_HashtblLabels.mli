(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*               Jacques Garrigue, Kyoto University RIMS               *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(*
  Modified by David Teller to extract relevant parts
*)

(**
   For information, see module {!Hashtbl}.

   @replace HashtblLabels
*)

  type ('a, 'b) t = ('a, 'b) Hashtbl.t
  val create : int -> ('a, 'b) t
  val clear : ('a, 'b) t -> unit
  val add : ('a, 'b) t -> key:'a -> data:'b -> unit
  val copy : ('a, 'b) t -> ('a, 'b) t
  val find : ('a, 'b) t -> 'a -> 'b
  val find_all : ('a, 'b) t -> 'a -> 'b list
  val mem : ('a, 'b) t -> 'a -> bool
  val remove : ('a, 'b) t -> 'a -> unit
  val replace : ('a, 'b) t -> key:'a -> data:'b -> unit
  val iter : f:(key:'a -> data:'b -> unit) -> ('a, 'b) t -> unit
  val fold :
      f:(key:'a -> data:'b -> 'c -> 'c) ->
        ('a, 'b) t -> init:'c -> 'c
  val length : ('a, 'b) t -> int
  module type HashedType = Hashtbl.HashedType
  module type S =
    sig
      type key
      and 'a t
      val create : int -> 'a t
      val clear : 'a t -> unit
      val copy : 'a t -> 'a t
      val add : 'a t -> key:key -> data:'a -> unit
      val remove : 'a t -> key -> unit
      val find : 'a t -> key -> 'a
      val find_all : 'a t -> key -> 'a list
      val replace : 'a t -> key:key -> data:'a -> unit
      val mem : 'a t -> key -> bool
      val iter : f:(key:key -> data:'a -> unit) -> 'a t -> unit
      val fold :
          f:(key:key -> data:'a -> 'b -> 'b) ->
          'a t -> init:'b -> 'b
      val length : 'a t -> int
    end
  module Make : functor (H : HashedType) -> S with type key = H.t
  val hash : 'a -> int
  external hash_param : int -> int -> 'a -> int
      = "caml_hash_univ_param" "noalloc"
