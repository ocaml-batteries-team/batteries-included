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
   For more information, see the documentation of module {!Map}

   @documents MapLabels
*)

  module type OrderedType = Map.OrderedType
  module type S =
    sig
      type key
      and (+'a) t
      val empty : 'a t
      val is_empty: 'a t -> bool
      val add : key:key -> data:'a -> 'a t -> 'a t
      val find : key -> 'a t -> 'a
      val remove : key -> 'a t -> 'a t
      val mem : key -> 'a t -> bool
      val iter : f:(key:key -> data:'a -> unit) -> 'a t -> unit
      val map : f:('a -> 'b) -> 'a t -> 'b t
      val mapi : f:(key -> 'a -> 'b) -> 'a t -> 'b t
      val fold :
          f:(key:key -> data:'a -> 'b -> 'b) ->
          'a t -> init:'b -> 'b
      val compare: cmp:('a -> 'a -> int) -> 'a t -> 'a t -> int
      val equal: cmp:('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  end
  module Make : functor (Ord : OrderedType) -> S with type key = Ord.t
