(* $Id: iMap.mli,v 1.1 2003/12/19 17:24:34 yori Exp $ *)
(* Copyright 2003 Yamagata Yoriyuki. distributed with LGPL *)
(* Modified by Edgar Friendly <thelema314@gmail.com> *)

(** DIET Maps from integers, packed using ranges *)

type +'a t = (int * int * 'a) BatAvlTree.tree

type key = int

val empty : 'a t

val is_empty : 'a t -> bool
    
val add : ?eq:('a -> 'a -> bool) -> int -> 'a -> 'a t -> 'a t

val add_range : ?eq:('a -> 'a -> bool) -> int -> int -> 'a -> 'a t -> 'a t

val find : int -> 'a t -> 'a

val remove : int -> 'a t -> 'a t

val remove_range : int -> int -> 'a t -> 'a t

val from : int -> 'a t -> 'a t

val after : int -> 'a t -> 'a t

val until : int -> 'a t -> 'a t

val before : int -> 'a t -> 'a t

val mem : int -> 'a t -> bool

val iter : (int -> 'a -> unit) -> 'a t -> unit

val iter_range : (int -> int -> 'a -> unit) -> 'a t -> unit

val map : ?eq:('b -> 'b -> bool) -> ('a -> 'b) -> 'a t -> 'b t

val mapi : ?eq:('b -> 'b -> bool) -> (int -> 'a -> 'b) -> 'a t -> 'b t
    
val fold : (int -> 'b -> 'a -> 'a) -> 'b t -> 'a -> 'a

val fold_range : (int -> int -> 'b -> 'a -> 'a) -> 'b t -> 'a -> 'a

val set_to_map : BatISet.t -> 'a -> 'a t

val domain : 'a t -> BatISet.t

val map_to_set : ('a -> bool) -> 'a t -> BatISet.t

val enum : 'a t -> (int * int * 'a) BatEnum.t

val fold2_range : (int -> int -> 'a option -> 'b option -> 'c -> 'c) -> 'a t -> 'b t -> 'c -> 'c

val union : ('a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t

val forall2_range : (int -> int -> 'a option -> 'b option -> bool) -> 'a t -> 'b t -> bool

(** Infix operators over a {!BatIMap} *)
module Infix : sig
  val (-->) : 'a t -> int -> 'a
    (** [map-->key] returns the current binding of [key] in [map],
        or raises [Not_found] if no such binding exists.
        Equivalent to [find key map]. *)

  val (<--) : 'a t -> int * 'a -> 'a t
    (** [map<--(key, value)] returns a map containing the same bindings as
        [map], plus a binding of [key] to [value]. If [key] was already bound
        in [map], its previous binding disappears. Equivalent to [add key value map]
        
        {b Important warning}: {!BatIMap.add} takes an optional argument, [eq] that
        is missing in this operator [<--]. As a consequence, using [<--] implies the
        use of {{:http://caml.inria.fr/pub/docs/manual-ocaml/libref/Pervasives.html#VAL(==)}Pervasives.(==)}
        as comparison function.
    *)
end

