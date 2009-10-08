(* $Id: iSet.mli,v 1.1 2003/12/19 17:24:34 yori Exp $ *)
(* Copyright 2003 Yamagata Yoriyuki. distributed with LGPL *)

type t = (int * int) AvlTree.tree

type elt = int

val empty : t

val is_empty : t -> bool

val mem : int -> t -> bool

val add : int -> t -> t

val add_range : int -> int -> t -> t

val singleton : int -> t

val remove : int -> t -> t

val remove_range : int -> int -> t -> t

val union : t -> t -> t

val inter : t -> t -> t

val diff : t -> t -> t

val compl : t -> t

val compare : t -> t -> int

val equal : t -> t -> bool

val subset : t -> t -> bool

val from : int -> t -> t

val after : int -> t -> t

val until : int -> t -> t

val before : int -> t -> t

val iter : (int -> unit) -> t -> unit

val iter_range : (int -> int -> unit) -> t -> unit

val fold : (int -> 'a -> 'a) -> t -> 'a -> 'a

val fold_range : (int -> int -> 'a -> 'a) -> t -> 'a -> 'a

val for_all : (int -> bool) -> t -> bool

val exists : (int -> bool) -> t -> bool
    
val filter : (int -> bool) -> t -> t

val partition : (int -> bool) -> t -> t * t

val cardinal : t -> int

val elements : t -> int list

val ranges : t -> (int * int) list

val min_elt : t -> int

val max_elt : t -> int

val choose : t -> int
