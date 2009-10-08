(* $Id: iMap.mli,v 1.1 2003/12/19 17:24:34 yori Exp $ *)
(* Copyright 2003 Yamagata Yoriyuki. distributed with LGPL *)
(* Modified by Edgar Friendly <thelema314@gmail.com> *)

type +'a t = (int * int * 'a) AvlTree.tree

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

val set_to_map : ISet.t -> 'a -> 'a t

val domain : 'a t -> ISet.t

val map_to_set : ('a -> bool) -> 'a t -> ISet.t

val enum : 'a t -> (int * int * 'a) Enum.t
