(* $Id: avlTree.mli,v 1.3 2003/06/18 15:11:07 yori Exp $ *)
(* Copyright 2003 Yamagata Yoriyuki. distributed with LGPL *)
(* Modified by Edgar Friendly <thelema314@gmail.com> *)

(** Internals of ISet and IMap, usable as generic tree library *)

type +'a tree

val empty : 'a tree

val is_empty : 'a tree -> bool

val make_tree : 'a tree -> 'a -> 'a tree -> 'a tree

val height : 'a tree -> int

(* If tree is empty, then raise Not_found *)
val left_branch : 'a tree -> 'a tree
val right_branch : 'a tree -> 'a tree
val root : 'a tree -> 'a

(* Utilities *)
val singleton_tree : 'a -> 'a tree
val split_leftmost : 'a tree -> 'a * 'a tree
val split_rightmost : 'a tree -> 'a * 'a tree

val concat : 'a tree -> 'a tree -> 'a tree

val iter : ('a -> unit) -> 'a tree -> unit

val fold : ('a -> 'b -> 'b) -> 'a tree -> 'b -> 'b

val enum : 'a tree -> 'a BatEnum.t
