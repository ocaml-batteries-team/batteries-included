(* Copyright 2003 Yamagata Yoriyuki. distributed with LGPL *)
(* Modified by Edgar Friendly <thelema314@gmail.com> *)

(** Internals of ISet and IMap, usable as generic tree library *)

type +'a tree

val empty : 'a tree

val is_empty : 'a tree -> bool

val make_tree : 'a tree -> 'a -> 'a tree -> 'a tree

val create : 'a tree -> 'a -> 'a tree -> 'a tree
(** [create l v r] is similar to [make_tree l v r] but performs no rebalancing;
    in other words, you should use this only when you {e know} that [l] and [r]
    are already balanced. *)

val height : 'a tree -> int

val left_branch : 'a tree -> 'a tree
(** @raise Not_found if the tree is empty *)

val right_branch : 'a tree -> 'a tree
(** @raise Not_found if the tree is empty *)

val root : 'a tree -> 'a
(** @raise Not_found if the tree is empty *)

(* Utilities *)
val singleton_tree : 'a -> 'a tree
val split_leftmost : 'a tree -> 'a * 'a tree
val split_rightmost : 'a tree -> 'a * 'a tree

val concat : 'a tree -> 'a tree -> 'a tree

val iter : ('a -> unit) -> 'a tree -> unit

val fold : ('a -> 'b -> 'b) -> 'a tree -> 'b -> 'b

val enum : 'a tree -> 'a BatEnum.t

(* Sanity checks *)

val check : 'a tree -> bool
(** Check that the tree is balanced according to the AVL tree rules.
    An AVL tree is balanced when for every node the height of the
    subnodes differs by at most 1.

    @since 2.3.0
*)

(**/**)
(* Helpers for testing *)

val check_height_cache : 'a tree -> bool
val check_height_balance : 'a tree -> bool

(**/**)
