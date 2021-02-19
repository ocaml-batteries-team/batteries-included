(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*         Gabriel Scherer, projet Parsifal, INRIA Saclay                 *)
(*                                                                        *)
(*   Copyright 2019 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

##V>=4.12##(** Either type.
##V>=4.12##
##V>=4.12##    Either is the simplest and most generic sum/variant type:
##V>=4.12##    a value of [('a, 'b) Either.t] is either a [Left (v : 'a)]
##V>=4.12##    or a [Right (v : 'b)].
##V>=4.12##
##V>=4.12##    It is a natural choice in the API of generic functions where values
##V>=4.12##    could fall in two different cases, possibly at different types,
##V>=4.12##    without assigning a specific meaning to what each case should be.
##V>=4.12##
##V>=4.12##    For example:

##V>=4.12##{[List.partition_map:
##V>=4.12##    ('a -> ('b, 'c) Either.t) -> 'a list -> 'b list * 'c list]}
##V>=4.12##
##V>=4.12##    If you are looking for a parametrized type where
##V>=4.12##    one alternative means success and the other means failure,
##V>=4.12##    you should use the more specific type {!Result.t}.
##V>=4.12##
##V>=4.12##    @since 4.12
##V>=4.12##*)

##V>=4.12##(* Unlike [result], no [either] type is made available in Stdlib,
##V>=4.12##   one needs to access [Either.t] explicitly:
##V>=4.12##
##V>=4.12##   - This type is less common in typical OCaml codebases,
##V>=4.12##     which prefer domain-specific variant types whose constructors
##V>=4.12##     carry more meaning.
##V>=4.12##   - Adding this to Stdlib would raise warnings in existing codebases
##V>=4.12##     that already use a constructor named Left or Right:
##V>=4.12##     + when opening a module that exports such a name,
##V>=4.12##       warning 45 is raised
##V>=4.12##     + adding a second constructor of the same name in scope kicks
##V>=4.12##       in the disambiguation mechanisms, and warning 41 may now
##V>=4.12##       be raised by existing code.
##V>=4.12##
##V>=4.12##   If the use becomes more common in the future we can always
##V>=4.12##   revisit this choice.
##V>=4.12##*)

##V>=4.12##type ('a, 'b) t = Left of 'a | Right of 'b (**)
##V>=4.12##(** A value of [('a, 'b) Either.t] contains
##V>=4.12##    either a value of ['a]  or a value of ['b] *)

##V>=4.12##val left : 'a -> ('a, 'b) t
##V>=4.12##(** [left v] is [Left v]. *)

##V>=4.12##val right : 'b -> ('a, 'b) t
##V>=4.12##(** [right v] is [Right v]. *)

##V>=4.12##val is_left : ('a, 'b) t -> bool
##V>=4.12##(** [is_left (Left v)] is [true], [is_left (Right v)] is [false]. *)

##V>=4.12##val is_right : ('a, 'b) t -> bool
##V>=4.12##(** [is_right (Left v)] is [false], [is_right (Right v)] is [true]. *)

##V>=4.12##val find_left : ('a, 'b) t -> 'a option
##V>=4.12##(** [find_left (Left v)] is [Some v], [find_left (Right _)] is [None] *)

##V>=4.12##val find_right : ('a, 'b) t -> 'b option
##V>=4.12##(** [find_right (Right v)] is [Some v], [find_right (Left _)] is [None] *)

##V>=4.12##val map_left : ('a1 -> 'a2) -> ('a1, 'b) t -> ('a2, 'b) t
##V>=4.12##(** [map_left f e] is [Left (f v)] if [e] is [Left v]
##V>=4.12##    and [e] if [e] is [Right _]. *)

##V>=4.12##val map_right : ('b1 -> 'b2) -> ('a, 'b1) t -> ('a, 'b2) t
##V>=4.12##(** [map_right f e] is [Right (f v)] if [e] is [Right v]
##V>=4.12##    and [e] if [e] is [Left _]. *)

##V>=4.12##val map :
##V>=4.12##  left:('a1 -> 'a2) -> right:('b1 -> 'b2) -> ('a1, 'b1) t -> ('a2, 'b2) t
##V>=4.12##(** [map ~left ~right (Left v)] is [Left (left v)],
##V>=4.12##    [map ~left ~right (Right v)] is [Right (right v)]. *)

##V>=4.12##val fold : left:('a -> 'c) -> right:('b -> 'c) -> ('a, 'b) t -> 'c
##V>=4.12##(** [fold ~left ~right (Left v)] is [left v], and
##V>=4.12##    [fold ~left ~right (Right v)] is [right v]. *)

##V>=4.12##val iter : left:('a -> unit) -> right:('b -> unit) -> ('a, 'b) t -> unit
##V>=4.12##(** [iter ~left ~right (Left v)] is [left v], and
##V>=4.12##    [iter ~left ~right (Right v)] is [right v]. *)

##V>=4.12##val for_all : left:('a -> bool) -> right:('b -> bool) -> ('a, 'b) t -> bool
##V>=4.12##(** [for_all ~left ~right (Left v)] is [left v], and
##V>=4.12##    [for_all ~left ~right (Right v)] is [right v]. *)

##V>=4.12##val equal :
##V>=4.12##  left:('a -> 'a -> bool) -> right:('b -> 'b -> bool) ->
##V>=4.12##  ('a, 'b) t -> ('a, 'b) t -> bool
##V>=4.12##(** [equal ~left ~right e0 e1] tests equality of [e0] and [e1] using [left]
##V>=4.12##    and [right] to respectively compare values wrapped by [Left _] and
##V>=4.12##    [Right _]. *)

##V>=4.12##val compare :
##V>=4.12##  left:('a -> 'a -> int) -> right:('b -> 'b -> int) ->
##V>=4.12##  ('a, 'b) t -> ('a, 'b) t -> int
##V>=4.12##(** [compare ~left ~right e0 e1] totally orders [e0] and [e1] using [left] and
##V>=4.12##    [right] to respectively compare values wrapped by [Left _ ] and [Right _].
##V>=4.12##    [Left _] values are smaller than [Right _] values. *)
