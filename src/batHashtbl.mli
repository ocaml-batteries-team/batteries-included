(*
 * BatHashtbl - extra functions over hashtables.
 * Copyright (C) 2003 Nicolas Cannasse
 *               2009 David Teller, LIFO, Universite d'Orleans
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version,
 * with the special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

(** Extra functions over hashtables. *)


(** Operations over hashtables.

    This module replaces Stdlib's
    {{:http://caml.inria.fr/pub/docs/manual-ocaml/libref/Hashtbl.html}Hashtbl}
    module.  All functions and types are provided here.

    @author Xavier Leroy (base module)
    @author Damien Doligez (base module)
    @author Nicolas Cannasse
    @author David Teller
*)

type ('a, 'b) t = ('a, 'b) Hashtbl.t
(** A Hashtable with keys of type 'a and values 'b *)

type statistics = Hashtbl.statistics

(**{1 Base operations}*)

val create : int -> ('a, 'b) t
(** [Hashtbl.create n] creates a new, empty hash table, with
    initial size [n].  For best results, [n] should be on the
    order of the expected number of elements that will be in
    the table.  The table grows as needed, so [n] is just an
    initial guess. *)

val length : ('a, 'b) t -> int
(** [Hashtbl.length tbl] returns the number of bindings in [tbl].
    Multiple bindings are counted multiply, so [Hashtbl.length]
    gives the number of times [Hashtbl.iter] calls its first argument. *)

val is_empty : ('a, 'b) t -> bool
(** [Hashtbl.is_empty tbl] returns [true] if there are no bindings
    in [tbl], false otherwise.*)

val add : ('a, 'b) t -> 'a -> 'b -> unit
(** [Hashtbl.add tbl x y] adds a binding of [x] to [y] in table [tbl].
    Previous bindings for [x] are not removed, but simply
    hidden. That is, after performing {!Hashtbl.remove}[ tbl x],
    the previous binding for [x], if any, is restored.
    (Same behavior as with association lists.) *)

val remove : ('a, 'b) t -> 'a -> unit
(** [Hashtbl.remove tbl x] removes the current binding of [x] in [tbl],
    restoring the previous binding if it exists.
    It does nothing if [x] is not bound in [tbl]. *)

val remove_all : ('a,'b) t -> 'a -> unit
(** Remove all bindings for the given key *)

val replace : ('a, 'b) t -> 'a -> 'b -> unit
(** [Hashtbl.replace tbl x y] replaces the current binding of [x]
    in [tbl] by a binding of [x] to [y].  If [x] is unbound in [tbl],
    a binding of [x] to [y] is added to [tbl].
    This is functionally equivalent to {!Hashtbl.remove}[ tbl x]
    followed by {!Hashtbl.add}[ tbl x y]. *)

val modify : 'a -> ('b -> 'b) -> ('a, 'b) t -> unit
(** [Hashtbl.modify k f tbl] replaces the first binding for [k] in [tbl]
    with [f] applied to that value.
    @raise Not_found if [k] is unbound in [tbl].
    @since 2.1 *)

val modify_def : 'b -> 'a -> ('b -> 'b) -> ('a, 'b) t -> unit
(** [Hashtbl.modify_def v k f tbl] does the same as [Hashtbl.modify k f tbl]
    but [f v] is inserted in [tbl] if [k] was unbound.
    @since 2.1 *)

val modify_opt : 'a -> ('b option -> 'b option) -> ('a, 'b) t -> unit
(** [Hashtbl.modify_opt k f tbl] allows to remove, modify or add a binding for
    [k] in [tbl]. [f] will be called with [None] if [k] was unbound.
    first previous binding of [k] in [tbl] will be deleted if [f] returns [None].
    Otherwise, the previous binding is replaced by the value produced by [f].
    @since 2.1 *)

val copy : ('a, 'b) t -> ('a, 'b) t
(** Return a copy of the given hashtable. *)

val clear : ('a, 'b) t -> unit
(** Empty a hash table. *)

val reset : ('a, 'b) t -> unit
(** Empty a hash table and shrink the size of the bucket table
    to its initial size.
    @since 3.9.0 and OCaml 4.00 *)

val stats : ('a, 'b) t -> statistics
(** [Hashtbl.stats tbl] returns statistics about the table [tbl]:
    number of buckets, size of the biggest bucket, distribution of
    buckets by size.
    @since 4.00.0 and batteries 3.5.1 *)

(** {1 Sequences} *)

##V>=4.07##val to_seq : ('a,'b) t -> ('a * 'b) Seq.t
##V>=4.07##(** Iterate on the whole table.  The order in which the bindings
##V>=4.07##    appear in the sequence is unspecified. However, if the table contains
##V>=4.07##    several bindings for the same key, they appear in reversed order of
##V>=4.07##    introduction, that is, the most recent binding appears first.
##V>=4.07##
##V>=4.07##    The behavior is not specified if the hash table is modified
##V>=4.07##    during the iteration.
##V>=4.07##
##V>=4.07##    @since 3.9.0 and OCaml 4.07 *)
##V>=4.07##
##V>=4.07##val to_seq_keys : ('a,_) t -> 'a Seq.t
##V>=4.07##(** Same as [Seq.map fst (to_seq m)]
##V>=4.07##    @since 3.9.0 and OCaml 4.07 *)
##V>=4.07##
##V>=4.07##val to_seq_values : (_,'b) t -> 'b Seq.t
##V>=4.07##(** Same as [Seq.map snd (to_seq m)]
##V>=4.07##    @since 3.9.0 and OCaml 4.07 *)
##V>=4.07##
##V>=4.07##val add_seq : ('a,'b) t -> ('a * 'b) Seq.t -> unit
##V>=4.07##(** Add the given bindings to the table, using {!add}
##V>=4.07##    @since 3.9.0 and OCaml 4.07 *)
##V>=4.07##
##V>=4.07##val replace_seq : ('a,'b) t -> ('a * 'b) Seq.t -> unit
##V>=4.07##(** Add the given bindings to the table, using {!replace}
##V>=4.07##    @since 3.9.0 and OCaml 4.07 *)
##V>=4.07##
##V>=4.07##val of_seq : ('a * 'b) Seq.t -> ('a, 'b) t
##V>=4.07##(** Build a table from the given bindings. The bindings are added
##V>=4.07##    in the same order they appear in the sequence, using {!replace_seq},
##V>=4.07##    which means that if two pairs have the same key, only the latest one
##V>=4.07##    will appear in the table.
##V>=4.07##    @since 3.9.0 and OCaml 4.07 *)

(**{1 Enumerations}*)

val keys : ('a,'b) t -> 'a BatEnum.t
(** Return an enumeration of all the keys of a hashtable.
    If the key is in the Hashtable multiple times, all occurrences
    will be returned.  *)

val values : ('a,'b) t -> 'b BatEnum.t
(** Return an enumeration of all the values of a hashtable. *)

val enum : ('a, 'b) t -> ('a * 'b) BatEnum.t
(** Return an enumeration of (key,value) pairs of a hashtable. *)

val of_enum : ('a * 'b) BatEnum.t -> ('a, 'b) t
(** Create a hashtable from a (key,value) enumeration. *)

(**{1 Lists}*)

val of_list : ('a * 'b) list -> ('a, 'b) t
(** Create a hashtable from a list of (key,value) pairs.
    @since 2.6.0 *)

val to_list : ('a, 'b) t -> ('a * 'b) list
(** Return the list of (key,value) pairs.
    @since 2.6.0 *)

val bindings : ('a, 'b) t -> ('a * 'b) list
(** Alias for [to_list].
    @since 2.6.0 *)

(**{1 Searching}*)

val find : ('a, 'b) t -> 'a -> 'b
(** [Hashtbl.find tbl x] returns the current binding of [x] in [tbl],
    or raises [Not_found] if no such binding exists. *)

val find_all : ('a, 'b) t -> 'a -> 'b list
(** [Hashtbl.find_all tbl x] returns the list of all data
    associated with [x] in [tbl].
    The current binding is returned first, then the previous
    bindings, in reverse order of introduction in the table. *)

val find_default : ('a,'b) t -> 'a -> 'b -> 'b
(** [Hashtbl.find_default tbl key default] finds a binding for [key],
    or return [default] if [key] is unbound in [tbl]. *)

val find_option : ('a,'b) t -> 'a -> 'b option
(** Find a binding for the key, or return [None] if no
    value is found *)

val find_opt : ('a, 'b) t -> 'a -> 'b option
(** [Stdlib]-compatible alias for {!find_option}.
    @since 3.9.0 *)

val exists : ('a -> 'b -> bool) -> ('a,'b) t -> bool
(** Check if at least one key-value pair satisfies [p k v] *)

val mem : ('a, 'b) t -> 'a -> bool
(** [Hashtbl.mem tbl x] checks if [x] is bound in [tbl]. *)

(*val exists : ('a,'b) t -> 'a -> bool
  (** [exists h k] returns true is at least one item with key [k] is
      found in the hashtable. *)*)

(**{1 Traversing}

   A number of higher-order functions are provided to allow
   purely functional traversal or transformation of hashtables.
   These functions are similar to their counterparts in module
   {!BatEnum}.

   Whenever you wish to traverse or transfor a hashtable, you have the
   choice between using the more general functions of {!BatEnum}, with
   {!keys}, {!values}, {!enum} and {!of_enum}, or the more optimized
   functions of this section.

   If you are new to OCaml or unsure about data structure, using the
   functions of {!BatEnum} is a safe bet. Should you wish to improve
   performance at the cost of generality, you will always be able to
   rewrite your code to make use of the functions of this section.
*)

val iter : ('a -> 'b -> unit) -> ('a, 'b) t -> unit
(** [Hashtbl.iter f tbl] applies [f] to all bindings in table [tbl].
    [f] receives the key as first argument, and the associated value
    as second argument. Each binding is presented exactly once to [f].
    The order in which the bindings are passed to [f] is unspecified.
    However, if the table contains several bindings for the same key,
    they are passed to [f] in reverse order of introduction, that is,
    the most recent binding is passed first. *)

val for_all : ('a -> 'b -> bool) -> ('a, 'b) t -> bool
(** [Hashtbl.for_all p tbl] check if the predicate [p k v]
    holds for all bindings currently in [tbl]. *)

val fold : ('a -> 'b -> 'c -> 'c) -> ('a, 'b) t -> 'c -> 'c
(** [Hashtbl.fold f tbl init] computes
    [(f kN dN ... (f k1 d1 (f k0 d0 init))...)],
    where [k0,k1..kN] are the keys of all bindings in [tbl],
    and [d0,d1..dN] are the associated values.
    Each binding is presented exactly once to [f].
    The order in which the bindings are passed to [f] is unspecified.
    However, if the table contains several bindings for the same key,
    they are passed to [f] in reverse order of introduction, that is,
    the most recent binding is passed first. *)

val map : ('a -> 'b -> 'c) -> ('a,'b) t -> ('a,'c) t
(** [map f x] creates a new hashtable with the same
    keys as [x], but with the function [f] applied to
    all the values *)

val map_inplace : ('a -> 'b -> 'b) -> ('a,'b) t -> unit
(** [map_inplace f x] replace all values currently bound in [x]
    by [f] applied to each value.

    @since 2.1 *)

val filter: ('a -> bool) -> ('key, 'a) t -> ('key, 'a) t
(** [filter f m] returns a new hashtable where only the values [a] of [m]
    such that [f a = true] remain.*)

val filter_inplace : ('a -> bool) -> ('key,'a) t -> unit
(** [filter_inplace f m] removes from [m] all bindings that does not
    satisfy the predicate f.

    @since 2.1 *)

val filteri: ('key -> 'a -> bool) -> ('key, 'a) t -> ('key, 'a) t
(** [filteri f m] returns a hashtbl where only the key, values pairs
    [key], [a] of [m] such that [f key a = true] remain. *)

val filteri_inplace : ('key -> 'a -> bool) -> ('key, 'a) t -> unit
(** [filteri_inplace f m] performs as filter_inplace but [f]
    receive the value in additiuon to the key.

    @since 2.1 *)

val filter_map: ('key -> 'a -> 'b option) -> ('key, 'a) t -> ('key, 'b) t
(** [filter_map f m] combines the features of [filteri] and [map].  It
    calls [f key0 a0], [f key1 a1], [f keyn an] where [a0,a1..an] are
    the elements of [m] and [key0..keyn] the corresponding keys. It
    returns a hashtbl with associations [keyi],[bi] where [f keyi ai =
    Some bi].  When [f] returns [None], the corresponding element of
    [m] is discarded. *)

val filter_map_inplace: ('key -> 'a -> 'a option) -> ('key, 'a) t -> unit
(** [filter_map_inplace f m] performs like filter_map but modify [m]
    inplace instead of creating a new Hashtbl. *)

val merge: ('a -> 'b option -> 'c option -> 'd option) ->
           ('a, 'b) t -> ('a, 'c) t -> ('a, 'd) t
(** [merge f a b] returns a new Hashtbl which is build from the bindings of
    [a] and [b] according to the function [f], that is given all defined keys
    one by one, along with the value from [a] (if defined) and the value from
    [b] (if defined), and has to return the (optional) resulting value.

    It is assumed that each key is bound at most once in [a] and [b].
    See [merge_all] for a more general alternative if this is not the case.
    @since 2.10.0
*)

val merge_all: ('a -> 'b list -> 'c list -> 'd list) ->
               ('a, 'b) t -> ('a, 'c) t -> ('a, 'd) t
(** [merge_all f a b] is similar to [merge], but passes to [f] all bindings
    for a key (most recent first, as returned by [find_all]). [f] must then
    return all the new bindings of the merged hashtable (or an empty list if
    that key should not be bound in the resulting hashtable). Those new
    bindings will be inserted in reverse, so that the head of the list will
    become the most recent binding in the merged hashtable.
    @since 2.10.0
*)

(** {1 The polymorphic hash primitive}*)

val hash : 'a -> int
(** [Hashtbl.hash x] associates a positive integer to any value of
    any type. It is guaranteed that
    if [x = y] or [Pervasives.compare x y = 0], then [hash x = hash y].
    Moreover, [hash] always terminates, even on cyclic
    structures. *)


(** {1 Boilerplate code}*)

(** {2 Printing}*)

val print :  ?first:string -> ?last:string -> ?sep:string -> ?kvsep:string ->
  ('a BatInnerIO.output -> 'b -> unit) ->
  ('a BatInnerIO.output -> 'c -> unit) ->
  'a BatInnerIO.output -> ('b, 'c) t -> unit

(** {1 Override modules}*)

(**
   The following modules replace functions defined in {!Hashtbl} with functions
   behaving slightly differently but having the same name. This is by design:
   the functions meant to override the corresponding functions of {!Hashtbl}.
*)

(** Operations on {!Hashtbl} without exceptions.

    @documents Hashtbl.Exceptionless
*)
module Exceptionless :
sig
  val find : ('a, 'b) t -> 'a -> 'b option
  val modify : 'a -> ('b -> 'b) -> ('a, 'b) t -> (unit, exn) BatPervasives.result
end

(** Infix operators over a {!BatHashtbl} *)
module Infix :
sig
  val (-->) : ('a, 'b) t -> 'a -> 'b
  (** [tbl-->x] returns the current binding of [x] in [tbl],
      or raises [Not_found] if no such binding exists.
      Equivalent to [Hashtbl.find tbl x]*)

  val (<--) : ('a, 'b) t -> 'a * 'b -> unit
    (** [tbl<--(x, y)] adds a binding of [x] to [y] in table [tbl].
        Previous bindings for [x] are not removed, but simply
        hidden. That is, after performing {!Hashtbl.remove}[ tbl x],
        the previous binding for [x], if any, is restored.
        (Same behavior as with association lists.)
        Equivalent to [Hashtbl.add tbl x y]*)
end

(** Operations on {!Hashtbl} with labels.

    This module overrides a number of functions of {!Hashtbl} by
    functions in which some arguments require labels. These labels are
    there to improve readability and safety and to let you change the
    order of arguments to functions. In every case, the behavior of the
    function is identical to that of the corresponding function of {!Hashtbl}.

    @documents Hashtbl.Labels
*)
module Labels :
sig
  val add : ('a, 'b) t -> key:'a -> data:'b -> unit
  val replace : ('a, 'b) t -> key:'a -> data:'b -> unit
  val iter : f:(key:'a -> data:'b -> unit) -> ('a, 'b) t -> unit
  val for_all : f:(key:'a -> data:'b -> bool) -> ('a, 'b) t -> bool
  val map : f:(key:'a -> data:'b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
  val map_inplace : f:(key:'a -> data:'b -> 'b) -> ('a,'b) t -> unit
  val filter : f:('a -> bool) -> ('key, 'a) t -> ('key, 'a) t
  val filter_inplace : f:('a -> bool) -> ('key, 'a) t -> unit
  val filteri : f:(key:'key -> data:'a -> bool) -> ('key, 'a) t -> ('key, 'a) t
  val filteri_inplace : f:(key:'key -> data:'a -> bool) -> ('key, 'a) t -> unit
  val filter_map : f:(key:'key -> data:'a -> 'b option) -> ('key, 'a) t -> ('key, 'b) t
  val filter_map_inplace : f:(key:'key -> data:'a -> 'a option) -> ('key, 'a) t -> unit
  val fold : f:(key:'a -> data:'b -> 'c -> 'c) -> ('a, 'b) t -> init:'c -> 'c
  val exists : f:(key:'a -> data:'b -> bool) -> ('a, 'b) t -> bool
  val modify : key:'a -> f:('b -> 'b) -> ('a, 'b) t -> unit
  val modify_def : default:'b -> key:'a -> f:('b -> 'b) -> ('a, 'b) t -> unit
  val modify_opt : key:'a -> f:('b option -> 'b option) -> ('a, 'b) t -> unit
  val merge: f:('a -> 'b option -> 'c option -> 'd option) ->
             left:('a, 'b) t -> right:('a, 'c) t -> ('a, 'd) t
  val merge_all: f:('a -> 'b list -> 'c list -> 'd list) ->
                 left:('a, 'b) t -> right:('a, 'c) t -> ('a, 'd) t
end

(** {1 Functorial interface} *)

module type HashedType =
sig
  type t
  (** The type of the hashtable keys. *)

  val equal : t -> t -> bool
  (** The equality predicate used to compare keys. *)

  val hash : t -> int
    (** A hashing function on keys. It must be such that if two keys are
        equal according to [equal], then they have identical hash values
        as computed by [hash].
        Examples: suitable ([equal], [hash]) pairs for arbitrary key
        types include
        ([(=)], {!Hashtbl.hash}) for comparing objects by structure,
        ([(fun x y -> compare x y = 0)], {!Hashtbl.hash})
        for comparing objects by structure and handling {!Pervasives.nan}
        correctly, and
        ([(==)], {!Hashtbl.hash}) for comparing objects by addresses
        (e.g. for cyclic keys). *)
end

(** The output signature of the functor {!Hashtbl.Make}. *)
module type S =
sig
  type key
  type 'a t
  val create : int -> 'a t
  val length : 'a t -> int
  val is_empty : 'a t -> bool
  val clear : 'a t -> unit
  val reset : 'a t -> unit
  val copy : 'a t -> 'a t
  val add : 'a t -> key -> 'a -> unit
  val remove : 'a t -> key -> unit
  val remove_all : 'a t -> key -> unit
  val find : 'a t -> key -> 'a
  val find_all : 'a t -> key -> 'a list
  val find_default : 'a t -> key ->  'a -> 'a
  val find_option : 'a t -> key -> 'a option
  val find_opt : 'a t -> key -> 'a option

  val replace : 'a t -> key -> 'a -> unit
  val mem : 'a t -> key -> bool
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val for_all : (key -> 'a -> bool) -> 'a t -> bool
  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val exists : (key -> 'a -> bool) -> 'a t -> bool
  val map : (key -> 'b -> 'c) -> 'b t -> 'c t
  val map_inplace : (key -> 'a -> 'a) -> 'a t -> unit
  val filter : ('a -> bool) -> 'a t -> 'a t
  val filter_inplace : ('a -> bool) -> 'a t -> unit
  val filteri : (key -> 'a -> bool) -> 'a t -> 'a t
  val filteri_inplace : (key -> 'a -> bool) -> 'a t -> unit
  val filter_map : (key -> 'a -> 'b option) -> 'a t -> 'b t
  val filter_map_inplace : (key -> 'a -> 'a option) -> 'a t -> unit
  val modify : key -> ('a -> 'a) -> 'a t -> unit
  val modify_def : 'a -> key -> ('a -> 'a) -> 'a t -> unit
  val modify_opt : key -> ('a option -> 'a option) -> 'a t -> unit
  val merge : (key -> 'a option -> 'b option -> 'c option) ->
             'a t -> 'b t -> 'c t
  val merge_all : (key -> 'a list -> 'b list -> 'c list) ->
    'a t -> 'b t -> 'c t
  val stats : 'a t -> statistics

##V>=4.07##  val to_seq : 'a t -> (key * 'a) Seq.t
##V>=4.07##  val to_seq_keys : _ t -> key Seq.t
##V>=4.07##  val to_seq_values : 'a t -> 'a Seq.t
##V>=4.07##  val add_seq : 'a t -> (key * 'a) Seq.t -> unit
##V>=4.07##  val replace_seq : 'a t -> (key * 'a) Seq.t -> unit
##V>=4.07##  val of_seq : (key * 'a) Seq.t -> 'a t

  val keys : 'a t -> key BatEnum.t
  val values : 'a t -> 'a BatEnum.t
  val enum : 'a t -> (key * 'a) BatEnum.t
  val to_list : 'a t -> (key * 'a) list
  val of_enum : (key * 'a) BatEnum.t -> 'a t
  val of_list : (key * 'a) list -> 'a t
  val print :  ?first:string -> ?last:string -> ?sep:string ->
    ('a BatInnerIO.output -> key -> unit) ->
    ('a BatInnerIO.output -> 'b -> unit) ->
    'a BatInnerIO.output -> 'b t -> unit

  (** {1 Override modules}*)

  (**
     The following modules replace functions defined in {!Hashtbl} with functions
     behaving slightly differently but having the same name. This is by design:
     the functions meant to override the corresponding functions of {!Hashtbl}.
  *)

  (** Operations on {!Hashtbl} without exceptions.

    @documents Hashtbl.S.Exceptionless*)
  module Exceptionless :
  sig
    val find : 'a t -> key -> 'a option
    val modify : key -> ('a -> 'a) -> 'a t -> (unit, exn) BatPervasives.result
  end

  (** Infix operators over a {!BatHashtbl} *)
  module Infix :
  sig
    val (-->) : 'a t -> key -> 'a
    (** [tbl-->x] returns the current binding of [x] in [tbl],
        or raises [Not_found] if no such binding exists.
        Equivalent to [Hashtbl.find tbl x]*)

    val (<--) : 'a t -> key * 'a -> unit
      (** [tbl<--(x, y)] adds a binding of [x] to [y] in table [tbl].
          Previous bindings for [x] are not removed, but simply
          hidden. That is, after performing {!Hashtbl.remove}[ tbl x],
          the previous binding for [x], if any, is restored.
          (Same behavior as with association lists.)
          Equivalent to [Hashtbl.add tbl x y]*)
  end

  (** Operations on {!Hashtbl} with labels.

    This module overrides a number of functions of {!Hashtbl} by
    functions in which some arguments require labels. These labels are
    there to improve readability and safety and to let you change the
    order of arguments to functions. In every case, the behavior of the
    function is identical to that of the corresponding function of {!Hashtbl}.

    @documents Hashtbl.S.Labels
  *)
  module Labels :
  sig
    val add : 'a t -> key:key -> data:'a -> unit
    val replace : 'a t -> key:key -> data:'a -> unit
    val iter : f:(key:key -> data:'a -> unit) -> 'a t -> unit
    val for_all : f:(key:key -> data:'a -> bool) -> 'a t -> bool
    val map : f:(key:key -> data:'a -> 'b) -> 'a t -> 'b t
    val map_inplace : f:(key:key -> data:'a -> 'a) -> 'a t -> unit
    val filter : f:('a -> bool) -> 'a t -> 'a t
    val filter_inplace : f:('a -> bool) -> 'a t -> unit
    val filteri : f:(key:key -> data:'a -> bool) -> 'a t -> 'a t
    val filteri_inplace : f:(key:key -> data:'a -> bool) -> 'a t -> unit
    val filter_map : f:(key:key -> data:'a -> 'b option) -> 'a t -> 'b t
    val filter_map_inplace : f:(key:key -> data:'a -> 'a option) -> 'a t -> unit
    val fold : f:(key:key -> data:'a -> 'b -> 'b) -> 'a t -> init:'b -> 'b
    val exists : f:(key:key -> data:'a -> bool) -> 'a t -> bool
    val modify : key:key -> f:('a -> 'a) -> 'a t -> unit
    val modify_def : default:'a -> key:key -> f:('a -> 'a) -> 'a t -> unit
    val modify_opt : key:key -> f:('a option -> 'a option) -> 'a t -> unit
    val merge : f:(key -> 'a option -> 'b option -> 'c option) ->
               left:'a t -> right:'b t -> 'c t
    val merge_all : f:(key -> 'a list -> 'b list -> 'c list) ->
                    left:'a t -> right:'b t -> 'c t
  end

end
(** The output signature of the functor {!Hashtbl.Make}. *)

module Make (H : HashedType) : S with type key = H.t and type 'a t = 'a Hashtbl.Make (H).t
(** Functor building an implementation of the hashtable structure.
    The functor [Hashtbl.Make] returns a structure containing
    a type [key] of keys and a type ['a t] of hash tables
    associating data of type ['a] to keys of type [key].
    The operations perform similarly to those of the generic
    interface, but use the hashing and equality functions
    specified in the functor argument [H] instead of generic
    equality and hashing. *)

(** Capabilities for hashtables.

    @documents Hashtbl.Cap
*)
module Cap :
sig

  type ('a, 'b, 'c) t constraint 'c = [< `Read | `Write ]
  (** The type of a hashtable. *)

  (**{1 Constructors}*)

  val create : int -> ('a, 'b, _) t

  external of_table  : ('a, 'b) Hashtbl.t -> ('a, 'b, _ ) t = "%identity"
  (** Adopt a regular hashtable as a capability hashtble, allowing
      to decrease capabilities if necessary.

      This operation involves no copying. In other words, in
      [let cap = of_table a in ...], any modification in [a]
      will also have effect on [cap] and reciprocally.*)

  external to_table  : ('a, 'b, [`Read | `Write]) t -> ('a, 'b) Hashtbl.t = "%identity"
  (** Return a capability hashtable as a regular hashtable.

      This operation requires both read and write permissions
      on the capability table and involves no copying. In other
      words, in [let a = of_table cap in ...], any modification
      in [a] will also have effect on [cap] and reciprocally.*)

  external read_only :  ('a, 'b, [>`Read])  t -> ('a, 'b, [`Read])  t = "%identity"
  (** Drop to read-only permissions.

      This operation involves no copying.*)

  external write_only : ('a, 'b, [>`Write]) t -> ('a, 'b, [`Write]) t = "%identity"
  (** Drop to write-only permissions.

      This operation involves no copying.*)

  (**{1 Base operations}*)

  val length : ('a, 'b, _) t -> int
  val is_empty : ('a, 'b, _) t -> bool
  val add : ('a, 'b, [>`Write]) t -> 'a -> 'b -> unit
  val remove : ('a, 'b, [>`Write]) t -> 'a -> unit
  val remove_all : ('a,'b, [>`Write]) t -> 'a -> unit
  val replace : ('a, 'b, [>`Write]) t -> 'a -> 'b -> unit
  val copy : ('a, 'b, [>`Read]) t -> ('a, 'b, _) t
  val clear : ('a, 'b, [>`Write]) t -> unit
  val stats : ('a, 'b, _) t -> statistics

  (**{1 Searching}*)

  val find : ('a, 'b, [>`Read]) t -> 'a -> 'b
  val find_all : ('a, 'b, [>`Read]) t -> 'a -> 'b list
  val find_default : ('a, 'b, [>`Read]) t -> 'a -> 'b -> 'b
  val find_option : ('a, 'b, [>`Read]) t -> 'a -> 'b option
  val exists : ('a -> 'b -> bool) -> ('a, 'b, [>`Read]) t -> bool

  val mem : ('a, 'b, [>`Read]) t -> 'a -> bool

  (*val exists : ('a,'b) t -> 'a -> bool
    (** [exists h k] returns true is at least one item with key [k] is
        found in the hashtable. *)*)

  (**{1 Traversing}*)

  val iter : ('a -> 'b -> unit) -> ('a, 'b, [>`Read]) t -> unit
  val for_all : ('a -> 'b -> bool) -> ('a, 'b, [>`Read]) t -> bool
  val fold : ('a -> 'b -> 'c -> 'c) -> ('a, 'b, [>`Read]) t -> 'c -> 'c
  val map : ('a -> 'b -> 'c) -> ('a, 'b, [>`Read]) t -> ('a, 'c, _) t
  val map_inplace : ('a -> 'b -> 'b) -> ('a, 'b, [>`Write]) t -> unit
  val filter : ('a -> bool) -> ('key, 'a, [>`Read]) t -> ('key, 'a, _) t
  val filter_inplace : ('a -> bool) -> ('key, 'a, [>`Write]) t -> unit
  val filteri : ('key -> 'a -> bool) -> ('key, 'a, [>`Read]) t -> ('key, 'a, _) t
  val filteri_inplace : ('key -> 'a -> bool) -> ('key, 'a, [>`Write]) t -> unit
  val filter_map : ('key -> 'a -> 'b option) -> ('key, 'a, [>`Read]) t -> ('key, 'b, _) t
  val filter_map_inplace : ('key -> 'a -> 'a option) -> ('key, 'a, [>`Write]) t -> unit
  val merge : ('key -> 'a option -> 'b option -> 'c option) ->
              ('key, 'a, [>`Read]) t -> ('key, 'b, [>`Read]) t -> ('key, 'c, _) t
  val merge_all : ('key -> 'a list -> 'b list -> 'c list) ->
                  ('key, 'a, [>`Read]) t -> ('key, 'b, [>`Read]) t -> ('key, 'c, _) t

  (**{1 Conversions}*)

  val keys : ('a,'b, [>`Read]) t -> 'a BatEnum.t
  val values : ('a, 'b, [>`Read]) t -> 'b BatEnum.t
  val enum : ('a, 'b, [>`Read]) t -> ('a * 'b) BatEnum.t
  val of_enum : ('a * 'b) BatEnum.t -> ('a, 'b, _) t
  val to_list : ('a, 'b, [>`Read]) t -> ('a * 'b) list
  val of_list : ('a * 'b) list -> ('a, 'b, _) t

  (** {1 Boilerplate code}*)

  (** {2 Printing}*)

  val print :  ?first:string -> ?last:string -> ?sep:string -> ?kvsep:string ->
    ('a BatInnerIO.output -> 'b -> unit) ->
    ('a BatInnerIO.output -> 'c -> unit) ->
    'a BatInnerIO.output -> ('b, 'c, [>`Read]) t -> unit

  (** {1 Override modules}*)

  (** Operations on {!BatHashtbl.Cap} without exceptions.*)
  module Exceptionless :
  sig
    val find : ('a, 'b, [>`Read]) t -> 'a -> 'b option
    val modify : 'a -> ('b -> 'b) -> ('a, 'b, [>`Read]) t -> (unit, exn) BatPervasives.result
  end

  (** Operations on {!BatHashtbl.Cap} with labels.*)
  module Labels :
  sig
    val add : ('a, 'b, [>`Write]) t -> key:'a -> data:'b -> unit
    val replace : ('a, 'b, [>`Write]) t -> key:'a -> data:'b -> unit
    val iter : f:(key:'a -> data:'b -> unit) -> ('a, 'b, [>`Read]) t -> unit
    val for_all : f:(key:'a -> data:'b -> bool) -> ('a, 'b, [>`Read]) t -> bool
    val map : f:(key:'a -> data:'b -> 'c) -> ('a, 'b, [>`Read]) t -> ('a, 'c, _) t
    val map_inplace : f:(key:'a -> data:'b -> 'b) -> ('a, 'b, [>`Write]) t -> unit
    val filter : f:('a -> bool) -> ('key, 'a, [>`Read]) t -> ('key, 'a, _) t
    val filter_inplace : f:('a -> bool) -> ('key, 'a, [>`Write]) t -> unit
    val filteri : f:(key:'key -> data:'a -> bool) -> ('key, 'a, [>`Read]) t -> ('key, 'a, _) t
    val filteri_inplace : f:(key:'key -> data:'a -> bool) -> ('key, 'a, [>`Write]) t -> unit
    val filter_map : f:(key:'key -> data:'a -> 'b option) -> ('key, 'a, [>`Read]) t -> ('key, 'b, _) t
    val filter_map_inplace : f:(key:'key -> data:'a -> 'a option) -> ('key, 'a, [>`Write]) t -> unit
    val fold : f:(key:'a -> data:'b -> 'c -> 'c) -> ('a, 'b, [>`Read]) t -> init:'c -> 'c
    val exists : f:(key:'a -> data:'b -> bool) -> ('a, 'b, [>`Read]) t -> bool
    val merge : f:('key -> 'a option -> 'b option -> 'c option) ->
                left:('key, 'a, [>`Read]) t -> right:('key, 'b, [>`Read]) t -> ('key, 'c, _) t
    val merge_all : f:('key -> 'a list -> 'b list -> 'c list) ->
                    left:('key, 'a, [>`Read]) t -> right:('key, 'b, [>`Read]) t -> ('key, 'c, _) t

  end

end (* Cap module *)
