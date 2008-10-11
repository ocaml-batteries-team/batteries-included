(* 
 * ExtHashtbl - extra functions over hashtables.
 * Copyright (C) 2003 Nicolas Cannasse
 *               2008 David Teller, LIFO, Universite d'Orleans
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
    
    @author Xavier Leroy (base module)
    @author Damien Doligez (base module)
    @author Nicolas Cannasse
    @author David Teller
    
    @documents Hashtbl
*)
module Hashtbl :
  sig



type ('a,'b) t =  ('a,'b)  Hashtbl.t
    (** The type of a hashtable. *)

(**{6 Base operations}*)

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

val copy : ('a, 'b) t -> ('a, 'b) t
(** Return a copy of the given hashtable. *)

val clear : ('a, 'b) t -> unit
(** Empty a hash table. *)


(**{6 Searching}*)

val find : ('a, 'b) t -> 'a -> 'b
(** [Hashtbl.find tbl x] returns the current binding of [x] in [tbl],
   or raises [Not_found] if no such binding exists. *)

val find_all : ('a, 'b) t -> 'a -> 'b list
(** [Hashtbl.find_all tbl x] returns the list of all data
   associated with [x] in [tbl].
   The current binding is returned first, then the previous
   bindings, in reverse order of introduction in the table. *)

val find_default : ('a,'b) t -> 'a -> 'b -> 'b
  (** Find a binding for the key, and return a default
      value if not found *)

val find_option : ('a,'b) Hashtbl.t -> 'a -> 'b option
  (** Find a binding for the key, or return [None] if no
      value is found *)

val mem : ('a, 'b) t -> 'a -> bool
(** [Hashtbl.mem tbl x] checks if [x] is bound in [tbl]. *)

(*val exists : ('a,'b) t -> 'a -> bool*)
  (** [exists h k] returns true is at least one item with key [k] is
      found in the hashtable. *)

(**{6 Traversing}*)
val iter : ('a -> 'b -> unit) -> ('a, 'b) t -> unit
(** [Hashtbl.iter f tbl] applies [f] to all bindings in table [tbl].
   [f] receives the key as first argument, and the associated value
   as second argument. Each binding is presented exactly once to [f].
   The order in which the bindings are passed to [f] is unspecified.
   However, if the table contains several bindings for the same key,
   they are passed to [f] in reverse order of introduction, that is,
   the most recent binding is passed first. *)

val fold : ('a -> 'b -> 'c -> 'c) -> ('a, 'b) t -> 'c -> 'c
(** [Hashtbl.fold f tbl init] computes
   [(f kN dN ... (f k1 d1 init)...)],
   where [k1 ... kN] are the keys of all bindings in [tbl],
   and [d1 ... dN] are the associated values.
   Each binding is presented exactly once to [f].
   The order in which the bindings are passed to [f] is unspecified.
   However, if the table contains several bindings for the same key,
   they are passed to [f] in reverse order of introduction, that is,
   the most recent binding is passed first. *)

val map : ('b -> 'c) -> ('a,'b) t -> ('a,'c) t
  (** [map f x] creates a new hashtable with the same
      keys as [x], but with the function [f] applied to
      all the values *)

(**{6 Conversions}*)

val keys : ('a,'b) t -> 'a Enum.t
  (** Return an enumeration of all the keys of a hashtable.
      If the key is in the Hashtable multiple times, all occurrences
      will be returned.  *)

val values : ('a,'b) t -> 'b Enum.t
  (** Return an enumeration of all the values of a hashtable. *)

val enum : ('a, 'b) t -> ('a * 'b) Enum.t
  (** Return an enumeration of (key,value) pairs of a hashtable. *)

val of_enum : ('a * 'b) Enum.t -> ('a, 'b) t
  (** Create a hashtable from a (key,value) enumeration. *)

(** {6 The polymorphic hash primitive}*)

val hash : 'a -> int
(** [Hashtbl.hash x] associates a positive integer to any value of
   any type. It is guaranteed that
   if [x = y] or [Pervasives.compare x y = 0], then [hash x = hash y].
   Moreover, [hash] always terminates, even on cyclic
   structures. *)

external hash_param : int -> int -> 'a -> int = "caml_hash_univ_param" "noalloc"
(** [Hashtbl.hash_param n m x] computes a hash value for [x], with the
   same properties as for [hash]. The two extra parameters [n] and
   [m] give more precise control over hashing. Hashing performs a
   depth-first, right-to-left traversal of the structure [x], stopping
   after [n] meaningful nodes were encountered, or [m] nodes,
   meaningful or not, were encountered. Meaningful nodes are: integers;
   floating-point numbers; strings; characters; booleans; and constant
   constructors. Larger values of [m] and [n] means that more
   nodes are taken into account to compute the final hash
   value, and therefore collisions are less likely to happen.
   However, hashing takes longer. The parameters [m] and [n]
   govern the tradeoff between accuracy and speed. *)

(** {6 Functorial interface} *)

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
(** The input signature of the functor {!Hashtbl.Make}. *)

module type S =
  sig
    type key
    type 'a t
    val create : int -> 'a t
    val length : 'a t -> int
    val is_empty : 'a t -> bool
    val clear : 'a t -> unit
    val copy : 'a t -> 'a t
    val add : 'a t -> key -> 'a -> unit
    val remove : 'a t -> key -> unit
    val remove_all : 'a t -> key -> unit
    val find : 'a t -> key -> 'a
    val find_all : 'a t -> key -> 'a list
    val find_default : 'a t -> key ->  'a -> 'a
    val find_option : 'a t -> key -> 'a option
    val replace : 'a t -> key -> 'a -> unit
    val mem : 'a t -> key -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val map : ('b -> 'c) -> 'b t -> 'c t
    val keys : 'a t -> key Enum.t
    val values : 'a t -> 'a Enum.t
    val enum : 'a t -> (key * 'a) Enum.t
    val of_enum : (key * 'a) Enum.t -> 'a t
  end
(** The output signature of the functor {!Hashtbl.Make}. *)
    
module Make (H : HashedType) : S with type key = H.t
  (** Functor building an implementation of the hashtable structure.
      The functor [Hashtbl.Make] returns a structure containing
      a type [key] of keys and a type ['a t] of hash tables
      associating data of type ['a] to keys of type [key].
      The operations perform similarly to those of the generic
      interface, but use the hashing and equality functions
      specified in the functor argument [H] instead of generic
      equality and hashing. *)

(** Capabilities for hashtables. *)
module Cap :
sig

  type ('a, 'b, 'c) t constraint 'c = [< `Read | `Write ]
      (** The type of a hashtable. *)
      
  (**{6 Constructors}*)

val create : int -> ('a, 'b, _) t
(** [Hashtbl.create n] creates a new, empty hash table, with
   initial size [n].  For best results, [n] should be on the
   order of the expected number of elements that will be in
   the table.  The table grows as needed, so [n] is just an
   initial guess. *)

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

(**{6 Base operations}*)
val length : ('a, 'b, _) t -> int
(** [Hashtbl.length tbl] returns the number of bindings in [tbl].
   Multiple bindings are counted multiply, so [Hashtbl.length]
   gives the number of times [Hashtbl.iter] calls its first argument. *)

val is_empty : ('a, 'b, _) t -> bool
  (** [Hashtbl.is_empty tbl] returns [true] if there are no bindings
      in [tbl], false otherwise.*)

val add : ('a, 'b, [>`Write]) t -> 'a -> 'b -> unit
(** [Hashtbl.add tbl x y] adds a binding of [x] to [y] in table [tbl].
   Previous bindings for [x] are not removed, but simply
   hidden. That is, after performing {!Hashtbl.remove}[ tbl x],
   the previous binding for [x], if any, is restored.
   (Same behavior as with association lists.) *)

val remove : ('a, 'b, [>`Write]) t -> 'a -> unit
(** [Hashtbl.remove tbl x] removes the current binding of [x] in [tbl],
   restoring the previous binding if it exists.
   It does nothing if [x] is not bound in [tbl]. *)

val remove_all : ('a,'b, [>`Write]) t -> 'a -> unit
  (** Remove all bindings for the given key *)

val replace : ('a, 'b, [>`Write]) t -> 'a -> 'b -> unit
(** [Hashtbl.replace tbl x y] replaces the current binding of [x]
   in [tbl] by a binding of [x] to [y].  If [x] is unbound in [tbl],
   a binding of [x] to [y] is added to [tbl].
   This is functionally equivalent to {!Hashtbl.remove}[ tbl x]
   followed by {!Hashtbl.add}[ tbl x y]. *)

val copy : ('a, 'b, [>`Read]) t -> ('a, 'b, _) t
(** Return a copy of the given hashtable. *)

val clear : ('a, 'b, [>`Write]) t -> unit
(** Empty a hash table. *)


(**{6 Searching}*)

val find : ('a, 'b, [>`Read]) t -> 'a -> 'b
(** [Hashtbl.find tbl x] returns the current binding of [x] in [tbl],
   or raises [Not_found] if no such binding exists. *)

val find_all : ('a, 'b, [>`Read]) t -> 'a -> 'b list
(** [Hashtbl.find_all tbl x] returns the list of all data
   associated with [x] in [tbl].
   The current binding is returned first, then the previous
   bindings, in reverse order of introduction in the table. *)

val find_default : ('a, 'b, [>`Read]) t -> 'a -> 'b -> 'b
  (** Find a binding for the key, and return a default
      value if not found *)

val find_option : ('a, 'b, [>`Read]) t -> 'a -> 'b option
  (** Find a binding for the key, or return [None] if no
      value is found *)

val mem : ('a, 'b, [>`Read]) t -> 'a -> bool
(** [Hashtbl.mem tbl x] checks if [x] is bound in [tbl]. *)

(*val exists : ('a,'b) t -> 'a -> bool*)
  (** [exists h k] returns true is at least one item with key [k] is
      found in the hashtable. *)

(**{6 Traversing}*)
val iter : ('a -> 'b -> unit) -> ('a, 'b, [>`Read]) t -> unit
(** [Hashtbl.iter f tbl] applies [f] to all bindings in table [tbl].
   [f] receives the key as first argument, and the associated value
   as second argument. Each binding is presented exactly once to [f].
   The order in which the bindings are passed to [f] is unspecified.
   However, if the table contains several bindings for the same key,
   they are passed to [f] in reverse order of introduction, that is,
   the most recent binding is passed first. *)

val fold : ('a -> 'b -> 'c -> 'c) -> ('a, 'b, [>`Read]) t -> 'c -> 'c
(** [Hashtbl.fold f tbl init] computes
   [(f kN dN ... (f k1 d1 init)...)],
   where [k1 ... kN] are the keys of all bindings in [tbl],
   and [d1 ... dN] are the associated values.
   Each binding is presented exactly once to [f].
   The order in which the bindings are passed to [f] is unspecified.
   However, if the table contains several bindings for the same key,
   they are passed to [f] in reverse order of introduction, that is,
   the most recent binding is passed first. *)

val map : ('b -> 'c) -> ('a, 'b, [>`Read]) t -> ('a, 'c, _) t
  (** [map f x] creates a new hashtable with the same
      keys as [x], but with the function [f] applied to
      all the values *)

(**{6 Conversions}*)

val keys : ('a,'b, [>`Read]) t -> 'a Enum.t
  (** Return an enumeration of all the keys of a hashtable.
      If the key is in the Hashtable multiple times, all occurrences
      will be returned.  *)

val values : ('a, 'b, [>`Read]) t -> 'b Enum.t
  (** Return an enumeration of all the values of a hashtable. *)

val enum : ('a, 'b, [>`Read]) t -> ('a * 'b) Enum.t
  (** Return an enumeration of (key,value) pairs of a hashtable. *)

val of_enum : ('a * 'b) Enum.t -> ('a, 'b, _) t
  (** Create a hashtable from a (key,value) enumeration. *)


end

(** {6 Boilerplate code}*)
(** {7 S-Expressions}*)

val t_of_sexp : (Sexplib.Sexp.t -> 'a) -> (Sexplib.Sexp.t -> 'b) -> Sexplib.Sexp.t -> ('a, 'b) t
val sexp_of_t : ('a -> Sexplib.Sexp.t) -> ('b -> Sexplib.Sexp.t) -> ('a, 'b) t -> Sexplib.Sexp.t

end
