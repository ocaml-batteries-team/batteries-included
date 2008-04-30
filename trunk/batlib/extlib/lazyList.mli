(* 
 * LazyListLabels - enumeration over abstract collection of elements.
 * Copyright (C) 2008 David Teller
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
(**
   A module for lazy list management.

   If you use syntax extension pa_comprehension, you'll probably want to take
   advantage of the syntax for lazy lists.
*)


exception Empty_list
  (** [Empty_list] is raised when an operation applied on an empty list
      is invalid. For instance, [hd nil] will raise [Empty_list]. *)
  
exception Invalid_index of int
  (** [Invalid_index] is raised when an indexed access on a list is
      out of list bounds. *)
  
exception Different_list_size of string
  (** [Different_list_size] is raised when applying functions such as
      [iter2] on two lists having different size. *)
  
  
(**
   {1  Lazyness}
*)

type 'a node_t = | Nil | Cons of 'a * 'a t 
(**
   The type of an item in the list.

   {b Note} this type is kept concrete so as to allow pattern-matching.
*)
and 'a t = ('a node_t) Lazy.t
(**
   The type of a lazy list.

   {b Note} this type is kept concrete so as to allow pattern-matching.
*)

(**
   The empty list.
*)
val nil : ('a node_t) lazy_t
  
(**
   Eager iteration

   [LazyList.iter f [^ a1; ...; an ^]] applies function [f] in turn to [a1; ...; an]. 
   It is equivalent to [begin f a1; f a2; ...; f an; () end]. In particular, it
   causes all the elements of the list to be evaluated.
*)
val iter : ('a -> 'b) -> 'a t -> unit 
  
(**
   Eager iteration, with indices

   [LazyList.iteri f [^ a1; ...; an ^]] applies function [f] in turn to [a1 0; ...; an (n - 1)]. 
   It is equivalent to [begin f a1 0; f a2 0; ...; f an (n-1); () end]. In particular, it
   causes all the elements of the list to be evaluated.
*)
val iteri : (int -> 'a -> 'b) -> 'a t -> unit
  
(**
   Lazy map

   [LazyList.map f [^ a1; ...; an ^]] applies function [f] to [a1, ..., an], and builds the list 
   [^ f a1; ...; f an ^]  with the results returned by [f]. Not tail-recursive. Evaluations
   of [f] take place only when the contents of the list are forced.
*)
val map : ('a -> 'b) -> 'a t -> 'b t
  
(**
   Eager fold_left

   [LazyList.fold_left f a [^ b1; ...; bn ^]] is [f (... (f (f a b1) b2) ...) bn]. This causes
   evaluation of all the elements of the list.
*)
val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a

(**
   Eager fold_right

   [LazyList.fold_right f a [^ b1; ...; bn ^]] is [f ( f (... (f (f a bn) ...) b2) b1]. This causes
   evaluation of all the elements of the list. Not tail-recursive.
*)
val fold_right : ('a -> 'b -> 'b) -> 'b -> 'a t -> 'b
  
(**
   {1  Common functions}
*)
(**
   Return the length (number of elements) of the given list.

   Causes the evaluation of all the elements of the list.
*)
val length : 'a t -> int
  
(**
   Return the first element of the given list. Raise [Empty_list] if the list is empty.

   Note: this function does not comply with the usual exceptionless error-management
   recommendations, as doing so would essentially render it useless.
*)
val hd : 'a t -> 'a
  
(**
   Return the given list without its first element. Raise [Empty_list] if the list is empty.

   Note: this function does not comply with the usual exceptionless error-management
   recommendations, as doing so would essentially render it useless.
*)
val tl : 'a t -> 'a t
  
val nth : 'a t -> int -> 'a
  (**  Obsolete. As [at_exn]*)

val at_exn : 'a t -> int -> 'a
  (** [at_exn l n] returns the n-th element of the list [l] or raise
      [Invalid_index] is the index is outside of [l] bounds. *)

val at : 'a t -> int -> ('a, [`Invalid_index of int]) Result.t
  (** If [n] is inside the bounds of [l], [at l n] returns [Ok x], where
      [x] is the n-th element of the list [l]. Otherwise, returns [Error
      (`Invalid_index(n))].*)

  
(**
   Eager list reversal.
*)
val rev : 'a t -> 'a t
  
(**
   Evaluate a list and append another list after this one.

   Cost is linear in the length of the first list, not tail-recursive.
*)
val eager_append : 'a t -> 'a t -> 'a t
  
(**
   Eager reverse-and-append

   Cost is linear in the length of the first list, tail-recursive.
*)
val rev_append : 'a t -> 'a t -> 'a t
  
(**
   Lazy append

   Cost is constant. All evaluation is delayed until the contents
   of the list are actually read. Reading itself is delayed by
   a constant.
*)
val append : 'a t -> 'a t -> 'a t
  
(**
   As lazy append
*)
val ( ^@^ ) : 'a t -> 'a t -> 'a t
  
(**
   Lazy concatenation of a list of lazy lists
*)
val concat : ('a t) list -> 'a t
  
(**
   Lazy concatenation of a lazy list of lazy lists
*)
val flatten : ('a t) t -> 'a t
  
(**
   {1  Conversions}
*)
(**
   Eager conversion to string.
*)
val to_list : 'a t -> 'a list
  
(**
   Lazy conversion to stream.
*)
val to_stream : 'a t -> 'a Stream.t
  
(**
   Eager conversion to array.
*)
val to_array : 'a t -> 'a array

(**
   Lazy conversion to enumeration
*)
val enum  : 'a t -> 'a Enum.t

(**
   Lazy conversion from lists

   Albeit slower than eager conversion, this is the default mechanism for converting from regular 
   lists to lazy lists.  This for two reasons :
   * if you're using lazy lists, total speed probably isn't as much an issue as start-up speed
   * this will let you convert regular infinite lists to lazy lists.
*)
val of_list : 'a list -> 'a t
  
(**
   Lazy conversion from stream.
*)
val of_stream : 'a Stream.t -> 'a t

(**
   Lazy conversion from enum.
*)
val of_enum : 'a Enum.t -> 'a t  

(**
   Eager conversion from lists
*)
val eager_of_list : 'a list -> 'a t
  
(**
   Lazy conversion from array
*)
val of_array : 'a array -> 'a t
  
(**
   {1  Predicates}
*)
(**
   Lazy filtering.

   [filter p l] returns all the elements of the list [l]  that satisfy the predicate [p]. 
   The order of the elements in the input list is preserved.
*)
val filter : ('a -> bool) -> 'a t -> 'a t
  
(**
   Eager existential.

   [exists p [^ a1; ...; an ^]] checks if at least one element of the list satisfies the predicate [p]. 
   That is, it returns [ (p a1) || (p a2) || ... || (p an) ].
*)
val exists : ('a -> bool) -> 'a t -> bool
  
(**
   Eager universal.

   [for_all p [^ a1; ...; an ^]] checks if all elements of the list satisfy the predicate [p]. 
   That is, it returns [(p a1) && (p a2) && ... && (p an)].
*)
val for_all : ('a -> bool) -> 'a t -> bool
  
(**
   Compute lazily a range of integers a .. b as a lazy list.

   The range is never empty. If a <= b, the range contains a, a+1 ... b.
   Otherwise, it contains a, a-1, ..., b.
*)
val range : int -> int -> int t
  
(**
   Lazily eliminate some elements and transform others.

   [map_filter f [^ a1; a2; ... ;an ^]] applies [f] to each
   [a1], ..., [an]. If [f ai] evaluates to [None], the element
   is not included in the result. Otherwise, if [f ai] evaluates 
   to [Some x], element [x] is included in the result.

   This is equivalent to
   [match f a1 with
     | Some x1 -> x1 ^:^ (match f a2 with
            |Some x2 -> x2 ^:^ (match ...
                       (match f an with
                            | Some xn -> [^ xn ^]
                            | None    -> [^ ^]
                       ) ... )
            | ...) 
     | None   -> ... ].
*)
val map_filter : ('a -> 'b option) -> 'a t -> 'b t
  
val mem : 'a -> 'a t -> bool
  
(**
   An infinite list of nothing
*)
val eternity : unit t
  
(**
   Generate a list from a function.
*)
val from : (unit -> 'a) -> 'a t
  

