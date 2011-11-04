(* 
 * LazyList - Lazily-computed lists of possibly infinite size
 * Copyright (C) 2009 David Rajchenbach-Teller, LIFO, Universite d'Orleans
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
(**  Lazy lists of elements.

     Lazy lists are similar to lists, with the exception that their contents are
     only computed whenever requested. This makes them particularly useful in
     contexts where streams of data are to be handled. 

     {b Note} For this documentation, we will assume the existence of
     a lazy list syntax extension such that [[^ ^]] is the empty lazy
     list and [[^ a;b;c ^]] is the lazy list containing elements [a],
     [b], [c].

     {b Note} Enumerations (as featured in module {!BatEnum}) and lazy
     lists (as featured in this module) are quite similar in
     purpose. Lazy lists are slightly higher level, insofar as no
     cloning is required to get them to work, which makes them
     slightly more useful in contexts where backtracking is
     common. Enumerations, on the other hand, are closer to
     traditional stream processing, and require more low-level marking
     whenever backtracking is required, but may be faster and more
     memory-efficient when used properly. Either choice is recommended
     over OCaml's built-in {!Stream}.

     @author David Teller
*)

(** {6 Exceptions} *)

exception Empty_list
  (** [Empty_list] is raised when an operation applied on an empty list
      is invalid. For instance, [hd nil] will raise [Empty_list]. *)
  
exception Invalid_index of int
  (** [Invalid_index] is raised when an indexed access on a list is
      out of list bounds. *)
  
exception Different_list_size of string
  (** [Different_list_size] is raised when applying functions such as
      [iter2] on two lists having different size. *)
  
exception No_more_elements
  (** See {!from} and {!from_loop} for more information on this exception.*)

(**{6  Type}

   {b Note} The types are kept concrete so as to allow pattern-matching.
   However, it is generally easier to manipulate {!nil} and {!cons}.*)

type 'a t = ('a node_t) Lazy.t 
(**The type of a lazy list.*)

and 'a node_t = | Nil | Cons of 'a * 'a t 
(**The type of an item in the list.*)

include BatEnum.Enumerable with type 'a enumerable = 'a t
include BatInterfaces.Mappable with type 'a mappable = 'a t

(** {6 Access } *)

val nil : 'a t
(**The empty list.*)

val cons : 'a -> 'a t -> 'a t
(**Build a list from a head and a tail.*)

val ( ^:^ ) : 'a -> 'a t -> 'a t
(**As [cons]: [x^:^l] is the lazy list with head [x] and tail [l]*)

val peek : 'a t -> 'a option
(**[peek l] returns the first element of [l], if it exists.*)

val get : 'a t -> ('a * 'a t) option
(**[get l] returns the head and tail of [l], if [l] is not empty.*)

(**
   {6 List creation}
*)


val from: (unit -> 'a) -> 'a t  
  (**[from next] creates a (possibly infinite) lazy list from the successive
     results of [next]. 
     The function {i may} raise {!LazyList.No_more_elements} to denote the end of the 
     list.*)

val from_while: (unit -> 'a option) -> 'a t
  (**[from next] creates a (possibly infinite) lazy list from the successive
     results of [next].
     The list ends whenever [next] returns [None]. *)

val from_loop: 'b -> ('b -> ('a * 'b)) -> 'a t
  (**[from_loop data next] creates a (possibly infinite) lazy list from
     the successive results of applying [next] to [data], then to the
     result, etc. The list ends whenever the function raises 
     {!LazyList.No_more_elements}*)

val seq: 'a -> ('a -> 'a) -> ('a -> bool) -> 'a t
  (** [seq init step cond] creates a sequence of data, which starts
      from [init],  extends by [step],  until the condition [cond]
      fails. E.g. [seq 1 ((+) 1) ((>) 100)] returns [^[1, 2, ... 99]^]. If [cond
      init] is false, the result is empty. *)

val unfold: 'b -> ('b -> ('a * 'b) option) -> 'a t
  (**[unfold data next] creates a (possibly infinite) lazy list from
     the successive results of applying [next] to [data], then to the
     result, etc. The list ends whenever the function returns [None]*)

val init : int -> (int -> 'a) -> 'a t
  (** Similar to [Array.init], [init n f] returns the lazy list 
      containing the results of (f 0),(f 1).... (f (n-1)).
      Raise [Invalid_arg "LazyList.init"] if n < 0.*)

val make : int -> 'a -> 'a t
  (** Similar to [String.make], [make n x] returns a
      list containing [n] elements [x].   *)

val range : int -> int -> int t
(**Compute lazily a range of integers a .. b as a lazy list.

   The range is empty if a <= b.*)
  

(**
   {6 Higher-order functions}
*)

val iter : ('a -> 'b) -> 'a t -> unit 
(**
   Eager iteration

   [iter f [^ a0; a1; ...; an ^]] applies function [f] in turn to [a0;
   a1; ...; an].  It is equivalent to [begin f a0; f a1; ...; f an; ()
   end]. In particular, it causes all the elements of the list to be
   evaluated.*)
  
val iteri : (int -> 'a -> 'b) -> 'a t -> unit
(**Eager iteration, with indices

   [iteri f [^ a0; a1; ...; an ^]] applies function [f] in turn to
   [a0; a1;...; an], along with the corresponding [0,1..n] index.  It
   is equivalent to [begin f 0 a0; f 1 a1; ...; f n an; ()
   end]. In particular, it causes all the elements of the list to be
   evaluated.*)


val map : ('a -> 'b) -> 'a t -> 'b t
(**Lazy map

   [map f [^ a0; a1; ... ^]] builds the list [[^ f a0; f a1; ... ^]]
   with the results returned by [f]. Not tail-recursive. Evaluations
   of [f] take place only when the contents of the list are forced.*)

  
val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t
(**Lazy map, with indices

   [mapi f [^ a0; a1; ... ^]] builds the list [[^ f 0 a0; f 1 a1;
   ... ^]] with the results returned by [f]. Not
   tail-recursive. Evaluations of [f] take place only when the
   contents of the list are forced.
*)

val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
(**Eager fold_left

   [LazyList.fold_left f a [^ b0; b1; ...; bn ^]] is [f (... (f (f
   a b0) b1) ...) bn]. This causes evaluation of all the elements of
   the list.*)


val fold_right : ('a -> 'b -> 'b) -> 'b -> 'a t -> 'b
(**Eager fold_right

   [fold_right f a [^ b0; b1; ...; bn ^]] is [f ( f (... (f (f a bn) ...) b1) b0]. This causes
   evaluation of all the elements of the list. Not tail-recursive.*)

(** {6 Finding}*)

val mem : 'a -> 'a t -> bool
  (** [mem x l] determines if [x] is part of [l].
      Evaluates all the elements of [l] which appear
      before [x].*)

val memq : 'a -> 'a t -> bool
  (** As [mem], but with physical equality*)

val find : ('a -> bool) -> 'a t -> 'a
  (** [find p l] returns the first element of [l] such as [p x]
      returns [true] or raises [Not_found] if such an element
      has not been found.*)

val rfind : ('a -> bool) -> 'a t -> 'a
  (** [rfind p l] returns the last element [x] of [l] such as [p x] returns
      [true] or raises [Not_found] if such element as not been found. *)

val find_exn : ('a -> bool) -> exn -> 'a t -> 'a
  (** [find_exn p e l] returns the first element of [l] such as [p x]
      returns [true] or raises [e] if such an element has not been found. *)

val rfind_exn : ('a -> bool) -> exn -> 'a t -> 'a
  (** [find_exn p e l] returns the last element of [l] such as [p x]
      returns [true] or raises [e] if such an element has not been found. *)

val findi : (int -> 'a -> bool) -> 'a t -> (int * 'a)
  (** [findi p e l] returns the first element [ai] of [l] along with its
      index [i] such that [p i ai] is true, or raises [Not_found] if no
      such element has been found. *)

val rfindi : (int -> 'a -> bool) -> 'a t -> (int * 'a)
  (** [findi p e l] returns the last element [ai] of [l] along with its
      index [i] such that [p i ai] is true, or raises [Not_found] if no
      such element has been found. *)

val index_of : 'a -> 'a t -> int option
  (** [index_of e l] returns the index of the first occurrence of [e]
      in [l], or [None] if there is no occurrence of [e] in [l] *)  

val index_ofq : 'a -> 'a t -> int option
  (** [index_ofq e l] behaves as [index_of e l] except it uses
      physical equality*)

val rindex_of : 'a -> 'a t -> int option
  (** [index_of e l] returns the index of the last occurrence of [e]
      in [l], or [None] if there is no occurrence of [e] in [l] *)  

val rindex_ofq : 'a -> 'a t -> int option
  (** [rindex_ofq e l] behaves as [rindex_of e l] except it uses
      physical equality*)
(**
   {6  Common functions}
*)
val next : 'a t -> 'a node_t
  (**Compute and return the next value of the list*)

val length : 'a t -> int
  (**Return the length (number of elements) of the given list.

     Causes the evaluation of all the elements of the list.*)

val is_empty : 'a t -> bool
  (** Returns [true] if the list is empty, false otherwise.*)

val would_at_fail: 'a t -> int -> bool
  (**[would_at_fail l n] returns [true] if [l] contains strictly less
     than [n] elements, [false] otherwise*)

val hd : 'a t -> 'a  
(**Return the first element of the given list. Raise [Empty_list] if the list is empty.

   Note: this function does not comply with the usual exceptionless error-management
   recommendations, as doing so would essentially render it useless.*)

val tl : 'a t -> 'a t
(**Return the given list without its first element. Raise [Empty_list] if the list is empty.

   Note: this function does not comply with the usual exceptionless error-management
   recommendations, as doing so would essentially render it useless.*)

val first : 'a t -> 'a
  (** As [hd]*)

val last : 'a t -> 'a
  (** Returns the last element of the list, or raise [Empty_list] if
      the list is empty. This function takes linear time and causes the
      evaluation of all elements of the list*)

val at : 'a t -> int -> 'a
(** [at l n] returns the element at index [n] (starting from [0]) in
    the list [l] or raise [Invalid_index] is the index is outside of
    [l] bounds. *)

val nth : 'a t -> int -> 'a
  (**  Obsolete. As [at]*)

(** {6 Association lists}

    These lists behave essentially as {!HashMap}, although they are
    typically faster for short number of associations, and much
    slower for for large number of associations. *)

val assoc : 'a -> ('a * 'b) t -> 'b
  (** [assoc a l] returns the value associated with key [a] in the list of
      pairs [l]. That is, [assoc a [^ ...; (a,b); ...^] = b]
      if [(a,b)] is the leftmost binding of [a] in list [l].
      Raise [Not_found] if there is no value associated with [a] in the
      list [l]. *)

val assq : 'a -> ('a * 'b) t -> 'b
  (** As {!assoc} but with physical equality *)

val mem_assoc : 'a -> ('a * 'b) t -> bool
  (** As {!assoc} but simply returns [true] if a binding exists, [false]
      otherwise. *)

val mem_assq : 'a -> ('a * 'b) t -> bool
  (** As {!mem_assoc} but with physical equality.*)


val rev : 'a t -> 'a t
  (** Eager list reversal.*)

(** {6 Transformations} *)

val eager_append : 'a t -> 'a t -> 'a t
(**Evaluate a list and append another list after this one.

   Cost is linear in the length of the first list, not tail-recursive.*)
  
val rev_append : 'a t -> 'a t -> 'a t
(**Eager reverse-and-append

   Cost is linear in the length of the first list, tail-recursive.*)

val append : 'a t -> 'a t -> 'a t
(**Lazy append

   Cost is constant. All evaluation is delayed until the contents
   of the list are actually read. Reading itself is delayed by
   a constant.*)

val ( ^@^ ) : 'a t -> 'a t -> 'a t
  (**As lazy append*)

  
val concat : ('a t) t -> 'a t
  (**Lazy concatenation of a lazy list of lazy lists*)

val flatten : ('a t) list -> 'a t
  (** Lazy concatenation of a list of lazy lists*)

val split_at : int -> 'a t -> 'a t * 'a t
  (** [split_at n l] returns two lists [l1] and [l2], [l1] containing the
      first [n] elements of [l] and [l2] the others. Raise [Invalid_index] if
      [n] is outside of [l] size bounds. *)
  
val split_nth : int -> 'a t -> 'a t * 'a t
  (** Obsolete. As [split_at]. *)

(**{6 Dropping elements}*)

val unique : ?cmp:('a -> 'a -> int) -> 'a t -> 'a t
  (** [unique cmp l] returns the list [l] without any duplicate element.
      Default comparator ( = ) is used if no comparison function specified. *)

val unique_eq : ?eq:('a -> 'a -> bool) -> 'a t -> 'a t
(** as [unique] except only uses an equality function.  Use for
    short lists when comparing is expensive compared to equality
    testing 
    @since 1.3.0
*)

val remove : 'a -> 'a t -> 'a t
(** [remove l x] returns the list [l] without the first element [x] found
    or returns  [l] if no element is equal to [x]. Elements are compared
    using ( = ). *)
  
val remove_if : ('a -> bool) -> 'a t -> 'a t
  (** [remove_if cmp l] is similar to [remove], but with [cmp] used
      instead of ( = ). *)
  
val remove_all : 'a -> 'a t -> 'a t
  (** [remove_all l x] is similar to [remove] but removes all elements that
      are equal to [x] and not only the first one. *)

val remove_all_such : ('a -> bool) -> 'a t -> 'a t
  (** [remove_all l x] is similar to [remove] but removes all elements that
      are equal to [x] and not only the first one. *)
  
val take : int -> 'a t -> 'a t
  (** [take n l] returns up to the [n] first elements from list [l], if
      available. *)
  
val drop : int -> 'a t -> 'a t
  (** [drop n l] returns [l] without the first [n] elements, or the empty
      list if [l] have less than [n] elements. *)
  
val take_while : ('a -> bool) -> 'a t -> 'a t
  (** [take_while f xs] returns the first elements of list [xs]
      which satisfy the predicate [f]. *)

val drop_while : ('a -> bool) -> 'a t -> 'a t
  (** [drop_while f xs] returns the list [xs] with the first
      elements satisfying the predicate [f] dropped. *)
  

(**
   {6  Conversions}
*)

val to_list : 'a t -> 'a list
(**Eager conversion to string.*)  

val to_stream : 'a t -> 'a Stream.t
(**Lazy conversion to stream.*)  

val to_array : 'a t -> 'a array
(** Eager conversion to array.*)

val enum  : 'a t -> 'a BatEnum.t
(**Lazy conversion to enumeration*)

val of_list : 'a list -> 'a t
(**Lazy conversion from lists

   Albeit slower than eager conversion, this is the default mechanism for converting from regular 
   lists to lazy lists.  This for two reasons :
   * if you're using lazy lists, total speed probably isn't as much an issue as start-up speed
   * this will let you convert regular infinite lists to lazy lists.*)
  

val of_stream : 'a Stream.t -> 'a t
(**Lazy conversion from stream.*)

val of_enum : 'a BatEnum.t -> 'a t  
(**Lazy conversion from enum.*)

val eager_of_list : 'a list -> 'a t
(**Eager conversion from lists.

   This function is much faster than {!of_list} but will freeze on cyclic lists.
*)

val of_array : 'a array -> 'a t
(**Eager conversion from array*)
  
(**
   {6  Predicates}
*)

val filter : ('a -> bool) -> 'a t -> 'a t
(**Lazy filtering.

   [filter p l] returns all the elements of the list [l]  that satisfy the predicate [p]. 
   The order of the elements in the input list is preserved.*)  

val exists : ('a -> bool) -> 'a t -> bool
(**Eager existential.

   [exists p [^ a0; a1; ... ^]] checks if at least one element of the list satisfies the predicate [p]. 
   That is, it returns [ (p a0) || (p a1) || ... ].*)  

val for_all : ('a -> bool) -> 'a t -> bool
(**Eager universal.

   [for_all p [^ a0; a1; ... ^]] checks if all elements of the list satisfy the predicate [p]. 
   That is, it returns [(p a0) && (p a1) && ... ].*)  

val filter_map : ('a -> 'b option) -> 'a t -> 'b t
(**Lazily eliminate some elements and transform others.

   [filter_map f [^ a0; a1; ... ^]] applies lazily [f] to each [a0],
   [a1]... If [f ai] evaluates to [None], the element is not included
   in the result. Otherwise, if [f ai] evaluates to [Some x], element
   [x] is included in the result.

   This is equivalent to
   [match f a0 with
     | Some x0 -> x0 ^:^ (match f a1 with
            | Some x1 -> x1 ^:^ ...
            | None -> [^ ^]) 
     | None   -> [^ ^] ].*)


(**{6 Misc.}*)    

val eternity : unit t
(** An infinite list of nothing*)
  
(**{6 Sorting}*)

val sort : ?cmp:('a -> 'a -> int) -> 'a t -> 'a t
  (** Sort the list using optional comparator (by default [compare]). *)

val stable_sort : ('a -> 'a -> int) -> 'a t -> 'a t

(**{6 Operations on two lists}*)
val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
(** [map2 f [^ a0; a1; ...^] [^ b0; b1; ... ^]] is [[^ f a0 b0; f a1
    b1; ... ^]]. Raise [Different_list_size] if the two lists have
    different lengths. Not tail-recursive, lazy. In particular, the
    exception is raised only after the shortest list has been
    entirely consumed. *)
  
val iter2 : ('a -> 'b -> unit) -> 'a t -> 'b t -> unit
(** [iter2 f [^ a0; ...; an ^] [^ b0; ...; bn ^]] calls in turn
    [f a0 b0; ...; f an bn]. Tail-recursive, eager.
    Raise [Different_list_size] if the two lists have
    different lengths. *)


val fold_left2 : ('a -> 'b -> 'c -> 'a) -> 'a -> 'b t -> 'c t -> 'a
  (** [fold_left2 f a [^ b0; b1; ...; bn ^] [^ c0; c1; ...; cn ^]] is
      [f (... (f (f a b0 c0) b1 c1) ...) bn cn]. Eager.
      Raise [Different_list_size] if the two lists have
      different lengths. *)

val fold_right2 : ('a -> 'b -> 'c -> 'c) -> 'a t -> 'b t -> 'c -> 'c
  (** [fold_right2 f [^ a0; a1; ...; an ^] [^ b0; b1; ...; bn ^] c] is
      [f a0 b0 (f a1 b1 (... (f an bn c) ...))]. Eager.
      Raise [Different_list_size] if the two lists have
      different lengths.  Tail-recursive. *)

val for_all2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
  (** Same as {!for_all}, but for a two-argument predicate.
      Raise [Different_list_size] if the two lists have
      different lengths. *)

val exists2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
  (** Same as {!exists}, but for a two-argument predicate.
      Raise [Different_list_size] if the two lists have
      different lengths. *)

val combine : 'a t -> 'b t -> ('a * 'b) t
  (** Transform a pair of lists into a list of pairs:
      [combine [^ a0; a1; ... ^] [^ b0; b1; ... ^]] is
      [[^ (a0, b0); (a1, b1); ... ^]].
      Raise [Different_list_size] if the two lists
      have different lengths.  Tail-recursive, lazy. *)

val uncombine : ('a * 'b) t -> 'a t * 'b t
  (** Divide a list of pairs into a pair of lists. *)

(** {6 Infix submodule regrouping all infix operators} *)
module Infix : sig
  val ( ^:^ ) : 'a -> 'a t -> 'a t
  val ( ^@^ ) : 'a t -> 'a t -> 'a t
end

(** {6 Boilerplate code}*)

(** {7 Printing}*)
  
val print : ?first:string -> ?last:string -> ?sep:string ->('a BatInnerIO.output -> 'b -> unit) ->  'a BatInnerIO.output -> 'b t -> unit


(** {6 Override modules}*)
  
(**
   The following modules replace functions defined in {!LazyList} with functions
   behaving slightly differently but having the same name. This is by design:
   the functions meant to override the corresponding functions of {!LazyList}.
   
*)

(** Exceptionless counterparts for error-raising operations*)
module Exceptionless : sig
  
  val find : ('a -> bool) -> 'a t -> 'a option
    (** [rfind p l] returns [Some x] where [x] is the first element of [l] such 
	that [p x] returns [true] or [None] if such element as not been found. *)

  val rfind : ('a -> bool) -> 'a t -> 'a option
    (** [rfind p l] returns [Some x] where [x] is the last element of [l] such 
	that [p x] returns [true] or [None] if such element as not been found. *)
    
  val findi : (int -> 'a -> bool) -> 'a t -> (int * 'a) option
    (** [findi p e l] returns [Some (i, ai)] where [ai] and [i] are respectively the 
	first element of [l] and its index, such that [p i ai] is true, 
	or [None] if no	such element has been found. *)

  val rfindi : (int -> 'a -> bool) -> 'a t -> (int * 'a) option
    (** [findi p e l] returns [Some (i, ai)] where [ai] and [i] are respectively the 
	last element of [l] and its index, such that [p i ai] is true, 
	or [None] if no	such element has been found. *)

  val split_at : int -> 'a t -> [`Ok of ('a t * 'a t) | `Invalid_index of int]
    (** Whenever [n] is inside of [l] size bounds, [split_at n l] returns 
	[`Ok (l1,l2)], where [l1] contains the first [n] elements of [l] and [l2] 
	contains the others. Otherwise, returns [`Invalid_index n] *)

  val at : 'a t -> int -> [`Ok of 'a | `Invalid_index of int]
    (** If [n] is inside the bounds of [l], [at l n] returns [`Ok x], where
	[x] is the n-th element of the list [l]. Otherwise, returns 
	[`Invalid_index n].*) 

  val assoc : 'a -> ('a * 'b) t -> 'b option
    (** [assoc a l] returns [Some b] where [b] is the value associated with key [a] 
	in the list of pairs [l]. That is, [assoc a [ ...; (a,b); ...] = Some b]
	if [(a,b)] is the leftmost binding of [a] in list [l].
	Return [None] if there is no value associated with [a] in the
		list [l]. *)

  val assq : 'a -> ('a * 'b) t -> 'b option
    (** As {!assoc} but with physical equality *)	    
end

(** Operations on {!LazyList} with labels.
    
    This module overrides a number of functions of {!List} by
    functions in which some arguments require labels. These labels are
    there to improve readability and safety and to let you change the
    order of arguments to functions. In every case, the behavior of the
    function is identical to that of the corresponding function of {!LazyList}.
*)
module Labels : sig
  val iter : f:('a -> 'b) -> 'a t -> unit 
  val iteri : f:(int -> 'a -> 'b) -> 'a t -> unit
  val map : f:('a -> 'b) -> 'a t -> 'b t
  val mapi : f:(int -> 'a -> 'b) -> 'a t -> 'b t
  val fold_left : f:('a -> 'b -> 'a) -> init:'a -> 'b t -> 'a
  val fold_right : f:('a -> 'b -> 'b) -> init:'b -> 'a t -> 'b
  val find : f:('a -> bool) -> 'a t -> 'a
  val rfind : f:('a -> bool) -> 'a t -> 'a
  val find_exn : f:('a -> bool) -> exn -> 'a t -> 'a
  val rfind_exn : f:('a -> bool) -> exn -> 'a t -> 'a
  val findi : f:(int -> 'a -> bool) -> 'a t -> (int * 'a)
  val rfindi : f:(int -> 'a -> bool) -> 'a t -> (int * 'a)
  val remove_if : f:('a -> bool) -> 'a t -> 'a t
  val remove_all_such : f:('a -> bool) -> 'a t -> 'a t
  val take_while : f:('a -> bool) -> 'a t -> 'a t
  val drop_while : f:('a -> bool) -> 'a t -> 'a t
  val filter : f:('a -> bool) -> 'a t -> 'a t
  val exists : f:('a -> bool) -> 'a t -> bool
  val for_all : f:('a -> bool) -> 'a t -> bool
  val filter_map : f:('a -> 'b option) -> 'a t -> 'b t
  val map2 : f:('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val iter2 : f:('a -> 'b -> unit) -> 'a t -> 'b t -> unit
  val fold_right2 : f:('a -> 'b -> 'c -> 'c) -> 'a t -> 'b t -> init:'c -> 'c
  val for_all2 : f:('a -> 'b -> bool) -> 'a t -> 'b t -> bool
  val exists2 : f:('a -> 'b -> bool) -> 'a t -> 'b t -> bool


  module Exceptionless : sig
    val find:  f:('a -> bool) -> 'a t -> 'a option
    val rfind: f:('a -> bool) -> 'a t -> 'a option
    val findi: f:(int -> 'a -> bool) -> 'a t -> (int * 'a) option
    val rfindi:f:(int -> 'a -> bool) -> 'a t -> (int * 'a) option
    val split_at: int -> 'a t -> [`Ok of ('a t * 'a t) | `Invalid_index of int]
    val at : 'a t -> int -> [`Ok of 'a | `Invalid_index of int]
    val assoc : 'a -> ('a * 'b) t -> 'b option
    val assq : 'a -> ('a * 'b) t -> 'b option
  end
end


