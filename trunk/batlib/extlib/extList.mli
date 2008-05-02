(*
 * ExtList - additional and modified functions for lists.
 * Copyright (C) 2003 Brian Hurt
 * Copyright (C) 2003 Nicolas Cannasse
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

(** Additional and modified functions for lists.

    The OCaml standard library provides a module for list functions.
    This ExtList module can be used to override the List module or
    as a standalone module. It provides new functions and modify
    the behavior of some other ones (in particular all functions
    are now {b tail-recursive}).

    The following functions have the same behavior as the [List]
    module ones but are tail-recursive: [map], [append], [concat],
    [flatten], [fold_right], [remove_assoc], [remove_assq],
    [split]. That means they will not
    cause a [Stack_overflow] when used on very long list.

    The implementation might be a little more slow in bytecode,
    but compiling in native code will not affect performances. 
*)

module List :
    sig

	val init : int -> (int -> 'a) -> 'a list
	(** Similar to [Array.init], [init n f] returns the list containing
	 the results of (f 0),(f 1).... (f (n-1)).
	 Raise [Invalid_arg "ExtList.init"] if n < 0.*)

	val make : int -> 'a -> 'a list
	  (** Similar to [String.make], [make n x] returns a
	      list containing [n] elements [x]. *)

	val iteri : (int -> 'a -> 'b) -> 'a list -> unit
	(** [iteri f l] will call [(f 0 a0);(f 1 a1) ... (f n an)] where
	 [a0..an] are the elements of the list [l]. *)

	val map : ('a -> 'b) -> 'a list -> 'b list
	  (** [map f [a1; ...; an]] applies function [f] to [a1, ..., an],
	      and builds the list [[f a1; ...; f an]]
	      with the results returned by [f].  Tail-recursive. *)

	val mapi : (int -> 'a -> 'b) -> 'a list -> 'b list
	(** [mapi f l] will build the list containing
	 [(f 0 a0);(f 1 a1) ... (f n an)] where [a0..an] are the elements of
	 the list [l]. *)

	val first : 'a list -> 'a
	(** Returns the first element of the list, or raise [Empty_list] if
	 the list is empty (similar to [hd]). *)

	val last : 'a list -> 'a
	(** Returns the last element of the list, or raise [Empty_list] if
	 the list is empty. This function takes linear time. *)

	val at : 'a list -> int -> 'a
	(** [at l n] returns the n-th element of the list [l] or raise
	 [Invalid_index] is the index is outside of [l] bounds. *)

	val rfind : ('a -> bool) -> 'a list -> 'a
	(** [rfind p l] returns the last element [x] of [l] such as [p x] returns
	 [true] or raises [Not_found] if such element as not been found. *)

	val find : ('a -> bool) -> 'a list -> 'a
	  (** [find p l] returns the first element of [l] such as [p x]
	      returns [true] or raises [Not_found] if such an element
	      has not been found.*)

	val find_exn : ('a -> bool) -> exn -> 'a list -> 'a
	(** [find_exn p e l] returns the first element of [l] such as [p x]
	 returns [true] or raises [e] if such an element has not been found. *)

	val findi : (int -> 'a -> bool) -> 'a list -> (int * 'a)
	(** [findi p e l] returns the first element [ai] of [l] along with its
	 index [i] such that [p i ai] is true, or raises [Not_found] if no
	 such element has been found. *)

	val index_of : 'a -> 'a list -> int option
        (** [index_of e l] returns the index of the first occurrence of [e]
	    in [l], or [None] if there is no occurrence of [e] in [l] *)

	val index_ofq : 'a -> 'a list -> int option
        (** [index_ofq e l] behaves as [index_of e l] except it uses
	    physical equality*)

	val rindex_of : 'a -> 'a list -> int option
        (** [rindex_of e l] returns the index of the last occurrence of [e]
	    in [l], or [None] if there is no occurrence of [e] in [l] *)

	val rindex_ofq : 'a -> 'a list -> int option
        (** [rindex_ofq e l] behaves as [rindex_of e l] except it uses
	    physical equality*)

	val unique : ?cmp:('a -> 'a -> bool) -> 'a list -> 'a list
	(** [unique cmp l] returns the list [l] without any duplicate element.
	 Default comparator ( = ) is used if no comparison function specified. *)

	val filter_map : ('a -> 'b option) -> 'a list -> 'b list
	(** [filter_map f l] call [(f a0) (f a1).... (f an)] where [a0..an] are
	 the elements of [l]. It returns the list of elements [bi] such as
	 [f ai = Some bi] (when [f] returns [None], the corresponding element of
	 [l] is discarded). *)

	val split_at : int -> 'a list -> 'a list * 'a list
	(** [split_at n l] returns two lists [l1] and [l2], [l1] containing the
	 first [n] elements of [l] and [l2] the others. Raise [Invalid_index] if
	 [n] is outside of [l] size bounds. *)

	val split_nth : int -> 'a list -> 'a list * 'a list
	(** Obsolete. As [split_at]. *)

	val remove : 'a -> 'a list -> 'a list
	(** [remove l x] returns the list [l] without the first element [x] found
	 or returns  [l] if no element is equal to [x]. Elements are compared
	 using ( = ). *)

	val remove_if : ('a -> bool) -> 'a list -> 'a list
	(** [remove_if cmp l] is similar to [remove], but with [cmp] used
	 instead of ( = ). *)

	val remove_all : 'a -> 'a list -> 'a list
	(** [remove_all l x] is similar to [remove] but removes all elements that
	 are equal to [x] and not only the first one. *)

	val take : int -> 'a list -> 'a list
	(** [take n l] returns up to the [n] first elements from list [l], if
	 available. *)

	val drop : int -> 'a list -> 'a list
	(** [drop n l] returns [l] without the first [n] elements, or the empty
	 list if [l] have less than [n] elements. *)

	val take_while : ('a -> bool) -> 'a list -> 'a list
	  (** [takewhile f xs] returns the first elements of list [xs]
	      which satisfy the predicate [f]. *)

	val takewhile :  ('a -> bool) -> 'a list -> 'a list
	  (** obsolete, as {!take_while} *)

	val drop_while : ('a -> bool) -> 'a list -> 'a list
	  (** [dropwhile f xs] returns the list [xs] with the first
	      elements satisfying the predicate [f] dropped. *)

	val dropwhile : ('a -> bool) -> 'a list -> 'a list
	  (** obsolete, as {!drop_while} *)
	(** {6 Enum functions} *)

	(** Enumerations are important in ExtLib, they are a good way to work with
	 abstract enumeration of elements, regardless if they are located in a list,
	 an array, or a file. *)

	val enum : 'a list -> 'a Enum.t
	(** Returns an enumeration of the elements of a list. *)

	val of_enum : 'a Enum.t -> 'a list
	(** Build a list from an enumeration. *)

	(** {6 Modified functions} *)

	(** Some minor modifications have been made to the specification of some
	 functions, especially concerning exceptions raised. *)

	val hd : 'a list -> 'a
	(** Returns the first element of the list or raise [Empty_list] if the
	 list is empty. *)

	val tl : 'a list -> 'a list
	(** Returns the list without its first elements or raise [Empty_list] if
	 the list is empty. *)

	val nth : 'a list -> int -> 'a
	(** Obsolete. As [at]. *)

	val sort : ?cmp:('a -> 'a -> int) -> 'a list -> 'a list
	(** Sort the list using optional comparator (by default [compare]). *)

	val map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
	  (** [List.map2 f [a1; ...; an] [b1; ...; bn]] is
	      [[f a1 b1; ...; f an bn]].
	      Raise [Different_list_size] if the two lists have
	      different lengths.  Tail-recursive. *)


	val iter2 : ('a -> 'b -> unit) -> 'a list -> 'b list -> unit
	  (** [List.iter2 f [a1; ...; an] [b1; ...; bn]] calls in turn
	      [f a1 b1; ...; f an bn].
	      Raise [Different_list_size] if the two lists have
	      different lengths. *)


	val fold_left2 : ('a -> 'b -> 'c -> 'a) -> 'a -> 'b list -> 'c list -> 'a
	  (** [List.fold_left2 f a [b1; ...; bn] [c1; ...; cn]] is
	      [f (... (f (f a b1 c1) b2 c2) ...) bn cn].
	      Raise [Different_list_size] if the two lists have
	      different lengths. *)

	val fold_right2 : ('a -> 'b -> 'c -> 'c) -> 'a list -> 'b list -> 'c -> 'c
	  (** [List.fold_right2 f [a1; ...; an] [b1; ...; bn] c] is
	      [f a1 b1 (f a2 b2 (... (f an bn c) ...))].
	      Raise [Different_list_size] if the two lists have
	      different lengths.  Tail-recursive. *)

	val for_all2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
	  (** Same as {!List.for_all}, but for a two-argument predicate.
	      Raise [Different_list_size] if the two lists have
	      different lengths. *)

	val exists2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
	  (** Same as {!List.exists}, but for a two-argument predicate.
	      Raise [Different_list_size] if the two lists have
	      different lengths. *)

	val combine : 'a list -> 'b list -> ('a * 'b) list
	  (** Transform a pair of lists into a list of pairs:
	      [combine [a1; ...; an] [b1; ...; bn]] is
	      [[(a1,b1); ...; (an,bn)]].
	      Raise [Different_list_size] if the two lists
	      have different lengths.  Tail-recursive. *)



	val append : 'a list -> 'a list -> 'a list
	  (** Catenate two lists.  Same function as the infix operator [@].
	      Tail-recursive (length of the first argument).*)


	val concat : 'a list list -> 'a list
	  (** Concatenate a list of lists.  The elements of the argument are all
	      concatenated together (in the same order) to give the result.
	      Tail-recursive
	      (length of the argument + length of the longest sub-list). *)


	val flatten : 'a list list -> 'a list
	  (** Same as [concat]. *)

	val fold_right : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
	  (** [List.fold_right f [a1; ...; an] b] is
	      [f a1 (f a2 (... (f an b) ...))].  Tail-recursive. *)

	val remove_assoc : 'a -> ('a * 'b) list -> ('a * 'b) list
	  (** [remove_assoc a l] returns the list of
	      pairs [l] without the first pair with key [a], if any.
	      Tail-recursive. *)

	val remove_assq : 'a -> ('a * 'b) list -> ('a * 'b) list
	  (** Same as {!List.remove_assoc}, but uses physical equality instead
	      of structural equality to compare keys.  Tail-recursive. *)

	  
	val split : ('a * 'b) list -> 'a list * 'b list
	  (** Transform a list of pairs into a pair of lists:
	      [split [(a1,b1); ...; (an,bn)]] is [([a1; ...; an], [b1; ...; bn])].
	      Tail-recursive.
	  *)


	val filter : ('a -> bool) -> 'a list -> 'a list
	  (** [filter p l] returns all the elements of the list [l]
	      that satisfy the predicate [p].  The order of the elements
	      in the input list is preserved.  *)

	val find_all : ('a -> bool) -> 'a list -> 'a list
	  (** [find_all] is another name for {!List.filter}. *)

	val partition : ('a -> bool) -> 'a list -> 'a list * 'a list
	  (** [partition p l] returns a pair of lists [(l1, l2)], where
	      [l1] is the list of all the elements of [l] that
	      satisfy the predicate [p], and [l2] is the list of all the
	      elements of [l] that do not satisfy [p].
	      The order of the elements in the input list is preserved. *)


	(** {6 Older functions} *)

	(** These functions are already part of the Ocaml standard library
		and have not been modified. Please refer to the Ocaml Manual for
		documentation. *)

	val length : 'a list -> int
	val rev_append : 'a list -> 'a list -> 'a list
	val rev : 'a list -> 'a list
	val rev_map : ('a -> 'b) -> 'a list -> 'b list
	val iter : ('a -> unit) -> 'a list -> unit
	val fold_left : ('b -> 'a -> 'b) -> 'b -> 'a list -> 'b
	val for_all : ('a -> bool) -> 'a list -> bool
	val exists : ('a -> bool) -> 'a list -> bool
	val find : ('a -> bool) -> 'a list -> 'a

	val mem : 'a -> 'a list -> bool
	val memq : 'a -> 'a list -> bool
	val assoc : 'a -> ('a * 'b) list -> 'b
	  (** [assoc a l] returns the value associated with key [a] in the list of
	      pairs [l]. That is,
	      [assoc a [ ...; (a,b); ...] = b]
	      if [(a,b)] is the leftmost binding of [a] in list [l].
	      Raise [Not_found] if there is no value associated with [a] in the
	      list [l]. *)

	val assq : 'a -> ('a * 'b) list -> 'b
	  (** As {!assoc} but with physical equality *)

	val mem_assoc : 'a -> ('a * 'b) list -> bool
	  (** As {!assoc} but simply returns [true] if a binding exists, [false]
	      otherwise. *)

	val mem_assq : 'a -> ('a * 'b) list -> bool
	  (** As {!mem_assoc} but with physical equality.*)

	val stable_sort : ('a -> 'a -> int) -> 'a list -> 'a list
	val fast_sort : ('a -> 'a -> int) -> 'a list -> 'a list
	val merge : ('a -> 'a -> int) -> 'a list -> 'a list -> 'a list

	(** {6 Exceptions} *)

	exception Empty_list
	(** [Empty_list] is raised when an operation applied on an empty list
		is invalid : [hd] for example. *)

	exception Invalid_index of int
	(** [Invalid_index] is raised when an indexed access on a list is
		out of list bounds. *)

	exception Different_list_size of string
	(** [Different_list_size] is raised when applying functions such as
		[iter2] on two lists having different size. *)


	module ExceptionLess : sig
	  (** Exceptionless counterparts for error-raising operations*)

	  val rfind : ('a -> bool) -> 'a list -> 'a option
	    (** [rfind p l] returns [Some x] where [x] is the last element of [l] such 
		that [p x] returns [true] or [None] if such element as not been found. *)

	  val findi : (int -> 'a -> bool) -> 'a list -> (int * 'a) option
	    (** [findi p e l] returns [Some (i, ai)] where [ai] and [i] are respectively the 
		first element of [l] and its index, such that [p i ai] is true, 
		or [None] if no	such element has been found. *)

	  val split_at : int -> 'a list -> (('a list * 'a list), [`Invalid_index of int]) Std.result
	    (** Whenever [n] is inside of [l] size bounds, [split_at n l] returns 
		[Ok(l1,l2)], where [l1] contains the first [n] elements of [l] and [l2] 
		contains the others. Otherwise, returns [`Invalid_index n] *)

	  val at : 'a list -> int -> ('a, [`Invalid_index of int]) Std.result
	    (** If [n] is inside the bounds of [l], [at l n] returns [Ok x], where
		[x] is the n-th element of the list [l]. Otherwise, returns [Error
		(`Invalid_index(n))].*)


	  val assoc : 'a -> ('a * 'b) list -> 'b option
	    (** [assoc a l] returns [Some b] where [b] is the value associated with key [a] 
		in the list of pairs [l]. That is, [assoc a [ ...; (a,b); ...] = Some b]
		if [(a,b)] is the leftmost binding of [a] in list [l].
		Return [None] if there is no value associated with [a] in the
		list [l]. *)

	  val assq : 'a -> ('a * 'b) list -> 'b option
	    (** As {!assoc} but with physical equality *)	    
	end
end

val ( @ ) : 'a list -> 'a list -> 'a list
(** the new implementation for ( @ ) operator, see [List.append]. *)
