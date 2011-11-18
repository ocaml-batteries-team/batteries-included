(*
 * BatList - additional and modified functions for lists.
 * Copyright (C) 2003 Brian Hurt
 * Copyright (C) 2003 Nicolas Cannasse
 * Copyright (C) 2008 Red Hat Inc.
 * Copyright (C) 2009 David Teller, LIFO, Universite d'Orleans
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
    This BatList module can be used to extend the List module or
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


    This module extends Stdlib's
    {{:http://caml.inria.fr/pub/docs/manual-ocaml/libref/List.html}List}
    module, go there for documentation on the rest of the functions
    and types.
    
*)

(** List operations.
    
    @documents List 
    
    @author Xavier Leroy (base module)
    @author Brian Hurt
    @author Nicolas Cannasse
    @author Richard W.M. Jones
    @author David Teller
*)
      type 'a t = 'a list
	  (**The type of lists*)

      include BatEnum.Enumerable with type 'a enumerable = 'a t
      include BatInterfaces.Mappable with type 'a mappable = 'a t

      (**{6 Base operations}*)

	val is_empty : 'a list -> bool
	  (** [is_empty e] returns true if [e] does not contains any element. *)

	val cons : 'a -> 'a list -> 'a list
	  (** [cons h t] returns the list starting with [h] and continuing as [t] *)

	val first : 'a list -> 'a
	  (** Returns the first element of the list, or raise [Empty_list] if
	      the list is empty (similar to [hd]). *)

	val last : 'a list -> 'a
	  (** Returns the last element of the list, or raise [Empty_list] if
	      the list is empty. This function takes linear time. *)

	val at : 'a list -> int -> 'a
	  (** [at l n] returns the n-th element of the list [l] or raise
	      [Invalid_index] is the index is outside of [l] bounds. *)

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

	(**{6 Constructors}*)
	  
	val make : int -> 'a -> 'a list
	  (** Similar to [String.make], [make n x] returns a
	      list containing [n] elements [x]. *)

	val init : int -> (int -> 'a) -> 'a list
	  (** Similar to [Array.init], [init n f] returns the list containing
	      the results of (f 0),(f 1).... (f (n-1)).
	      Raise [Invalid_arg "BatList.init"] if n < 0.*)


	(**{6 Iterators}*)

	val iteri : (int -> 'a -> 'b) -> 'a list -> unit
	(** [iteri f l] will call [(f 0 a0);(f 1 a1) ... (f n an)] where
	 [a0..an] are the elements of the list [l]. *)

	val map : ('a -> 'b) -> 'a list -> 'b list
	  (** [map f [a0; a1; ...; an]] applies function [f] to [a0, a1, ..., an],
	      and builds the list [[f a0; f a1; ...; f an]]
	      with the results returned by [f].  Tail-recursive. *)
        (* why that formulation emphasizing "applies function f to
           ..." ? Because map is specifically designed to respect
           a left-to-right order of evaluation *)

	val mapi : (int -> 'a -> 'b) -> 'a list -> 'b list
	(** [mapi f l] will build the list containing
	 [(f 0 a0);(f 1 a1) ... (f n an)] where [a0..an] are the elements of
	 the list [l]. *)

	val fold_right : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
	  (** [List.fold_right f [a0; a1; ...; an] b] is
	      [f a0 (f a1 (... (f an b) ...))].  Tail-recursive. *)

	val reduce : ('a -> 'a -> 'a) -> 'a list -> 'a
	  (** [List.reduce f h::t] is [fold_left f h t].  

	      @raise Empty_list on empty lists. *)

	val max : 'a list -> 'a
	  (** [max l] returns the largest value in [l] as judged by
	      [Pervasives.compare] *)

	val min : 'a list -> 'a
	  (** [min l] returns the smallest value in [l] as judged by
	      [Pervasives.compare] *)

	val sum : int list -> int
	  (** [sum l] returns the sum of the elements of [l] *)

	val fsum : float list -> float
	  (** [sum l] returns the sum of the elements of [l] *)

	(** {6 Iterators on two lists} *)

	val iter2 : ('a -> 'b -> unit) -> 'a list -> 'b list -> unit
	  (** [List.iter2 f [a0; a1; ...; an] [b0; b1; ...; bn]] calls in turn
	      [f a0 b0; f a1 b1; ...; f an bn].
	      @raise Different_list_size if the two lists have
	      different lengths. *)

	val map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
	  (** [List.map2 f [a0; a1; ...; an] [b0; b1; ...; bn]] is
	      [[f a0 b0; f a1 b1; ...; f an bn]].
	      @raise Different_list_size if the two lists have
	      different lengths.  Tail-recursive. *)

	val fold_left2 : ('a -> 'b -> 'c -> 'a) -> 'a -> 'b list -> 'c list -> 'a
	  (** [List.fold_left2 f a [b0; b1; ...; bn] [c0; c1; ...; cn]] is
	      [f (... (f (f a b0 c0) b1 c1) ...) bn cn].
	      @raise Different_list_size if the two lists have
	      different lengths. *)

	val fold_right2 : ('a -> 'b -> 'c -> 'c) -> 'a list -> 'b list -> 'c -> 'c
	  (** [List.fold_right2 f [a0; a1; ...; an] [b0; b1; ...; bn] c] is
	      [f a0 b0 (f a1 b1 (... (f an bn c) ...))].
	      
	      @raise Different_list_size if the two lists have
	      different lengths.  Tail-recursive. *)

	  (**{6 List scanning}*)

	val for_all2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
	  (** Same as {!List.for_all}, but for a two-argument predicate.
	      
	      @raise Invalid_argument if the two lists have
	      different lengths. *)

	val exists2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
	  (** Same as {!List.exists}, but for a two-argument predicate.
	      
	      @raise Invalid_argument if the two lists have
	      different lengths. *)


	(**{6 List searching}*)


	val find_exn : ('a -> bool) -> exn -> 'a list -> 'a
	(** [find_exn p e l] returns the first element of [l] such as [p x]
	 returns [true] or raises [e] if such an element has not been found. *)

	val findi : (int -> 'a -> bool) -> 'a list -> (int * 'a)
	(** [findi p e l] returns the first element [ai] of [l] along with its
	 index [i] such that [p i ai] is true, or raises [Not_found] if no
	 such element has been found. *)

	val find_map : ('a -> 'b option) -> 'a list -> 'b
        (** [find_map pred list] finds the first element of [list] for which
            [pred element] returns [Some r].  It returns [r] immediately
            once found or raises [Not_found] if no element matches the
            predicate.  See also {!filter_map}. *)


	val rfind : ('a -> bool) -> 'a list -> 'a
	(** [rfind p l] returns the last element [x] of [l] such as [p x] returns
	 [true] or raises [Not_found] if such element as not been found. *)

	val filter : ('a -> bool) -> 'a list -> 'a list
	  (** [filter p l] returns all the elements of the list [l]
	      that satisfy the predicate [p].  The order of the elements
	      in the input list is preserved.  *)

	val filter_map : ('a -> 'b option) -> 'a list -> 'b list
	(** [filter_map f l] calls [(f a0) (f a1).... (f an)] where [a0,a1..an] are
	 the elements of [l]. It returns the list of elements [bi] such as
	 [f ai = Some bi] (when [f] returns [None], the corresponding element of
	 [l] is discarded). *)

	val find_all : ('a -> bool) -> 'a list -> 'a list
	  (** [find_all] is another name for {!List.filter}. *)

	val partition : ('a -> bool) -> 'a list -> 'a list * 'a list
	  (** [partition p l] returns a pair of lists [(l1, l2)], where
	      [l1] is the list of all the elements of [l] that
	      satisfy the predicate [p], and [l2] is the list of all the
	      elements of [l] that do not satisfy [p].
	      The order of the elements in the input list is preserved. *)

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
	 Default comparator ( = ) is used if no comparison function specified. 
	 
	 This function takes O(n²) time.
	 @see 'sort_unique' to save time in cases when reordering the list is acceptable
	 *)

	val unique_eq : ?eq:('a -> 'a -> bool) -> 'a list -> 'a list
	(** As [unique] except comparator label is ~eq.  
	    @since 1.3.0
	 *)

	val unique_cmp : ?cmp:('a -> 'a -> int) -> 'a list -> 'a list
	(** As [unique], except comparator parameter returns an int
	    @since 1.3.0 *)

	(**{6 Association lists}*)

	val assoc_inv : 'b -> ('a * 'b) list -> 'a
	  (** [assoc_inv b l] returns the key associated with value [b] in the list of
	      pairs [l]. That is,
	      [assoc b [ ...; (a,b); ...] = a]
	      if [(a,b)] is the leftmost binding of [a] in list [l].
	      Raise [Not_found] if there is no key associated with [b] in the
	      list [l]. *)

	val remove_assoc : 'a -> ('a * 'b) list -> ('a * 'b) list
	  (** [remove_assoc a l] returns the list of
	      pairs [l] without the first pair with key [a], if any.
	      Tail-recursive. *)

	val remove_assq : 'a -> ('a * 'b) list -> ('a * 'b) list
	  (** Same as {!List.remove_assoc}, but uses physical equality instead
	      of structural equality to compare keys.  Tail-recursive. *)


	(** {6 List transformations}*)

	val split_at : int -> 'a list -> 'a list * 'a list
	(** [split_at n l] returns two lists [l1] and [l2], [l1] containing the
	 first [n] elements of [l] and [l2] the others. Raise [Invalid_index] if
	 [n] is outside of [l] size bounds. *)

	val split_nth : int -> 'a list -> 'a list * 'a list
	(** Obsolete. As [split_at]. *)

	val remove : 'a list -> 'a -> 'a list
	(** [remove l x] returns the list [l] without the first element [x] found
	 or returns  [l] if no element is equal to [x]. Elements are compared
	 using ( = ). *)

	val remove_if : ('a -> bool) -> 'a list -> 'a list
	(** [remove_if cmp l] is similar to [remove], but with [cmp] used
	 instead of ( = ). *)

	val remove_all : 'a list -> 'a -> 'a list
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
	  
	  
	val drop_while : ('a -> bool) -> 'a list -> 'a list
	  (** [dropwhile f xs] returns the list [xs] with the first
	      elements satisfying the predicate [f] dropped. *)


	val interleave : ?first:'a -> ?last:'a -> 'a -> 'a list -> 'a list
	  (** [interleave ~first ~last sep [a0;a1;a2;...;an]] returns
	      [first; a0; sep; a1; sep; a2; sep; ...; sep; an] *)

	(** {6 BatEnum functions} 
	    
	    Abstraction layer.*)

	val enum : 'a list -> 'a BatEnum.t
	(** Returns an enumeration of the elements of a list. This enumeration may
	    be used to visit elements of the list in forward order (i.e. from the
	    first element to the last one)*)

	val of_enum : 'a BatEnum.t -> 'a list
	(** Build a list from an enumeration. In the result, elements appear in the
	    same order as they did in the source enumeration. *)

	val backwards : 'a list -> 'a BatEnum.t
	(** Returns an enumeration of the elements of a list. This enumeration may
	    be used to visit elements of the list in backwards order (i.e. from the
	    last element to the first one)*)

	val of_backwards : 'a BatEnum.t -> 'a list
	(** Build a list from an enumeration. The first element of the enumeration
	    becomes the last element of the list, the second element of the enumeration
	    becomes the second-to-last element of the list... *)



	  (** {6 List of pairs}*)
	  
	val split : ('a * 'b) list -> 'a list * 'b list
	  (** Transform a list of pairs into a pair of lists:
	      [split [(a0,b0); (a1,b1); ...; (an,bn)]] is [([a0; a1; ...; an], [b0; b1; ...; bn])].
	      Tail-recursive.
	  *)

	val combine : 'a list -> 'b list -> ('a * 'b) list
	  (** Transform a pair of lists into a list of pairs:
	      [combine [a0; a1; ...; an] [b0; b1; ...; bn]] is
	      [[(a0,b0); (a1,b1); ...; (an,bn)]].
	      @raise Different_list_size if the two lists
	      have different lengths.  Tail-recursive. *)

	(** {6 Utilities}*)

	val make_compare : ('a -> 'a -> int) -> 'a list -> 'a list -> int
	  (** [make_compare c] generates the lexicographical order on lists
	      induced by [c]*)

	val sort : ?cmp:('a -> 'a -> int) -> 'a list -> 'a list
	  (** Sort the list using optional comparator (by default [compare]). *)

	  
	val sort_unique : ('a -> 'a -> int) -> 'a list -> 'a list
	(** [sort_unique cmp l] returns the list [l] sorted and without any duplicate element. [cmp] is a usual comparison function providing linear order. 
	
	  This function takes O(n log n) time.
	 *)
	  
	val group : ('a -> 'a -> int) -> 'a list -> 'a list list
	  (** [group cmp l] returns list of groups and each group consists of elements judged equal by comparison function [cmp]. Groups in the resulting list appear in order given by [cmp]. All groups are always nonempty. [group] returns [[]] only if [l] is empty.
	  
For example [group cmp [f;c;b;e;d;a]] can give [[[a;b];[c];[d;e;f]]] if following conditions are met:
	  [cmp a b = 0], [cmp b c = -1], [cmp c d = -1], [cmp d e = 0],...
	  *)  

	val cartesian_product : 'a list -> 'b list -> ('a * 'b) list
	(** Different from [List.combine], this returns every pair
	    of elements formed out of the two lists.
	    [cartesian_product [a0; a1; ...; an] [b0; b1; ...; bn] =
	    [(a0,b0);(a0,b1); ...; (a0,bn); (a1,b0); ..; (a1, bn);
	    ...; (an,bn)]].  The lists can be of unequal size. *)

	val n_cartesian_product : 'a list list -> 'a list list
	(** Given n lists, return the n-way cartesian product of
	    these lists.  Given [[a;b];[c];[d;e;f]], returns
	    [[a;c;d];[a;c;e];[a;c;f];[b;c;d];[b;c;e];[b;c;f]], all
	    ways of choosing one element from each input list. *)

	(** {6 Boilerplate code}*)

	(** {7 Printing}*)
	  
	val print : ?first:string -> ?last:string -> ?sep:string -> ('a BatInnerIO.output -> 'b -> unit) ->  'a BatInnerIO.output -> 'b list -> unit
	  (**Print the contents of a list*)

	val sprint : ?first:string -> ?last:string -> ?sep:string -> ('a BatInnerIO.output -> 'b -> unit) -> 'b list -> string
	  (** Using a string printer, print a list to a string (as sprintf vs. printf)     @deprecated use {!BatIO.to_string}. *)

        val t_printer : 'a BatValue_printer.t -> 'a t BatValue_printer.t

	(** {6 Obsolete functions} *)

	val nth : 'a list -> int -> 'a
	(** Obsolete. As [at]. *)

	val takewhile :  ('a -> bool) -> 'a list -> 'a list
	  (** obsolete, as {!take_while} *)

	val dropwhile : ('a -> bool) -> 'a list -> 'a list
	  (** obsolete, as {!drop_while} *)

	(** {6 Override modules}*)
	  
	(**
	   The following modules replace functions defined in {!List} with functions
	   behaving slightly differently but having the same name. This is by design:
	   the functions meant to override the corresponding functions of {!List}.
	   
	*)

	(** Exceptionless counterparts for error-raising operations*)
	module Exceptionless : sig

          val find : ('a -> bool) -> 'a list -> 'a option
            (** [find p l] returns [Some x] where [x] is the first element
                of [l] such as [p x] returns [true] or [None] if such an
                element has not been found.*)

	  val rfind : ('a -> bool) -> 'a list -> 'a option
	    (** [rfind p l] returns [Some x] where [x] is the last element of [l] such 
		that [p x] returns [true] or [None] if such element as not been found. *)

	  val findi : (int -> 'a -> bool) -> 'a list -> (int * 'a) option
	    (** [findi p e l] returns [Some (i, ai)] where [ai] and [i] are respectively the 
		first element of [l] and its index, such that [p i ai] is true, 
		or [None] if no	such element has been found. *)

	  val split_at : int -> 'a list -> [`Ok of ('a list * 'a list) | `Invalid_argument of string]
	    (** Whenever [n] is inside of [l] size bounds, [split_at n l] returns 
		[Ok(l1,l2)], where [l1] contains the first [n] elements of [l] and [l2] 
		contains the others. Otherwise, returns [`Invalid_index n] *)

	  val at : 'a list -> int -> [`Ok of 'a | `Invalid_argument of string]
	    (** If [n] is inside the bounds of [l], [at l n] returns [Ok x], where
		[x] is the n-th element of the list [l]. Otherwise, returns [Error
		(`Invalid_index(n))].*)


	  val assoc : 'a -> ('a * 'b) list -> 'b option
	    (** [assoc a l] returns [Some b] where [b] is the value associated with key [b]
		in the list of pairs [l]. That is, [assoc a [ ...; (a,b); ...] = Some b]
		if [(a,b)] is the leftmost binding of [a] in list [l].
		Return [None] if there is no value associated with [a] in the
		list [l]. *)

	  val assoc_inv : 'b -> ('a * 'b) list -> 'a option
	    (** [assoc_inv b l] returns [Some a] where [a] is the key associated with value [b]
		in the list of pairs [l]. That is, [assoc b [ ...; (a,b); ...] = Some a]
		if [(a,b)] is the leftmost binding of [a] in list [l].
		Return [None] if there is no key associated with [b] in the
		list [l]. *)


	  val assq : 'a -> ('a * 'b) list -> 'b option
	    (** As {!assoc} but with physical equality. *)

	  val find_map : ('a -> 'b option) -> 'a list -> 'b option
	(** [find_map f xs] returns [Some y] such that [x] is the first
	    element of the list where [f x] returns [Some y].  It returns [None]
	    if no such element exists. *)

	  val hd : ('a list -> 'a option)
	(** [hd l] returns [Some x] such that [x] is the first element of the given list [l]. 
	    Returns [None] if list [l] is empty. *)
	
	  val tl : ('a list -> 'a list option)
	(** [tl l] returns [Some x] such that [x] is the given list [l] without its first element. 
	    Returns [None] if list [l] is empty *)
 
end

	(** {6 Infix submodule regrouping all infix operators} *)
	module Infix : sig
	  val ( @ ) : 'a list -> 'a list -> 'a list
	end

	(** Operations on {!List} with labels.
	    
	    This module overrides a number of functions of {!List} by
	    functions in which some arguments require labels. These labels are
	    there to improve readability and safety and to let you change the
	    order of arguments to functions. In every case, the behavior of the
	    function is identical to that of the corresponding function of {!List}.
	 *)
	module Labels : sig
	  val init : int -> f:(int -> 'a) -> 'a list
	  val iter : f:('a -> unit) -> 'a list -> unit
	  val iteri : f:(int -> 'a -> 'b) -> 'a list -> unit
	  val map : f:('a -> 'b) -> 'a list -> 'b list
	  val mapi : f:(int -> 'a -> 'b) -> 'a list -> 'b list
	  val rev_map : f:('a -> 'b) -> 'a list -> 'b list
	  val fold_left : f:('a -> 'b -> 'a) -> init:'a -> 'b list -> 'a
	  val fold_right : f:('a -> 'b -> 'b) -> 'a list -> init:'b -> 'b
	  val iter2 : f:('a -> 'b -> unit) -> 'a list -> 'b list -> unit
	  val map2 : f:('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
	  val rev_map2 : f:('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
	  val fold_left2 : f:('a -> 'b -> 'c -> 'a) -> init:'a -> 'b list -> 'c list -> 'a
	  val fold_right2 : f:('a -> 'b -> 'c -> 'c) -> 'a list -> 'b list -> init:'c -> 'c
	  val for_all : f:('a -> bool) -> 'a list -> bool
	  val exists : f:('a -> bool) -> 'a list -> bool
	  val for_all2 : f:('a -> 'b -> bool) -> 'a list -> 'b list -> bool
	  val exists2 : f:('a -> 'b -> bool) -> 'a list -> 'b list -> bool
	  val find : f:('a -> bool) -> 'a list -> 'a
	  val find_exn : f:('a -> bool) -> exn -> 'a list -> 'a
	  val findi : f:(int -> 'a -> bool) -> 'a list -> (int * 'a)
	  val rfind : f:('a -> bool) -> 'a list -> 'a
	  val filter : f:('a -> bool) -> 'a list -> 'a list
	  val filter_map : f:('a -> 'b option) -> 'a list -> 'b list
	  val find_all : f:('a -> bool) -> 'a list -> 'a list
	  val partition : f:('a -> bool) -> 'a list -> 'a list * 'a list
	  val remove_if : f:('a -> bool) -> 'a list -> 'a list
	  val take_while : f:('a -> bool) -> 'a list -> 'a list
	  val drop_while : f:('a -> bool) -> 'a list -> 'a list
	  val stable_sort : ?cmp:('a -> 'a -> int) -> 'a list -> 'a list
	  val fast_sort : ?cmp:('a -> 'a -> int) -> 'a list -> 'a list
	  val merge : cmp:('a -> 'a -> int) -> 'a list -> 'a list -> 'a list
          module LExceptionless : sig
            val find : f:('a -> bool) -> 'a list -> 'a option
            val rfind : f:('a -> bool) -> 'a list -> 'a option
            val findi : f:(int -> 'a -> bool) -> 'a list -> (int * 'a) option
            val split_at : int -> 'a list -> [`Ok of ('a list * 'a list) | `Invalid_argument of string]
            val at : 'a list -> int -> [`Ok of 'a | `Invalid_argument of string]
            val assoc : 'a -> ('a * 'b) list -> 'b option
            val assoc_inv : 'b -> ('a * 'b) list -> 'a option
            val assq : 'a -> ('a * 'b) list -> 'b option
          end
	end


val ( @ ) : 'a list -> 'a list -> 'a list
(** the new implementation for ( @ ) operator, see [List.append]. *)
