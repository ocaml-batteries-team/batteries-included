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
(** Returns the first element of the list, or @raise Empty_list if
    the list is empty (similar to [hd]). *)

val hd : 'a list -> 'a
(** Similar to [first], but @raise Failure if the list is empty. *)

val tl : 'a list -> 'a list
(** Return the given list without its first element.
    @raise Failure if the list is empty. *)

val last : 'a list -> 'a
(** Returns the last element of the list, or @raise Empty_list if
    the list is empty. This function takes linear time. *)

val length : 'a list -> int
(** Return the length (number of elements) of the given list. *)

val at : 'a list -> int -> 'a
(** [at l n] returns the n-th element of the list [l] or
    @raise Invalid_argument is the index is outside of [l] bounds.  O(l) *)

val rev : 'a list -> 'a list
(** List reversal. *)

val append : 'a list -> 'a list -> 'a list
(** Catenate two lists.  Same function as the infix operator [@].
    Tail-recursive O(length of the first argument).*)

val rev_append : 'a list -> 'a list -> 'a list
(** [List.rev_append l1 l2] reverses [l1] and concatenates it to [l2]. *)

val concat : 'a list list -> 'a list
(** Concatenate a list of lists.  The elements of the argument are all
    concatenated together (in the same order) to give the result.
    Tail-recursive
    (length of the argument + length of the longest sub-list). *)

val flatten : 'a list list -> 'a list
(** Same as [concat]. *)

val singleton : 'a -> 'a list
(** Create a list consisting of exactly one element.

    @since 2.1
*)


(**{6 Constructors}*)


val make : int -> 'a -> 'a list
(** Similar to [String.make], [make n x] returns a
    list containing [n] elements [x]. *)

val init : int -> (int -> 'a) -> 'a list
(** Similar to [Array.init], [init n f] returns the list containing
    the results of (f 0),(f 1).... (f (n-1)).
    @raise Invalid_argument if n < 0.*)

val unfold: 'b -> ('b -> ('a * 'b) option) -> 'a list
(** [unfold init f] creates a list by repeatedly applying [f] to the
    second element of its own result, starting from the initial value
    [init]. The first element of each result is accumulated in
    a list. The list is terminated and returned as soon as [f] returns
    [None].

    Example: [List.unfold 0 (fun x -> if x = 3 then None else Some (string_of_int x, x+1))]
    will return [["0";"1";"2"]]

    @since 2.1
*)

(**{6 Iterators}*)

val iter : ('a -> unit) -> 'a list -> unit
(** [List.iter f [a1; ...; an]] applies function [f] in turn to
    [a1; ...; an]. It is equivalent to
    [begin f a1; f a2; ...; f an; () end]. *)

val iteri : (int -> 'a -> unit) -> 'a list -> unit
(** [iteri f l] will call [(f 0 a0);(f 1 a1) ... (f n an)] where
    [a0..an] are the elements of the list [l]. *)

val map : ('a -> 'b) -> 'a list -> 'b list
(** [map f [a0; a1; ...; an]] applies function [f] to [a0, a1, ..., an],
    and builds the list [[f a0; f a1; ...; f an]]
    with the results returned by [f].  Tail-recursive. *)
(* why that formulation emphasizing "applies function f to
    ..." ? Because map is specifically designed to respect
    a left-to-right order of evaluation *)

val rev_map : ('a -> 'b) -> 'a list -> 'b list
(** [List.rev_map f l] gives the same result as
    {!List.rev}[ (]{!List.map}[ f l)]. *)

val mapi : (int -> 'a -> 'b) -> 'a list -> 'b list
(** [mapi f l] will build the list containing
    [(f 0 a0);(f 1 a1) ... (f n an)] where [a0..an] are the elements of
    the list [l]. *)

val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
(** [List.fold_left f a [b1; ...; bn]] is
    [f (... (f (f a b1) b2) ...) bn]. *)

val fold_right : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
(** [List.fold_right f [a0; a1; ...; an] b] is
    [f a0 (f a1 (... (f an b) ...))].  Tail-recursive. *)

val reduce : ('a -> 'a -> 'a) -> 'a list -> 'a
(** [List.reduce f h::t] is [fold_left f h t].

    @raise Invalid_argument on empty list. *)

val max : 'a list -> 'a
(** [max l] returns the largest value in [l] as judged by
    [Pervasives.compare] *)

val min : 'a list -> 'a
(** [min l] returns the smallest value in [l] as judged by
    [Pervasives.compare] *)

val sum : int list -> int
(** [sum l] returns the sum of the integers of [l] 
    @raise Invalid_argument on the empty list.
 *)

val fsum : float list -> float
(** [fsum l] returns the sum of the floats of [l] 
    @raise Invalid_argument on the empty list.
 *)

val min_max : ?cmp:('a -> 'a -> int) -> 'a list -> 'a * 'a
(** [min_max l] returns the pair (smallest, largest) from [l] as judged by
    [Pervasives.compare] (by default). You can provide another
    comparison function via the optional [cmp] parameter.
    @raise Invalid_argument on an empty list.

    @since 2.1
 *)

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

val rev_map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
(** [List.rev_map2 f l1 l2] gives the same result as
    {!List.rev}[ (]{!List.map2}[ f l1 l2)], but is tail-recursive and
    more efficient. *)

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

val mem : 'a -> 'a list -> bool
(** [mem a l] is true if and only if [a] is equal
    to an element of [l]. *)

val memq : 'a -> 'a list -> bool
(** Same as {!List.mem}, but uses physical equality instead of structural
    equality to compare list elements. *)


(**{7 Unary predicate, One list}*)

val for_all : ('a -> bool) -> 'a list -> bool
(** [for_all p [a1; ...; an]] checks if all elements of the list
    satisfy the predicate [p]. That is, it returns
    [(p a1) && (p a2) && ... && (p an)]. *)

val exists : ('a -> bool) -> 'a list -> bool
(** [exists p [a1; ...; an]] checks if at least one element of
    the list satisfies the predicate [p]. That is, it returns
    [(p a1) || (p a2) || ... || (p an)]. *)

(**{7 Binary predicate, Two lists}*)

val for_all2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
(** Same as {!List.for_all}, but for a two-argument predicate.

    @raise Invalid_argument if the two lists have
    different lengths. *)

val exists2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
(** Same as {!List.exists}, but for a two-argument predicate.

    @raise Invalid_argument if the two lists have
    different lengths. *)


(**{6 List searching}*)

val find : ('a -> bool) -> 'a list -> 'a
(** [find p l] returns the first element of the list [l]
    that satisfies the predicate [p].
    @raise Not_found if there is no value that satisfies [p] in the
    list [l]. *)

val find_exn : ('a -> bool) -> exn -> 'a list -> 'a
(** [find_exn p e l] returns the first element of [l] such as [p x]
    returns [true] or raises [e] if such an element has not been found. *)

val findi : (int -> 'a -> bool) -> 'a list -> (int * 'a)
(** [findi p e l] returns the first element [ai] of [l] along with its
    index [i] such that [p i ai] is true, or @raise Not_found if no
    such element has been found. *)

val find_map : ('a -> 'b option) -> 'a list -> 'b
(** [find_map pred list] finds the first element of [list] for which
    [pred element] returns [Some r].  It returns [r] immediately
    once found or @raise Not_found if no element matches the
    predicate.  See also {!filter_map}. *)


val rfind : ('a -> bool) -> 'a list -> 'a
(** [rfind p l] returns the last element [x] of [l] such as [p x] returns
    [true] or @raise Not_found if such element as not been found. *)

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

val unique : ?eq:('a -> 'a -> bool) -> 'a list -> 'a list
(** [unique cmp l] returns the list [l] without any duplicate element.
    The default comparator ( = ) is used if no comparison function
    specified.

    Implementation Note: The current implementation removes any
    elements where the tail of the list contains an equal element,
    thus it keeps the *last* copy of each equal element.

    This function takes O(n^2) time.
    @see 'sort_unique' to save time in cases when reordering the list is
    acceptable
    @since 2.0
*)

val unique_cmp : ?cmp:('a -> 'a -> int) -> 'a list -> 'a list
(** As [unique], except comparator parameter returns an int.  Default
    comparator is [Pervasives.compare].  This function takes O(n log n)
    time.

    Implementation Note: The current implementation removes subsequent
    elements that compare as equal to earlier elements in the list,
    thus it keeps the *first* copy of each equal element.

    @since 1.3.0 *)

val unique_hash : ?hash:('a -> int) -> ?eq:('a -> 'a -> bool) -> 'a list -> 'a list
(** As [unique], except uses a hash table to cut down the expected
    runtime to linear, assuming a good hash function.  [?hash]
    defaults to [Hashtbl.hash] and [?eq] defaults to [(=)].

    Implementation Note: The current implementation removes subsequent
    elements that hash and compare as equal to earlier elements in the
    list, thus it keeps the *first* copy of each equal element.

    @since 2.0.0
*)

(**{6 Association lists}*)

val assoc : 'a -> ('a * 'b) list -> 'b
(** [assoc a l] returns the value associated with key [a] in the list of
    pairs [l]. That is,
    [assoc a [ ...; (a,b); ...] = b]
    if [(a,b)] is the leftmost binding of [a] in list [l].
    @raise Not_found if there is no value associated with [a] in the
    list [l]. *)

val assoc_inv : 'b -> ('a * 'b) list -> 'a
(** [assoc_inv b l] returns the key associated with value [b] in the list of
    pairs [l]. That is, [assoc b [ ...; (a,b); ...] = a]
    if [(a,b)] is the leftmost binding of [a] in list [l].
    @raise Not_found if there is no key associated with [b] in the
    list [l]. *)

val remove_assoc : 'a -> ('a * 'b) list -> ('a * 'b) list
(** [remove_assoc a l] returns the list of
    pairs [l] without the first pair with key [a], if any.
    Tail-recursive. *)

val mem_assoc : 'a -> ('a * 'b) list -> bool
(** Same as {!List.assoc}, but simply return true if a binding exists,
    and false if no bindings exist for the given key. *)

val assq : 'a -> ('a * 'b) list -> 'b
(** Same as {!List.assoc}, but uses physical equality instead of structural
    equality to compare keys. *)

val assq_inv : 'b -> ('a * 'b) list -> 'a
(** Same as {!List.assoc_inv}, but uses physical equality instead of structural
    equality to compare keys. *)

val remove_assq : 'a -> ('a * 'b) list -> ('a * 'b) list
(** Same as {!List.remove_assoc}, but uses physical equality instead
    of structural equality to compare keys.  Tail-recursive. *)

val mem_assq : 'a -> ('a * 'b) list -> bool
(** Same as {!List.mem_assoc}, but uses physical equality instead of
    structural equality to compare keys. *)

val modify : 'a -> ('b -> 'b) -> ('a * 'b) list -> ('a * 'b) list
(** [modify a f l] returns the same list as [l] but with value associated
    to key [a] replaced with [f a].

    @raise Not_found if no value is associated with [a] in [l]
    @since 2.1 *)

val modify_def : 'b -> 'a -> ('b -> 'b) -> ('a * 'b) list -> ('a * 'b) list
(** [modify_def dfl a f l] performs as [modify a f l] except that it
    add an association from [a] to [f dfl] instead of raising [Not_found].

    @since 2.1 *)

val modify_opt : 'a -> ('b option -> 'b option) -> ('a * 'b) list -> ('a * 'b) list
(** [modify_opt a f l] allows to modify the binding for [a] in [l]
    or absence thereof.

    @since 2.1 *)

(** {6 List transformations}*)

val split_at : int -> 'a list -> 'a list * 'a list
(** [split_at n l] returns two lists [l1] and [l2], [l1] containing the
    first [n] elements of [l] and [l2] the others. @raise Invalid_argument if
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
(** [take_while p xs] returns the (possibly empty) longest prefix of
    elements of [xs] that satisfy the predicate [p].*)

val drop_while : ('a -> bool) -> 'a list -> 'a list
(** [drop_while p xs] returns the suffix remaining after
    [take_while p xs]. *)
    
val span : ('a -> bool) -> 'a list -> 'a list * 'a list
(** [span], applied to a predicate [p] and a list [xs], returns a 
    tuple where first element is longest prefix (possibly empty) of xs 
    of elements that satisfy p and second element is the remainder of
    the list. This is equivalent to [(take_while p xs, drop_while p xs)],
    but is done in one pass. 

    @since 2.1
*)

val nsplit : ('a -> bool) -> 'a list -> 'a list list
(** [nsplit], applied to a predicate [p] and a list [xs], returns a
    list of lists. [xs] is split when [p x] is true and [x] is excluded
    from the result.
    
    If elements that satisfy [p] are consecutive, or at the beginning
    or end of the input list, the output list will contain empty lists
    marking their position. For example,
    [split (fun n -> n<0) [-1;2;-2;-3;4;-5]] is [[[];[2];[];[4];[]]].
    This is consistent with the behavior of [String.nsplit], where
    [String.nsplit ";" "1;2;;3;" = ["1";"2";"";"3";""]].
    
    Note that for any [xss : 'a list list] and [sep : 'a], we always have
    that [flatten (interleave [sep] (nsplit ((=) sep) xss))] is [xss].

    @since 2.1
*)

val group_consecutive : ('a -> 'a -> bool) -> 'a list -> 'a list list
(** The [group_consecutive] function takes a list and returns a list of lists such 
    that the concatenation of the result is equal to the argument. Moreover, each 
    sublist in the result contains only equal elements. For example, 
    [group_consecutive (=) [3;3;4;3;3] =  [[3;3];[4];[3;3]]].

    {b Note:} In the next major version, this function is intended to replace the 
    current [group], which also sorts its input before grouping, and which will
    therefore be renamed into something more pertinent, such as [classify], 
    [regroup], or [group_sort].

    @since 2.1
*)

val interleave : ?first:'a -> ?last:'a -> 'a -> 'a list -> 'a list
(** [interleave ~first ~last sep [a0;a1;a2;...;an]] returns
    [first; a0; sep; a1; sep; a2; sep; ...; sep; an; last] *)


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
    becomes the last element of the list, the second element of the
    enumeration
    becomes the second-to-last element of the list... *)



(** {6 List of pairs}*)

val split : ('a * 'b) list -> 'a list * 'b list
(** Transform a list of pairs into a pair of lists:
    [split [(a0,b0); (a1,b1); ...; (an,bn)]] is [([a0; a1; ...; an], [b0;
    b1; ...; bn])].
    Tail-recursive.
*)

val combine : 'a list -> 'b list -> ('a * 'b) list
(** Transform a pair of lists into a list of pairs:
    [combine [a0; a1; ...; an] [b0; b1; ...; bn]] is
    [[(a0,b0); (a1,b1); ...; (an,bn)]].
    @raise Different_list_size if the two lists
    have different lengths.  Tail-recursive. *)


(** {6 Sorting}*)


val sort : ('a -> 'a -> int) -> 'a list -> 'a list
(** Sort a list in increasing order according to a comparison
    function.  The comparison function must return 0 if its arguments
    compare as equal, a positive integer if the first is greater,
    and a negative integer if the first is smaller (see Array.sort for
    a complete specification).  For example,
    {!Pervasives.compare} is a suitable comparison function.
    The resulting list is sorted in increasing order.
    [List.sort] is guaranteed to run in constant heap space
    (in addition to the size of the result list) and logarithmic
    stack space.

    The current implementation uses Merge Sort. It runs in constant
    heap space and logarithmic stack space.
*)

val stable_sort : ('a -> 'a -> int) -> 'a list -> 'a list
(** Same as {!List.sort}, but the sorting algorithm is guaranteed to
    be stable (i.e. elements that compare equal are kept in their
    original order) .

    The current implementation uses Merge Sort. It runs in constant
    heap space and logarithmic stack space.
*)

val fast_sort : ('a -> 'a -> int) -> 'a list -> 'a list
(** Same as {!List.sort} or {!List.stable_sort}, whichever is faster
    on typical input. *)

val merge : ('a -> 'a -> int) -> 'a list -> 'a list -> 'a list
(** Merge two lists:
    Assuming that [l1] and [l2] are sorted according to the
    comparison function [cmp], [merge cmp l1 l2] will return a
    sorted list containting all the elements of [l1] and [l2].
    If several elements compare equal, the elements of [l1] will be
    before the elements of [l2].
    Not tail-recursive (sum of the lengths of the arguments).
*)

val sort_unique : ('a -> 'a -> int) -> 'a list -> 'a list
(** [sort_unique cmp l] returns the list [l] sorted and without any duplicate
    element. [cmp] is a usual comparison function providing total order.

    This function takes O(n log n) time.
*)



(** {6 Utilities}*)


val group : ('a -> 'a -> int) -> 'a list -> 'a list list
(** [group cmp l] returns list of groups and each group consists of
    elements judged equal by comparison function [cmp]. Groups in the resulting
    list appear in order given by [cmp]. All groups are always nonempty. [group]
    returns [[]] only if [l] is empty.

    For example [group cmp [f;c;b;e;d;a]] can give [[[a;b];[c];[d;e;f]]] if
    following conditions are met:
    [cmp a b = 0], [cmp b c = -1], [cmp c d = -1], [cmp d e = 0],...
    
    See the note on [group_consecutive].
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

val transpose : 'a list list -> 'a list list
(** Transposes a list of lists, turning rows of the input into columns
    of the output and vice versa.

    @since 2.0.0
*)

(** {6 Boilerplate code}*)

(** {7 Printing}*)

val print : ?first:string -> ?last:string -> ?sep:string -> ('a
      BatInnerIO.output -> 'b -> unit) ->  'a BatInnerIO.output -> 'b list -> unit
(**Print the contents of a list*)

open BatOrd
val eq : 'a eq -> 'a list eq
val ord : 'a ord -> 'a list ord
val compare : 'a comp -> 'a list comp

(** Comparison and equality for lists based on element comparison and
    equality *)

module Eq (T : Eq) : Eq with type t = T.t list
module Ord (T : Ord) : Ord with type t = T.t list
module Comp (T : Comp) : Comp with type t = T.t list



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
   behaving slightly differently but having the same name. This is by
   design:
   the functions meant to override the corresponding functions of {!List}.

*)

(** Exceptionless counterparts for error-raising operations*)
module Exceptionless : sig

  val find : ('a -> bool) -> 'a list -> 'a option
  (** [find p l] returns [Some x] where [x] is the first element
      of [l] such as [p x] returns [true] or [None] if such an
      element has not been found.*)

  val rfind : ('a -> bool) -> 'a list -> 'a option
  (** [rfind p l] returns [Some x] where [x] is the last element of [l]
      such
      that [p x] returns [true] or [None] if such element as not been found. *)

  val findi : (int -> 'a -> bool) -> 'a list -> (int * 'a) option
  (** [findi p e l] returns [Some (i, ai)] where [ai] and [i] are
      respectively the
      first element of [l] and its index, such that [p i ai] is true,
      or [None] if no  such element has been found. *)

  val split_at : int -> 'a list -> [`Ok of ('a list * 'a list) |
                                    `Invalid_argument of string]
  (** Whenever [n] is inside of [l] size bounds, [split_at n l] returns
      [Ok(l1,l2)], where [l1] contains the first [n] elements of [l] and [l2]
      contains the others. Otherwise, returns [`Invalid_argument n] *)

  val at : 'a list -> int -> [`Ok of 'a | `Invalid_argument of string]
  (** If [n] is inside the bounds of [l], [at l n] returns [Ok x], where
      [x] is the n-th element of the list [l]. Otherwise, returns [Error
      (`Invalid_argument(n))].*)


  val assoc : 'a -> ('a * 'b) list -> 'b option
  (** [assoc a l] returns [Some b] where [b] is the value associated with
      key [b]
      in the list of pairs [l]. That is, [assoc a [ ...; (a,b); ...] = Some b]
      if [(a,b)] is the leftmost binding of [a] in list [l].
      Return [None] if there is no value associated with [a] in the
      list [l]. *)

  val assoc_inv : 'b -> ('a * 'b) list -> 'a option
  (** [assoc_inv b l] returns [Some a] where [a] is the key associated with
      value [b]
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
  (** [hd l] returns [Some x] such that [x] is the first element of the given
      list [l].
      Returns [None] if list [l] is empty. *)

  val tl : ('a list -> 'a list option)
  (** [tl l] returns [Some x] such that [x] is the given list [l] without its
      first element.
      Returns [None] if list [l] is empty *)

  val last : 'a list -> 'a option
    (** [last l] returns either [Some x] where [x] is the last element of the list, or [None] if
        the list is empty. This function takes linear time. *)
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
  val iteri : f:(int -> 'a -> unit) -> 'a list -> unit
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
  val merge : ?cmp:('a -> 'a -> int) -> 'a list -> 'a list -> 'a list
  module LExceptionless : sig
    val find : f:('a -> bool) -> 'a list -> 'a option
    val rfind : f:('a -> bool) -> 'a list -> 'a option
    val findi : f:(int -> 'a -> bool) -> 'a list -> (int * 'a) option
    val split_at : int -> 'a list ->
      [`Ok of ('a list * 'a list) |`Invalid_argument of string]
    val at : 'a list -> int -> [`Ok of 'a | `Invalid_argument of string]
    val assoc : 'a -> ('a * 'b) list -> 'b option
    val assoc_inv : 'b -> ('a * 'b) list -> 'a option
    val assq : 'a -> ('a * 'b) list -> 'b option
  end
end


val ( @ ) : 'a list -> 'a list -> 'a list
  (** Tail recursive [List.append]. *)
