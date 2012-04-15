

(** Unicode characters.

   This module implements Unicode characters.
*)

(* Copyright (C) 2002, 2003, 2004, 2011 Yamagata Yoriyuki. *)

(* This library is free software; you can redistribute it and/or *)
(* modify it under the terms of the GNU Lesser General Public License *)
(* as published by the Free Software Foundation; either version 2 of *)
(* the License, or (at your option) any later version. *)

(* As a special exception to the GNU Library General Public License, you *)
(* may link, statically or dynamically, a "work that uses this library" *)
(* with a publicly distributed version of this library to produce an *)
(* executable file containing portions of this library, and distribute *)
(* that executable file under terms of your choice, without any of the *)
(* additional requirements listed in clause 6 of the GNU Library General *)
(* Public License. By "a publicly distributed version of this library", *)
(* we mean either the unmodified Library as distributed by the authors, *)
(* or a modified version of this library that is distributed under the *)
(* conditions defined in clause 3 of the GNU Library General Public *)
(* License. This exception does not however invalidate any other reasons *)
(* why the executable file might be covered by the GNU Library General *)
(* Public License . *)

(* This library is distributed in the hope that it will be useful, *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU *)
(* Lesser General Public License for more details. *)

(* You should have received a copy of the GNU Lesser General Public *)
(* License along with this library; if not, write to the Free Software *)
(* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 *)
(* USA *)

(* You can contact the authour by sending email to *)
(* yori@users.sourceforge.net *)

module UChar : sig
  type t

  exception Out_of_range

(** [char_of u] returns the Latin-1 representation of [u].
    If [u] can not be represented by Latin-1, raises Out_of_range *)
  val char_of : t -> char

(** [of_char c] returns the Unicode character of the Latin-1 character [c] *)
  val of_char : char -> t

(** [code u] returns the Unicode code number of [u]. *)
  external code : t -> int = "%identity"

(** [chr n] returns the Unicode character with the code number [n].
    If n does not lay in the valid range of Unicode or designates a
    surrogate charactor, raises Out_of_range *)
  val chr : int -> t

(** Equality by code point comparison *)
  val eq : t -> t -> bool

(** [compare u1 u2] returns,
    a value > 0 if [u1] has a larger Unicode code number than [u2],
    0 if [u1] and [u2] are the same Unicode character,
    a value < 0 if [u1] has a smaller Unicode code number than [u2]. *)
  val compare : t -> t -> int

(** Aliases of [type t] *)
  type uchar = t

(** Alias of [code] *)
  val int_of : uchar -> int

(** Alias of [chr] *)
  val of_int : int -> uchar
end

(** UTF-8 encoded Unicode strings. The type is normal string. *)

(* Copyright (C) 2002, 2003, 2011 Yamagata Yoriyuki.  *)

(* This library is free software; you can redistribute it and/or *)
(* modify it under the terms of the GNU Lesser General Public License *)
(* as published by the Free Software Foundation; either version 2 of *)
(* the License, or (at your option) any later version. *)

(* As a special exception to the GNU Library General Public License, you *)
(* may link, statically or dynamically, a "work that uses this library" *)
(* with a publicly distributed version of this library to produce an *)
(* executable file containing portions of this library, and distribute *)
(* that executable file under terms of your choice, without any of the *)
(* additional requirements listed in clause 6 of the GNU Library General *)
(* Public License. By "a publicly distributed version of this library", *)
(* we mean either the unmodified Library as distributed by the authors, *)
(* or a modified version of this library that is distributed under the *)
(* conditions defined in clause 3 of the GNU Library General Public *)
(* License. This exception does not however invalidate any other reasons *)
(* why the executable file might be covered by the GNU Library General *)
(* Public License . *)

(* This library is distributed in the hope that it will be useful, *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU *)
(* Lesser General Public License for more details. *)

(* You should have received a copy of the GNU Lesser General Public *)
(* License along with this library; if not, write to the Free Software *)
(* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 *)
(* USA *)

(* You can contact the authour by sending email to *)
(* yori@users.sourceforge.net *)

module UTF8 : sig

(** UTF-8 encoded Unicode strings. The type is normal string. *)
  type t = string

  exception Malformed_code

(** [validate s]
    successes if s is valid UTF-8, otherwise raises Malformed_code.
    Other functions assume strings are valid UTF-8, so it is prudent
    to test their validity for strings from untrusted origins. *)
  val validate : t -> unit

(* All functions below assume string are valid UTF-8.  If not,
 * the result is unspecified. *)

(** [get s n] returns [n]-th Unicode character of [s].
    The call requires O(n)-time. *)
  val get : t -> int -> UChar.t

(** [init len f]
    returns a new string which contains [len] Unicode characters.
    The i-th Unicode character is initialized by [f i] *)
  val init : int -> (int -> UChar.t) -> t

(** [length s] returns the number of Unicode characters contained in s *)
  val length : t -> int

(** Positions in the string represented by the number of bytes from the head.
    The location of the first character is [0] *)
  type index = int

(** [nth s n] returns the position of the [n]-th Unicode character.
    The call requires O(n)-time *)
  val nth : t -> int -> index

(** The position of the head of the first Unicode character. *)
  val first : t -> index

(** The position of the head of the last Unicode character. *)
  val last : t -> index

(** [look s i]
    returns the Unicode character of the location [i] in the string [s]. *)
  val look : t -> index -> UChar.t

(** [out_of_range s i]
    tests whether [i] is a position inside of [s]. *)
  val out_of_range : t -> index -> bool

(** [compare_index s i1 i2] returns
    a value < 0 if [i1] is the position located before [i2],
    0 if [i1] and [i2] points the same location,
    a value > 0 if [i1] is the position located after [i2]. *)
  val compare_index : t -> index -> index -> int

(** [next s i]
    returns the position of the head of the Unicode character
    located immediately after [i].
    If [i] is inside of [s], the function always successes.
    If [i] is inside of [s] and there is no Unicode character after [i],
    the position outside [s] is returned.
    If [i] is not inside of [s], the behaviour is unspecified. *)
  val next : t -> index -> index

(** [prev s i]
    returns the position of the head of the Unicode character
    located immediately before [i].
    If [i] is inside of [s], the function always successes.
    If [i] is inside of [s] and there is no Unicode character before [i],
    the position outside [s] is returned.
    If [i] is not inside of [s], the behaviour is unspecified. *)
  val prev : t -> index -> index

(** [move s i n]
    returns [n]-th Unicode character after [i] if n >= 0,
    [n]-th Unicode character before [i] if n < 0.
    If there is no such character, the result is unspecified. *)
  val move : t -> index -> int -> index

(** [iter f s]
    applies [f] to all Unicode characters in [s].
    The order of application is same to the order
    of the Unicode characters in [s]. *)
  val iter : (UChar.t -> unit) -> t -> unit

(** Code point comparison by the lexicographic order.
    [compare s1 s2] returns
    a positive integer if [s1] > [s2],
    0 if [s1] = [s2],
    a negative integer if [s1] < [s2]. *)
  val compare : t -> t -> int

(** Buffer module for UTF-8 strings *)
  module Buf : sig
    (** Buffers for UTF-8 strings. *)
    type buf

	  (** [create n] creates the buffer with the initial size [n]-bytes. *)
    val create : int -> buf

	(* The rest of functions is similar to the ones of Buffer in stdlib. *)
	(** [contents buf] returns the contents of the buffer. *)
    val contents : buf -> t

	(** Empty the buffer,
	    but retains the internal storage which was holding the contents *)
    val clear : buf -> unit

	(** Empty the buffer and de-allocate the internal storage. *)
    val reset : buf -> unit

	(** Add one Unicode character to the buffer. *)
    val add_char : buf -> UChar.t -> unit

	(** Add the UTF-8 string to the buffer. *)
    val add_string : buf -> t -> unit

	(** [add_buffer b1 b2] adds the contents of [b2] to [b1].
	    The contents of [b2] is not changed. *)
    val add_buffer : buf -> buf -> unit
  end with type buf = Buffer.t

  (* Returns the length of the Unicode character starting with the
     given byte *)
  val length0 : int -> int

end

(* Rope: a simple implementation of ropes as described in

   Boehm, H., Atkinson, R., and Plass, M. 1995. Ropes: an alternative to
   strings. Softw. Pract. Exper. 25, 12 (Dec. 1995), 1315-1330.

   Motivated by Luca de Alfaro's extensible array implementation Vec.

   Copyright (C) 2011 Yoriyuki Yamagata <yoriyuki.y@gmail.com>
   2007 Mauricio Fernandez <mfp@acm.org>
   http://eigenclass.org

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version,
   with the following special exception:

   You may link, statically or dynamically, a "work that uses the
   Library" with a publicly distributed version of the Library to
   produce an executable file containing portions of the Library, and
   distribute that executable file under terms of your choice, without
   any of the additional requirements listed in clause 6 of the GNU
   Library General Public License. By "a publicly distributed version
   of the Library", we mean either the unmodified Library as
   distributed by the author, or a modified version of the Library that is
   distributed under the conditions defined in clause 2 of the GNU
   Library General Public License. This exception does not however
   invalidate any other reasons why the executable file might be
   covered by the GNU Library General Public License.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
   Library General Public License for more details.

   The GNU Library General Public License is available at
   http://www.gnu.org/copyleft/lgpl.html; to obtain it, you can also
   write to the Free Software Foundation, Inc., 59 Temple Place -
   Suite 330, Boston, MA 02111-1307, USA.
*)

(** Heavyweight strings ("ropes")

This module implements ropes as described in
Boehm, H., Atkinson, R., and Plass, M. 1995. Ropes: an alternative to
strings. Softw. Pract. Exper. 25, 12 (Dec. 1995), 1315-1330.

Ropes are an alternative to strings which support efficient operations:
- determining the length of a rope in constant time
- appending or prepending a small rope to an arbitrarily large one in amortized constant time
- concat, substring, insert, remove operations in amortized logarithmic time
- access to and modification of ropes in logarithmic time

{8 Functional nature and persistence}

All operations are non-destructive: the original rope is never modified. When a
new rope is returned as the result of an operation, it will share as much data
as possible with its "parent". For instance, if a rope of length [n] undergoes
[m] operations (assume [n >> m]) like set, append or prepend, the modified
rope will only require [O(m)] space in addition to that taken by the
original one.

However, Rope is an amortized data structure, and its use in a persistent setting
can easily degrade its amortized time bounds. It is thus mainly intended to be used
ephemerally. In some cases, it is possible to use Rope persistently with the same
amortized bounds by explicitly rebalancing ropes to be reused using [balance].
Special care must be taken to avoid calling [balance] too frequently; in the limit,
calling [balance] after each modification would defeat the purpose of amortization.


{8 Limitations}

The length of ropes is limited to approximately 700 Mb on 32-bit
architectures, 220 Gb on 64 bit architectures.

@author Mauricio Fernandez
*)

module Text : sig
  type t
    (** The type of the ropes. *)

  exception Out_of_bounds
    (** Raised when an operation violates the bounds of the rope. *)

  exception Invalid_rope
  (** An exception thrown when some operation required a rope
      and received an unacceptable rope.*)

  val max_length : int
    (** Maximum length of the rope (number of UTF-8 characters). *)

  (** {6 Creation and conversions} *)

  val empty : t
    (** The empty rope. *)

  val of_latin1 : string -> t
    (** Constructs a unicode rope from a latin-1 string. *)

  val of_string : string -> t
    (** [of_string s] returns a rope corresponding to the UTF-8 encoded string [s].*)

  val to_string : t -> string
    (** [to_string t] returns a UTF-8 encoded string representing [t]*)

  val of_uchar : UChar.t -> t
    (** [of_uchar c] returns a rope containing exactly character [c].*)

  val of_char : char -> t
    (** [of_char c] returns a rope containing exactly Latin-1 character [c].*)

  val make : int -> UChar.t -> t
    (** [make i c] returns a rope of length [i] consisting of [c] chars;
        it is similar to String.make *)

  val join : t -> t list -> t
    (** Same as {!concat} *)

  val explode : t -> UChar.t list
    (** [explode s] returns the list of characters in the rope [s]. *)

  val implode : UChar.t list -> t
    (** [implode cs] returns a rope resulting from concatenating
        the characters in the list [cs]. *)


  (** {6 Properties } *)

  val is_empty : t -> bool
    (** Returns whether the rope is empty or not. *)

  val length : t -> int
    (** Returns the length of the rope ([O(1)]).
        This is number of UTF-8 characters. *)

  val height : t -> int
    (** Returns the height (depth) of the rope. *)

  val balance : t -> t
    (** [balance r] returns a balanced copy of the [r] rope. Note that ropes are
        automatically rebalanced when their height exceeds a given threshold, but
        [balance] allows to invoke that operation explicity. *)

  (** {6 Operations } *)

  val append : t -> t -> t
    (** [append r u] concatenates the [r] and [u] ropes. In general, it operates
        in [O(log(min n1 n2))] amortized time.
        Small ropes are treated specially and can be appended/prepended in
        amortized [O(1)] time. *)

  val ( ^^^ ): t -> t -> t
    (** As {!append}*)

  val append_char : UChar.t -> t -> t
    (** [append_char c r] returns a new rope with the [c] character at the end
        in amortized [O(1)] time. *)

  val prepend_char : UChar.t -> t -> t
    (** [prepend_char c r] returns a new rope with the [c] character at the
        beginning in amortized [O(1)] time. *)

  val get : t -> int -> UChar.t
    (** [get r n] returns the (n+1)th character from the rope [r]; i.e.
        [get r 0] returns the first character.
        Operates in worst-case [O(log size)] time.
        Raises Out_of_bounds if a character out of bounds is requested. *)

  val set : t -> int -> UChar.t -> t
    (** [set r n c] returns a copy of rope [r]  where the (n+1)th character
        has been set to [c]. See also {!get}.
        Operates in worst-case [O(log size)] time. *)

  val sub : t -> int -> int -> t
    (** [sub r m n] returns a sub-rope of [r] containing all characters
        whose indexes range from [m] to [m + n - 1] (included).
        Raises Out_of_bounds in the same cases as sub.
        Operates in worst-case [O(log size)] time. *)

  val insert : int -> t -> t -> t
    (** [insert n r u] returns a copy of the [u] rope where [r] has been
        inserted between the characters with index [n] and [n + 1] in the
        original rope. The length of the new rope is
        [length u + length r].
        Operates in amortized [O(log(size r) + log(size u))] time. *)

  val remove : int -> int -> t -> t
    (** [remove m n r] returns the rope resulting from deleting the
        characters with indexes ranging from [m] to [m + n - 1] (included)
        from the original rope [r]. The length of the new rope is
        [length r - n].
        Operates in amortized [O(log(size r))] time. *)

  val concat : t -> t list -> t
  (** [concat sep sl] concatenates the list of ropes [sl],
     inserting the separator rope [sep] between each. *)


  (** {6 Iteration} *)

  val iter : (UChar.t -> unit) -> t -> unit
    (** [iter f r] applies [f] to all the characters in the [r] rope,
        in order. *)

  val iteri : ?base:int -> (int -> UChar.t -> unit) -> t -> unit
    (** Operates like [iter], but also passes the index of the character
        to the given function. *)

  val range_iter : (UChar.t -> unit) -> int -> int -> t -> unit
    (** [rangeiter f m n r] applies [f] to all the characters whose
        indices [k] satisfy [m] <= [k] < [m + n].
        It is thus equivalent to [iter f (sub m n r)], but does not
        create an intermediary rope. [rangeiter] operates in worst-case
        [O(n + log m)] time, which improves on the [O(n log m)] bound
        from an explicit loop using [get].
        Raises Out_of_bounds in the same cases as [sub]. *)

  val range_iteri :
    (int -> UChar.t -> unit) -> ?base:int -> int -> int -> t -> unit
    (** As [range_iter], but passes base + index of the character in the
        subrope defined by next to arguments. *)

  val fold : ('a -> UChar.t -> 'a ) -> 'a -> t -> 'a
    (** [Rope.fold f a r] computes [ f (... (f (f a r0) r1)...) rN-1 ]
        where [rn = Rope.get n r ] and [N = length r]. *)

  val init : int -> (int -> UChar.t) -> t
    (** [init l f] returns the rope of length [l] with the chars f 0 , f
        1 , f 2 ... f (l-1). *)

  val map : (UChar.t -> UChar.t) -> t -> t
   (** [map f s] returns a rope where all characters [c] in [s] have been
        replaced by [f c]. **)

  val filter_map : (UChar.t -> UChar.t option) -> t -> t
  (** [filter_map f l] calls [(f a0) (f a1).... (f an)] where [a0..an] are
      the characters of [l]. It returns the list of elements [bi] such as
      [f ai = Some bi] (when [f] returns [None], the corresponding element of
      [l] is discarded). *)

  val filter : (UChar.t -> bool) -> t -> t
    (** [filter f s] returns a copy of rope [s] in which only
        characters [c] such that [f c = true] remain.*)



  (** {6 Finding}*)

  val index : t -> UChar.t -> int
  (** [Rope.index s c] returns the position of the leftmost
      occurrence of character [c] in rope [s].
      Raise [Not_found] if [c] does not occur in [s]. *)

  val index_from : t -> int -> UChar.t -> int
    (** Same as {!Rope.index}, but start searching at the character
        position given as second argument.  [Rope.index s c] is
        equivalent to [Rope.index_from s 0 c].*)

  val rindex : t -> UChar.t -> int
  (** [Rope.rindex s c] returns the position of the rightmost
     occurrence of character [c] in rope [s].
     Raise [Not_found] if [c] does not occur in [s]. *)

  val rindex_from : t -> int -> UChar.t -> int
  (** Same as {!rindex}, but start
     searching at the character position given as second argument.
     [rindex s c] is equivalent to
     [rindex_from s (length s - 1) c]. *)

  val contains : t -> UChar.t -> bool
  (** [contains s c] tests if character [c]
     appears in the rope [s]. *)

  val contains_from : t -> int -> UChar.t -> bool
  (** [contains_from s start c] tests if character [c] appears in
        the subrope of [s] starting from [start] to the end of [s].

        @raise Invalid_argument if [start] is not a valid index of [s]. *)

  val rcontains_from : t -> int -> UChar.t -> bool
  (** [rcontains_from s stop c] tests if character [c]
     appears in the subrope of [s] starting from the beginning
     of [s] to index [stop].
     @raise Invalid_argument if [stop] is not a valid index of [s]. *)

  val find : t -> t -> int
   (** [find s x] returns the starting index of the first occurrence of
        rope [x] within rope [s].

        {b Note} This implementation is optimized for short ropes.

        @raise Invalid_rope if [x] is not a subrope of [s]. *)

  val find_from : t -> int -> t -> int
    (** [find_from s ofs x] behaves as [find s x] but starts searching
        at offset [ofs]. [find s x] is equivalent to [find_from s 0 x].*)

  val rfind : t -> t -> int
    (** [rfind s x] returns the starting index of the last occurrence
        of rope [x] within rope [s].

        {b Note} This implementation is optimized for short ropes.

        @raise Invalid_rope if [x] is not a subrope of [s]. *)

  val rfind_from : t -> int -> t -> int
    (** [rfind_from s ofs x] behaves as [rfind s x] but starts searching
        at offset [ofs]. [rfind s x] is equivalent to [rfind_from s (length s - 1) x].*)


  val starts_with : t -> t -> bool
  (** [starts_with s x] returns [true] if [s] is starting with [x], [false] otherwise. *)

  val ends_with : t -> t -> bool
  (** [ends_with s x] returns [true] if the rope [s] is ending with [x], [false] otherwise. *)

  val exists : t -> t -> bool
  (** [exists str sub] returns true if [sub] is a subrope of [str] or
        false otherwise. *)

  val left : t -> int -> t
  (**[left r len] returns the rope containing the [len] first characters of [r]*)

  val right : t -> int -> t
  (**[left r len] returns the rope containing the [len] last characters of [r]*)

  val head : t -> int -> t
  (**as {!left}*)

  val tail : t -> int -> t
  (**[tail r pos] returns the rope containing all but the [pos] first characters of [r]*)

  val strip : ?chars:(UChar.t list) -> t -> t
    (** Returns the rope without the chars if they are at the beginning or
        at the end of the rope. By default chars are " \t\r\n". *)

  val lchop : t -> t
  (** Returns the same rope but without the first character.
        does nothing if the rope is empty. *)

  val rchop : t -> t
  (** Returns the same rope but without the last character.
        does nothing if the rope is empty. *)

  val slice : ?first:int -> ?last:int -> t -> t
    (** [slice ?first ?last s] returns a "slice" of the rope
        which corresponds to the characters [s.[first]],
        [s.[first+1]], ..., [s[last-1]]. Note that the character at
        index [last] is {b not} included! If [first] is omitted it
        defaults to the start of the rope, i.e. index 0, and if
        [last] is omitted is defaults to point just past the end of
        [s], i.e. [length s].  Thus, [slice s] is equivalent to
        [copy s].

        Negative indexes are interpreted as counting from the end of
        the rope. For example, [slice ~last:-2 s] will return the
        rope [s], but without the last two characters.

        This function {b never} raises any exceptions. If the
        indexes are out of bounds they are automatically clipped.
    *)

  val splice : t -> int -> int -> t -> t
  (** [splice s off len rep] returns the rope in which the section of [s]
        indicated by [off] and [len] has been cut and replaced by [rep].

      Negative indices are interpreted as counting from the end of the string.*)

  val fill : t -> int -> int -> UChar.t -> t
  (** [fill s start len c] returns the rope in which
     characters number [start] to [start + len - 1] of [s] has
      been replaced by [c].

     @raise Invalid_argument if [start] and [len] do not
      designate a valid subrope of [s]. *)

  val blit : t -> int -> t -> int -> int -> t
  (** [blit src srcoff dst dstoff len] returns a copy
      of [dst] in which [len] characters have been copied
     from rope [src], starting at character number [srcoff], to
     rope [dst], starting at character number [dstoff]. It works
     correctly even if [src] and [dst] are the same rope,
     and the source and destination chunks overlap.

      @raise Invalid_argument if [srcoff] and [len] do not
     designate a valid subrope of [src], or if [dstoff] and [len]
     do not designate a valid subrope of [dst]. *)

  val concat : t -> t list -> t
  (** [concat sep sl] concatenates the list of ropes [sl],
     inserting the separator rope [sep] between each. *)

  val replace : str:t -> sub:t -> by:t -> bool * t
  (** [replace ~str ~sub ~by] returns a tuple constisting of a boolean
        and a rope where the first occurrence of the rope [sub]
        within [str] has been replaced by the rope [by]. The boolean
        is [true] if a substitution has taken place, [false] otherwise. *)

  (** {6 Splitting around}*)

  val split : t -> t -> t * t
  (** [split s sep] splits the rope [s] between the first
        occurrence of [sep].
        @raise Invalid_rope if the separator is not found. *)

  val rsplit : t -> t -> t * t
    (** [rsplit s sep] splits the rope [s] between the last
        occurrence of [sep].
        @raise Invalid_rope if the separator is not found. *)

  val nsplit : t -> t -> t list
    (** [nsplit s sep] splits the rope [s] into a list of ropes
        which are separated by [sep].
        [nsplit "" _] returns the empty list. *)

  val nsplitp : t -> (UChar.t -> bool) -> t list

  val compare : t -> t -> int
  (** The comparison function for ropes, with the same specification as
        {!Pervasives.compare}.  Along with the type [t], this function [compare]
        allows the module [Rope] to be passed as argument to the functors
        {!Set.Make} and {!Map.Make}. *)

  val print : 'a BatInnerIO.output -> t -> unit
    (** Prints a rope to the given out_channel *)

end
