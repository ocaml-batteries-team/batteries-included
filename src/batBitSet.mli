(*
 * Bitset - Efficient bit sets
 * Copyright (C) 2003 Nicolas Cannasse
 * Copyright (C) 2008 David Teller
 * Copyright (C) 2012 Sylvain Le Gall
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

(** Efficient bit sets.

    A bitset is an array of boolean values that can be accessed with
    indexes like an array but provides a better memory usage (divided
    by Sys.word_size; either 32 or 64) for a very small speed
    trade-off.  It can provide efficient storage of dense sets of
    nonnegative integers near zero.  Sparse sets should use {!BatSet}, sets with
    large ranges of contiguous ints should use {!BatISet}.

    @author Nicolas Cannasse
    @author David Teller (Boilerplate code)
*)

type t

val empty : unit ->  t
(** Create an empty bitset of capacity 0, the bitset will
    automatically expand when needed.

    Example: [BitSet.empty ()]
*)

val create : int -> t
(** Create an empty bitset with at least an initial capacity (in number of bits).

    Example: [BitSet.create 0 = BitSet.empty ()]
    @raise Invalid_argument on negative capacity
*)

val create_full : int -> t
(** Create a full bitset with at least initial capacity (in number of bits).
    All the bit under the defined capacity will be set.

    Example: [BitSet.count (BitSet.create_full n) = n]
    @raise Invalid_argument on negative capacity
*)

val copy : t -> t
(** Copy a bitset : further modifications of first one will not affect the
    copy.

    Example: [
    let a = Bitset.create 8 in
    let b = BitSet.copy a in
    BitSet.set a 6;
    BitSet.mem a 6 && not (BitSet.mem b 6)]
*)

val mem : t -> int -> bool
(** [mem s n] returns true if nth-bit in the bitset [s] is set,
    or false otherwise.

    Example: [let a = BitSet.create_full 256 in not (BitSet.mem a 300)]
    @raise Invalid_argument on negative index ([n < 0])
*)

val count : t -> int
(** [count s] returns the number of bits set in the bitset [s]. Also
    known as Population Count, or [cardinal] for sets.

    Example: [BitSet.count (BitSet.of_list [6;4;2;2;1]) = 4]
*)

val next_set_bit : t -> int -> int option
(** [next_set_bit s n] returns [Some m] when [m] is the next set
    element with index greater than or equal [n], or None if no such
    element exists (i.e. [n] is greater than the largest element)

    More efficient than scanning with repeated [BitSet.mem].
    @raise Invalid_argument on negative index ([n < 0])
*)

(** {6 In-place Update} *)

(** These functions modify an existing bitset. *)

val set : t -> int -> unit
(** [set s n] sets the [n]th-bit in the bitset [s] to true.
    @raise Invalid_argument on negative index ([n < 0])
*)

val unset : t -> int -> unit
(** [unset s n] sets the [n]th-bit in the bitset [s] to false.
    @raise Invalid_argument on negative index ([n < 0])
*)

val put : t -> bool -> int -> unit
(** [put s v n] sets the nth-bit in the bitset [s] to [v].
    @raise Invalid_argument on negative index ([n < 0])
*)

val toggle : t -> int -> unit
(** [toggle s n] changes the nth-bit value in the bitset [s].
    @raise Invalid_argument on negative index ([n < 0])
*)

val intersect : t -> t -> unit
(** [intersect s t] sets [s] to the intersection of the sets [s] and [t]. *)

val unite : t -> t -> unit
(** [unite s t] sets [s] to the union of the sets [s] and [t]. *)

val differentiate : t -> t -> unit
(** [differentiate s t] removes the elements of [t] from [s]. *)

val differentiate_sym : t -> t -> unit
(** [differentiate_sym s t] sets [s] to the symmetrical difference of the
    sets [s] and [t]. *)

(** {6 Return new bitset} *)

(** These functions return a new bitset that shares nothing with the
    input bitset.  This is not as efficient as the in-place update. *)

val add : int -> t -> t
(** [add n s] returns a copy of [s] with bit [n] true.
    @raise Invalid_argument on negative index ([n < 0])
*)

val remove : int -> t -> t
(** [remove n s] returns a copy of [s] with bit [n] false.
    @raise Invalid_argument on negative index ([n < 0])
*)

val inter : t -> t -> t
(** [inter s t] returns the intersection of sets [s] and [t]. *)

val union : t -> t -> t
(** [union s t] return the union of sets [s]  and [t]. *)

val diff : t -> t -> t
(** [diff s t] returns [s]-[t]. *)

val sym_diff : t -> t -> t
(** [sym_diff s t] returns the symmetrical difference of [s] and [t]. *)

(** {6 Boilerplate code}*)

val print: 'a BatInnerIO.output -> t -> unit
(* Print the given BitSet to the given output channel.  This
   function prints a BitSet as a boolean vector, and pads to a multiple
   of 8 bits with zeros.  Thus, the bitset containing only 1 and 3 is
   printed as ["01010000"].  *)

val enum : t -> int BatEnum.t
(** [enum s] returns an enumeration of bits which are set
    in the bitset [s]. *)

val of_enum : ?cap:int -> int BatEnum.t -> t
(** [of_enum ~cap e] builds a bitset of capacity [cap] an enumeration
    of ints [e].

    Note: Performance of this function may be poor if enumeration is
    in increasing order and the max.
*)

val of_list : ?cap:int -> int list -> t
(** As [of_enum], but from a list *)

val compare : t -> t -> int
(** [compare s1 s2] compares two bitsets using a lexicographic
    ordering.  Highest bit indexes are compared first. The capacity of
    the bitsets is not important for this comparison, only the bits
    starting with the highest set bit and going down.  *)

val equal : t -> t -> bool
(** [equal s1 s2] returns true if, and only if, all bits values in s1 are
    the same as in s2. *)

val ord : t -> t -> BatOrd.order
(** [ord s1 s2] returns [BatOrd.Lt], [BatOrd.Eq] or [BatOrd.Gt] if [compare s1 s2]
    is, respectively, [< 0], [0] or [> 0]. *)

(** {6 Internals} *)
val capacity : t -> int
  (** [capacity s] returns the number of bits, both set and unset, stored
      in [s].  This is guaranteed to be larger than the largest element
      (set bit index) in [s].


  *)
