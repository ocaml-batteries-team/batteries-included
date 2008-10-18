(* Rope: a simple implementation of ropes as described in
 
Boehm, H., Atkinson, R., and Plass, M. 1995. Ropes: an alternative to
strings. Softw. Pract. Exper. 25, 12 (Dec. 1995), 1315-1330.
 
Motivated by Luca de Alfaro's extensible array implementation Vec.
 
Copyright (C) 2007 Mauricio Fernandez <mfp@acm.org>
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

 
open ExtUChar
open ExtUTF8
 
type t
  (** The type of the ropes. *)
 
exception Out_of_bounds
  (** Raised when an operation violates the bounds of the rope. *)
 
val max_length : int
  (** Maximum length of the rope. *)
 
(** {6 Creation and conversions} *)
 
val empty : t
  (** The empty rope. *)
 
val of_latin1: string -> t
  (** Constructs a unicode rope from a latin-1 string. *)

val of_ustring : UTF8.t -> t
  (** [of_string s] returns a rope corresponding to the string [s].
Operates in [O(n)] time. *)
 
val to_ustring : t -> UTF8.t
  (** [to_string r] returns the string corresponding to the rope [r]. *)
 
val of_char: UChar.t -> t
  (** [of_char c] returns a rope containing exactly character [c].*)

val make : int -> UChar.t -> t
  (** [make i c] returns a rope of length [i] consisting of [c] chars;
      it is similar to String.make *)

val lowercase: t -> t
  (** [lowercase s] returns a lowercase copy of rope [s].

      Note that, for some languages, the number of characters in
      [lowercase s] is not the same as the number of characters in
      [s].*)

val uppercase: t -> t
  (** [uppercase s] returns a uppercase copy of rope [s].
      
      Note that, for some languages, the number of characters in
      [uppercase s] is not the same as the number of characters in
      [s].*)

(** {6 Properties } *)
 
val is_empty : t -> bool
  (** Returns whether the rope is empty or not. *)
 
val length : t -> int
  (** Returns the length of the rope ([O(1)]). *)
 
val height : t -> int
  (** Returns the height (depth) of the rope. *)
 
val balance : t -> t
  (** [balance r] returns a balanced copy of the [r] rope. Note that ropes are
      automatically rebalanced when their height exceeds a given threshold, but
      [balance] allows to invoke that operation explicity. *)
 
(** {6 Operations } *)
 
val concat : t -> t -> t
  (** [concat r u] concatenates the [r] and [u] ropes. In general, it operates
      in [O(log(min n1 n2))] amortized time.
      Small ropes are treated specially and can be appended/prepended in
      amortized [O(1)] time. *)
 
val append_char : UChar.t -> t -> t
  (** [append_char c r] returns a new rope with the [c] character at the end
      in amortized [O(1)] time. *)
  
val prepend_char : UChar.t -> t -> t
  (** [prepend_char c r] returns a new rope with the [c] character at the
      beginning in amortized [O(1)] time. *)
  
val get : int -> t -> UChar.t
  (** [get n r] returns the (n+1)th character from the rope [r]; i.e.
      [get 0 r] returns the first character.
      Operates in worst-case [O(log size)] time.
      Raises Out_of_bounds if a character out of bounds is requested. *)
  
val set : int -> UChar.t -> t -> t
  (** [set n c r] returns a copy of rope [r]  where the (n+1)th character
      has been set to [c]. See also {!get}.
      Operates in worst-case [O(log size)] time. *)
  
val sub : int -> int -> t -> t
  (** [sub m n r] returns a sub-rope of [r] containing all characters
      whose indexes range from [m] to [m + n - 1] (included).
      Raises Out_of_bounds in the same cases as String.sub.
      Operates in worst-case [O(log size)] time. *)
  
val insert : int -> t -> t -> t
  (** [insert n r u] returns a copy of the [u] rope where [r] has been
      inserted between the characters with index [n] and [n + 1] in the
      original string. The length of the new rope is
      [length u + length r].
      Operates in amortized [O(log(size r) + log(size u))] time. *)
  
val remove : int -> int -> t -> t
  (** [remove m n r] returns the rope resulting from deleting the
      characters with indexes ranging from [m] to [m + n - 1] (included)
      from the original rope [r]. The length of the new rope is
      [length r - n].
      Operates in amortized [O(log(size r))] time. *)
  
(** {6 Iteration} *)
  
val iter : (UChar.t -> unit) -> t -> unit
  (** [iter f r] applies [f] to all the characters in the [r] rope,
      in order. *)
  
val iteri : (int -> UChar.t -> unit) -> t -> unit
  (** Operates like iter, but also passes the index of the character
      to the given function. *)
  
val range_iter : (UChar.t -> unit) -> int -> int -> t -> unit
  (** [rangeiter f m n r] applies [f] to all the characters whose
      indices [k] satisfy [m] <= [k] < [m + n].
      It is thus equivalent to [iter f (sub m n r)], but does not
      create an intermediary rope. [rangeiter] operates in worst-case
      [O(n + log m)] time, which improves on the [O(n log m)] bound
      from an explicit loop using [get].
      Raises Out_of_bounds in the same cases as [sub]. *)
  
val bulk_iter : (UTF8.t -> unit) -> t -> unit
  (** as iter but over larger chunks of data *)
  
val fold : ('a -> UChar.t -> 'a ) -> 'a -> t -> 'a
  (** [Rope.fold f a r] computes [ f (... (f (f a r0) r1)...) rN-1 ]
      where [rn = Rope.get n r ] and [N = length r]. *)

val bulk_fold : ('a -> UTF8.t -> 'a) -> 'a -> t -> 'a
  (** As {!fold} but over larger chunks of data.*)

val enum: t -> UChar.t Enum.t
val bulk_enum: t -> UTF8.t Enum.t
val of_enum: UChar.t Enum.t -> t
val of_bulk_enum: UTF8.t Enum.t -> t
val backwards: t -> UChar.t Enum.t
val of_backwards: UChar.t Enum.t -> t
val bulk_backwards: t -> UTF8.t Enum.t
val of_bulk_backwards: UTF8.t Enum.t -> t

(** {6 Boilerplate code}*)
(** {7 S-Expressions}*)

val t_of_sexp : Sexplib.Sexp.t -> t
val sexp_of_t : t -> Sexplib.Sexp.t

(** {7 Printing}*)

val print: 'a InnerIO.output -> t -> unit

