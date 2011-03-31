(* 
 * Stream - streams and stream parsers
 * Copyright (C) 1997 Daniel de Rauglaudre
 *               2007 Zheng Li
 *               2008 David Teller
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
   Streams and stream parsers

   {b Note} This module is provided essentially for backwards-compatibility.
   If you feel like using [Stream.t], please take a look at [BatEnum]
   or [LazyList].

   This module is based on {{:http://www.pps.jussieu.fr/~li/software/sdflow/}Zheng Li's SDFlow}

    This module extends Stdlib's
    {{:http://caml.inria.fr/pub/docs/manual-ocaml/libref/Stream.html}Stream}
    module, go there for documentation on the rest of the functions
    and types.
*)

(** Streams and parsers. 

    Streams are a read-and-forget data structure, comparable to enumerations.
    In Batteries Included, streams are deprecated in favor of enumerations,
    defined in module {!BatEnum}.

    @author Zheng Li (SDFlow)
    @author David Teller

    @documents Stream
*)
open Stream


include BatEnum.Enumerable with type 'a enumerable = 'a t
include BatInterfaces.Mappable with type 'a mappable = 'a t

(** {6 Conversion functions} *)

val enum : 'a t -> 'a BatEnum.t
(** Convert a stream to an enumeration.
    Reading the resulting enumeration will consume elements from the stream.
    This is the preferred manner of converting from a stream to any other
    data structure.*)

val of_enum : 'a BatEnum.t -> 'a t
(** Convert an enumeration to a stream.
    Reading the resulting stream will consume elements from the enumeration.
    This is the preferred manner of creating a stream.*)

val of_input :   BatIO.input    -> char t
(** Convert an [input] to a stream.*)

val to_list : 'a t -> 'a list
(** Convert a stream to a list *)

val to_string : char t -> string
(** convert stream of chars to string, using buffer *)

val to_string_fmt : ('a -> string, unit, string) format -> 'a t -> string
(** convert stream to string, using Printf with given format *)

val to_string_fun : ('a -> string) -> 'a t -> string
(** convert stream to string, using given conversion function *)

(** {6 Stream consumers} *)

val on_output:   'a BatIO.output-> char t -> unit
(** Convert an [output] to a stream.*)


(** {6 Stream builders}

   Warning: these functions create streams with fast access; it is illegal
   to mix them with streams built with [[< >]]; would raise [Failure]
   when accessing such mixed streams.
*)

(** {6 Other constructors} *)

val of_fun : (unit -> 'a) -> 'a t
(** [Stream.from f] returns a stream built from the function [f].
   To create a new stream element, the function [f] is called with
   the current stream count. The user function [f] must return either
   [Some <value>] for a value or [None] to specify the end of the
   stream. *)

(** {6 Stream iterators} *)

val foldl : ('a -> 'b -> 'a * bool option) -> 'a -> 'b t -> 'a
  (** [foldl f init stream] is a lazy fold_left. [f accu elt] should return
      [(new_accu, state)] where [new_accu] is normal accumulation result, and
      [state] is a flag representing whether the computation should continue
      and whether the last operation is valid: [None] means continue, [Some b]
      means stop where [b = true] means the last addition is still valid and [b
      = false] means the last addition is invalid and should be revert. *)

val foldr : ('a -> 'b lazy_t -> 'b) -> 'b -> 'a t -> 'b
  (** [foldr f init stream] is a lazy fold_right. Unlike the normal fold_right,
      the accumulation parameter of [f elt accu] is lazy, hence it can decide
      not to force the evaluation of [accu] if the current element [elt] can
      determin the result by itself. *)

val fold : ('a -> 'a -> 'a * bool option) -> 'a t -> 'a
  (** [fold] is [foldl] without initialization value, where the first
      element of stream is taken as [init]. It raises [End_of_stream] exception
      when the input stream is empty. *)


val filter : ('a -> bool) -> 'a t -> 'a t
  (** [filter test stream] picks all the elements satisfying [test] from [stream]
      and return the results in the same order as a stream. *)

(** {6 Computation over stream}

    All the functions in this part are lazy.
*)

val map : ('a -> 'b) -> 'a t -> 'b t
  (** [map f stream] applies [f] in turn to elements from [stream] and return the
      results as a stream in the same order. *)

val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  (** [map2 f streama streamb] applies [f] in turn to elements of corresponding
      positions from [streama] and [streamb]. The results are constructed in the
      same order as a stream. If one stream is short, excess elements of the longer
      stream are ignored. *)

val scanl : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a t
(** [scanl f init stream] returns a stream of successive reduced
    values from the left: [scanl f init [< 'e0; 'e1; ... >]] is
    equivalent to
      [[< 'init; '(f init e0); '(f (f init e0) e1); ... >]]
*)

val scan : ('a -> 'a -> 'a) -> 'a t -> 'a t
(** [scan] is similar to [scanl] but without the [init] value:
    [scanl f init [< 'e0; 'e1; 'e2; ... >]] is equivalent to
      [[< 'e0; '(f e0 e1); '(f (f e0 e1) e2); ... >]]
*)

val concat : 'a t t -> 'a t
(** concatenate a stream of streams *)

val take : int -> 'a t -> 'a t
  (** [take n stream] returns the prefix of [stream] of length [n], or [stream]
      itself if [n] is greater than the length of [stream] *)

val drop : int -> 'a t -> 'a t
  (** [drop n stream] returns the suffix of [stream] after the first [n] elements,
      or a empty stream if [n] is greater than the length of [stream] *)

val take_while : ('a -> bool) -> 'a t -> 'a t
  (** [take_while test stream] returns the longest (possibly empty) prefix of
      [stream] of elements that satisfy [test]. *)

val drop_while : ('a -> bool) -> 'a t -> 'a t
  (** [drop_while test stream] returns the remaining suffix of [take_while test
      stream]. *)

(** {6 Streams pair arithmetic}

    All the functions in this part are lazy.
*)

val dup : 'a t -> 'a t * 'a t
  (** [dup stream] returns a pair of streams which are identical to [stream]. Note
      that stream is a destructive data structure, the point of [dup] is to
      return two streams can be used independently. *)

val comb : 'a t * 'b t -> ('a * 'b) t
  (** [comb] transform a pair of stream into a stream of pairs of corresponding
      elements. If one stream is short, excess elements of the longer stream are
      ignored. *)

val split : ('a * 'b) t -> 'a t * 'b t
  (** [split] is the opposite of [comb] *)

val merge : (bool -> 'a -> bool) -> 'a t * 'a t -> 'a t
  (** [merge test (streama, streamb)] merge the elements from [streama] and
      [streamb] into a single stream. The [bool] type here represents the id of the
      two input streams where [true] is the first and [false] represents the
      second. The [test] function is applied to each element of the output stream
      together with the id of the input stream from which it was extracted, to
      decide which stream should the next element come from. The first element is
      always taken from [streama]. When a stream runs out of elements, the merge
      process will continue to take elements from the other stream until both
      streams reach their ends. *)

val switch : ('a -> bool) -> 'a t -> 'a t * 'a t
  (** [switch test stream] split [stream] into two streams, where the first stream have
      all the elements satisfying [test], the second stream is opposite. The
      order of elements in the source stream is preserved. *)


(** {6 Stream arithmetic} 

    All the functions in this part are lazy.*)

val cons : 'a -> 'a t -> 'a t
  (** [cons x stream] equals [[<'x; stream>]]. *)

val apnd : 'a t -> 'a t -> 'a t
  (** [apnd fla flb] equals [[<fla;flb>]]. *)

val is_empty : 'a t -> bool
  (** [is_empty stream] tests whether [stream] is empty. But note that it forces
      the evaluation of the head element if any. *)

(** {6 Predefined parsers} *)

val next : 'a t -> 'a
(** Return the first element of the stream and remove it from the
   stream. Raise Stream.Failure if the stream is empty. *)



module StreamLabels : sig
(**
   {b Note} This module is provided essentially for backwards-compatibility.
   If you feel like using [Stream.t], please take a look at [BatEnum]
   or [LazyList] and [GenParser].

   See the complete [Stream] module for the function documentations.
*)


val iter : f:('a -> unit) -> 'a t -> unit

val to_string_fmt : fmt:('a -> string, unit, string) format -> 'a t -> string

val to_string_fun : fn:('a -> string) -> 'a t -> string

val foldl : f:('a -> 'b -> 'a * bool option) -> init:'a -> 'b t -> 'a

val foldr : f:('a -> 'b lazy_t -> 'b) -> init:'b -> 'a t -> 'b

val fold : f:('a -> 'a -> 'a * bool option) -> init:'a t -> 'a

val filter : f:('a -> bool) -> 'a t -> 'a t

val map : f:('a -> 'b) -> 'a t -> 'b t

val map2 : f:('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t

val scanl : f:('a -> 'b -> 'a) -> 'a -> 'b t -> 'a t

val scan : f:('a -> 'a -> 'a) -> 'a t -> 'a t

val take_while : f:('a -> bool) -> 'a t -> 'a t

val drop_while : f:('a -> bool) -> 'a t -> 'a t

val merge : f:(bool -> 'a -> bool) -> 'a t * 'a t -> 'a t

val switch : f:('a -> bool) -> 'a t -> 'a t * 'a t

end
