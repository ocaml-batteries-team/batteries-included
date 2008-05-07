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
   If you feel like using [Stream.t], please take a look at [Enum]
   or [LazyList] and [GenParser].
*)
module Stream : sig
(** Streams and parsers. *)
type 'a t = 'a Stream.t
(** The type of streams holding values of type ['a]. *)

exception Failure
(** Raised by parsers when none of the first components of the stream
   patterns is accepted. *)

exception Error of string
(** Raised by parsers when the first component of a stream pattern is
   accepted, but one of the following components is rejected. *)

(** {6 Conversion functions} *)
val enum : 'a t -> 'a Enum.t

val of_fun  : (unit -> 'a) -> 'a t

val of_enum : 'a Enum.t -> 'a t

val of_string  : string -> char t

val of_channel : in_channel -> char t
(** Obsolete *)

val of_list : 'a list -> 'a t
(** Return the stream holding the elements of the list in the same
   order. *)

val to_channel : out_channel -> char t -> unit
(** Obsolete *)

val of_input :   IO.input    -> char t

val to_output:   'a IO.output-> char t -> unit

(** {6 Stream builders}

   Warning: these functions create streams with fast access; it is illegal
   to mix them with streams built with [[< >]]; would raise [Failure]
   when accessing such mixed streams.
*)

val from : (int -> 'a option) -> 'a t
(** [Stream.from f] returns a stream built from the function [f].
   To create a new stream element, the function [f] is called with
   the current stream count. The user function [f] must return either
   [Some <value>] for a value or [None] to specify the end of the
   stream. *)

(** {6 Other constructors} *)

val of_fun : (unit -> 'a) -> 'a t
(** [Stream.from f] returns a stream built from the function [f].
   To create a new stream element, the function [f] is called with
   the current stream count. The user function [f] must return either
   [Some <value>] for a value or [None] to specify the end of the
   stream. *)

val range : int -> int option -> int t
(** [range p q] creates an enumeration of integers [[p, p+1, ..., q]].*)

val ( -- ) : int -> int -> int t
(** As [range]

    [5 -- 10] is the enumeration 5,6,7,8,9,10*)

val seq : 'a -> ('a -> 'a) -> ('a -> bool) -> 'a t
  (** [seq init step cond] creates a sequence of data as stream, which starts
      from [init],  extends by [step],  until the condition [cond]
      fails. E.g. [seq 1 ((+) 1) ((>) 100)] returns [[1, 2, ... 99]]. If [cond
      init] is false, the result is an empty stream. *)

val repeat : int option -> 'a -> 'a t
  (** [repeat (Some times) x] creates a stream sequence filled with [times] times of
      [x]. It return infinite stream when the option is [None]. It returns empty
      stream when [times <= 0] *)

val cycle : int option -> 'a t -> 'a t
  (** [cycle] is similar to [repeat], except that the content to fill is a
      substream rather than a single element. Note that [times] represents the
      times of repeating not the length of stream. *) 


(** {6 Stream iterators} *)

val iter : ('a -> unit) -> 'a t -> unit
(** [Stream.iter f s] scans the whole stream s, applying function [f]
   in turn to each stream element encountered. *)

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
  (** [scanl f init stream] returns a stream of successive reduced values from the
      left: [scanl f init [<'e1;'e2;..>] = [<'init; '((f init e1) as e1'); '(f
      e1' e2); ..>]] *)

val scan : ('a -> 'a -> 'a) -> 'a t -> 'a t
  (** [scan] is similar to [scanl] but without the [init] value: [scan f
      [<'e1;'e2;..>] = [<'e1;'(f e1 e2);..>]]. *)

val while_do : int option -> ('a -> bool) -> ('a t -> 'a t) -> 'a t -> 'a t
  (** [while_do (Some n) cont f stream] tests each element of input [stream] with
      [cont] to see whether it should continue to loop. If false, the element
      is added to the output stream; if true, the element is added to the input
      stream of [f] whose output is then merged back with the input [stream]. The
      optional argument [size] is the maximum number of elements running in
      parallel inside the loop. The default value is [1], in which case the
      order of elements is preserved, but it also means if an element loops
      forever any elements behind it won't exceed. 
  *)

val do_while : int option -> ('a -> bool) -> ('a t -> 'a t) -> 'a t -> 'a t
  (** similar to [while_do], the stream elements are send to [f] before [cont]
      test. *)


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

val span : ('a -> bool) -> 'a t -> 'a t * 'a t
  (** [span test stream] is equivalent to [(take_while test stream, drop_while test
      stream)]. *)

val break : ('a -> bool) -> 'a t -> 'a t * 'a t
  (** [break test stream] is equivalent to [span (fun x -> not (test x)) stream] *)

val group : ('a -> bool) -> 'a t -> 'a t t
  (** [group test stream] devides [stream] into a stream of sub-streams, where
      each sub-stream is the longest continuous stream of elements whose [test]
      results are the same. *)

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

(** {6 Streams array arithmetic}

    All the functions in this part are lazy.
*)

val dupn : int -> 'a t -> 'a t array
  (** [dupn] is the array version of [dup]. [dupn n fl] produces an array of
      [n] streams all identical to the input stream, but can be used
      independently. *)


val combn : 'a t array -> 'a array t
  (** [combn] is the array version of [comb]. It takes an array of streams and
      combines the elements of
      corresponding positions in each stream as arrays, the result is returned as
      a stream of the same order. When any of the stream runs out of elements, the
      rest elements of all streams are simply ignored. *)

val splitn: int -> 'a array t -> 'a t array
  (** [splitn] is the array version of [split], also the opposite of [combn]. 
      It transform a stream of arrays to an
      array of streams. Considering the fact that arries in OCaml can be various
      in length, also considering the requirement that [splitn] should be fully
      lazy (hence shouldn't look into the heading element), [splitn] takes an
      extra integer parameter [n] as the length of the destination stream
      array. Any arrays from the input array stream should have a least [n]
      elements, and in each of them, any elements beyond the [nth] position are
      simply discarded. *)

val mergen : (int -> 'a -> int) -> 'a t array -> 'a t
  (** [mergen] is the array version of [merge]. It takes a array of streams and
      merge them into a single stream. The order of elements is determined by the
      first parameter [f], which will be applied on each element of the output
      stream and its originate stream id to decide the next element's stream id. The
      first element always comes from the [0th] stream. If [f] chooses a stream
      running out of element, it will take the element from the next non-empty
      stream, until all the input streams are empty. *)

val switchn: int -> ('a -> int) -> 'a t -> 'a t array
  (** [switchn] is the array version of [switch], also the opposite of
      [mergen]. [switch n f fl] split [fl] to an array of [n] streams, [f] is
      applied to each element of [fl] to decide the id of its destination
      stream. *)



(** {6 Stream arithmetic} 

    All the functions in this part are lazy.
*)

val cons : 'a -> 'a t -> 'a t
  (** [cons x stream] equals [[<'x; stream>]]. *)

val apnd : 'a t -> 'a t -> 'a t
  (** [apnd fla flb] equals [[<fla;flb>]]. *)

val is_empty : 'a t -> bool
  (** [is_empty stream] tests whether [stream] is empty. But note that it forces
      the evaluation of the head element if any. *)

(** {6 Concurrency} *)
val farm: 
  int option -> (int -> int) option -> ('a -> int) option -> 
  (int -> ('a t -> 'b t)) -> 'a t -> 'b t
  (** [farm] is a task farm generator. [farm ~par:n ~size:s ~path:p gen] will
      produce a super stream processor of type ['a t -> 'b t] constructed
      from [n] sub stream processor of type ['a t -> 'b t] generated by
      applying [gen] to a series of id [0, 1, ..., n-1]. The elements of the
      input stream of the result processor will be feed to corresponding sub stream
      processors according to function [p], which maps an element to the id of
      the destination sub stream processor. The size (capacity) function [s] maps
      an id of sub stream processor to the maximum number of elements that can
      run concurrently inside that processor. The output streams of these sub
      stream processors are collected and merged into the output stream of the
      super stream processor. The merging mechanics will first try to pick
      element from the next nearest (counting from the processor from which the
      last element is collected, in ascending order) stream processor having
      reached its capacity, if there's no such processor, it will pick element
      from the next nearest stream processor with active element inside. The
      default value of [par] and [size] are both [1], the default [path] value
      is a round-robin starts from [0]. In this case, the evaluation is always
      strict. *)


(** {6 Predefined parsers} *)

val next : 'a t -> 'a
(** Return the first element of the stream and remove it from the
   stream. Raise Stream.Failure if the stream is empty. *)

val empty : 'a t -> unit
(** Return [()] if the stream is empty, else raise [Stream.Failure]. *)


(** {6 Useful functions} *)

val peek : 'a t -> 'a option
(** Return [Some] of "the first element" of the stream, or [None] if
   the stream is empty. *)

val junk : 'a t -> unit
(** Remove the first element of the stream, possibly unfreezing
   it before. *)

val count : 'a t -> int
(** Return the current count of the stream elements, i.e. the number
   of the stream elements discarded. *)

val npeek : int -> 'a t -> 'a list
(** [npeek n] returns the list of the [n] first elements of
   the stream, or all its remaining elements if less than [n]
   elements are available. *)

(**/**)

(** {6 For system use only, not for the casual user} *)

val iapp : 'a t -> 'a t -> 'a t
val icons : 'a -> 'a t -> 'a t
val ising : 'a -> 'a t

val lapp : (unit -> 'a t) -> 'a t -> 'a t
val lcons : (unit -> 'a) -> 'a t -> 'a t
val lsing : (unit -> 'a) -> 'a t

val sempty : 'a t
val slazy : (unit -> 'a t) -> 'a t

val dump : ('a -> unit) -> 'a t -> unit


end

module StreamLabels : sig
(**
   {b Note} This module is provided essentially for backwards-compatibility.
   If you feel like using [Stream.t], please take a look at [Enum]
   or [LazyList] and [GenParser].
*)


(** Streams and parsers. *)

type 'a t = 'a Stream.t

(** {6 Conversion functions} *)
val enum : 'a t -> 'a Enum.t

val of_fun  : (unit -> 'a) -> 'a t

val of_enum : 'a Enum.t -> 'a t

val of_string  : string -> char t

val of_channel : in_channel -> char t
(** Obsolete *)

val of_list : 'a list -> 'a t
(** Return the stream holding the elements of the list in the same
   order. *)

val to_channel : out_channel -> char t -> unit
(** Obsolete *)

val of_input :   IO.input    -> char t

val to_output:   'a IO.output-> char t -> unit

(** {6 Stream builders}

   Warning: these functions create streams with fast access; it is illegal
   to mix them with streams built with [[< >]]; would raise [Failure]
   when accessing such mixed streams.
*)

val from : (int -> 'a option) -> 'a t
(** [Stream.from f] returns a stream built from the function [f].
   To create a new stream element, the function [f] is called with
   the current stream count. The user function [f] must return either
   [Some <value>] for a value or [None] to specify the end of the
   stream. *)

(** {6 Other constructors} *)

val of_fun : (unit -> 'a) -> 'a t
(** [Stream.from f] returns a stream built from the function [f].
   To create a new stream element, the function [f] is called with
   the current stream count. The user function [f] must return either
   [Some <value>] for a value or [None] to specify the end of the
   stream. *)

val range : ?until:int -> int -> int t
(** [range p until:q] creates an enumeration of integers [[p, p+1, ..., q]].
    If [until] is omitted, the enumeration is not bounded. Behaviour is 
    not-specified once [max_int] has been reached.*)

val ( -- ) : int -> int -> int t
(** As [range], without the label. 

    [5 -- 10] is the enumeration 5,6,7,8,9,10*)

val seq : 'a -> ('a -> 'a) -> ('a -> bool) -> 'a t
  (** [seq init step cond] creates a sequence of data as stream, which starts
      from [init],  extends by [step],  until the condition [cond]
      fails. E.g. [seq 1 ((+) 1) ((>) 100)] returns [[1, 2, ... 99]]. If [cond
      init] is false, the result is an empty stream. *)

val repeat : ?times:int -> 'a -> 'a t
  (** [repeat ~times:n x] creates a stream sequence filled with [n] times of
      [x]. It return infinite stream when [~times] is absent. It returns empty
      stream when [times <= 0] *)

val cycle : ?times:int -> 'a t -> 'a t
  (** [cycle] is similar to [repeat], except that the content to fill is a
      substream rather than a single element. Note that [times] represents the
      times of repeating not the length of stream. *) 


(** {6 Stream iterators} *)

val iter : f:('a -> unit) -> 'a t -> unit
(** [Stream.iter f s] scans the whole stream s, applying function [f]
   in turn to each stream element encountered. *)

val foldl : f:('a -> 'b -> 'a * bool option) -> init:'a -> 'b t -> 'a
  (** [foldl f init stream] is a lazy fold_left. [f accu elt] should return
      [(new_accu, state)] where [new_accu] is normal accumulation result, and
      [state] is a flag representing whether the computation should continue
      and whether the last operation is valid: [None] means continue, [Some b]
      means stop where [b = true] means the last addition is still valid and [b
      = false] means the last addition is invalid and should be revert. *)

val foldr : f:('a -> 'b lazy_t -> 'b) -> init:'b -> 'a t -> 'b
  (** [foldr f init stream] is a lazy fold_right. Unlike the normal fold_right,
      the accumulation parameter of [f elt accu] is lazy, hence it can decide
      not to force the evaluation of [accu] if the current element [elt] can
      determin the result by itself. *)

val fold : f:('a -> 'a -> 'a * bool option) -> init:'a t -> 'a
  (** [fold] is [foldl] without initialization value, where the first
      element of stream is taken as [init]. It raises [End_of_stream] exception
      when the input stream is empty. *)


val filter : f:('a -> bool) -> 'a t -> 'a t
  (** [filter test stream] picks all the elements satisfying [test] from [stream]
      and return the results in the same order as a stream. *)

(** {6 Computation over stream}

    All the functions in this part are lazy.
*)

val map : f:('a -> 'b) -> 'a t -> 'b t
  (** [map f stream] applies [f] in turn to elements from [stream] and return the
      results as a stream in the same order. *)

val map2 : f:('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  (** [map2 f streama streamb] applies [f] in turn to elements of corresponding
      positions from [streama] and [streamb]. The results are constructed in the
      same order as a stream. If one stream is short, excess elements of the longer
      stream are ignored. *)

val scanl : f:('a -> 'b -> 'a) -> 'a -> 'b t -> 'a t
  (** [scanl f init stream] returns a stream of successive reduced values from the
      left: [scanl f init [<'e1;'e2;..>] = [<'init; '((f init e1) as e1'); '(f
      e1' e2); ..>]] *)

val scan : f:('a -> 'a -> 'a) -> 'a t -> 'a t
  (** [scan] is similar to [scanl] but without the [init] value: [scan f
      [<'e1;'e2;..>] = [<'e1;'(f e1 e2);..>]]. *)

val while_do : ?size:int -> f:('a -> bool) -> ('a t -> 'a t) -> 'a t -> 'a t
  (** [while_do ~size:n cont f stream] tests each element of input [stream] with
      [cont] to see whether it should continue to loop. If false, the element
      is added to the output stream; if true, the element is added to the input
      stream of [f] whose output is then merged back with the input [stream]. The
      optional argument [size] is the maximum number of elements running in
      parallel inside the loop. The default value is [1], in which case the
      order of elements is preserved, but it also means if an element loops
      forever any elements behind it won't exceed. 
  *)

val do_while : ?size:int -> f:('a -> bool) -> ('a t -> 'a t) -> 'a t -> 'a t
  (** similar to [while_do], the stream elements are send to [f] before [cont]
      test. *)


val concat : 'a t t -> 'a t
  (** concatenate a stream of streams *)

val take : int -> 'a t -> 'a t
  (** [take n stream] returns the prefix of [stream] of length [n], or [stream]
      itself if [n] is greater than the length of [stream] *)

val drop : int -> 'a t -> 'a t
  (** [drop n stream] returns the suffix of [stream] after the first [n] elements,
      or a empty stream if [n] is greater than the length of [stream] *)

val take_while : f:('a -> bool) -> 'a t -> 'a t
  (** [take_while test stream] returns the longest (possibly empty) prefix of
      [stream] of elements that satisfy [test]. *)

val drop_while : f:('a -> bool) -> 'a t -> 'a t
  (** [drop_while test stream] returns the remaining suffix of [take_while test
      stream]. *)

val span : f:('a -> bool) -> 'a t -> 'a t * 'a t
  (** [span test stream] is equivalent to [(take_while test stream, drop_while test
      stream)]. *)

val break : f:('a -> bool) -> 'a t -> 'a t * 'a t
  (** [break test stream] is equivalent to [span (fun x -> not (test x)) stream] *)

val group : f:('a -> bool) -> 'a t -> 'a t t
  (** [group test stream] devides [stream] into a stream of sub-streams, where
      each sub-stream is the longest continuous stream of elements whose [test]
      results are the same. *)

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

val merge : f:(bool -> 'a -> bool) -> 'a t * 'a t -> 'a t
  (** [merge test (streama, streamb)] merge the elements from [streama] and
      [streamb] into a single stream. The [bool] type here represents the id of the
      two input streams where [true] is the first and [false] represents the
      second. The [test] function is applied to each element of the output stream
      together with the id of the input stream from which it was extracted, to
      decide which stream should the next element come from. The first element is
      always taken from [streama]. When a stream runs out of elements, the merge
      process will continue to take elements from the other stream until both
      streams reach their ends. *)

val switch : f:('a -> bool) -> 'a t -> 'a t * 'a t
  (** [switch ~f:test stream] split [stream] into two streams, where the first stream have
      all the elements satisfying [test], the second stream is opposite. The
      order of elements in the source stream is preserved. *)

(** {6 Streams array arithmetic}

    All the functions in this part are lazy.
*)

val dupn : int -> 'a t -> 'a t array
  (** [dupn] is the array version of [dup]. [dupn n fl] produces an array of
      [n] streams all identical to the input stream, but can be used
      independently. *)


val combn : 'a t array -> 'a array t
  (** [combn] is the array version of [comb]. It takes an array of streams and
      combines the elements of
      corresponding positions in each stream as arrays, the result is returned as
      a stream of the same order. When any of the stream runs out of elements, the
      rest elements of all streams are simply ignored. *)

val splitn: int -> 'a array t -> 'a t array
  (** [splitn] is the array version of [split], also the opposite of [combn]. 
      It transform a stream of arrays to an
      array of streams. Considering the fact that arries in OCaml can be various
      in length, also considering the requirement that [splitn] should be fully
      lazy (hence shouldn't look into the heading element), [splitn] takes an
      extra integer parameter [n] as the length of the destination stream
      array. Any arrays from the input array stream should have a least [n]
      elements, and in each of them, any elements beyond the [nth] position are
      simply discarded. *)

val mergen : f:(int -> 'a -> int) -> 'a t array -> 'a t
  (** [mergen] is the array version of [merge]. It takes a array of streams and
      merge them into a single stream. The order of elements is determined by the
      first parameter [f], which will be applied on each element of the output
      stream and its originate stream id to decide the next element's stream id. The
      first element always comes from the [0th] stream. If [f] chooses a stream
      running out of element, it will take the element from the next non-empty
      stream, until all the input streams are empty. *)

val switchn: int -> f:('a -> int) -> 'a t -> 'a t array
  (** [switchn] is the array version of [switch], also the opposite of
      [mergen]. [switch n f fl] split [fl] to an array of [n] streams, [f] is
      applied to each element of [fl] to decide the id of its destination
      stream. *)



(** {6 Stream arithmetic} 

    All the functions in this part are lazy.
*)

val cons : 'a -> 'a t -> 'a t
  (** [cons x stream] equals [[<'x; stream>]]. *)

val apnd : 'a t -> 'a t -> 'a t
  (** [apnd fla flb] equals [[<fla;flb>]]. *)

val is_empty : 'a t -> bool
  (** [is_empty stream] tests whether [stream] is empty. But note that it forces
      the evaluation of the head element if any. *)

(** {6 Concurrency} *)
val farm: 
  ?par:int -> ?size:(int -> int) -> ?path:('a -> int) -> 
  (int -> ('a t -> 'b t)) -> 'a t -> 'b t
  (** [farm] is a task farm generator. [farm ~par:n ~size:s ~path:p gen] will
      produce a super stream processor of type ['a t -> 'b t] constructed
      from [n] sub stream processor of type ['a t -> 'b t] generated by
      applying [gen] to a series of id [0, 1, ..., n-1]. The elements of the
      input stream of the result processor will be feed to corresponding sub stream
      processors according to function [p], which maps an element to the id of
      the destination sub stream processor. The size (capacity) function [s] maps
      an id of sub stream processor to the maximum number of elements that can
      run concurrently inside that processor. The output streams of these sub
      stream processors are collected and merged into the output stream of the
      super stream processor. The merging mechanics will first try to pick
      element from the next nearest (counting from the processor from which the
      last element is collected, in ascending order) stream processor having
      reached its capacity, if there's no such processor, it will pick element
      from the next nearest stream processor with active element inside. The
      default value of [par] and [size] are both [1], the default [path] value
      is a round-robin starts from [0]. In this case, the evaluation is always
      strict. *)


(** {6 Predefined parsers} *)

val next : 'a t -> 'a
(** Return the first element of the stream and remove it from the
   stream. Raise Stream.Failure if the stream is empty. *)

val empty : 'a t -> unit
(** Return [()] if the stream is empty, else raise [Stream.Failure]. *)


(** {6 Useful functions} *)

val peek : 'a t -> 'a option
(** Return [Some] of "the first element" of the stream, or [None] if
   the stream is empty. *)

val junk : 'a t -> unit
(** Remove the first element of the stream, possibly unfreezing
   it before. *)

val count : 'a t -> int
(** Return the current count of the stream elements, i.e. the number
   of the stream elements discarded. *)

val npeek : int -> 'a t -> 'a list
(** [npeek n] returns the list of the [n] first elements of
   the stream, or all its remaining elements if less than [n]
   elements are available. *)

(**/**)

(** {6 For system use only, not for the casual user} *)

val iapp : 'a t -> 'a t -> 'a t
val icons : 'a -> 'a t -> 'a t
val ising : 'a -> 'a t

val lapp : (unit -> 'a t) -> 'a t -> 'a t
val lcons : (unit -> 'a) -> 'a t -> 'a t
val lsing : (unit -> 'a) -> 'a t

val sempty : 'a t
val slazy : (unit -> 'a t) -> 'a t

val dump : ('a -> unit) -> 'a t -> unit

end
