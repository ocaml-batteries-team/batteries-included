(**  Monadic results of computations that can raise exceptions *)

(** The type of a result.  A result is either [Ok x] carrying the
    normal return value [x] or is [Error e] carrying some indication of an
    error.  The value associated with a bad result is usually an exception
    ([exn]) that can be raised.
    @since 1.0
*)
type ('a, 'e) t = ('a, 'e) BatPervasives.result = Ok of 'a | Error of 'e

val ok : 'a -> ('a, 'b) t
(** [ok v] is [Ok v].
    @since NEXT_RELEASE *)

val error : 'e -> ('a, 'e) t
(** [error e] is [Error e].
    @since NEXT_RELEASE *)

val value : ('a, 'e) t -> default:'a -> 'a
(** [value r ~default] is [v] if [r] is [Ok v] and [default] otherwise.
    @since NEXT_RELEASE *)

val default: 'a -> ('a, _) t -> 'a
(** [default d r] evaluates to [d] if [r] is [Error] else [x] when [r] is
    [Ok x].
    @see 'value' or a slightly different signature.
    @since 2.0 *)

val get_ok : ('a, 'e) t -> 'a
(** [get_ok r] is [v] if [r] is [Ok v] and @raise Invalid_argument
    otherwise.
    @since NEXT_RELEASE *)

val get_error : ('a, 'e) t -> 'e
(** [get_error r] is [e] if [r] is [Error e] and @raise Invalid_argument
    otherwise.
    @since NEXT_RELEASE *)

val get : ('a, exn) t -> 'a
(** [get (Ok x)] returns [x], and [get (Error e)] raises [e].  This
    function is, in a way, the opposite of the [catch] function
    @since 2.0 *)

val catch: ('a -> 'e) -> 'a -> ('e, exn) t
(** Execute a function and catch any exception as a result.  This
    function encapsulates code that could throw an exception and returns
    that exception as a value.
    @since 1.0 *)

val catch2: ('a -> 'b -> 'c) -> 'a -> 'b -> ('c, exn) t
(** As [catch] but two parameters.  This saves a closure construction
    @since 2.0 *)

val catch3: ('a -> 'b -> 'c -> 'd) -> 'a -> 'b -> 'c -> ('d, exn) t
(** As [catch] but three parameters.  This saves a closure construction
    @since 2.0 *)

val bind : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
(** [bind r f] is [f v] if [r] is [Ok v] and [r] if [r] is [Error _].
    @since NEXT_RELEASE *)

val join : (('a, 'e) t, 'e) t -> ('a, 'e) t
(** [join rr] is [r] if [rr] is [Ok r] and [rr] if [rr] is [Error _].
    @since NEXT_RELEASE *)

val map : ('a -> 'b) -> ('a, 'e) t -> ('b, 'e) t
(** [map f r] is [Ok (f v)] if [r] is [Ok v] and [r] if [r] is [Error _].
    @since NEXT_RELEASE *)

val map_error : ('e -> 'f) -> ('a, 'e) t -> ('a, 'f) t
(** [map_error f r] is [Error (f e)] if [r] is [Error e] and [r] if
    [r] is [Ok _].
    @since NEXT_RELEASE *)

val map_both : ('a1 -> 'a2) -> ('b1 -> 'b2) -> ('a1, 'b1) t -> ('a2, 'b2) t
(** [map_both f g (Ok x)] returns [Ok (f x)] and [map_both f g (Error e)] returns [Error (g e)].
    @since 2.6.0 *)

val map_default : 'b -> ('a -> 'b) -> ('a, _) t -> 'b
(** [map_default d f r] evaluates to [d] if [r] is [Error] else [f x]
    when [r] is [Ok x]
    @since 2.0 *)

val fold : ok:('a -> 'c) -> error:('e -> 'c) -> ('a, 'e) t -> 'c
(** [fold ~ok ~error r] is [ok v] if [r] is [Ok v] and [error e] if [r]
    is [Error e].
    @since NEXT_RELEASE *)

val iter : ('a -> unit) -> ('a, 'e) t -> unit
(** [iter f r] is [f v] if [r] is [Ok v] and [()] otherwise.
    @since NEXT_RELEASE *)

val iter_error : ('e -> unit) -> ('a, 'e) t -> unit
(** [iter_error f r] is [f e] if [r] is [Error e] and [()] otherwise.
    @since NEXT_RELEASE *)

(** {1:preds Predicates and comparisons} *)

val is_ok : ('a, 'e) t -> bool
(** [is_ok (Ok _)] is [true], otherwise [false].
    @since 2.0 *)

val is_error : ('a, 'e) t -> bool
(** [is_error r] is [true] iff [r] is [Error _].
    @since NEXT_RELEASE *)

val is_bad : ('a, 'e) t -> bool
(** Same as [is_error].
    @since 2.0 *)

(** [is_exn e1 r] is [true] iff [r] is [Error e2] with [e1=e2] *)
val is_exn : exn -> ('a, exn) t -> bool

val equal :
  ok:('a -> 'a -> bool) -> error:('e -> 'e -> bool) -> ('a, 'e) t ->
  ('a, 'e) t -> bool
(** [equal ~ok ~error r0 r1] tests equality of [r0] and [r1] using [ok]
    and [error] to respectively compare values wrapped by [Ok _] and
    [Error _].
    @since NEXT_RELEASE *)

val compare :
  ok:('a -> 'a -> int) -> error:('e -> 'e -> int) -> ('a, 'e) t ->
  ('a, 'e) t -> int
(** [compare ~ok ~error r0 r1] totally orders [r0] and [r1] using [ok] and
    [error] to respectively compare values wrapped by [Ok _ ] and [Error _].
    [Ok _] values are smaller than [Error _] values.
    @since NEXT_RELEASE *)

(** {1:convert Converting} *)

val to_option : ('a, _) t -> 'a option
(** [to_option r] is [r] as an option, mapping [Ok v] to [Some v] and
    [Error _] to [None].
    @since 1.0 *)

val of_option: 'a option -> ('a, unit) t
(** Convert an [option] to a [result]
    @since 1.0 *)

val to_list : ('a, 'e) t -> 'a list
(** [to_list r] is [[v]] if [r] is [Ok v] and [[]] otherwise.
    @since NEXT_RELEASE *)

val to_seq : ('a, 'e) t -> 'a BatSeq.t
(** [to_seq r] is [r] as a sequence. [Ok v] is the singleton sequence
    containing [v] and [Error _] is the empty sequence.
    @since NEXT_RELEASE *)

(** {6 The Result Monad} *)

(** This monad is very similar to the option monad, but instead of
    being [None] when an error occurs, the first error in the sequence is
    preserved as the return value. *)

module Monad : sig

  (** Monadic composition.

      [bind r f] proceeds as [f x] if [r] is [Ok x], or returns [r] if
      [r] is an error.
      @since 2.0
  *)
  val bind:    ('a, 'e) t -> ('a -> ('c, 'e) t) -> ('c, 'e) t

  (** as [bind] *)
  val ( >>= ): ('a, 'e) t -> ('a -> ('c, 'e) t) -> ('c, 'e) t

  (** Monadic return, just encapsulates the given value with Ok *)
  val return : 'a -> ('a, _) t
end

(** {6 Infix} *)

(** This infix module provides the operator [(>>=)] *)
module Infix : sig
  val ( >>= ): ('a, 'e) t -> ('a -> ('c, 'e) t) -> ('c, 'e) t
end

(** Print a result as Ok(x) or Error(exn) *)
val print : ('b BatInnerIO.output -> 'a -> unit) -> 'b BatInnerIO.output -> ('a, exn) t -> unit
