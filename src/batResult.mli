(**  Monadic results of computations that can raise exceptions *)

(** The type of a result.  A result is either [Ok x] carrying the
    normal return value [x] or is [Bad e] carrying some indication of an
    error.  The value associated with a bad result is usually an exception
    ([exn]) that can be raised.
    @since 1.0
*)
type ('a, 'b) t = ('a, 'b) BatPervasives.result = Ok of 'a | Bad of 'b

(** Execute a function and catch any exception as a result.  This
    function encapsulates code that could throw an exception and returns
    that exception as a value.
    @since 1.0
*)
val catch: ('a -> 'b) -> 'a -> ('b, exn) t

(** As [catch] but two paramaters.  This saves a closure construction
    @since 2.0
*)
val catch2: ('a -> 'b -> 'c) -> 'a -> 'b -> ('c, exn) t

(** As [catch] but three paramaters.  This saves a closure construction
    @since 2.0
*)
val catch3: ('a -> 'b -> 'c -> 'd) -> 'a -> 'b -> 'c -> ('d, exn) t


(** [get (Ok x)] returns [x], and [get (Bad e)] raises [e].  This
    function is, in a way, the opposite of the [catch] function
    @since 2.0
*)
val get : ('a, exn) t -> 'a

(** [default d r] evaluates to [d] if [r] is [Bad] else [x] when [r] is
    [Ok x]
    @since 2.0
*)
val default: 'a -> ('a, _) t -> 'a

(** [map_default d f r] evaluates to [d] if [r] is [Bad] else [f x]
    when [r] is [Ok x]
    @since 2.0
*)
val map_default : 'b -> ('a -> 'b) -> ('a, _) t -> 'b

(** [is_ok (Ok _)] is [true], otherwise [false].
    @since 2.0
*)
val is_ok : ('a, 'b) t -> bool

(** [is_bad (Bad _)] is [true], otherwise [false]
    @since 2.0
*)
val is_bad : ('a, 'b) t -> bool

(** [is_exn e1 r] is [true] iff [r] is [Bad e2] with [e1=e2] *)
val is_exn : exn -> ('a, exn) t -> bool

(** Convert an [option] to a [result]
    @since 1.0 *)
val of_option: 'a option -> ('a, unit) t

(** Convert a [result] to an [option]
    @since 1.0 *)
val to_option: ('a, _) t -> 'a option


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
  val bind:    ('a, 'b) t -> ('a -> ('c, 'b) t) -> ('c, 'b) t

  (** as [bind] *)
  val ( >>= ): ('a, 'b) t -> ('a -> ('c, 'b) t) -> ('c, 'b) t

  (** Monadic return, just encapsulates the given value with Ok *)
  val return : 'a -> ('a, _) t
end

(** {6 Infix} *)

(** This infix module provides the operator [(>>=)] *)
module Infix : sig
  val ( >>= ): ('a, 'b) t -> ('a -> ('c, 'b) t) -> ('c, 'b) t
end

(** Print a result as Ok(x) or Bad(exn) *)
val print : ('b BatInnerIO.output -> 'a -> unit) -> 'b BatInnerIO.output -> ('a, exn) t -> unit
