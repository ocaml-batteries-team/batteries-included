open Std
(**
*)

type ('a, 'b) t = ('a, 'b) result 
    (** The type of a result*)

val catch: ('a -> 'b) -> 'a -> ('b, exn) result
  (** Execute a function and catch any exception as a [!result]*)

val of_option: 'a option -> ('a, unit) result
  (** Convert an [option] to a [result]*)

val to_option: ('a, _) result -> 'a option
  (** Convert a [result] to an [option]*)

val bind:    ('a, 'b) result -> ('a -> ('c, 'b) result) -> ('c, 'b) result
  (** Monadic composition.

      [bind r f] proceeds as [f x] if [r] is [Ok x], or returns [r] if
      [r] is an error.*)

val ( >>= ): ('a, 'b) result -> ('a -> ('c, 'b) result) -> ('c, 'b) result
  (** as [bind] *)

(** {6 Boilerplate code}*)
(** {7 S-Expressions}*)

val t_of_sexp : (Sexplib.Sexp.t -> 'a) -> (Sexplib.Sexp.t -> 'b) -> Sexplib.Sexp.t -> ('a, 'b) t
val sexp_of_t : ('a -> Sexplib.Sexp.t) -> ('b -> Sexplib.Sexp.t) -> ('a, 'b) t -> Sexplib.Sexp.t

