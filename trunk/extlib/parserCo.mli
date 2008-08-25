(** Parser combinators*)

type 'a state =
  | Eof
  | State of 'a

(**
   A source for parsing.
*)
module Source :
sig
  type ('a, 'b) t
    (** A source of elements of type ['a], with a user-defined state
	of type ['b] *)

  val get_state : ('a, 'b) t -> 'b state
(*  val set_state : ('a, 'b) t -> 'b -> unit*)
  val set_full_state : ('a, 'b) t -> 'c -> ('a  -> 'c -> 'c) -> ('a, 'c) t

  val of_lazy_list : 'a LazyList.t -> 'b -> ('a  -> 'b -> 'b) -> ('a, 'b) t
  val of_enum      : 'a Enum.t     -> 'b -> ('a  -> 'b -> 'b) -> ('a, 'b) t
  val of_lexer     : Lexing.lexbuf -> (string, (Lexing.position * Lexing.position)) t
    (**Create a source from a lexer, as implemented by OCamlLex.
       User states contain the start position and the end position of the lexeme.*)
end

(** {6 Primitives} *)

type ('a, 'b, 'c) t
  (**A parser for elements of type ['a], producing 
     elements of type ['b], with user-defined states
     of type ['c].*)

val eof : (_, unit, _) t
  (**Accept the end of an enumeration.*)

val either : ('a, 'b, 'c) t list -> ('a, 'b, 'c) t
  (**Accept one of several parsers.*)

(*val risk : ('a, 'b) t list -> ('a, 'b) t
  (**Accept one of several parsers -- but without backtracking.*)*)

val ( <|> ) : ('a, 'b, 'c) t -> ('a, 'b, 'c) t -> ('a, 'b, 'c) t
  (**Accept one of two parsers*)

val maybe : ('a, 'b, 'c) t -> ('a, 'b option, 'c) t
  (**Accept an optional argument.*)

val ( ~? ): ('a, 'b, 'c) t -> ('a, 'b option, 'c) t
  (**As [maybe] *)

val bind : ('a, 'b, 'c) t -> ('b -> ('a, 'd, 'c) t ) -> ('a, 'd, 'c) t
  (**Monadic-style combination:

     [bind p f] results in a new parser which behaves as [p]
     then, in case of success, applies [f] to the result.*)

(*val compose: ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t*)
  (**Composition of two successive parsers.

     [compose p q] results in a new parser which feeds the
     consecutive results of [p] into [q]. In case of error,
     positions are taken from [p].*)


val ( >>= ) : ('a, 'b, 'c) t -> ('b -> ('a, 'd, 'c) t ) -> ('a, 'd, 'c) t
  (** As [bind]*)

val ( >>> ) : ('a, _, 'c) t -> ('a, 'd, 'c) t  -> ('a, 'd, 'c) t
  (** As [bind], but ignoring the result *)

val cons : ('a, 'b, 'c) t -> ('a, 'b list, 'c) t -> ('a, 'b list, 'c) t
  (** [cons p q] applies parser [p] then parser [q] and
      conses the results into a list.*)

val ( >::) : ('a, 'b, 'c) t -> ('a, 'b list, 'c) t -> ('a, 'b list, 'c) t
  (** As [cons] *)

val label: string -> ('a, 'b, 'c) t -> ('a, 'b, 'c) t
  (**Give a name to a parser, for debugging purposes.*)

val state: (_, 'b state, 'b) t 
  (**Succeed and return the state of the parser*)

val any: ('a, 'a, _) t
  (**Accept any singleton value.*)

val return: 'b -> (_, 'b, _) t
  (**A parser which always succeds*)

val satisfy: ('a -> bool) -> ('a, 'a, _) t
  (**[satisfy p] accepts one value [p x] such that [p x = true]*)

val filter: ('b -> bool) -> ('a, 'b, 'c) t ->  ('a, 'b, 'c) t
  (**[filter f p] is only accepts values [x] such that [p]
     accepts [x] and [f (p x)] is [true]*)

val run: ('a, 'b, 'c) t -> ('a, 'c) Source.t -> ('b, 'c state * string list) Std.result
  (**[run p s] executes parser [p] on source [s]. In case of
     success, returns [Ok v], where [v] is the return value of [v].
     In case of failure, returns [Error f], with [f] containing
     details on the parsing error.*)

(*
val enum_runs: ('a, 'b) t -> ('a, 'c) Source.t -> 'b Enum.t
val list_runs: ('a, 'b) t -> ('a, 'c) Source.t -> 'b LazyList.t
*)



val fail: (_, _, _) t
  (**Always fail, without consuming anything.*)

val lookahead: ('a, 'b, 'c) t -> ('a, 'b option, 'c) t
  (**[lookahead p] behaves as [maybe p] but without consuming anything*)



(** {6 Utilities} *)
val exactly : 'a -> ('a, 'a, 'c) t
  (**Accept exactly one singleton.*)

val zero_plus : ?sep:('a, _, 'c) t -> ('a, 'b, 'c) t -> ('a, 'b list, 'c) t
  (**Accept a (possibly empty) list of expressions.*)

val ( ~* ) : ('a, 'b, 'c) t -> ('a, 'b list, 'c) t
  (**As [zero_plus] without arguments.*)

val one_plus :  ?sep:('a, _, 'c) t -> ('a, 'b, 'c) t -> ('a, 'b list, 'c) t
  (**Accept a (non-empty) list of expressions*)

val ( ~+ ) : ('a, 'b, 'c) t -> ('a, 'b list, 'c) t
  (**As [one_plus]*)

val times : int -> ('a, 'b, 'c) t -> ('a, 'b list, 'c) t
  (**[time n p] accepts a list of [n] expressions accepted by [p]*)

val ( ^^ ) : ('a, 'b, 'c) t -> int -> ('a, 'b list, 'c) t
  (**[p ^^ n] is the same thing as [times n p] *)

val post_map : ('b -> 'c) -> ('a, 'b, 'd) t ->  ('a, 'c, 'd) t
  (**Pass the (successful) result of some parser through a map.*)

val one_of : 'a list -> ('a, 'a, 'c) t
  (**Accept one of several values.
     Faster and more convenient than combining [satisfy] and [either].*)

val none_of : 'a list -> ('a, 'a, 'c) t
  (**Accept any value not in a list
     Faster and more convenient than combining [satisfy] and [either].*)

val range: 'a -> 'a -> ('a, 'a, 'c) t
  (**Accept any element from a given range.*)

val scan: ('a, _, 'c) t -> ('a, 'a list, 'c) t
  (**Use a parser to extract list of tokens, but return
     that list of tokens instead of whatever the original
     parser returned.*)

val sat: ('a -> bool) -> ('a, unit, 'c) t
  (** As [satisfy], but without result. *)

