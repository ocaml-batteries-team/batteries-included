(** Parser combinators*)

type loc =
    {
      offset : int;
      line   : int
    }

type position = 
  | Eof
  | Loc of loc

type failure =
    {
      labels  : string list;
      position: position;
    }

exception Failed of failure

(**
   A source for parsing.
*)
module Source :
sig
  type 'a t

  val of_lazy_list : 'a LazyList.t -> 'a t
  val of_enum      : 'a Enum.t     -> 'a t
  val pos_offset   : 'a t          -> ('a * loc) t
  val pos_newlines : ('a -> bool)  -> 'a t -> ('a * loc) t

  val set_pos      : 'a t -> ('a -> loc ) -> 'a t
  val set_compare  : 'a t -> ('a -> 'a -> int) -> 'a t
end

(** {6 Primitives} *)

type ('a, 'b) t
  (**A parser for elements of type ['a],
     producing elements of type ['b] *)

val eof : ('a, unit) t
  (**Accept the end of an enumeration.*)

val either : ('a, 'b) t list -> ('a, 'b) t
  (**Accept one of several parsers.*)

(*val risk : ('a, 'b) t list -> ('a, 'b) t
  (**Accept one of several parsers -- but without backtracking.*)*)

val ( <|> ) : ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t
  (**Accept one of two parsers*)

val maybe : ('a, 'b) t -> ('a, 'b option) t
  (**Accept an optional argument.*)

val ( ~? ): ('a, 'b) t -> ('a, 'b option) t
  (**As [maybe] *)

val bind : ('a, 'b) t -> ('b -> ('a, 'c) t ) -> ('a, 'c) t
  (**Monadic-style combination:

     [bind p f] results in a new parser which behaves as [p]
     then, in case of success, applies [f] to the result.*)

(*val compose: ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t*)
  (**Composition of two successive parsers.

     [compose p q] results in a new parser which feeds the
     consecutive results of [p] into [q]. In case of error,
     positions are taken from [p].*)


val ( >>= ) : ('a, 'b) t -> ('b -> ('a, 'c) t ) -> ('a, 'c) t
  (** As [bind]*)

val ( >>> ) : ('a, _) t -> ('a, 'b) t -> ('a, 'b) t

val cons : ('a, 'b) t -> ('a, 'b list) t -> ('a, 'b list) t
  (** [cons p q] applies parser [p] then parser [q] and
      conses the results into a list.*)

val ( >::) : ('a, 'b) t -> ('a, 'b list) t -> ('a, 'b list) t
  (** As [cons] *)

val label: string -> ('a, 'b) t -> ('a, 'b) t
  (**Give a name to a parser, for debugging purposes.*)

val pos: ('a, position) t 
  (**Succeed and return the position of the parser*)

val loc: ('a, loc) t 
  (**Succeed and return the loc of the parser*)

val any: ('a, 'a) t
  (**Accept any singleton value.*)

val return: 'b -> ('a, 'b) t
  (**A parser which always succeds*)

val satisfy: ('a -> bool) -> ('a, 'a) t
  (**[satisfy p] accepts one value [p x] such that [p x = true]*)

val filter: ('b -> bool) -> ('a, 'b) t ->  ('a, 'b) t
  (**[filter f p] is only accepts values [x] such that [p]
     accepts [x] and [f (p x)] is [true]*)

val run: ('a, 'b) t -> 'a Source.t -> ('b, failure) Std.result
  (**[run p s] executes parser [p] on source [s]. In case of
     success, returns [Ok v], where [v] is the return value of [v].
     In case of failure, returns [Error f], with [f] containing
     details on the parsing error.*)

val enum_runs: ('a, 'b) t -> 'a Source.t -> 'b Enum.t
val list_runs: ('a, 'b) t -> 'a Source.t -> 'b LazyList.t
(*val source_map:('a, 'b) t -> 'a Source.t -> 'b Source.t*)



val fail: ('a, _) t
  (**Always fail, without consuming anything.*)

val lookahead: ('a, 'b) t -> ('a, 'b option) t
  (**[lookahead p] behaves as [maybe p] but without consuming anything*)



(** {6 Utilities} *)
val exactly : 'a -> ('a, 'a) t
  (**Accept exactly one singleton*)

val zero_plus : ?sep:('a, _) t -> ('a, 'b) t -> ('a, 'b list) t
  (**Accept a (possibly empty) list of expressions*)

val ( ~* ) : ('a, 'b) t -> ('a, 'b list) t
  (**As [zero_plus]*)

val one_plus :  ?sep:('a, _) t -> ('a, 'b) t -> ('a, 'b list) t
  (**Accept a (non-empty) list of expressions*)

val ( ~+ ) : ('a, 'b) t -> ('a, 'b list) t
  (**As [one_plus]*)

val times : int -> ('a, 'b) t -> ('a, 'b list) t
  (**[time n p] accepts a list of [n] expressions accepted by [p]*)

val ( ^^ ) : ('a, 'b) t -> int -> ('a, 'b list) t
  (**[p ^^ n] is the same thing as [times n p] *)

val map : ('b -> 'c) -> ('a, 'b) t ->  ('a, 'c) t
  (**Pass the (successful) result of some parser through a map.*)

val one_of : 'a list -> ('a, 'a) t
  (**Accept one of several values.
     Faster and more convenient than combining [satisfy] and [either].*)

val none_of : 'a list -> ('a, 'a) t
  (**Accept any value not in a list
     Faster and more convenient than combining [satisfy] and [either].*)

val range: 'a -> 'a -> ('a, 'a) t
  (**Accept any element from a given range.*)

val scan: ('a, _) t -> ('a, 'a list) t
  (**Use a parser to extract list of tokens, but return
     that list of tokens instead of whatever the original
     parser returned.*)

val sat: ('a -> bool) -> ('a, unit) t
  (** As [satisfy], but without result. *)

