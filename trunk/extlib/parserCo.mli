(** Parser combinators*)

type actual_loc     =
    {
      offset : int;
      line   : int
    }

type loc =
  | Eof 
  | Loc of actual_loc

type failure =
    {
      labels : string list;
      loc    : loc
    }

(** {6 Primitives} *)

type ('a, 'b) t
  (**A parser for elements of type ['a],
     producing elements of type ['b] *)

val eof : ('a, unit) t
  (**Accept the end of an enumeration.*)

val either : ('a, 'b) t list -> ('a, 'b) t
  (**Accept one of several parsers.*)

val ( <|> ) : ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t
  (**Accept one of two parsers*)

val maybe : ('a, 'b) t -> ('a, 'b option) t
  (**Accept an optional argument.*)

val ( ~? ): ('a, 'b) t -> ('a, 'b option) t
  (**As [maybe] *)

val bind : ('a, 'b) t -> ('b -> ('a, 'c) t ) -> ('a, 'c) t
  (**Monadic-style combination:

     [bind p f] results in a new parser which behaves as [p]
     then, in case of success and applies [f] to the result.*)

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

val any: ('a, 'a) t
  (**Accept any singleton value.*)

val return: 'b -> ('a, 'b) t
  (**A parser which always succeds*)

val satisfy: ('a -> bool) -> ('a, 'a) t
  (**[satisfy p] accepts one value [p x] such that [p x = true]*)

val filter: ('b -> bool) -> ('a, 'b) t ->  ('a, 'b) t
  (**[filter f p] is only accepts values [x] such that [p]
     accepts [x] and [f (p x)] is [true]*)

val run: ('a, 'b) t -> ?newline:'a -> 'a Enum.t -> 'b
  (**[run p ~newline:nl e] runs parser [p] on a source [e], using [nl] as
     a line delimitter, and returns the first success.
     In case of error, raises [Failure].*)

val run_filter: ('a, 'b) t -> ?newline:'a -> 'a Enum.t -> 'b Enum.t
  (** [run_filter p ~newline:nl e] runs parser [p] on a source [e], using
      [nl] as a line delimitter, and returns consecutive successes as
      an enumeration. *)

val run_filter_list: ('a, 'b) t -> ?newline:'a -> 'a LazyList.t -> 'b LazyList.t
  (** [run_filter p ~newline:nl e] runs parser [p] on a source [e], using
      [nl] as a line delimitter, and returns consecutive successes as
      an enumeration. *)

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

