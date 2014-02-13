(*
 * ParserCo - A simple monadic parser combinator library
 * Copyright (C) 2008 David Teller
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

(** A simple parser combinator library.

    This module permits the simple definition of highly modular, dynamic
    parsers with unlimited backtracking. It may be used to parse any
    form of enumeration, including regular text, latin-1 text, bits, etc.

    This library is vastly more powerful than {!Lexing}, {!Str}, {!Parsing}
    or {!Scanf}. It is also considerably slower.

    Module {!CharParser} contains pre-defined parsers to deal
    specifically with latin-1 text. Module {!Genlex} contains a number
    of pre-defined parsers to deal specifically with programming
    languages.

    {b Note} This library is still very rough and needs much testing.
*)

(**
   {6 Base definitions}
*)

(**The current state of the parser.

   The actual set of states is defined by the user. States are
   typically used to convey informations, such as position in the file
   (i.e. line number and character).

*)
type 'a state =
  | Eof         (**The end of the source has been reached.*)
  | State of 'a

type 'a report = Report of ('a state * string * 'a report) list
(**The final result of parsing*)

(** A source for parsing.  Unless you are parsing from exotic sources,
    you will probably not need to use this module directly. Rather, use
    {!CharParser.source_of_string} or {!CharParser.source_of_enum}.
*)
module Source :
sig
  type ('a, 'b) t
  (** A source of elements of type ['a], with a user-defined state
      of type ['b] *)

  val get_state : ('a, 'b) t -> 'b state
  val set_full_state : ('a, 'b) t -> 'c -> ('a  -> 'c -> 'c) -> ('a, 'c) t

  val of_enum      : 'a BatEnum.t     -> 'b -> ('a  -> 'b -> 'b) -> ('a, 'b) t

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

(*val compose: ('a, 'b, 'c) t -> ('b, 'd, 'c) t -> ('a, 'd, 'c) t
  (**Composition of two successive parsers.

     [compose p q] results in a new parser which feeds the
     consecutive results of [p] into [q]. In case of error,
     positions are taken from [p].*)*)


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

val suspend : ('a, 'b, 'c) t -> ('a, (unit -> ('b, 'c report) BatPervasives.result), 'c) t
(**[suspend s] returns the state of the parser in a form that can be
   resumed by calling the returned function. evaluation will resume
   from parser s *)

val run: ('a, 'b, 'c) t -> ('a, 'c) Source.t -> ('b, 'c report) BatPervasives.result
(**[run p s] executes parser [p] on source [s]. In case of
   success, returns [Ok v], where [v] is the return value of [p].
   In case of failure, returns [Bad f], with [f] containing
   details on the parsing error.*)


(*val enum_runs: ('a, 'b, 'c) t -> ('a, 'c) Source.t -> 'b BatEnum.t
  val list_runs: ('a, 'b, 'c) t -> ('a, 'c) Source.t -> 'b LazyList.t*)




val fail: (_, _, _) t
(**Always fail, without consuming anything.*)

val fatal: (_, _, _) t

val lookahead: ('a, 'b, 'c) t -> ('a, 'b option, 'c) t
(**[lookahead p] behaves as [maybe p] but without consuming anything*)



(** {6 Utilities} *)
(** {7 Singletons} *)

val exactly : 'a -> ('a, 'a, 'c) t
(**Accept exactly one singleton.*)

val one_of : 'a list -> ('a, 'a, 'c) t
(**Accept one of several values.
   Faster and more convenient than combining [satisfy] and [either].*)

val none_of : 'a list -> ('a, 'a, 'c) t
(**Accept any value not in a list
   Faster and more convenient than combining [satisfy] and [either].*)

val range: 'a -> 'a -> ('a, 'a, 'c) t
(**Accept any element from a given range.*)

(** {7 Repetitions} *)

val zero_plus : ?sep:('a, _, 'c) t -> ('a, 'b, 'c) t -> ('a, 'b list, 'c) t
(**Accept a (possibly empty) list of expressions.*)

val ignore_zero_plus : ?sep:('a, _, 'c) t -> ('a, _, 'c) t -> ('a, unit, 'c) t
(**Ignore a (possibly empty) list of expressions.
   Optimized version of [zero_plus], for use when the
   list of expressions is unimportant.*)

val ( ~* ) : ('a, 'b, 'c) t -> ('a, 'b list, 'c) t
(**As [zero_plus] without arguments.*)

val one_plus :  ?sep:('a, _, 'c) t -> ('a, 'b, 'c) t -> ('a, 'b list, 'c) t
(**Accept a (non-empty) list of expressions*)

val ignore_one_plus : ?sep:('a, _, 'c) t -> ('a, _, 'c) t -> ('a, unit, 'c) t
(**Ignore a (non-empty) list of expressions.
   Optimized version of [one_plus], for use when the
   list of expressions is unimportant.*)

val ( ~+ ) : ('a, 'b, 'c) t -> ('a, 'b list, 'c) t
(**As [one_plus]*)

val times : int -> ('a, 'b, 'c) t -> ('a, 'b list, 'c) t
(**[times n p] accepts a list of [n] expressions accepted by [p]*)

val ( ^^ ) : ('a, 'b, 'c) t -> int -> ('a, 'b list, 'c) t
(**[p ^^ n] is the same thing as [times n p] *)

val must: ('a, 'b, 'c) t -> ('a, 'b, 'c) t
(**Prevent backtracking.*)

val should: ('a, 'b, 'c) t -> ('a, 'b, 'c) t
(**Prevent backtracking.*)


(** {7 Maps}*)

val post_map : ('b -> 'c) -> ('a, 'b, 'd) t ->  ('a, 'c, 'd) t
(**Pass the (successful) result of some parser through a map.*)

val source_map: ('a, 'b, 'c) t -> ('a, 'c) Source.t -> ('b, 'c) Source.t

val scan: ('a, _, 'c) t -> ('a, 'a list, 'c) t
(**Use a parser to extract list of tokens, but return
   that list of tokens instead of whatever the original
   parser returned.*)

(** {7 Others}*)

val sat: ('a -> bool) -> ('a, unit, _) t
(**[satisfy p] accepts one value [p x] such that [p x = true]*)

val debug_mode : bool ref
(**If set to [true], debugging information will be printed to the standard error.*)

(** {6 Infix submodule regrouping all infix operators} *)
module Infix : sig
  val ( <|> ) : ('a, 'b, 'c) t -> ('a, 'b, 'c) t -> ('a, 'b, 'c) t
  val ( ~? ): ('a, 'b, 'c) t -> ('a, 'b option, 'c) t
  val ( >>= ) : ('a, 'b, 'c) t -> ('b -> ('a, 'd, 'c) t ) -> ('a, 'd, 'c) t
  val ( >>> ) : ('a, _, 'c) t -> ('a, 'd, 'c) t  -> ('a, 'd, 'c) t
  val ( >::) : ('a, 'b, 'c) t -> ('a, 'b list, 'c) t -> ('a, 'b list, 'c) t
  val ( ~* ) : ('a, 'b, 'c) t -> ('a, 'b list, 'c) t
  val ( ~+ ) : ('a, 'b, 'c) t -> ('a, 'b list, 'c) t
  val ( ^^ ) : ('a, 'b, 'c) t -> int -> ('a, 'b list, 'c) t
end
