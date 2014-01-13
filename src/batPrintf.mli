(*
 * BatPrintf - Extended Printf module
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

open BatInnerIO

(** Formatted output functions (also known as unparsing).

    @author Xavier Leroy
    @author Pierre Weiss
    @author David Teller
*)

(**
   {6 General overview}

   The functions of this module produce output according to a
   {!Pervasives.format}, as described below. Some functions write to
   the standard output (i.e. the screen), some to error channels,
   some to strings or to buffers, or some to abstract outputs.

   {b Note} The types used in this module are confusing at first.
   If you are a beginner, you should probably ignore them in a
   first time and concentrate on formats.

   For a first explanation, we will concentrate on function {!printf}.
   As all the functions in this module, the behavior of {!printf} is
   dictated by a {!format}. This format is a string, composed of
   regular text and directives, and which dictates how to interpret
   the other arguments passed to the function. Every directive starts
   with character [%].  The most common directive is [%s], which
   serves to display a string, something quite useful for
   pretty-printing or translation.  Anther common directive is [%i],
   which serves to display an integer.

   For instance, ["foobar"] is a format with no directive. Calling
   [printf "foobar"] prints ["foobar"] on the screen and returns
   [()]. On the other hand, ["%s"] is a format with one directive for
   printing strings. [printf "%s"] does nothing yet but returns a
   function with type [string -> unit]. In turn, [printf "%s"
   "foobar"] prints ["foobar"] on the screen and returns [()]. The
   main interest of this module is that directives may be combined
   together and with text, to allow more complex printing. For instance
   [printf "(%s)\n"] is a function with type [string -> unit] which,
   when passed string ["foobar"] prints ["(foobar)"] and ends the
   line. Similarly, [printf "Here's the result: %s.\n\tComputation
   took %i seconds.\n" "foobar" 5] prints
   {[Here's the result: foobar
         Computation took 5 seconds.]}

   Note that [\n] (the newline character) and [\t] (the tabulation)
   are not specific to this module but rather part of the conventions
   on characters strings in OCaml.

   Other directives and functions make this module extremely useful
   for printing, pretty-printing and translation of messages to
   the user's language. For more information, see the documentation
   of {!format} and the various functions.*)



(**
   {6 Formats}
*)

type ('a, 'b, 'c) t = ('a, 'b, 'c) Pervasives.format
(**
   The format to use for displaying the various arguments passed to the function.

   Syntactically, the format is a character string which contains two types
   of objects: plain characters, which are simply copied, and directives,
   each of which causes the conversion and printing of arguments.

   {7 Simple directives}

   All directives start with the [%] character. In their simplest form,
   a directive is [%] followed by exactly one character:

   - [%d], [%i], [%n], [%l], [%L], or [%N]: convert an integer argument to
     signed decimal.
   - [%u]: convert an integer argument to unsigned decimal.
   - [%x]: convert an integer argument to unsigned hexadecimal,
     using lowercase letters.
   - [%X]: convert an integer argument to unsigned hexadecimal,
     using uppercase letters.
   - [%o]: convert an integer argument to unsigned octal.
   - [%s]: insert a string argument.
   - [%S]: insert a string argument in OCaml syntax (double quotes, escapes).
   - [%c]: insert a character argument.
   - [%C]: insert a character argument in OCaml syntax (single quotes, escapes).
   - [%f]: convert a floating-point argument to decimal notation,
     in the style [dddd.ddd].
   - [%F]: convert a floating-point argument to OCaml syntax ([dddd.]
     or [dddd.ddd] or [d.ddd e+-dd]).
   - [%e] or [%E]: convert a floating-point argument to decimal notation,
     in the style [d.ddd e+-dd] (mantissa and exponent).
   - [%g] or [%G]: convert a floating-point argument to decimal notation,
     in style [%f] or [%e], [E] (whichever is more compact).
   - [%B]: convert a boolean argument to the string [true] or [false]
   - [%b]: convert a boolean argument (for backward compatibility; do not
     use in new programs).
   - [%ld], [%li], [%lu], [%lx], [%lX], [%lo]: convert an [int32] argument to
     the format specified by the second letter (decimal, hexadecimal, etc).
   - [%nd], [%ni], [%nu], [%nx], [%nX], [%no]: convert a [nativeint] argument to
     the format specified by the second letter.
   - [%Ld], [%Li], [%Lu], [%Lx], [%LX], [%Lo]: convert an [int64] argument to
     the format specified by the second letter.
   - [!]: take no argument and flush the output.
   - [%]: take no argument and output one [%] character.
   - [,]: the no-op delimiter for conversion specifications


   {7 Unparsers}

   - [%a]: user-defined printer. Typically, this printer corresponds to two
     arguments: a printing function [f], with type ['a output -> 'c -> unit]
     and the item [x] you want to print, with type ['c]. Item [x] will
     be printing by calling [f out x], where [out] is the output you are
     currently using -- if you are calling {!printf}, this output is
     the standard output (i.e. the screen), if you are calling {!eprintf},
     this will be the error channel, if you are calling {!fprintf}, this
     will be the output you provided yourself, etc. More generally, if your
     {!format} has type [('a, 'b, 'd) format] or [('a, 'b, 'd, 'e) format4],
     the printing function [f] must have type ['b -> 'c -> 'd], where
     [x] has type ['d].
   - [%t]: same as [%a] but takes only a printing function [f],
     without an item. If your {!format} has type [('a, 'b, 'd) format]
     or [('a, 'b, 'd, 'e) format4], function [f] must have type
     ['b -> 'd].

   {7 Formatting formats}
   - [%\{ fmt %\}]: convert a {!format} to a string. The format argument
     must have the same type as the internal format string [fmt].
     In other words, [printf "%\{ %s %\}"] accepts an argument
     whose type must be the same as that of format ["%s"], and
     prints that format argument as if it were a character string.
   - [%( fmt %)]: format string substitution. Takes a format string
     argument and substitutes it to the internal format string [fmt]
     to print following arguments. The argument must have the same
     type as [fmt]. [printf "%\{ %s %\}"] accepts an argument
     whose type must be the same as that of format ["%s"], and
     uses that argument to print the following arguments.

   {7 Additional options}
   The general format of directives is

   [% \[flags\] \[width\] \[.precision\] type]

   [type] is one of [d], [i], [n], [l], [L], [N], [u], [x] ...,
   [( fmt %)] and behaves as explained above.

   The optional [flags] are:
   - [-]: left-justify the output (default is right justification).
   - [0]: for numerical conversions, pad with zeroes instead of spaces.
   - [+]: for numerical conversions, prefix number with a [+] sign if positive.
   - space: for numerical conversions, prefix number with a space if positive.
   - [#]: request an alternate formatting style for numbers.

   The optional [width] is an integer indicating the minimal
   width of the result. For instance, [%6d] prints an integer,
   prefixing it with spaces to fill at least 6 characters.

   The optional [precision] is a dot [.] followed by an integer
   indicating how many digits follow the decimal point in the [%f],
   [%e], and [%E] conversions. For instance, [%.4f] prints a [float] with
   4 fractional digits.

   The integer in a [width] or [precision] can also be specified as
   [*], in which case an extra integer argument is taken to specify
   the corresponding [width] or [precision]. This integer argument
   precedes immediately the argument to print.
   For instance, [%.*f] prints a [float] with as many fractional
   digits as the value of the argument given before the float.
*)


(** {6 Common functions}*)

val printf: ('b, 'a output, unit) t -> 'b
(**The usual [printf] function, prints to the standard output {!stdout}, i.e. normally
   to the screen. If you are lost, this is probably the function you're looking for.*)

val eprintf: ('b, 'a output, unit) t -> 'b
(**The usual [eprintf] function, prints to the standard error output {!stderr}, used
   to display warnings and errors. Otherwise identical to {!printf}.*)

val sprintf:  ('a, unit, string) t -> 'a
(** A function which doesn't print its result but returns it as a string. Useful
    for building messages, for translation purposes or for display in a window,
    for instance.

    While this function is quite convenient, don't abuse it to create very large
    strings such as files, that's not its role. For this kind of usage, prefer
    the more modular and usually faster {!fprintf}.

    Note that any function called with [%a] should return strings, i.e.
    should have type [unit -> string].*)

val sprintf2: ('a, 'b output, unit, string) format4 -> 'a
(** A function which doesn't print its result but returns it as a string. Useful
    for building messages, for translation purposes or for display in a window,
    for instance.

    While this function is quite convenient, don't abuse it to create very large
    strings such as files, that's not its role. For this kind of usage, prefer
    the more modular and usually faster {!fprintf}.
    Note that any function called with [%a] should be able to print its result,
    i.e. should have type ['b output -> unit].

    Warning: a partial application of this function can only be used once,
    because the {!BatInnerIO.output} that it uses is closed afterwards.
    Example: [let f = sprintf2 "%a" Int.print in [f 1; f 2]] will fail. *)


(** {6 General functions}*)

val fprintf: 'a output -> ('b, 'a output, unit) t -> 'b
(**General function. This function prints to any output. Typically,
   if you are attempting to build a large output such as a file,
   this is probably the function you are looking for. If you are
   writing a pretty-printer, this is probably the function you are
   looking for. If you are you are looking for a function to use for
   argument [%a] with {!printf}, {!eprintf}, {!sprintf2},
   {!ifprintf}, {!bprintf2}, {!kfprintf}, {!ksprintf2}, {!kbprintf2}
   or any other function with type [(_, _ output, unit) format] or
   [(_, _ output, unit, _) format4], this is also probably the
   function you are looking for.*)


val ifprintf: _        -> ('b, 'a output, unit) t -> 'b
(**As {!fprintf} but doesn't actually print anything.
   Sometimes useful for debugging.*)

val bprintf: Buffer.t  -> ('a, Buffer.t, unit) t -> 'a
(**As {!fprintf}, but with buffers instead of outputs.
   In particular, any unparser called with [%a] should
   write to a buffer rather than to an output*)

val bprintf2: Buffer.t  -> ('b, 'a output, unit) t -> 'b
(**As {!printf} but writes to a buffer instead
   of printing to the output. By opposition to
   {!bprintf}, only the result is changed with
   respect to {!printf}, not the inner workings.*)

(**{6 Functions with continuations}*)

val kfprintf : ('a output -> 'b) -> 'a output -> ('c, 'a output, unit, 'b) format4 -> 'c
(**Same as [fprintf], but instead of returning immediately, passes the [output] to its first
   argument at the end of printing.*)

val ksprintf: (string -> 'a) -> ('b, unit, string, 'a) format4 -> 'b
(** Same as [sprintf] above, but instead of returning the string,
    passes it to the first argument. *)
val ksprintf2: (string -> 'b) -> ('c, 'a output, unit, 'b) format4 -> 'c
(** Same as [sprintf2] above, but instead of returning the string,
    passes it to the first argument. *)

val kbprintf : (Buffer.t -> 'a) ->
  Buffer.t -> ('b, Buffer.t, unit, 'a) format4 -> 'b
(** Same as [bprintf], but instead of returning immediately,
    passes the buffer to its first argument at the end of printing. *)

val kbprintf2 : (Buffer.t -> 'b) ->  Buffer.t -> ('c, 'a output, unit, 'b) format4 -> 'c
(** Same as [bprintf2], but instead of returning immediately,
    passes the buffer to its first argument at the end of printing.*)

val kprintf : (string -> 'a) -> ('b, unit, string, 'a) format4 -> 'b
(** @deprecated This is a deprecated synonym for [ksprintf]. *)


(**
   {6 About formats}

   You only need to read this if you intend to create your new printf-like functions,
   which happens generally by toying with {!mkprintf}.


   {7 Format4}

   [('a, 'b, 'c, 'd) format4] is the type of arguments for
   [printf]-style functions such that
   - ['a] is the type of arguments, with a return type of ['d]
   {ul
   {- if your format looks like ["%s"], ['a] is [string -> 'd]}
   {- if your format looks like ["%s%s"], ['a] is [string -> string -> 'd]}
   {- ...}
   }
   - ['b] is the type of the first argument given to unparsers
   (i.e. functions introduced with [%a] or [%t])
   {ul
   {- if your unparsers take a [unit] argument, ['b] should be
   [unit]}
   {- if your unparsers take a [string output], ['b] should be
   [string output]}
   {- ...}
   }
   - ['c] is the {b final} return type of unparsers
   {ul
   {- if you have an unparser introduced with [%t] and its result
   has type [unit], ['c] should be [unit]}
   {- if you have an unparser introduced with [%a] and its type is
   [string output -> string -> unit], ['c] should be [unit]}
   {- ...}
   }
   - ['d] is the final return value of the function once all
   arguments have been printed

   {7 Format}
   [('a, 'b, 'c) format] or [('a, 'b, 'c) t] is just a shortcut for [('a, 'b, 'c, 'c) format4].

   {7 Important}
   Note that {!Obj.magic} is involved behind this, so be careful.
*)

(**/**)

(* For OCaml system internal use only. Don't call directly. *)

module CamlinternalPr : sig

  module Sformat : sig
    type index;;

    val index_of_int : int -> index;;
    external int_of_index : index -> int = "%identity";;
    external unsafe_index_of_int : int -> index = "%identity";;

    val succ_index : index -> index;;

    val sub : ('a, 'b, 'c, 'd, 'e, 'f) format6 -> index -> int -> string;;
    val to_string : ('a, 'b, 'c, 'd, 'e, 'f) format6 -> string;;
    external length : ('a, 'b, 'c, 'd, 'e, 'f) format6 -> int
      = "%string_length";;
    external get : ('a, 'b, 'c, 'd, 'e, 'f) format6 -> int -> char
      = "%string_safe_get";;
    external unsafe_to_string : ('a, 'b, 'c, 'd, 'e, 'f) format6 -> string
      = "%identity";;
    external unsafe_get : ('a, 'b, 'c, 'd, 'e, 'f) format6 -> int -> char
      = "%string_unsafe_get";;

  end;;

  module Tformat : sig

    type ac = {
      mutable ac_rglr : int;
      mutable ac_skip : int;
      mutable ac_rdrs : int;
    };;

    val ac_of_format : ('a, 'b, 'c, 'd, 'e, 'f) format6 -> ac;;

    val sub_format :
      (('a, 'b, 'c, 'd, 'e, 'f) format6 -> int) ->
      (('a, 'b, 'c, 'd, 'e, 'f) format6 -> int -> char -> int) ->
      char ->
      ('a, 'b, 'c, 'd, 'e, 'f) format6 ->
      int ->
      int

    val summarize_format_type : ('a, 'b, 'c, 'd, 'e, 'f) format6 -> string

    val scan_format : ('a, 'b, 'c, 'd, 'e, 'f) format6 ->
      'g array ->
      Sformat.index ->
      int ->
      (Sformat.index -> string -> int -> 'h) ->
      (Sformat.index -> 'i -> 'j -> int -> 'h) ->
      (Sformat.index -> 'k -> int -> 'h) ->
      (Sformat.index -> int -> 'h) ->
      (Sformat.index -> ('l, 'm, 'n, 'o, 'p, 'q) format6 -> int -> 'h) ->
      'h

    val kapr :
      (('a, 'b, 'c, 'd, 'e, 'f) format6 -> Obj.t array -> 'g) ->
      ('a, 'b, 'c, 'd, 'e, 'f) format6 ->
      'g

  end;;

end;;

(**/**)
