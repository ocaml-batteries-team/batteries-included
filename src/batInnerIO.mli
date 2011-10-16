(* 
 * BatInnerIO - Abstract input/output (inner module)
 * Copyright (C) 2003 Nicolas Cannasse
 *               2008 David Teller
 *               2008 Philippe Strauss
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
   Core of the BatIO module.

   This module contains the core definitions of {!BatIO}, so as to avoid circular
   dependencies between modules which only need simple functions of {!BatIO} and
   that module itself.
   
   Don't use this module, use {!BatIO}.

   @author Nicolas Cannasse
   @author David Teller
   @author Philippe Strauss
   @author Edgar Friendly
*)

type input
type 'a output

exception No_more_input
(** This exception is raised when reading on an input with the [read] or
  [nread] functions while there is no available token to read. *)

exception Input_closed
(** This exception is raised when reading on a closed input. *)

exception Output_closed
(** This exception is raised when reading on a closed output. *)

val read : input -> char
(** Read a single char from an input or raise [No_more_input] if
  no input available. *)

val read_all : input -> string
(** read all the contents of the input until [No_more_input] is raised. *)

val pipe : unit -> input * unit output
(** Create a pipe between an input and an ouput. Data written from
  the output can be read from the input. *)

val nread : input -> int -> string
(** [nread i n] reads a string of size up to [n] from an input.
  The function will raise [No_more_input] if no input is available.
  It will raise [Invalid_argument] if [n] < 0. *)

val really_nread : input -> int -> string
(** [really_nread i n] reads a string of exactly [n] characters
    from the input. Raises [No_more_input] if at least [n] characters are
    not available. Raises [Invalid_argument] if [n] < 0. *)

val input : input -> string -> int -> int -> int
(** [input i s p l] reads up to [l] characters from the given input, storing
  them in string [s], starting at character number [p]. It returns the actual
  number of characters read or raise [No_more_input] if no character can be
  read. It will raise [Invalid_argument] if [p] and [l] do not designate a
  valid substring of [s]. *)

val really_input : input -> string -> int -> int -> int
(** [really_input i s p l] reads exactly [l] characters from the given input,
  storing them in the string [s], starting at position [p]. For consistency with
  {!BatIO.input} it returns [l]. Raises [No_more_input] if at [l] characters are
  not available. Raises [Invalid_argument] if [p] and [l] do not designate a
  valid substring of [s]. *)

val close_in : input -> unit
(** Close the input. It can no longer be read from. *)

(*val auto_close_in : input -> input
(** Create a new channel which will close automatically once there is nothing
    left to read.*)*)

val write : 'a output -> char -> unit
(** Write a single char to an output. *)

val nwrite : 'a output -> string -> unit
(** Write a string to an output. *)

val write_buf: 'a output -> Buffer.t -> unit
(** Write the contents of a buffer to an output.*)

val output : 'a output -> string -> int -> int -> int
(** [output o s p l] writes up to [l] characters from string [s], starting at
  offset [p]. It returns the number of characters written. It will raise
  [Invalid_argument] if [p] and [l] do not designate a valid substring of [s]. *)

val really_output : 'a output -> string -> int -> int -> int
(** [really_output o s p l] writes exactly [l] characters from string [s] onto
  the the output, starting with the character at offset [p]. For consistency with
  {!BatIO.output} it returns [l]. Raises [Invalid_argument] if [p] and [l] do not
  designate a valid substring of [s]. *)

val flush : 'a output -> unit
(** Flush an output. *)

val flush_all : unit -> unit
(** Flush all outputs. *)

val close_out : 'a output -> 'a
(** Close the output and return its accumulator data.
    It can no longer be written. *)

val close_all : unit -> unit
(** Close all outputs.
    Ignore errors.*)

val input_string : string -> input
(** Create an input that will read from a string. *)

val output_string : unit -> string output
(** Create an output that will write into a string in an efficient way.
  When closed, the output returns all the data written into it. *)

val output_buffer : Buffer.t -> string output
(** Create an output that will append its results at the end of a buffer
    in an efficient way. Closing  returns the whole contents of the buffer
    -- the buffer remains usable.*)

val on_close_out : 'a output -> ('a output -> unit) -> unit
  (**
     Register a function to be triggered just before an output is closed.
  *)

val create_in :
  read:(unit -> char) ->
  input:(string -> int -> int -> int) -> 
  close:(unit -> unit) -> input
(** Fully create an input by giving all the needed functions. 

    {b Note} Do {e not} use this function for creating an input
    which reads from one or more underlying inputs. Rather, use
    {!wrap_in}.
*)

val inherit_in:
  ?read:(unit -> char) ->
  ?input:(string -> int -> int -> int) -> 
  ?close:(unit -> unit) -> 
  input -> input
(**
   Simplified and optimized version of {!wrap_in} whenever only
   one input appears as dependency.
*)


val wrap_in :
  read:(unit -> char) ->
  input:(string -> int -> int -> int) -> 
  close:(unit -> unit) -> 
  underlying:(input list) ->
  input
(** Fully create an input reading from other inputs by giving all the needed functions. 

    This function is a more general version of {!create_in}
    which also handles dependency management between inputs.
*)




val create_out :
  write:(char -> unit) ->
  output:(string -> int -> int -> int) ->   
  flush:(unit -> unit) -> 
  close:(unit -> 'a) -> 
  'a output
(** 
    Fully create an output by giving all the needed functions.

    @param write  Write one character to the output (see {!write}).
    @param output Write a (sub)string to the output (see {!output}).
    @param flush  Flush any buffers of this output  (see {!flush}).
    @param close  Close this output. The output will be automatically
    flushed.

    {b Note} Do {e not} use this function for creating an output which
    writes to one or more underlying outputs. Rather, use {!wrap_out}.
*)

val inherit_out:
  ?write:(char -> unit) ->
  ?output:(string -> int -> int -> int) -> 
  ?flush:(unit -> unit) ->
  ?close:(unit -> unit) -> 
  _ output -> unit output
(**
   Simplified and optimized version of {!wrap_out} whenever only
   one output appears as dependency.
*)


val wrap_out :
  write:(char -> unit)         ->
  output:(string -> int -> int -> int) ->   
  flush:(unit -> unit)         -> 
  close:(unit -> 'a)           -> 
  underlying:('b output list)  -> 
  'a output
(**
   Fully create an output that writes to one or more underlying outputs.

   This function is a more general version of {!create_out},
   which also handles dependency management between outputs.

   To illustrate the need for dependency management, let us consider
   the following values:
   - an output [out]
   - a function [f : _ output -> _ output], using {!create_out} to
   create a new output for writing some data to an underyling
   output (for instance, a function comparale to {!tab_out} or a
   function performing transparent compression or transparent
   traduction between encodings)

   With these values, let us consider the following scenario
   - a new output [f out] is created
   - some data is written to [f out] but not flushed
   - output [out] is closed, perhaps manually or as a consequence
   of garbage-collection, or because the program has ended
   - data written to [f out] is flushed.

   In this case, data reaches [out] only after [out] has been closed,
   which violates the protocol.  Despite appearances, it is quite easy
   to reach such situation, especially in short programs.

   The solution is to use [wrap_out] rather than [create_out] in [f].
   Specifying that [f out] writes on [out] will then let the run-time
   flush and close [f out] when [out] is closed for any reason, which
   in turn avoids the issue.

   @param write  Write one character to the output (see {!write}).
   @param output Write a (sub)string to the output (see {!output}).
   @param flush  Flush any buffers of this output  (see {!flush}).
   @param close  Close this output. The output will be automatically
   flushed.
   @param underlying The list of outputs to which the new output will
   write.

   {b Note} Function [close] should {e not} close [underlying]
   yourself. This is a common mistake which may cause sockets or
   standard output to be closed while they are still being used by
   another part of the program.
*)

val default_buffer_size : int
  (**The default size of buffers.*)


(** {6 Binary files API}

	Here is some API useful for working with binary files, in particular
	binary files generated by C applications. By default, encoding of
	multibyte integers is low-endian. The BigEndian module provide multibyte
	operations with other encoding.
*)

exception Overflow of string
(** Exception raised when a read or write operation cannot be completed. *)

val read_byte : input -> int
(** Read an unsigned 8-bit integer. *)

val read_signed_byte : input -> int
(** Read an signed 8-bit integer. *)

val read_ui16 : input -> int
(** Read an unsigned 16-bit word. *)

val read_i16 : input -> int
(** Read a signed 16-bit word. *)

val read_i32 : input -> int
(** Read a signed 32-bit integer. Raise [Overflow] if the
  read integer cannot be represented as a Caml 31-bit integer. *)

val read_real_i32 : input -> int32
(** Read a signed 32-bit integer as an OCaml int32. *)

val read_i64 : input -> int64
(** Read a signed 64-bit integer as an OCaml int64. *)

val read_float : input -> float
(** Read an IEEE single precision floating point value. *)

val read_double : input -> float
(** Read an IEEE double precision floating point value. *)

val read_string : input -> string
(** Read a null-terminated string. *)

val read_line : input -> string
(** Read a LF or CRLF terminated string. *)

val write_byte : 'a output -> int -> unit
(** Write an unsigned 8-bit byte. *)

val write_ui16 : 'a output -> int -> unit
(** Write an unsigned 16-bit word. *)

val write_i16 : 'a output -> int -> unit
(** Write a signed 16-bit word. *)

val write_i32 : 'a output -> int -> unit
(** Write a signed 32-bit integer. *) 

val write_real_i32 : 'a output -> int32 -> unit
(** Write an OCaml int32. *)

val write_i64 : 'a output -> int64 -> unit
(** Write an OCaml int64. *)

val write_double : 'a output -> float -> unit
(** Write an IEEE double precision floating point value. *)

val write_float : 'a output -> float -> unit
(** Write an IEEE single precision floating point value. *)

val write_string : 'a output -> string -> unit
(** Write a string and append an null character. *)

val write_line : 'a output -> string -> unit
(** Write a line and append a LF (it might be converted
	to CRLF on some systems depending on the underlying BatIO). *)

external cast_output : 'a output -> unit output = "%identity"
(** You can safely transform any output to an unit output in a safe way 
  by using this function. *)

(**
   {6 For compatibility purposes}
*)

val input_channel : ?autoclose:bool -> ?cleanup:bool -> in_channel -> input
(** Create an input that will read from a channel. 

    @param autoclose If true or unspecified, the {!type: input}
    will be automatically closed when the underlying [in_channel]
    has reached its end.

    @param cleanup If true, the channel
    will be automatically closed when the {!type: input} is closed.
    Otherwise, you will need to close the channel manually.
*)

val output_channel : ?cleanup:bool -> out_channel -> unit output
(** Create an output that will write into a channel. 

    @param cleanup If true, the channel
    will be automatically closed when the {!type: output} is closed.
    Otherwise, you will need to close the channel manually.
*) 
(*
val to_input_channel : input -> in_channel
(** Create a channel that will read from an input.

    {b Note} This function is very costly and is provided
    essentially for debugging purposes or for reusing legacy
    libraries which can't be adapted. As a general rule, if
    you can avoid using this function, don't use it.*)

val to_output_channel: _ output -> out_channel
(** Create a channel that will write to an output

    {b Note} This function is very costly and is provided
    essentially for debugging purposes or for reusing legacy
    libraries which can't be adapted. As a general rule, if
    you can avoid using this function, don't use it.*)
*)
(** {6 Standard inputs/outputs} *)

val stdin : input
(** Standard input, as per Unix/Windows conventions (by default, keyboard).*)

val stdout: unit output
(** Standard output, as per Unix/Windows conventions (by default, console).

    Use this output to display regular messages.*)

val stderr: unit output
(** Standard error output, as per Unix/Windows conventions.
   
    Use this output to display warnings and error messages.
*)

val stdnull: unit output
(** An output which discards everything written to it.

    Use this output to ignore messages.*)

(** {6 Comparison}

    The following modules may be useful to create hashtables of inputs or outputs.
*)

module Input :
sig
  type t = input
  val compare : input -> input -> int
    (**A total order on inputs*)

  val hash    : input -> int
    (**A hash function for inputs*)

  val equal : input -> input -> bool
end

module Output :
sig
  type t = unit output
  val compare : _ output -> _ output -> int
    (**A total order on outputs*)

  val hash    : _ output -> int
    (**A hash function for outputs*)

  val equal : _ output -> _ output -> bool
end




(** Formatted output functions (also known as unparsing).

    Note: Don't use this module directly, use {!BatPrintf.Printf}

    @author Xavier Leroy
    @author Pierre Weiss
    @author David Teller

    @documents Printf
    @documented Batteries.Languages.Printf
*)
module Printf : sig


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
   foobar] prints ["foobar"] on the screen and returns [()]. The
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
   - [%S]: insert a string argument in Caml syntax (double quotes, escapes).
   - [%c]: insert a character argument.
   - [%C]: insert a character argument in Caml syntax (single quotes, escapes).
   - [%f]: convert a floating-point argument to decimal notation,
     in the style [dddd.ddd].
   - [%F]: convert a floating-point argument to Caml syntax ([dddd.]
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
      i.e. should have type ['b output -> unit].*)

  
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

   [('a, 'b, 'c, 'd) format4] = is the type of arguments for
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

end

(**/**)
(**{6 Internals}*)

(**
   {7 Optimized access to fields}
*)

val get_output : _ output -> (string -> int -> int -> int)
val get_flush  : _ output -> (unit -> unit)

val lock : BatConcurrent.lock ref
(**
   A reference to a set of locking operations.
*)


(**
   {7 Facilities for debugging}
*)

val get_output_id : _ output -> int
val get_input_id  : input -> int
