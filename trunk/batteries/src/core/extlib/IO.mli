(* 
 * IO - Abstract input/output
 * Copyright (C) 2003 Nicolas Cannasse
 *               2008 David Teller (contributor)
 *               2008 Philippe Strauss (contributor)
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

(** High-order abstract I/O.
    
    IO module simply deals with abstract inputs/outputs. It provides a
    set of methods for working with these IO as well as several
    constructors that enable to write to an underlying channel, buffer,
    or enum.
*)

type input
(** The abstract input type. *)

type 'a output
(** The abstract output type, ['a] is the accumulator data, it is returned
	when the [close_out] function is called. *)

exception No_more_input
(** This exception is raised when reading on an input with the [read] or
  [nread] functions while there is no available token to read. *)

exception Input_closed
(** This exception is raised when reading on a closed input. *)

exception Output_closed
(** This exception is raised when reading on a closed output. *)

(** {6 Standard inputs/outputs} *)

val stdin : input
(** Standard input, as per Unix/Windows conventions (by default, keyboard).*)

val stdout: unit output
(** Standard output, as per Unix/Windows conventions (by default, console).*)

val stderr: unit output
(** Standard error output, as per Unix/Windows conventions.*)

val stdnull: unit output
(** An output which discards everything written to it.*)

(** {6 Standard API} *)

val read : input -> char
(** Read a single char from an input or raise [No_more_input] if
  no input available. *)

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
  {!IO.input} it returns [l]. Raises [No_more_input] if at [l] characters are
  not available. Raises [Invalid_argument] if [p] and [l] do not designate a
  valid substring of [s]. *)

val close_in : input -> unit
(** Close the input. It can no longer be read from. *)

val write : 'a output -> char -> unit
(** Write a single char to an output. *)

val nwrite : 'a output -> string -> unit
(** Write a string to an output. *)

val output : 'a output -> string -> int -> int -> int
(** [output o s p l] writes up to [l] characters from string [s], starting at
  offset [p]. It returns the number of characters written. It will raise
  [Invalid_argument] if [p] and [l] do not designate a valid substring of [s]. *)

val really_output : 'a output -> string -> int -> int -> int
(** [really_output o s p l] writes exactly [l] characters from string [s] onto
  the the output, starting with the character at offset [p]. For consistency with
  {!IO.output} it returns [l]. Raises [Invalid_argument] if [p] and [l] do not
  designate a valid substring of [s]. *)

val flush : 'a output -> unit
(** Flush an output. *)

val close_out : 'a output -> 'a
(** Close the output and return its accumulator data.
  It can no longer be written. *)

(** {6 Creation of IO Inputs/Outputs} 

    To open a file for reading/writing, see {!File.open_file_in}
    and {!File.open_file_out}*)

val input_string : string -> input
(** Create an input that will read from a string. *)

val output_string : unit -> string output
(** Create an output that will write into a string in an efficient way.
  When closed, the output returns all the data written into it. *)

val output_buffer : Buffer.t -> string output
(** Create an output that will append its results at the end of a buffer
    in an efficient way. Closing  returns the whole contents of the buffer
    -- the buffer remains usable.*)
    

val input_enum : char Enum.t -> input
(** Create an input that will read from an [enum]. *)

val output_enum : unit -> char Enum.t output
(** Create an output that will write into an [enum]. The 
  final enum is returned when the output is closed. *)

val combine : ('a output * 'a output) -> 'a output
(** [combine (a,b)] creates a new [output] [c] such that
    writing to [c] will actually write to both [a] and [b] *)

val create_in :
  read:(unit -> char) ->
  input:(string -> int -> int -> int) -> close:(unit -> unit) -> input
(** Fully create an input by giving all the needed functions. *)

val create_out :
  write:(char -> unit) ->
  output:(string -> int -> int -> int) ->   
  flush:(unit -> unit) -> close:(unit -> 'a) -> 'a output
(** Fully create an output by giving all the needed functions. *)

val tab_out : int -> 'a output -> 'a output
(** Create an output shifted to the right by a number of white spaces.

    [tab_out n out] produces a new output for writing into [out], in
    which every new line starts with [n] white spaces.
    Raises [Invalid_argument] if [n] < 0.*)



(** {6 Utilities} *)

val read_all : input -> string
(** read all the contents of the input until [No_more_input] is raised. *)

val pipe : unit -> input * unit output
(** Create a pipe between an input and an ouput. Data written from
  the output can be read from the input. *)

val copy : input -> _ output -> unit
(** Read everything from an input and copy it to an output.*)

val pos_in : input -> input * (unit -> int)
(** Create an input that provide a count function of the number of bytes
  read from it. *)

val pos_out : 'a output -> 'a output * (unit -> int)
(** Create an output that provide a count function of the number of bytes
  written through it. *)

external cast_output : 'a output -> unit output = "%identity"
(** You can safely transform any output to an unit output in a safe way 
  by using this function. *)



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
	to CRLF on some systems depending on the underlying IO). *)

(** Same as operations above, but use big-endian encoding *)
module BigEndian :
sig

	val read_ui16 : input -> int
	val read_i16 : input -> int
	val read_i32 : input -> int
	val read_real_i32 : input -> int32
	val read_i64 : input -> int64
	val read_double : input -> float
	val read_float: input -> float
	val write_ui16 : 'a output -> int -> unit
	val write_i16 : 'a output -> int -> unit
	val write_i32 : 'a output -> int -> unit
	val write_real_i32 : 'a output -> int32 -> unit
	val write_i64 : 'a output -> int64 -> unit
	val write_double : 'a output -> float -> unit
	val write_float  : 'a output -> float -> unit

	val ui16s_of : input -> int Enum.t
	val i16s_of : input -> int Enum.t
	val i32s_of : input -> int Enum.t
	val real_i32s_of : input -> int32 Enum.t
	val i64s_of : input -> int64 Enum.t
	val doubles_of : input -> float Enum.t

	val write_byte_enum : 'a output -> int Enum.t -> unit
	val write_ui16_enum : 'a output -> int Enum.t -> unit
	val write_i16_enum : 'a output -> int Enum.t -> unit
	val write_i32_enum : 'a output -> int Enum.t -> unit
	val write_real_i32_enum : 'a output -> int32 Enum.t -> unit
	val write_i64_enum : 'a output -> int64 Enum.t -> unit
	val write_double_enum : 'a output -> float Enum.t -> unit
	val write_string_enum : 'a output -> string Enum.t -> unit
	val write_line_enum : 'a output -> string Enum.t -> unit

end


(** {6 Bits API}

	This enable you to read and write from an IO bit-by-bit or several bits
	at the same time.
*)

type in_bits
type out_bits

exception Bits_error

val input_bits : input -> in_bits
(** Read bits from an input *)

val output_bits : 'a output -> out_bits
(** Write bits to an output *)

val read_bits : in_bits -> int -> int
(** Read up to 31 bits, raise Bits_error if n < 0 or n > 31 *)

val write_bits : out_bits -> nbits:int -> int -> unit
(** Write up to 31 bits represented as a value, raise Bits_error if nbits < 0
 or nbits > 31 or the value representation excess nbits. *)

val flush_bits : out_bits -> unit
(** Flush remaining unwritten bits, adding up to 7 bits which values 0. *)

val drop_bits : in_bits -> unit
(** Drop up to 7 buffered bits and restart to next input character. *)

(**
   {6 For compatibility purposes}
*)

val input_channel : in_channel -> input
(** Create an input that will read from a channel. *)

val output_channel : out_channel -> unit output
(** Create an output that will write into a channel. *) 



(** {6 Generic IO Object Wrappers}

	Theses OO Wrappers have been written to provide easy support of ExtLib
	IO by external librairies. If you want your library to support ExtLib
	IO without actually requiring ExtLib to compile, you can should implement
	the classes [in_channel], [out_channel], [poly_in_channel] and/or
	[poly_out_channel] which are the common IO specifications established
	for ExtLib, OCamlNet and Camomile.

	(see http://www.ocaml-programming.de/tmp/IO-Classes.html for more details).
*)

class in_channel : input ->
  object
	method input : string -> int -> int -> int
	method close_in : unit -> unit
  end

class out_channel : 'a output ->
  object
	method output : string -> int -> int -> int
	method flush : unit -> unit
	method close_out : unit -> unit
  end

class in_chars : input ->
  object
	method get : unit -> char
	method close_in : unit -> unit
  end

class out_chars : 'a output ->
  object
	method put : char -> unit
	method flush : unit -> unit
	method close_out : unit -> unit
  end

val from_in_channel : #in_channel -> input
val from_out_channel : #out_channel -> unit output
val from_in_chars : #in_chars -> input
val from_out_chars : #out_chars -> unit output

(** {6 Enumeration API}*)

val bytes_of : input -> int Enum.t
(** Read an enumeration of unsigned 8-bit integers. *)

val signed_bytes_of : input -> int Enum.t
(** Read an enumeration of signed 8-bit integers. *)

val ui16s_of : input -> int Enum.t
(** Read an enumeration of unsigned 16-bit words. *)

val i16s_of : input -> int Enum.t
(** Read an enumartion of signed 16-bit words. *)

val i32s_of : input -> int Enum.t
(** Read an enumeration of signed 32-bit integers. Raise [Overflow] if the
  read integer cannot be represented as a Caml 31-bit integer. *)

val real_i32s_of : input -> int32 Enum.t
(** Read an enumeration of signed 32-bit integers as OCaml [int32]s. *)

val i64s_of : input -> int64 Enum.t
(** Read an enumeration of signed 64-bit integers as OCaml [int64]s. *)

val doubles_of : input -> float Enum.t
(** Read an enumeration of IEEE double precision floating point values. *)

val strings_of : input -> string Enum.t
(** Read an enumeration of null-terminated strings. *)

val lines_of : input -> string Enum.t
(** Read an enumeration of LF or CRLF terminated strings. *)

val chars_of : input -> char Enum.t
(** Read an enumeration of Latin-1 characters. 

    {b Note} Usually faster than calling [read] several times.*)

val bits_of : in_bits -> int Enum.t
(** Read an enumeration of bits *)

val write_byte_enum : 'a output -> int Enum.t -> unit
(** Write an enumeration of unsigned 8-bit bytes. *)

val write_ui16_enum : 'a output -> int Enum.t -> unit
(** Write an enumeration of unsigned 16-bit words. *)

val write_i16_enum : 'a output -> int Enum.t -> unit
(** Write an enumeration of signed 16-bit words. *)

val write_i32_enum : 'a output -> int Enum.t -> unit
(** Write an enumeration of signed 32-bit integers. *) 

val write_real_i32_enum : 'a output -> int32 Enum.t -> unit
(** Write an enumeration of OCaml int32s. *)

val write_i64_enum : 'a output -> int64 Enum.t -> unit
(** Write an enumeration of OCaml int64s. *)

val write_double_enum : 'a output -> float Enum.t -> unit
(** Write an enumeration of IEEE double precision floating point value. *)

val write_string_enum : 'a output -> string Enum.t -> unit
(** Write an enumeration of strings, appending null characters. *)

val write_line_enum : 'a output -> string Enum.t -> unit
(** Write an enumeration of lines, appending a LF (it might be converted
    to CRLF on some systems depending on the underlying IO). *)

val write_bits_enum : nbits:int -> out_bits -> int Enum.t -> unit
(** Write an enumeration of bits*)

(** {6 Printing} *)


val printf : 'a output -> ('b, 'a output, unit) format -> 'b
(** A [fprintf]-style unparser. For more information
    about printing, see the documentation of {!Printf}.

    @obsolete Prefer {!Languages.Printf.fprintf}*)

(**/*)
module Printf : sig
  (** Formatted output functions (also known as unparsing).*)

  (**
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
  
(**/**)
val kprintf : (string -> 'a) -> ('b, unit, string, 'a) format4 -> 'b
  (** A deprecated synonym for [ksprintf]. *)
  (**/**)

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
   arguments have been applied.
   
   {7 Format}
   [('a, 'b, 'c) format] or [('a, 'b, 'c) {!t}] is just a shortcut for [('a, 'b, 'c, 'c) format4].
   
   {7 Important}
   Note that {!Obj.magic} is involved behind this, so be careful.
*)

end
(**/**)

val make_list_printer: ('a output -> 'b -> unit) -> string -> string -> string -> ('a output -> 'b list -> unit)
(** Make a list printer

    [make_list_printer printer start_symbol end_symbol separator] creates a printer for
    lists, which prints [start_symbol] at the beginning of the list, [end_symbol] at
    the end, uses [printer] for each element of the contents and separates these
    elements with [separator]
*)

val lmargin : int -> ('b output -> 'a -> unit) -> 'b output -> 'a -> unit
(** [lmargin n p] behaves as [p], with the exception that every new line from this
    point will be shifted to the right by [n] white spaces*)


(**/**)
val comb : ('a output * 'a output) -> 'a output
(** Old name of [combine]*)
