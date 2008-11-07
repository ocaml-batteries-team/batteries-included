(* 
 * IO - Abstract input/output
 * Copyright (C) 2003 Nicolas Cannasse
 *               2008 David Teller (contributor)
 *               2008 Philippe Strauss (contributor)
 *               2008 Edgar Friendly (contributor)
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

    If you are looking for a way to read data from a file, a network
    connexion, the keyboard... or to write data to a file, to the
    network, to the screen... chances are that the operations are
    defined in this module. Note that this module does not define
    how to open files, network connexions, etc.

    To open files, see {!File}.

    @author Nicolas Cannasse
    @author David Teller
    @author Philippe Strauss
    @author Edgar Friendly

    @documents InnerIO
*)

open ExtUChar
open InnerIO

type input = InnerIO.input
(** The abstract input type. *)

type 'a output = 'a InnerIO.output
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
(** Standard output, as per Unix/Windows conventions (by default, console).

    Use this output to display regular messages.*)

val stderr: unit output
(** Standard error output, as per Unix/Windows conventions.
   
    Use this output to display warnings and error messages.
*)

val stdnull: unit output
(** An output which discards everything written to it.

    Use this output to ignore messages.*)

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

val write_buf: 'a output -> Buffer.t -> unit
(** Write the contents of a buffer to an output.*)

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
(** Flush an output.

    If previous write operations have caused errors, this may trigger an exception.*)

val flush_all : unit -> unit
(** Flush all outputs, ignore errors. *)

val close_out : 'a output -> 'a
(** Close the output and return its accumulator data.

    The output is flushed before being closed and can no longer be
    written. Attempting to flush or write after the output has been
    closed will have no effect.*)

(** {6 Unicode extensions} 

    All of the following functions deal only with UTF-8 encoded inputs/outputs
*)

val read_uchar: input -> UChar.t
(** Read one UChar from a UTF-8 encoded input*)
 
val read_rope: input -> int -> Rope.t
(** Read up to n uchars from a UTF-8 encoded input*)
 
val read_uline: input -> Rope.t
(** Read a line of UTF-8*)
 
val read_uall : input -> Rope.t
(** Read the whole contents of a UTF-8 encoded input*)
 
val write_uchar: _ output -> UChar.t -> unit
(** Write one uchar to a UTF-8 encoded output.*)

val write_rope : _ output -> Rope.t -> unit
(** Write a character rope onto a UTF-8 encoded output.*)

val write_uline: _ output -> Rope.t -> unit
(** Write one line onto a UTF-8 encoded output.*)

val write_uchars : _ output -> UChar.t Enum.t -> unit
(** Write an enumeration of characters onto a UTF-8 encoded output.*)

val write_ulines : _ output -> Rope.t Enum.t -> unit
(** Write an enumeration of lines onto a UTF-8 encoded output.*)

val write_ropes : _ output -> Rope.t Enum.t -> unit
(** Write an enumeration of ropes onto a UTF-8 encoded output.*)

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

val tab_out : ?tab:char -> int -> 'a output -> unit output
(** Create an output shifted to the right by a number of white spaces
    (or [tab], if given).

    [tab_out n out] produces a new output for writing into [out], in
    which every new line starts with [n] white spaces.
    Raises [Invalid_argument] if [n] < 0.

    Closing [tab_out n out] does not close [out].
*)



(** {6 Utilities} *)

val read_all : input -> string
(** read all the contents of the input until [No_more_input] is raised. *)

val pipe : unit -> input * unit output
(** Create a pipe between an input and an ouput. Data written from
    the output can be read from the input. *)

val copy : input -> _ output -> unit
(** Read everything from an input and copy it to an output.

    This function flushes the [output].*)

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

(** Same operations as module {!IO}, but with big-endian encoding *)
module BigEndian :
sig

  (** This module redefines the operations of module {!IO} which behave
      differently on big-endian [input]s/[output]s.

      Generally, to use this module you will wish to either open both
      {!IO} and {!BigEndian}, so as to import a big-endian version of
      {!IO}, as per
      [open System.IO, BigEndian in ...], 
      or to redefine locally {!IO} to use big-endian encodings
      [let module IO = struct 
          include System.IO 
          include BigEndian
       in ...]
  *)

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


	val read_double : input -> float
	  (** Read an IEEE double precision floating point value. *)

	val read_float: input -> float
	  (** Read an IEEE single precision floating point value. *)

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

	val write_float  : 'a output -> float -> unit
	  (** Write an IEEE single precision floating point value. *)

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

	val write_bytes : 'a output -> int Enum.t -> unit
	  (** Write an enumeration of unsigned 8-bit bytes. *)

	val write_ui16s : 'a output -> int Enum.t -> unit
	  (** Write an enumeration of unsigned 16-bit words. *)

	val write_i16s : 'a output -> int Enum.t -> unit
	  (** Write an enumeration of signed 16-bit words. *)

	val write_i32s : 'a output -> int Enum.t -> unit
	  (** Write an enumeration of signed 32-bit integers. *) 

	val write_real_i32s : 'a output -> int32 Enum.t -> unit
	  (** Write an enumeration of OCaml int32s. *)

	val write_i64s : 'a output -> int64 Enum.t -> unit
	  (** Write an enumeration of OCaml int64s. *)

	val write_doubles : 'a output -> float Enum.t -> unit
	  (** Write an enumeration of IEEE double precision floating point value. *)

	val write_strings : 'a output -> string Enum.t -> unit
	  (** Write an enumeration of strings, appending null characters. *)

	val write_lines : 'a output -> string Enum.t -> unit
	  (** Write an enumeration of lines, appending a LF (it might be converted
	      to CRLF on some systems depending on the underlying IO). *)

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

(** {6 Enumeration API}

    All these enumerations close their input once they are completely consumed.
*)

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
 
val ulines_of : input -> Rope.t Enum.t
(** offer the lines of a UTF-8 encoded input as an enumeration*)

val chars_of : input -> char Enum.t
(** Read an enumeration of Latin-1 characters. 

    {b Note} Usually faster than calling [read] several times.*)

val uchars_of : input -> UChar.t Enum.t
(** offer the characters of an UTF-8 encoded input as an enumeration*)

val bits_of : in_bits -> int Enum.t
(** Read an enumeration of bits *)

val write_bytes : 'a output -> int Enum.t -> unit
(** Write an enumeration of unsigned 8-bit bytes. *)

val write_ui16s : 'a output -> int Enum.t -> unit
(** Write an enumeration of unsigned 16-bit words. *)

val write_i16s : 'a output -> int Enum.t -> unit
(** Write an enumeration of signed 16-bit words. *)

val write_i32s : 'a output -> int Enum.t -> unit
(** Write an enumeration of signed 32-bit integers. *) 

val write_real_i32s : 'a output -> int32 Enum.t -> unit
(** Write an enumeration of OCaml int32s. *)

val write_i64s : 'a output -> int64 Enum.t -> unit
(** Write an enumeration of OCaml int64s. *)

val write_doubles : 'a output -> float Enum.t -> unit
(** Write an enumeration of IEEE double precision floating point value. *)

val write_strings : 'a output -> string Enum.t -> unit
(** Write an enumeration of strings, appending null characters. *)

val write_lines : 'a output -> string Enum.t -> unit
(** Write an enumeration of lines, appending a LF (it might be converted
    to CRLF on some systems depending on the underlying IO). *)

val write_bitss : nbits:int -> out_bits -> int Enum.t -> unit
(** Write an enumeration of bits*)

(** {6 Printing} *)


val printf : 'a output -> ('b, 'a output, unit) format -> 'b
(** A [fprintf]-style unparser. For more information
    about printing, see the documentation of {!Printf}.

    @obsolete Prefer {!Languages.Printf.fprintf}*)

val lmargin : int -> ('b output -> 'a -> unit) -> 'b output -> 'a -> unit
(** [lmargin n p] behaves as [p], with the exception that every new line from this
    point will be shifted to the right by [n] white spaces*)



(*val make_list_printer: ('a output -> 'b -> unit) -> string -> string -> string -> ('a output -> 'b list -> unit)
(** Make a list printer

    [make_list_printer printer start_symbol end_symbol separator] creates a printer for
    lists, which prints [start_symbol] at the beginning of the list, [end_symbol] at
    the end, uses [printer] for each element of the contents and separates these
    elements with [separator]
*)*)


val default_buffer_size : int

(**/**)
val comb : ('a output * 'a output) -> 'a output
(** Old name of [combine]*)
