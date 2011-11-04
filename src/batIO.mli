(* 
 * BatIO - Abstract input/output
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
    
    This module deals with {!type: input}s and {!type:
    output}s. Inputs are manners of getting information from the
    outside world and into your program (for instance, reading from
    the network, from a file, etc.)  Outputs are manners of getting
    information out from your program and into the outside world (for
    instance, sending something onto the network, onto a file, etc.)
    In other words, if you are looking for a way to modify files, read
    from the network, etc., you're in the right place.

    To perform I/O, you first need to {e open} your {!type: input} or
    your {!type: output}. Chances are that there is an {e opening}
    operation for this task. Note that most opening operations are
    defined in their respective module.  Operations for opening files
    are defined in module {!File}, operations for opening
    communications with the network or with other processes are
    defined in module {!Unix}. Opening operations related to
    compression and decompression are defined in module {!Compress},
    etc.

    Once you have opened an {!type: input}, you may read the data it
    contains by using functions such as {!read} (to read one
    character), {!nread} or {!val: input} (to read one string) or one
    of the [read_*] functions. If you need not one information but a
    complete enumeration, for instance for processing many information
    before writing them, you may also convert the input into an
    enumeration, by using one of the [*s_of] functions.

    Once you have opened an {!type: output}, you may write data to
    this output by using functions scuh as {!write} (to write one
    char), {!nwrite} or {!val: output} (to write one string) or one of
    the [write_*] functions. If you have not just one piece of data
    but a complete enumeration, you may write this whole enumeration
    to the output by using one of the [write_*s] functions. Note that
    most operations on output are said to be {e buffered}. This means
    that small writing operations may be automatically delayed and
    grouped into large writing operations, as these are generally
    faster and induce less wear on the hardware. Occasionally, you
    may wish to force all waiting operations to take place {e now}.
    For this purpose, you may either function {!flush} or function
I    {!flush_out}.
    
    Once you have finished using your {!type: input} or your {!type:
    output}, chances are that you will want to close it. This is not a
    strict necessity, as OCaml will eventually close it for you when
    it detects that you have no more need of that {!type:
    input}/{!type: output}, but this is generally a good policy, as
    this will let other programs access the resources which are
    currently allocated to that {!type:input}/{!type:output} --
    typically, under Windows, if you are reading the contents of a
    file from a program, no other program may read the contents of
    that file simultaneously and you may also not rename or move the
    file to another directory. To close an {!type: input}, use
    function {!close_in} and to close an {!type: output}, use function
    {!close_out}.

    {b Note} Some {!type:input}s are built on top of other
    {!type:input}s to provide transparent translations (e.g.
    on-the-fly decompression of a file or network information) and
    that some {!type:output}s are built on top of other
    {!type:output}s for the same purpose (e.g. on-the-fly compression
    of a file or network information). In this case, closing the
    "outer" {!type:input}/{!type:output} (e.g. the
    decompressor/compressor) will {e not} close the "inner"
    {!type:input}/{!type:output} (e.g. access to the file or to the
    network). You will need to close the "inner"
    {!type:input}/{!type:output}, which will automatically flush
    the outer {!type:input}/{!type:output} and close it.

    @author Nicolas Cannasse
    @author David Teller
    @author Philippe Strauss
    @author Edgar Friendly

    @documents BatInnerIO
*)

open BatCamomile
open BatInnerIO

type input = BatInnerIO.input
(** The abstract input type. *)

type 'a output = 'a BatInnerIO.output
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
(** Standard input, as per Unix/Windows conventions (by default, keyboard).

    Example: [if read_line stdin |> Int.of_string > 10 then failwith "too big a number read"; ]
*)

val stdout: unit output
(** Standard output, as per Unix/Windows conventions (by default, console).

    Use this output to display regular messages.
    Example: [
       write_string stdout "Enter your name:";
       let name = read_line stdin in
       write_line stdout ("Your name is " ^ name);
    ]
*)

val stderr: unit output
(** Standard error output, as per Unix/Windows conventions.
   
    Use this output to display warnings and error messages.

    Example: [
       write_line stderr "Error on Internet - please delete google.com";
    ]
*)

val stdnull: unit output
(** An output which discards everything written to it.

    Use this output to ignore messages.

    Example: [
      let out_ch = if debug then stderr else stdnull in
      write_line out_ch "Program running.";
    ]
*)

(** {6 Standard API} *)

val read : input -> char
(** Read a single char from an input or raise [No_more_input] if
    no input is available.

    Example: [let rec skip_line ch = if read ch = '\n' then skip_line ch else ();]
*)

val nread : input -> int -> string
(** [nread i n] reads a string of size up to [n] from an input.
  The function will raise [No_more_input] if no input is available.
  It will raise [Invalid_argument] if [n] < 0.

    Example: [let read_md5 ch = nread ch 32]
*)

val really_nread : input -> int -> string
(** [really_nread i n] reads a string of exactly [n] characters
  from the input. Raises [No_more_input] if at least [n] characters are
  not available. Raises [Invalid_argument] if [n] < 0.

    Example: [let read_md5 ch = really_nread ch 32]
*)

val input : input -> string -> int -> int -> int
  (** [input i s p l] reads up to [l] characters from the given input,
      storing them in string [s], starting at character number [p]. It
      returns the actual number of characters read (which may be 0) or
      raise [No_more_input] if no character can be read. It will raise
      [Invalid_argument] if [p] and [l] do not designate a valid
      substring of [s].

      Example: [let map_ch f ?(block_size=100) =
      let b = String.create block_size in
      try while true do
        let l = input ch b 0 block_size in
        f b 0 l;
      done with No_more_input -> ()]
  *)

val really_input : input -> string -> int -> int -> int
  (** [really_input i s p l] reads exactly [l] characters from the
      given input, storing them in the string [s], starting at
      position [p]. For consistency with {!BatIO.input} it returns
      [l]. Raises [No_more_input] if at [l] characters are not
      available. Raises [Invalid_argument] if [p] and [l] do not
      designate a valid substring of [s].

      Example: [let _ = really_input stdin b 0 3]

  *)

val close_in : input -> unit
(** Close the input. It can no longer be read from.

    Example: [close_in network_in;]
*)

val write : 'a output -> char -> unit
(** Write a single char to an output.

    Example: [write stdout 'x';]
*)

val nwrite : 'a output -> string -> unit
(** Write a string to an output.

    Example: [nwrite stdout "Enter your name: ";]
*)

val write_buf: 'a output -> Buffer.t -> unit
(** Write the contents of a buffer to an output.

    Example: [let b = Buffer.create 10 in for i = 1 to 100 do Buffer.add (string_of_int i); Buffer.add " "; done; nwrite stdout b;]
*)

val output : 'a output -> string -> int -> int -> int
(** [output o s p l] writes up to [l] characters from string [s], starting at
  offset [p]. It returns the number of characters written. It will raise
  [Invalid_argument] if [p] and [l] do not designate a valid substring of [s].

    Example: [let str = "Foo Bar Baz" in let written = output stdout str 2 4;]

    This writes "o Ba" to stdout.
*)

val really_output : 'a output -> string -> int -> int -> int
(** [really_output o s p l] writes exactly [l] characters from string [s] onto
  the the output, starting with the character at offset [p]. For consistency with
  {!BatIO.output} it returns [l]. Raises [Invalid_argument] if [p] and [l] do not
  designate a valid substring of [s].

    This function is useful for networking situations where the output
    buffer might fill resulting in not the entire substring being
    readied for transmission.  Uses [output] internally, and will
    raise [Sys_blocked_io] in the case that any call returns 0.
*)

val flush : 'a output -> unit
  (** Flush an output.

      If previous write operations have caused errors, this may trigger an exception.

      Example: [flush stdout;]
*)

val flush_all : unit -> unit
(** Flush all outputs, ignore errors.

    Example: [flush_all ();]
*)


val close_out : 'a output -> 'a
(** Close the output and return its accumulator data.

    The output is flushed before being closed and can no longer be
    written. Attempting to flush or write after the output has been
    closed will have no effect.

    Example: [
    let strout = output_string () in
    write strout 'x';
    if 2+3>5 then write strout "y";
    print_string (close_out strout) ]
*)

(**/**)
val close_all : unit -> unit
(** Close all outputs.

    Ignore errors. Automatically called at the end of your program.
    You probably should never use it manually, as it also closes
    [stdout], [stderr], [stdnull].

    Example: [close_all ();]
*)
(**/**)

(** {6 Creation of BatIO Inputs/Outputs} 

    To open a file for reading/writing, see {!File.open_file_in}
    and {!File.open_file_out}*)

val input_string : string -> input
(** Create an input that will read from a string.

    Example: [
    let inch = input_string "1234554321" in
    let str1 = nread inch 3 in (* "123" *)
    let str2 = nread inch 5 in (* "45543" *)
    let str3 = nread inch 2 in (* "21" *)
    try string_of_char(read inch) with BatIO.No_more_input -> "End of string";
    ]
*)

val output_string : unit -> string output
(** Create an output that will write into a string in an efficient way.
  When closed, the output returns all the data written into it. *)

val output_buffer : Buffer.t -> string output
(** Create an output that will append its results at the end of a buffer
    in an efficient way. Closing  returns the whole contents of the buffer
    -- the buffer remains usable.*)
    

val input_enum : char BatEnum.t -> input
(** Create an input that will read from an [enum]. *)

val output_enum : unit -> char BatEnum.t output
(** Create an output that will write into an [enum]. The 
    final enum is returned when the output is closed. *)

val combine : ('a output * 'b output) -> ('a * 'b) output
(** [combine (a,b)] creates a new [output] [c] such that
    writing to [c] will actually write to both [a] and [b] *)

val tab_out : ?tab:char -> int -> 'a output -> unit output
  (** Create an output shifted to the right by a number of spaces
      (or other character as specified by [tab]).

      [tab_out n out] produces a new output for writing into [out], in
      which every new line starts with [n] spaces.
      Raises [Invalid_argument] if [n] < 0.

      Closing [tab_out n out] does not close [out]. Rather,
      closing [out] closes [tab_out n out].
  *)

(*val repeat: int -> 'a output -> unit output
(** [repeat n out] create an output in which every character or string is repeated 
    [n] times to [out].*)*)

(** {6 Utilities} *)

val read_all : input -> string
(** read all the contents of the input until [No_more_input] is raised. *)

val read_uall : input -> BatRope.t
(** Read the whole contents of a UTF-8 encoded input*)

val pipe : unit -> input * unit output
(** Create a pipe between an input and an ouput. Data written from
    the output can be read from the input. 
*)

val copy : ?buffer:int -> input -> _ output -> unit
(** Read everything from an input and copy it to an output.

    @param buffer The size of the buffer to use for copying, in
    bytes. By default, this is 4,096b.
*)

val pos_in : input -> input * (unit -> int)
  (** Create an input that provide a count function of the number of bytes
      read from it. *)

val progress_in : input -> (unit -> unit) -> input 
  (** [progress_in inp f] create an input that calls [f ()]
      whenever some content is succesfully read from it.*)

val pos_out : 'a output -> unit output * (unit -> int)
(** Create an output that provide a count function of the number of bytes
    written through it. *)

val progress_out : 'a output -> (unit -> unit) -> unit output
  (** [progress_out out f] create an output that calls [f ()]
      whenever some content is succesfully written to it.*)

external cast_output : 'a output -> unit output = "%identity"
(** You can safely transform any output to an unit output in a safe way 
    by using this function. *)



(** {6 Binary files API}

    Here is some API useful for working with binary files, in particular
    binary files generated by C applications. By default, encoding of
    multibyte integers is low-endian. The {!BigEndian} module provide multibyte
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

val read_uchar: input -> UChar.t
(** Read one UChar from a UTF-8 encoded input*)

val read_string : input -> string
(** Read a null-terminated string. *)

val read_rope: input -> int -> BatRope.t
(** Read up to n uchars from a UTF-8 encoded input*)

val read_line : input -> string
  (** Read a LF or CRLF terminated string. If the source runs out of
      input before a LF is found, returns a string of the remaining input.
      Will raise [No_more_input] only if no characters are available. *)

val read_uline: input -> BatRope.t
(** Read a line of UTF-8*)

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

val write_uchar: _ output -> UChar.t -> unit
(** Write one uchar to a UTF-8 encoded output.*)

val write_float : 'a output -> float -> unit
(** Write an IEEE single precision floating point value. *)

val write_string : 'a output -> string -> unit
(** Write a string and append an null character. *)

val write_rope : _ output -> BatRope.t -> unit
(** Write a character rope onto a UTF-8 encoded output.*)

val write_line : 'a output -> string -> unit
(** Write a line and append a line end.
    
    This adds the correct line end for your operating system.  That
    is, if you are writing to a file and your system imposes that
    files should end lines with character LF (or ['\n']), as Unix,
    then a LF is inserted at the end of the line. If your system
    favors CRLF (or ['\r\n']), then this is what will be inserted.*)

val write_uline: _ output -> BatRope.t -> unit
(** Write one line onto a UTF-8 encoded output.*)

(** Same operations as module {!BatIO}, but with big-endian encoding *)
module BigEndian :
sig

  (** This module redefines the operations of module {!BatIO} which behave
      differently on big-endian [input]s/[output]s.

      Generally, to use this module you will wish to either open both
      {!BatIO} and {!BigEndian}, so as to import a big-endian version of
      {!BatIO}, as per
      [open System.BatIO, BigEndian in ...], 
      or to redefine locally {!BatIO} to use big-endian encodings
      [module BatIO = System.BatIO include BigEndian]

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

	val ui16s_of : input -> int BatEnum.t
	  (** Read an enumeration of unsigned 16-bit words. *)

	val i16s_of : input -> int BatEnum.t
	  (** Read an enumartion of signed 16-bit words. *)

	val i32s_of : input -> int BatEnum.t
	  (** Read an enumeration of signed 32-bit integers. Raise [Overflow] if the
	      read integer cannot be represented as a Caml 31-bit integer. *)

	val real_i32s_of : input -> int32 BatEnum.t
	  (** Read an enumeration of signed 32-bit integers as OCaml [int32]s. *)

	val i64s_of : input -> int64 BatEnum.t
	  (** Read an enumeration of signed 64-bit integers as OCaml [int64]s. *)

	val doubles_of : input -> float BatEnum.t
	  (** Read an enumeration of IEEE double precision floating point values. *)

	val write_bytes : 'a output -> int BatEnum.t -> unit
	  (** Write an enumeration of unsigned 8-bit bytes. *)

	val write_ui16s : 'a output -> int BatEnum.t -> unit
	  (** Write an enumeration of unsigned 16-bit words. *)

	val write_i16s : 'a output -> int BatEnum.t -> unit
	  (** Write an enumeration of signed 16-bit words. *)

	val write_i32s : 'a output -> int BatEnum.t -> unit
	  (** Write an enumeration of signed 32-bit integers. *) 

	val write_real_i32s : 'a output -> int32 BatEnum.t -> unit
	  (** Write an enumeration of OCaml int32s. *)

	val write_i64s : 'a output -> int64 BatEnum.t -> unit
	  (** Write an enumeration of OCaml int64s. *)

	val write_doubles : 'a output -> float BatEnum.t -> unit
	  (** Write an enumeration of IEEE double precision floating point value. *)

end


(** {6 Bits API}

    This enable you to read and write from an BatIO bit-by-bit or several bits
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
   {6 Creating new types of inputs/outputs}
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

val wrap_in :
  read:(unit -> char) ->
  input:(string -> int -> int -> int) -> 
  close:(unit -> unit) -> 
  underlying:(input list) ->
  input
(** Fully create an input reading from other inputs by giving all 
    the needed functions. 

    This function is a more general version of {!create_in}
    which also handles dependency management between inputs.

    {b Note} When you create an input which reads from another
    input, function [close] should {e not} close the inputs of 
    [underlying]. Doing so is a common error, which could result
    in inadvertently closing {!stdin} or a network socket, etc.
*)

val inherit_in:
  ?read:(unit -> char) ->
  ?input:(string -> int -> int -> int) -> 
  ?close:(unit -> unit) -> 
  input -> input
  (** Simplified and optimized version of {!wrap_in} which may be used
      whenever only one input appears as dependency.

      [inherit_in inp] will return an input identical to [inp].
      [inherit_in ~read inp] will return an input identical to
      [inp] except for method [read], etc.

      You do not need to close [inp] in [close].
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

   In this case, data reaches [out] only after [out] has been closed.
   Despite appearances, it is quite easy to reach such situation,
   especially in short programs.

   If, instead, [f] uses [wrap_out], then when output [out] is closed,
   [f out] is first automatically flushed and closed, which avoids the
   issue.

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

val inherit_out:
  ?write:(char -> unit) ->
  ?output:(string -> int -> int -> int) -> 
  ?flush:(unit -> unit) ->
  ?close:(unit -> unit) -> 
  'a output -> unit output
(**
   Simplified and optimized version of {!wrap_out} whenever only
   one output appears as dependency.

   [inherit_out out] will return an output identical to [out].
   [inherit_out ~write out] will return an output identical to
   [out] except for its [write] method, etc.

   You do not need to close [out] in [close].
*)

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


val to_input_channel : input -> in_channel
(** Create a channel that will read from an input.

    {b Note} This function is extremely costly and is provided
    essentially for debugging purposes or for reusing legacy
    libraries which can't be adapted. As a general rule, if
    you can avoid using this function, don't use it.*)

(** {6 Generic BatIO Object Wrappers}

    Theses OO Wrappers have been written to provide easy support of
    BatIO by external librairies. If you want your library to support
    BatIO without actually requiring Batteries to compile, you can
    should implement the classes [in_channel], [out_channel],
    [poly_in_channel] and/or [poly_out_channel] which are the common
    BatIO specifications established for ExtLib, OCamlNet and
    Camomile.
    
    (see http://www.ocaml-programming.de/tmp/BatIO-Classes.html for more details).

    {b Note} In this version of Batteries Included, the object wrappers are {e not}
    closed automatically by garbage-collection.
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

val bytes_of : input -> int BatEnum.t
(** Read an enumeration of unsigned 8-bit integers. *)

val signed_bytes_of : input -> int BatEnum.t
(** Read an enumeration of signed 8-bit integers. *)

val ui16s_of : input -> int BatEnum.t
(** Read an enumeration of unsigned 16-bit words. *)

val i16s_of : input -> int BatEnum.t
(** Read an enumartion of signed 16-bit words. *)

val i32s_of : input -> int BatEnum.t
(** Read an enumeration of signed 32-bit integers. Raise [Overflow] if the
  read integer cannot be represented as a Caml 31-bit integer. *)

val real_i32s_of : input -> int32 BatEnum.t
(** Read an enumeration of signed 32-bit integers as OCaml [int32]s. *)

val i64s_of : input -> int64 BatEnum.t
(** Read an enumeration of signed 64-bit integers as OCaml [int64]s. *)

val doubles_of : input -> float BatEnum.t
(** Read an enumeration of IEEE double precision floating point values. *)

val strings_of : input -> string BatEnum.t
(** Read an enumeration of null-terminated strings. *)

val lines_of : input -> string BatEnum.t
(** Read an enumeration of LF or CRLF terminated strings. *)
 
val chunks_of : int -> input -> string BatEnum.t
(** Read an input as an enumeration of strings of given length.  If the input isn't a multiple of that length, the final string will be smaller than the rest. *)

val ulines_of : input -> BatRope.t BatEnum.t
(** offer the lines of a UTF-8 encoded input as an enumeration*)

val chars_of : input -> char BatEnum.t
(** Read an enumeration of Latin-1 characters. 

    {b Note} Usually faster than calling [read] several times.*)

val uchars_of : input -> UChar.t BatEnum.t
(** offer the characters of an UTF-8 encoded input as an enumeration*)

val bits_of : in_bits -> int BatEnum.t
(** Read an enumeration of bits *)

val write_bytes : 'a output -> int BatEnum.t -> unit
(** Write an enumeration of unsigned 8-bit bytes. *)

val write_chars : 'a output -> char BatEnum.t -> unit
(** Write an enumeration of chars. *)

val write_uchars : _ output -> UChar.t BatEnum.t -> unit
(** Write an enumeration of characters onto a UTF-8 encoded output.*)

val write_ui16s : 'a output -> int BatEnum.t -> unit
(** Write an enumeration of unsigned 16-bit words. *)

val write_i16s : 'a output -> int BatEnum.t -> unit
(** Write an enumeration of signed 16-bit words. *)

val write_i32s : 'a output -> int BatEnum.t -> unit
(** Write an enumeration of signed 32-bit integers. *) 

val write_real_i32s : 'a output -> int32 BatEnum.t -> unit
(** Write an enumeration of OCaml int32s. *)

val write_i64s : 'a output -> int64 BatEnum.t -> unit
(** Write an enumeration of OCaml int64s. *)

val write_doubles : 'a output -> float BatEnum.t -> unit
(** Write an enumeration of IEEE double precision floating point value. *)

val write_strings : 'a output -> string BatEnum.t -> unit
(** Write an enumeration of strings, appending null characters.*)

val write_chunks: 'a output -> string BatEnum.t -> unit
(** Write an enumeration of strings, without appending null characters.*)

val write_lines : 'a output -> string BatEnum.t -> unit
(** Write an enumeration of lines, appending a LF (it might be converted
    to CRLF on some systems depending on the underlying BatIO). *)

val write_ropes : 'a output -> BatRope.t BatEnum.t -> unit
(** Write an enumeration of ropes onto a UTF-8 encoded output,
    without appending a line-end.*)

val write_ulines : _ output -> BatRope.t BatEnum.t -> unit
(** Write an enumeration of lines onto a UTF-8 encoded output.*)

val write_bitss : nbits:int -> out_bits -> int BatEnum.t -> unit
(** Write an enumeration of bits*)

(** {6 Printing} *)


val printf : 'a output -> ('b, 'a output, unit) format -> 'b
(** A [fprintf]-style unparser. For more information
    about printing, see the documentation of {!Printf}.

    @obsolete Prefer {!Languages.Printf.fprintf}*)


val default_buffer_size : int
(**The default size for internal buffers.*)

(**
   {6 Thread-safety}
*)

val synchronize_in : ?lock:BatConcurrent.lock -> input  -> input
(**[synchronize_in inp] produces a new {!type: input} which reads from [input]
   in a thread-safe way. In other words, a lock prevents two distinct threads
   from reading from that input simultaneously, something which would potentially
   wreak havoc otherwise

   @param lock An optional lock. If none is provided, the lock will be specific
   to this [input]. Specifiying a custom lock may be useful to associate one
   common lock for several inputs and/or outputs, for instance in the case
   of pipes.
*)

val synchronize_out: ?lock:BatConcurrent.lock -> _ output -> unit output
(**[synchronize_out out] produces a new {!type: output} which writes to [output]
   in a thread-safe way. In other words, a lock prevents two distinct threads
   from writing to that output simultaneously, something which would potentially
   wreak havoc otherwise

   @param lock An optional lock. If none is provided, the lock will be specific
   to this [output]. Specifiying a custom lock may be useful to associate one
   common lock for several inputs and/or outputs, for instance in the case
   of pipes.
*)


(**
   {6 Thread-safety internals}

   Unless you are attempting to adapt Batteries Included to a new model of
   concurrency, you probably won't need this.
*)

val lock: BatConcurrent.lock ref
(**
   A lock used to synchronize internal operations.

   By default, this is {!BatConcurrent.nolock}. However, if you're
   using a version of Batteries compiled in threaded mode, this uses
   {!BatMutex}. If you're attempting to use Batteries with another
   concurrency model, set the lock appropriately.
*)

val lock_factory: (unit -> BatConcurrent.lock) ref
  (**
     A factory used to create locks. This is used transparently by {!synchronize_in}
     and {!synchronize_out}.

     By default, this always returns {!BatConcurrent.nolock}. However,
     if you're using a version of Batteries compiled in threaded mode,
     this uses {!BatMutex}.  *)

val to_string : (string output -> 'a -> unit) -> 'a -> string

val to_format: ('a BatInnerIO.output -> 'b -> unit) -> Format.formatter -> 'b -> unit

(**/**)
val comb : ('a output * 'a output) -> 'a output
(** Old name of [combine]*)

(**
   {6 Debugging facilities}
*)

val get_output_id : _ output -> int
val get_input_id  : input -> int
