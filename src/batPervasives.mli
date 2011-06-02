(*
 * ExtPervasives - Additional functions
 * Copyright (C) 1996 Xavier Leroy
 *               2003 Nicolas Cannasse
 *               2007 Zheng Li
 *               2008 David Teller
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

(** {6 Additional functions.}

    @author Xavier Leroy (Base module)
    @author Nicolas Cannasse
    @author David Teller
    @author Zheng Li
*)

open BatIO

(** The initially opened module.
    
    This module provides the basic operations over the built-in types
    (numbers, booleans, strings, exceptions, references, lists, arrays,
    input-output channels, ...)
    
    This module is automatically opened at the beginning of each compilation.
    All components of this module can therefore be referred by their short
    name, without prefixing them by [BatPervasives].

    @author Xavier Leroy (Base module)
    @author Nicolas Cannasse
    @author David Teller
    @author Zheng Li
*)



(** {6 String operations}

   More string operations are provided in module {!BatString}.
*)

val uppercase : string -> string
(** Return a copy of the argument, with all lowercase letters
    translated to uppercase, including accented letters of the ISO
    Latin-1 (8859-1) character set. *)

val lowercase : string -> string
(** Return a copy of the argument, with all uppercase letters
    translated to lowercase, including accented letters of the ISO
    Latin-1 (8859-1) character set. *)


(** {6 String conversion functions} 

    These are the most common string conversion functions.  For
    additional string conversion functions, see in the corresponding
    module (e.g. for conversion between [int32] and [string],
    see module {!Int32}).
*)

val string_of_char : char -> string
(** creates a string from a char. *)

val dump : 'a -> string
(** Attempt to convert a value to a string.

    Since types are lost at compile time, the representation might not
    match your type. For example, None will be printed 0 since they
    share the same runtime representation. *)


(** {6 List operations}

    More list operations are provided in module {!List}.
*)

val ( @ ) : 'a list -> 'a list -> 'a list
(** List concatenation. *)


(** {6 Input/output} 

    This section only contains the most common input/output operations.
    More operations may be found in modules {!BatIO} and {!File}.
*)

val stdin : input
(** Standard input, as per Unix/Windows conventions (by default, keyboard).

    Use this input to read what the user is writing on the keyboard.*)

val stdout: unit output
(** Standard output, as per Unix/Windows conventions (by default, console).

    Use this output to display regular messages.*)

val stderr: unit output
(** Standard error output, as per Unix/Windows conventions.
   
    Use this output to display warnings and error messages.*)

val stdnull: unit output
(** An output which discards everything written to it.

    Use this output to ignore messages.*)

val flush_all : unit -> unit
(** Write all pending data to output channels, ignore all errors.

    It is normally not necessary to call this function, as all pending
    data is written when an output channel is closed or when the
    program itself terminates, either normally or because of an
    uncaught exception. However, this function is useful for
    debugging, as it forces pending data to be written immediately.
*)

(** {7 Output functions on standard output} *)


val print_bool : bool -> unit
(** Print a boolean on standard output. *)

val print_guess : 'a BatIO.output -> 'b -> unit
  (** Attempt to print the representation of a runtime value on the
      standard output.  See remarks for {!dump}. This function is
      useful mostly for debugging. As a general rule, it should not be
      used in production code.*)

val print_all : input -> unit
  (** Print the contents of an input to the standard output.*)

(** {7 Output functions on standard error} *)

val prerr_bool : bool -> unit
(** Print a boolean to stderr. *)

val prerr_guess : 'a -> unit
  (** Attempt to print the representation of a runtime value on the
      error output.  See remarks for {!dump}. This function is
      useful mostly for debugging.*)

val prerr_all : input -> unit
  (** Print the contents of an input to the error output.*)

(** {7 General output functions} *)

val open_out : ?mode:(BatFile.open_out_flag list) -> 
               ?perm:BatFile.permission           ->
  string -> unit BatIO.output
  (** Open the named file for writing, and return a new output channel
      on that file. You will need to close the file once you have
      finished using it.
      
      You may use optional argument [mode] to decide whether the
      output will overwrite the contents of the file (by default) or
      to add things at the end of the file, whether the file should be
      created if it does not exist yet (the default) or not, whether
      this operation should proceed if the file exists already (the
      default) or not, whether the file should be opened as text
      (the default) or as binary, and whether the file should be
      opened for non-blocking operations.

      You may use optional argument [perm] to specify the permissions
      of the file, as per Unix conventions. By default, files are created
      with default permissions (which depend on your setup).

      Raise [Sys_error] if the file could not be opened. *)

val open_out_bin : string -> unit BatIO.output
  (** Same as {!open_out}, but the file is opened in binary mode, so
      that no translation takes place during writes. On operating
      systems that do not distinguish between text mode and binary
      mode, this function behaves like {!open_out} without any
      [mode] or [perm]. *)

val open_out_gen : open_flag list -> int -> string -> unit BatIO.output
  (**
     [open_out_gen mode perm filename] opens the named file for writing,
     as described above. The extra argument [mode]
     specifies the opening mode. The extra argument [perm] specifies
     the file permissions, in case the file must be created.

     @deprecated Use {!open_out instead}*)

val flush : unit BatIO.output -> unit
  (** Flush the buffer associated with the given output, performing
      all pending writes on that channel. Interactive programs must be
      careful about flushing standard output and standard error at the
      right time. *)



val output_char : unit BatIO.output -> char -> unit
(** Write the character on the given output channel. *)

val output_string : unit BatIO.output -> string -> unit
(** Write the string on the given output channel. *)

val output_rope : unit BatIO.output -> BatRope.t -> unit
(** Write the rope on the given output channel. *)

val output : unit BatIO.output -> string -> int -> int -> unit
(** [output oc buf pos len] writes [len] characters from string [buf],
   starting at offset [pos], to the given output channel [oc].
   Raise [Invalid_argument "output"] if [pos] and [len] do not
   designate a valid substring of [buf]. *)

val output_byte : unit BatIO.output -> int -> unit
(** Write one 8-bit integer (as the single character with that code)
   on the given output channel. The given integer is taken modulo
   256. *)

val output_binary_int : unit BatIO.output -> int -> unit
  (** Write one integer in binary format (4 bytes, big-endian)
      on the given output channel.
      The given integer is taken modulo 2{^32}.
      The only reliable way to read it back is through the
      {!Pervasives.input_binary_int} function. The format is compatible across
      all machines for a given version of Objective Caml. *)

val output_binary_float : unit BatIO.output -> float -> unit
  (** Write one float in binary format (8 bytes, IEEE 754 double format)
      on the given output channel.
      The only reliable way to read it back is through the
      {!Pervasives.input_binary_float} function. The format is compatible across
      all machines for a given version of Objective Caml. *)

val output_value : unit BatIO.output -> 'a -> unit
  (** Write the representation of a structured value of any type
      to a channel. Circularities and sharing inside the value
      are detected and preserved. The object can be read back,
      by the function {!input_value}. See the description of module
      {!Marshal} for more information. {!output_value} is equivalent
      to {!Marshal.output} with an empty list of flags. *)


val close_out : unit BatIO.output -> unit
  (** Close the given channel, flushing all buffered write operations.
      Output functions raise a [Sys_error] exception when they are
      applied to a closed output channel, except [close_out] and [flush],
      which do nothing when applied to an already closed channel.
      Note that [close_out] may raise [Sys_error] if the operating
      system signals an error when flushing or closing. *)
  
val close_out_noerr : unit BatIO.output -> unit
  (** Same as [close_out], but ignore all errors. *)
 
(** {7 General input functions} *)

val open_in : ?mode:(BatFile.open_in_flag list) -> 
  ?perm:BatFile.permission -> 
  string -> BatIO.input
(** Open the named file for reading. You will need to close the file once you have
    finished using it.
    
    You may use optional argument [mode] to decide whether the opening
    should fail if the file doesn't exist yet (by default) or whether
    the file should be created if it doesn't exist yet, whether the
    opening should fail if the file already exists or not (by
    default), whether the file should be read as binary (by default)
    or as text, and whether reading should be non-blocking.

    You may use optional argument [perm] to specify the permissions of
    the file, should it be created, as per Unix conventions. By
    default, files are created with default permissions (which depend
    on your setup).

    Raise [Sys_error] if the file could not be opened. *)


val open_in_bin : string -> BatIO.input
(** Same as {!Pervasives.open_in}, but the file is opened in binary mode,
   so that no translation takes place during reads. On operating
   systems that do not distinguish between text mode and binary
   mode, this function behaves like {!Pervasives.open_in}. *)

val open_in_gen : open_flag list -> int -> string -> BatIO.input
(** [open_in mode perm filename] opens the named file for reading,
    as described above. The extra arguments [mode] and [perm]
    specify the opening mode and file permissions.
    {!Pervasives.open_in} and {!Pervasives.open_in_bin} are special
    cases of this function.
    
    @deprecated Use {!open_in instead}*)


val input_char : BatIO.input -> char
(** Read one character from the given input channel.
   Raise [End_of_file] if there are no more characters to read. *)

val input_line : BatIO.input -> string
(** Read characters from the given input channel, until a
   newline character is encountered. Return the string of
   all characters read, without the newline character at the end.
   Raise [End_of_file] if the end of the file is reached
   at the beginning of line. *)

val input : BatIO.input -> string -> int -> int -> int
(** [input ic buf pos len] reads up to [len] characters from
   the given channel [ic], storing them in string [buf], starting at
   character number [pos].
   It returns the actual number of characters read, between 0 and
   [len] (inclusive).
   A return value of 0 means that the end of file was reached.
   A return value between 0 and [len] exclusive means that
   not all requested [len] characters were read, either because
   no more characters were available at that time, or because
   the implementation found it convenient to do a partial read;
   [input] must be called again to read the remaining characters,
   if desired.  (See also {!Pervasives.really_input} for reading
   exactly [len] characters.)
   Exception [Invalid_argument "input"] is raised if [pos] and [len]
   do not designate a valid substring of [buf]. *)

val really_input : BatIO.input -> string -> int -> int -> unit
(** [really_input ic buf pos len] reads [len] characters from channel [ic],
    storing them in string [buf], starting at character number [pos].
    Raise [End_of_file] if the end of file is reached before [len]
    characters have been read.
    Raise [Invalid_argument "really_input"] if
    [pos] and [len] do not designate a valid substring of [buf]. *)

val input_byte : BatIO.input -> int
(** Same as {!Pervasives.input_char}, but return the 8-bit integer representing
    the character.
    Raise [End_of_file] if an end of file was reached. *)

val input_binary_int : BatIO.input -> int
(** Read an integer encoded in binary format (4 bytes, big-endian)
    from the given input channel. See {!Pervasives.output_binary_int}.
    Raise [End_of_file] if an end of file was reached while reading the
    integer. *)

val input_binary_float : BatIO.input -> float
(** Read a float encoded in binary format (8 bytes, IEEE 754 double format)
    from the given input channel. See {!Pervasives.output_binary_float}.
    Raise [End_of_file] if an end of file was reached while reading the
    float. *)

val input_value : BatIO.input -> 'a
(** Read the representation of a structured value, as produced
    by {!output_value}, and return the corresponding value.
    This function is identical to {!Marshal.input};
    see the description of module {!Marshal} for more information,
    in particular concerning the lack of type safety. *)
  
val close_in : BatIO.input -> unit
  (** Close the given channel.  Input functions raise a [Sys_error]
      exception when they are applied to a closed input channel,
      except [close_in], which does nothing when applied to an already
      closed channel.  Note that [close_in] may raise [Sys_error] if
      the operating system signals an error. *)
  
val close_in_noerr : BatIO.input -> unit
(** Same as [close_in], but ignore all errors. *)
  

(**
   {6 Fundamental functions and operators}
*)

external identity : 'a -> 'a = "%identity"
(** The identity function. *)

val undefined : ?message:string -> 'a -> 'b
(** The undefined function.

    Evaluating [undefined x] always fails and raises an exception 
    "Undefined". Optional argument [message] permits the 
    customization of the error message.*)


val ( |> ) : 'a -> ('a -> 'b) -> 'b
(** Function application. [x |> f] is equivalent to [f x]. 

    This operator is commonly used to write a function composition by
    order of evaluation (the order used in object-oriented
    programming) rather than by inverse order (the order typically
    used in functional programming).  

    For instance, [g (f x)] means "apply [f] to [x], then apply [g] to
    the result." The corresponding notation in most object-oriented
    programming languages would be somewhere along the lines of [x.f.g
    ()], or "starting from [x], apply [f], then apply [g]." In OCaml,
    operator ( |> ) this latest notation maps to [x |> f |> g], or
    
    This operator may also be useful for composing sequences of
    function calls without too many parenthesis. *)

val ( **>  ) : ('a -> 'b) -> 'a -> 'b
  (** Function application. [f **> x] is equivalent to [f x]. 
      
      This operators may be useful for composing sequences of
      function calls without too many parenthesis.

      {b Note} The name of this operator is not written in stone.
      It is bound to change soon.*)

val ( |- ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
(** Function composition. [f |- g] is [fun x -> g (f x)]. 
    This is also equivalent to applying [<**] twice.*)

val ( -| ) : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b
(** Function composition. [f -| g] is [fun x -> f (g x)]. Mathematically, this is
    operator o.*)

val flip : ( 'a -> 'b -> 'c ) -> 'b -> 'a -> 'c
  (** Argument flipping. 
      
      [flip f x y] is [f y x]. Don't abuse this function, it may shorten considerably
      your code but it also has the nasty habit of making it harder to read.*)
      

val ( *** ) : ('a -> 'b) -> ('c -> 'd) -> 'a * 'c -> 'b * 'd
(** Function pairing.

    [f *** g] is [fun (x,y) -> (f x, g y)].*)

val ( &&& ) : ('a -> 'b) -> ('a -> 'c) -> 'a -> ('b * 'c)
  (** Applying two functions to the same argument.

      [ f &&& g] is [fun x -> (f x, g x)]. *)

val first : ('a -> 'b) -> ('a * 'c) -> ('b * 'c)
(** Apply a function to the first element of a pair.

    [first f (x, y)] is [(f x, y)]
 *)

val second : ('a -> 'b) -> ('c * 'a) -> ('c * 'b)
(** Apply a function to the second element of a pair.

    [second f (x, y)] is [(x, f y)]
 *)


val curry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c
(** Convert a function which accepts a pair of arguments into
    a function which accepts two arguments.

    [curry f] is [fun x y -> f (x,y)]*)

val uncurry : ('a -> 'b -> 'c) -> 'a * 'b -> 'c
  (** Convert a function which accepts a two arguments into a function
      which accepts a pair of arguments.

      [uncurry f] is [fun (x, y) -> f x y]*)

val const : 'a -> (_ -> 'a)
(** Ignore its second argument.

    [const x] is the function which always returns [x].*)

val unique : unit -> int
(** Returns an unique identifier every time it is called.

    {b Note} This is thread-safe.*)

val tap : ('a -> unit) -> 'a -> 'a
  (** Allows application of a function in the middle of a pipe
      sequence without disturbing the sequence.  [x |> tap f]
      evaluates to [x], but has the side effect of [f x].  Useful for
      debugging. *)

val finally : (unit -> unit) -> ('a -> 'b) -> 'a -> 'b 
  (** [finally fend f x] calls [f x] and then [fend()] even if [f x] raised
      an exception. *)

val with_dispose : dispose:('a -> unit) -> ('a -> 'b) -> 'a -> 'b
(** [with_dispose dispose f x] invokes [f] on [x], calling [dispose x]
    when [f] terminates (either with a return value or an
    exception). *)

val args : unit -> string BatEnum.t
  (** An enumeration of the arguments passed to this program through the command line.

      [args ()] is given by the elements of [Sys.argv], minus the first element.*)

(**/**)
val invisible_args : int ref
(** The number of arguments which must never be returned by [args]

    Typically, [invisible_args] is [1], to drop the name of the executable. However,
    in some circumstances, it may be useful to pretend that some arguments need not
    be parsed.
*)
(**/**)


val exe  : string
  (** The name of the current executable.

      [exe] is given by the first argument of [Sys.argv]*)


(**
   {6 Enumerations}

   In OCaml Batteries Included, all data structures are enumerable,
   which means that they support a number of standard operations,
   transformations, etc. The general manner of {i enumerating} the
   contents of a data structure is to invoke the [enum] function of
   your data structure.

   For instance, you may use the {!foreach} loop to apply a function
   [f] to all the consecutive elements of a string [s]. For this
   purpose, you may write either [foreach (String.enum s) f] or [open
   String in foreach (enum s) f]. Either possibility states that you
   are enumerating through a character string [s]. Should you prefer
   your enumeration to proceed from the end of the string to the
   beginning, you may replace {! String.enum} with {!
   String.backwards}. Therefore, either [foreach (String.backwards s)
   f] or [open String in foreach (backwards s) f] will apply [f]
   to all the consecutive elements of string [s], from the last to
   the first.

   Similarly, you may use {!List.enum} instead of {!String.enum} to
   visit the elements of a list in the usual order, or
   {!List.backwards} instead of {!String.backwards} to visit them
   in the opposite order, or {!Hashtbl.enum} for hash tables, etc.

   More operations on enumerations are defined in module {!BatEnum},
   including the necessary constructors to make your own structures
   enumerable.

   The various kinds of loops are detailed further in this documentation.
*)

val foreach: 'a BatEnum.t -> ('a -> unit) ->  unit
  (** Imperative loop on an enumeration.

      [foreach e f] applies function [f] to each successive element of [e].
      For instance, [foreach (1 -- 10) print_int] invokes function [print_int]
      on [1], [2], ..., [10], printing [12345678910].

      {b Note} This function is one of the many loops available on
      enumerations.  Other commonly used loops are {!iter} (same usage
      scenario as [foreach], but with different notations), {!map}
      (convert an enumeration to another enumeration) or {!fold}
      (flatten an enumeration by applying an operation to each
      element).

  *)

(**
   {7 General-purpose loops}

   {topic loops}

   The following functions are the three main general-purpose loops
   available in OCaml. By opposition to the loops available in
   imperative languages, OCaml loops are regular functions, which
   may be passed, composed, currified, etc. In particular, each
   of these loops may be considered either as a manner of applying
   a function to a data structure or as transforming a function
   into another function which will act on a whole data structure.

   For instance, if [f] is a function operating on one value, you may
   lift this function to operate on all values of an enumeration (and
   consequently on all values of any data structure of OCaml Batteries
   Included) by applying {!iter}, {!map} or {!fold} to this function.
*)


val iter : ('a -> unit) -> 'a BatEnum.t -> unit
  (** Imperative loop on an enumeration. This loop is typically used
      to lift a function with an effect but no meaningful result and
      get it to work on enumerations.

      If [f] is a function [iter f] is a function which behaves as [f]
      but acts upon enumerations rather than individual elements. As
      indicated in the type of [iter], [f] must produce values of type
      [unit] (i.e. [f] has no meaningful result) the resulting function
      produces no meaningful result either.

      In other words, [iter f] is a function which, when applied upon
      an enumeration [e], calls [f] with each element of [e] in turn.

      For instance, [iter f (1 -- 10)] invokes function [f] on [1],
      [2], ..., [10] and produces value [()].
  *)

val map : ('a -> 'b) -> 'a BatEnum.t -> 'b BatEnum.t
  (** Transformation loop on an enumeration, used to build an enumeration
      from another enumeration. This loop is typically used to transform
      an enumeration into another enumeration with the same number of
      elements, in the same order.

      If [f] is a function, [map f e] is a function which behaves as
      [f] but acts upon enumerations rather than individual elements --
      and builds a new enumeration from the results of each application.

      In other words, [map f] is a function which, when applied
      upon an enumeration containing elements [e0], [e1], ...,
      produces enumeration [f e0], [f e1], ...

      For instance, if [odd] is the function which returns [true]
      when applied to an odd number or [false] when applied to
      an even number, [map odd (1 -- 10)] produces enumeration
      [true], [false], [true], ..., [false].

      Similarly, if [square] is the function [fun x -> x * x],
      [map square (1 -- 10)] produces the enumeration of the
      square numbers of all numbers between [1] and [10].
  *)


val filter_map : ('a -> 'b option) -> 'a BatEnum.t -> 'b BatEnum.t
  (** Similar to a map, except that you can skip over some items of the
      incoming enumeration by returning None instead of Some value.
      Think of it as a {!filter} combined with a {!map}.
  *)


val reduce : ('a -> 'a -> 'a) -> 'a BatEnum.t -> 'a
  (** Transformation loop on an enumeration, used to build a single value
      from an enumeration.

      If [f] is a function and [e] is an enumeration, [reduce f e] applies
      function [f] to the first two elements of [e], then to the result of this
      expression and to the third element of [e], then to the result of this
      new expression and to the fourth element of [e]...

      In other words, [reduce f e] returns [a0] if [e] contains only
      one element [a0], otherwise [f (... (f (f a0) a1) ...) aN] where
      [a0,a1..aN] are the elements of [e]. 

      @raises Not_found if [e] is empty.

      For instance, if [add] is the function [fun x y -> x + y],
      [reduce add] is the function which computes the sum of the
      elements of an enumeration -- and doesn't work on empty
      enumerations. Therefore, [reduce add (1 -- 10)]
      produces result [55].
  *)

val fold : ('b -> 'a -> 'b) -> 'b -> 'a BatEnum.t -> 'b
  (** Transformation loop on an enumeration, used to build a single value
      from an enumeration. This is the most powerful general-purpose
      loop and also the most complex.

      If [f] is a function, [fold f v e] applies [f v] to the first
      element of [e], then, calling [acc_1] the result of this
      operation, applies [f acc_1] to the second element of [e], then,
      calling [acc_2] the result of this operation, applies [f acc_2]
      to the third element of [e]...

      In other words, [fold f v e] returns [v] if [e] is empty,
      otherwise [f (... (f (f v a0) a1) ...) aN] where a0,a1..aN are
      the elements of [e]. 

      For instance, if [add] is the function [fun x y -> x + y],
      [fold add 0] is the function which computes the sum of the
      elements of an enumeration. Therefore, [fold add 0 (1 -- 10)]
      produces result [55].
  *)

val scanl : ('b -> 'a -> 'b) -> 'b -> 'a BatEnum.t -> 'b BatEnum.t
  (** Functional loop on an enumeration, used to build an enumeration
      from both an enumeration and an initial value. This function may
      be seen as a variant of {!fold} which returns not only the final
      result of {!fold} but the enumeration of all the intermediate
      results of {!fold}.

      If [f] is a function, [scanl f v e] is applies [f v] to the first
      element of [e], then, calling [acc_1] the result of this
      operation, applies [f acc_1] to the second element of [e], then,
      calling [acc_2] the result of this operation, applies [f acc_2]
      to the third element of [e]...

      For instance, if [add] is the function [fun x y -> x + y],
      [scanl add 0] is the function which computes the sum of the
      elements of an enumeration. Therefore, [scanl add 0 (1 -- 10)]
      produces result the enumeration with elements [0, 1, 3, 6, 10,
      15, 21, 28, 36, 45, 55].  *)

val ( /@ ) : 'a BatEnum.t -> ('a -> 'b) -> 'b BatEnum.t

val ( @/ ) : ('a -> 'b) -> 'a BatEnum.t -> 'b BatEnum.t
  (**
     Mapping operators.

     These operators have the same meaning as function {!map} but are
     sometimes more readable than this function, when chaining
     several transformations in a row.
  *)

val ( //@ ) : 'a BatEnum.t -> ('a -> 'b option) -> 'b BatEnum.t

val ( @// ) : ('a -> 'b option) -> 'a BatEnum.t -> 'b BatEnum.t
  (**
    Map combined with filter. Same as {!filter_map}.
  *)

(**
   {7 Other operations on enumerations}
*)

val exists: ('a -> bool) -> 'a BatEnum.t -> bool
(** [exists f e] returns [true] if there is some [x] in [e] such
    that [f x]*)

val for_all: ('a -> bool) -> 'a BatEnum.t -> bool
(** [exists f e] returns [true] if for every [x] in [e], [f x] is true*)



val find : ('a -> bool) -> 'a BatEnum.t -> 'a
  (** [find f e] returns the first element [x] of [e] such that [f x] returns
      [true], consuming the enumeration up to and including the
      found element, or, raises [Not_found] if no such element exists
      in the enumeration, consuming the whole enumeration in the search.
      
      Since [find] consumes a prefix of the enumeration, it can be used several 
      times on the same enumeration to find the next element. *)

val peek : 'a BatEnum.t -> 'a option
  (** [peek e] returns [None] if [e] is empty or [Some x] where [x] is
      the next element of [e]. The element is not removed from the
      enumeration. *)

val get : 'a BatEnum.t -> 'a option
  (** [get e] returns [None] if [e] is empty or [Some x] where [x] is
      the next element of [e], in which case the element is removed
      from the enumeration. *)

val push : 'a BatEnum.t -> 'a -> unit
  (** [push e x] will add [x] at the beginning of [e]. *)
  
val junk : 'a BatEnum.t -> unit
  (** [junk e] removes the first element from the enumeration, if any. *)

val filter : ('a -> bool) -> 'a BatEnum.t -> 'a BatEnum.t
  (** [filter f e] returns an enumeration over all elements [x] of [e] such
      as [f x] returns [true]. *)

val ( // ) : 'a BatEnum.t -> ('a -> bool) -> 'a BatEnum.t
(** Filtering (pronounce this operator name "such that").

    For instance, [(1 -- 37) // odd] is the enumeration of all odd
    numbers between 1 and 37.*)

val concat : 'a BatEnum.t BatEnum.t -> 'a BatEnum.t
  (** [concat e] returns an enumeration over all elements of all enumerations
      of [e]. *)


val ( -- ) : int -> int -> int BatEnum.t
(** Enumerate numbers.

    [5 -- 10] is the enumeration 5,6,7,8,9,10.
    [10 -- 5] is the empty enumeration*)

val ( --^ ) : int -> int -> int BatEnum.t
(** Enumerate numbers, without the right endpoint

    [5 -- 10] is the enumeration 5,6,7,8,9.
*)

val ( --. ) : (float * float) -> float -> float BatEnum.t
(** [(a, step) --. b)] creates a float enumeration from [a] to [b] with an
    increment of [step] between elements.

    [(5.0, 1.0) --. 10.0] is the enumeration 5.0,6.0,7.0,8.0,9.0,10.0.
    [(10.0, -1.0) --. 5.0] is the enumeration 10.0,9.0,8.0,7.0,6.0,5.0.
    [(10.0, 1.0) --. 1.0] is the empty enumeration. *)

val ( --- ) : int -> int -> int BatEnum.t
(** As [--], but accepts enumerations in reverse order.

    [5 --- 10] is the enumeration 5,6,7,8,9,10.
    [10 --- 5] is the enumeration 10,9,8,7,6,5.*)

val ( --~ ) : char -> char -> char BatEnum.t
(** As ( -- ), but for characters.*)

val print :  ?first:string -> ?last:string -> ?sep:string -> ('a BatInnerIO.output -> 'b -> unit) -> 'a BatInnerIO.output -> 'b BatEnum.t -> unit
(** Print and consume the contents of an enumeration.*)

(**/**)

(**
   {6 Printing directives}
*)

(** Flags used to modify the printing behaviour *)
type printer_flags = {
  pf_width : int option;
  (** If with of printed material is less than this one, padding is
      added *)
  pf_frac_digits : int option;
  (** When printing a float, print this many digits after the decimal point *)
  pf_padding_char : char;
  (** Character used for padding *)
  pf_justify : [ `right | `left ];
  (** Where to add the padding. Defaults to [`right] *)
  pf_positive_prefix : char option;
  (** Prefix positive numbers with this character *)
}

val default_printer_flags : printer_flags
  (** Default printer flags *)

(** {7 Equivalent of classical directives} *)

val printer_a : ((unit BatIO.output -> 'a -> unit) -> 'a -> 'b, 'b) BatPrint.directive
val printer_t : ((unit BatIO.output -> unit) -> 'a, 'a) BatPrint.directive
val printer_B : (bool -> 'a, 'a) BatPrint.directive
val printer_c : (char -> 'a, 'a) BatPrint.directive
val printer_C : (char -> 'a, 'a) BatPrint.directive
val printer_s : ?flags : printer_flags -> (string -> 'a, 'a) BatPrint.directive
val printer_S : ?flags : printer_flags -> (string -> 'a, 'a) BatPrint.directive

val printer_d : ?flags : printer_flags -> (int -> 'a, 'a) BatPrint.directive
val printer_i    : ?flags : printer_flags -> (int -> 'a, 'a) BatPrint.directive
val printer_int  : ?flags : printer_flags -> (int -> 'a, 'a) BatPrint.directive
val printer_u    : ?flags : printer_flags -> (int -> 'a, 'a) BatPrint.directive
val printer_uint : ?flags : printer_flags -> (int -> 'a, 'a) BatPrint.directive
val printer_x : ?flags : printer_flags -> (int -> 'a, 'a) BatPrint.directive
val printer_X : ?flags : printer_flags -> (int -> 'a, 'a) BatPrint.directive
val printer_hex : ?flags : printer_flags -> (int -> 'a, 'a) BatPrint.directive
val printer_HEX : ?flags : printer_flags -> (int -> 'a, 'a) BatPrint.directive
val printer_o : ?flags : printer_flags -> (int -> 'a, 'a) BatPrint.directive
val printer_oct : ?flags : printer_flags -> (int -> 'a, 'a) BatPrint.directive
val printer_ld : ?flags : printer_flags -> (int32 -> 'a, 'a) BatPrint.directive
val printer_li : ?flags : printer_flags -> (int32 -> 'a, 'a) BatPrint.directive
val printer_lu : ?flags : printer_flags -> (int32 -> 'a, 'a) BatPrint.directive
val printer_lx : ?flags : printer_flags -> (int32 -> 'a, 'a) BatPrint.directive
val printer_lX : ?flags : printer_flags -> (int32 -> 'a, 'a) BatPrint.directive
val printer_lo : ?flags : printer_flags -> (int32 -> 'a, 'a) BatPrint.directive
val printer_Ld : ?flags : printer_flags -> (int64 -> 'a, 'a) BatPrint.directive
val printer_Li : ?flags : printer_flags -> (int64 -> 'a, 'a) BatPrint.directive
val printer_Lu : ?flags : printer_flags -> (int64 -> 'a, 'a) BatPrint.directive
val printer_Lx : ?flags : printer_flags -> (int64 -> 'a, 'a) BatPrint.directive
val printer_LX : ?flags : printer_flags -> (int64 -> 'a, 'a) BatPrint.directive
val printer_Lo : ?flags : printer_flags -> (int64 -> 'a, 'a) BatPrint.directive
val printer_nd : ?flags : printer_flags -> (nativeint -> 'a, 'a) BatPrint.directive
val printer_ni : ?flags : printer_flags -> (nativeint -> 'a, 'a) BatPrint.directive
val printer_nu : ?flags : printer_flags -> (nativeint -> 'a, 'a) BatPrint.directive
val printer_nx : ?flags : printer_flags -> (nativeint -> 'a, 'a) BatPrint.directive
val printer_nX : ?flags : printer_flags -> (nativeint -> 'a, 'a) BatPrint.directive
val printer_no : ?flags : printer_flags -> (nativeint -> 'a, 'a) BatPrint.directive

val printer_f : ?flags : printer_flags -> (float -> 'a, 'a) BatPrint.directive
val printer_F : ?flags : printer_flags -> (float -> 'a, 'a) BatPrint.directive

(** {7 Batteries-specific directives} *)

val printer_format : (('a, 'b) BatPrint.format -> 'a, 'b) BatPrint.directive
  (** [printer_format] takes a format, then the arguments of the format and
      print according to it. For instance,

      {[sprintf p"x = %format * %d" p"%d + %d" 1 3 5]} produces ["x = 1 + 3 * 5"]
  *)

val printer_sc : ?flags : printer_flags -> ([> `Read] BatString.Cap.t -> 'a, 'a) BatPrint.directive
val printer_Sc : ?flags : printer_flags -> ([> `Read] BatString.Cap.t -> 'a, 'a) BatPrint.directive
val printer_rope : (BatRope.t -> 'a, 'a) BatPrint.directive
val printer_utf8 : (BatUTF8.t -> 'a, 'a) BatPrint.directive
val printer_obj : (< print : unit BatIO.output -> unit; .. > -> 'a, 'a) BatPrint.directive
val printer_exn : (exn -> 'a, 'a) BatPrint.directive

(** {7 Value printers} *)

val bool_printer : bool BatValue_printer.t
val int_printer : int BatValue_printer.t
val char_printer : char BatValue_printer.t
val int32_printer : int32 BatValue_printer.t
val int64_printer : int64 BatValue_printer.t
val nativeint_printer : nativeint BatValue_printer.t
val float_printer : float BatValue_printer.t
val string_printer : string BatValue_printer.t
val list_printer : 'a BatValue_printer.t -> 'a list BatValue_printer.t
val array_printer : 'a BatValue_printer.t -> 'a array BatValue_printer.t
val option_printer : 'a BatValue_printer.t -> 'a option BatValue_printer.t
val maybe_printer : 'a BatValue_printer.t -> 'a option BatValue_printer.t
val exn_printer : exn BatValue_printer.t

(**/**)

(**
   {6 Results}
*)

type ('a, 'b) result = ('a, 'b) BatStd.result =
  | Ok  of 'a
  | Bad of 'b


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

