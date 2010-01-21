(*
 * Print - Functional unparsing
 * Copyright (C) 2009 Jeremie Dimino
 * Copyright (C) 2009 David Rajchenbach-Teller, LIFO, Universite d'Orleans (documentation)
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

(** Formatted output functions (also known as unparsing).

    This module define a printf facility similar to the one defined by
    {!BatPrintf} but in a safer and extensible way.

    Classical printf implementation (in ocaml and other languages)
    deals with format strings as regular strings which are parsed at
    runtime. This approach is problematic considering typing and make
    it almost impossible to define new directives.

    This module attempts to handle the problem in a more functional
    way. For convenience it can be used with the syntax extension
    [batteries.pa_string.syntax].

    Look at the example ["examples/snippets/test_printf.ml"] for an
    example of the use of this module.

    @author Jeremie Dimino
    @author David Rajchenbach-Teller (documentation)

    @documents Print
*)

(**
   {6 General overview}

   The functions of this module produce output, according to {!directive}s,
   as described below. Output may be used to write to the screen, to error
   channels, to strings, to buffers, to the network, etc.

   This module is very powerful and allows concise and extensible definition
   of output formats. The downside is that this module may be quite confusing
   at first. Don't worry, it's worth it.

   The foremost concept is that of {!type:format}. A {!type:format} is
   a description of how informations should be printed. For instance,
   a format such as [p"%d\n"] may be used to display an integer (this
   is what {[%d]} means -- [d] stands for decimal) and will end the
   line at the end of this integer (this is what {[\n]} means
   everywhere in OCaml -- it stands for newline). Similarly, a format
   such as [p"(%d, %d)\n"] may be used to display two integers as a
   pair, and then end the line: {[%d]} and {[\n]} have the same
   meaning as previously, while the other characters are interpreted
   as regular text. Therefore, when applied to numbers 5 and 10, this
   format will output ["(5, 10)\n"].

   Formats may be applied using functions such as {!printf}. For
   instance, [Print.printf p"(%d, %d)\n" 5 10] will print on the
   screen ["(5, 10)\n"]. Similarly, if [foo] is an {!type:BatIO.output}
   which may be used to write on a file, [Printf.fprintf foo p"(%d,
   %d)\n" 5 10] will write ["(5, 10)\n"] to this file.

   Let us consider formats more in detail.

   {7 Formats}
   
   The simplest format is the empty format, or [p""] -- notice the [p]
   (for "print"), all formats start with this letter. This format is
   unable to display any information. Actually, this format is only
   able to output the empty string, which is not very useful. Its type
   is [('a, 'a, 'b) format]. This ['b] states that it can be used with
   any kind of output.

   Slightly more complex is format [p"foo"]. This format is also
   unable to display any information but, when applied, it will output
   text ["foo"]. Its type is also [('a, 'a, 'b) format]. More generally,
   any text without special characters may be used as a format, which
   can be used to display exactly that text, without any additional
   information.

   Formats also support {e directives}, which serve to insert
   additional pieces of information inside the text. Directives are
   always introduced with special character [%]. For instance, format
   [p"%d"] may be used to display integers. Its type is [(int -> 'a,
   'a, 'b) format]. Here, type parameter [int -> 'a] states that this
   directive expects an integer. A more complex format such as [p"some
   text before the number%d"] or, equivalently, [p"some text before
   the number%(d)"] would have the same type [(int -> 'a, 'a, 'b)
   format]. Again,[p"some text before the number%(d)some text after
   the number"] would have the same type -- in this case, parenthesis
   are compulsory, for reasons we will detail later. When applied with
   argument 5, this last format will output text [p"some text before the
   number5some text after the number"].

   {7 Directives}
   
   {8 General-purpose directives}

   General-purpose directives start with [{%{}] and end with [{}}]. By default,
   functions of this module recognize the following general-purpose directives:

   - [{%{int}}] or [{%{Int.t}}] format an integer with the usual notation (i.e. signed decimal).
   - [{%{char}}] or [{%{Char.t}}] insert a character
   - [{%{string}}] or [{%{String.t}}] insert a string
   - [{%{float}}] or [{%{Float.t}}] format a floating-point number as a decimal, in the style [dddd.ddd]
   - [{%{bool}}] or [{%{Bool.t}}] format a boolean argument to the string ["true"] or ["false"]
   - [{%{int32}}] or [{%{Int32.t}}] format an integer with the usual notation (i.e. signed decimal).
   - [{%{int64}}] or [{%{Int64.t}}] format an integer with the usual notation (i.e. signed decimal).
   - [{%{nativeint}}] or [{%{Native_int.t}}] format an integer with the usual notation (i.e. signed decimal).
   - [{%{Rope.t}}] insert a {!Rope}
   - [{%{foo list}}] or [{%{foo List.t}}], where [foo] is the name of another directive, such as [int], [char], [string], [int list]. etc., format a list of items (for more control on list formatting, see {!List.print}
   - [{%{foo array}}] or [{%{foo Array.t}}], where [foo] is the name of another directive, such as [int], [char], [string], etc., format a array of items (for more control on array formatting, see {!Array.print}
   - [{%{foo maybe}}] , where [foo] is the name of another directive, such as [int], [char], [string], etc., format an optional [x] item to nothing if [x] is [None] or to the result of directive [foo] applied to [y], if [x] is [Some y]
   - [{%{foo option}}] or [{%{foo Option.t}}], where [foo] is the name of another directive, such as [int], [char], [string], etc., format an ['a option] to OCaml syntax, i.e. ["None"] or ["Some bar"]

   Additional directives may be plugged-in.


   {8 Short-hand directives}

   A number of short-hand directives are also provided for most common
   situations.  By default, functions of this module recognize the
   following short-hand directives:

   - [%d], [%i], [%n], [%l], [%L], or [%N]: format an integer with the
   usual notation (i.e. signed decimal). 
   - [%u]: format an integer as an unsigned decimal.
   - [%x]: format an integer as an unsigned hexadecimal, using lowercase letters.
   - [%X]: format an integer as an unsigned hexadecimal, using upper letters.
   - [%o]: format an integer as an unsigned octal.
   - [%s]: insert a string.
   - [%S]: insert a quoted string in Caml syntax (double quotes, escapes).
   - [%sc]: insert a read-only/read-write string (see {!String.Cap}).
   - [%Sc]: insert a quoted read-only/read-write string in Caml syntax (double quotes, escapes).
   - [%rope]: insert a rope (see {!Rope}).
   - [%utf8]: insert a UTF-8 encoded string.
   - [%c]: insert a character argument.
   - [%C]: insert a character argument in Caml syntax (single quotes, escapes).
   - [%f]: format a floating-point number as a decimal,
   in the style [dddd.ddd].
   - [%F]: format a floating-point number in Caml syntax ([dddd.]
   or [dddd.ddd] or [d.ddd e+-dd]).
   - [%e] or [%E]: format a floating-point number as a decimal,
   in the style [d.ddd e+-dd] (mantissa and exponent).
   - [%g] or [%G]: format a floating-point number as a decimal
   in style [%f] or [%e], [E] (whichever is more compact).
   - [%B]: format a boolean argument to the string ["true"] or ["false"]
   - [%b]: format a boolean argument (for backward compatibility; do not
   use in new programs).
   - [%ld], [%li], [%lu], [%lx], [%lX], [%lo]: format an [int32] 
   respectively as a signed decimal/a signed decimal/an unsigned decimal/
   an unsigned hexadecimal in lowercase, an unsigned decimal in uppercase/
   an unsigned octal.
   - [%nd], [%ni], [%nu], [%nx], [%nX], [%no]: format a [nativeint] 
   respectively as a signed decimal/a signed decimal/an unsigned decimal/
   an unsigned hexadecimal in lowercase, an unsigned decimal in uppercase/
   an unsigned octal.
   - [%Ld], [%Li], [%Lu], [%Lx], [%LX], [%Lo]: format an [int64] 
   respectively as a signed decimal/a signed decimal/an unsigned decimal/
   an unsigned hexadecimal in lowercase, an unsigned decimal in uppercase/
   an unsigned octal.
   - [%obj]: format an object using its method [print: 'a output -> unit]
   - [%!]: take no argument and flush the output.
   - [%%]: take no argument and output one [%] character.

   Additional short-hand directives may also be plugged-in.


   {7 Extending formats}

   In addition to the above default set of directives, you may define your
   own directives or override existing ones. This is actually quite simple.

   {8 Adding general-purpose directives (with a module)}

   When defining a new data structure [Foo.t], it is usually a good
   idea to implement a function [Foo.t_printer], with type [Foo.t
   BatValue_printer]. That's it. Once this function is created, you may
   use directive [{%{Foo.t}}] in any printing function of this module.

   If the data structure has type arguments, as is the case of lists,
   arrays, etc., you will probably want to be able to print the
   contents of your data structure. For this purpose, you may define
   printers which accept as arguments other printers. For instance,
   the type of [List.t_printer] is actually ['a BatValue_printer.t -> 'a
   list BatValue_printer.t], etc. If you have given such a type to
   [Foo.t_printer], you may use [{%{Foo.t bar}}] in any printing
   function of this module, where [bar] is the name of another
   general-purpose directive.

   {8 Adding general-purpose directives (without a module)}

   An alternative technique for adding general-purpose directives is
   to define functions outside of their module. For instance, to
   plug-in a general-purpose directive for elements of type [foo], it
   is sufficient to define a function [bar_printer], with type [foo
   BatValue_printer]. That's it. Once this function is created, you may
   use directive [{%{bar}}] in any printing function of this module.

   Again, you may also choose to define functions which take other
   printer as arguments, as before.

   {8 Adding short-hand directives}

   Short-hands are convenient and easier to write but are also less
   powerful than general-purpose directives, insofar as they may not
   accept printers as arguments and may not be passed as arguments
   to printers.

   To define a short-hand directive [%foo] for formatting elements of type [bar],
   it is sufficient to create a function [printer_foo] with type [(bar
   -> 'a, 'a) directive]. Assuming that you already have a function
   [string_of_bar: bar -> string], [printer_foo] may be implemented
   simply as
   {[
   let printer_foo k x = k (fun oc -> BatIO.nwrite oc (string_of_bar x))
   ]}
   
   That's it. Once this function is created you may use directive [%foo]
   in any printing function of this module.

   To improve the flexibility of your newly created directive, you may wish
   to add an optional argument [flags] to function [printer_foo], to handle
   flags, as detailed in the following section.
   
   {7 Flags}

   The general format of directives is

   [% ( \[options\] \[width\] \[.precision\] name )]

   [name] is one of [d], [i], [n], [l], [L], [N], [u], [x] ..., and
   behaves as explained above.

   The optional [options] are:
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

   For more informations on supporting flags in your directives, see
   the documentation of type {!directive}.
*)

(** {6 Directives and formats} 

    You can skip this section if you are only interested in using this module
    and not in extending its behavior or understanding its internal mechanisms.
*)

type ('a, 'b) directive = ((unit BatInnerIO.output -> unit) -> 'b) -> 'a
  (** The underlying type of a directive. Directives are the basic elements of
      formats.

      A directive takes as arguments:

      - a continuation
      - any arguments they need

      its goal is to create a printer which prints the arguments of
      the directives and to pass it to the continuation.

      For example, directive [%d] has the following type

      {[
        val printer_d : (int -> 'a, 'a) directive
      ]}

      And here is a possible implementation:

      {[
        let printer_d k x = k (fun oc -> BatIO.nwrite oc (string_of_int x))
      ]}

      Additionally, directives can takes ``flags'' (like in ["%2d"]
      where [2] is here a width). Flags are passed to the
      directive as optional argument before the continuation, for
      example:

      {[
        val printer_d : ?width : int -> (int -> 'a, 'a) directive
      ]}

      with:

      {[
        let printer_d ?width k x =
          let str = string_of_int x in
          let str = match width with
            | None ->
                str
            | Some n ->
                let len = String.length str in
                if len < n then
                  String.make (n - len) ' ' ^ str
                else
                  str
          in
          k (fun oc -> BatIO.nwrite oc str)
      ]}

      Standard flags, i.e. the ones supported by the syntax extension,
      are:

      - the boolean flags (which must default to [false]):

        - [-] with label [left_justify]
        - [0] with label [pad_with_zeros]
        - [+] with label [prefix_with_plus]
        - [ ] with lavel [prefix_with_space]
        - [#] with label [alternate]

      - the integer flags [width] and [precision]
  *)

val literal : string -> ('a, 'a) directive
  (** [literal str] create a directive which do not take any argument
      from a literal string *)

(**/**)

type pattern = string
    (** A pattern is a string where directives are replaced by indices:

        For example the format string (with the syntax extension)
        [p"%s = %d"] will produce the pattern ["%(0) = %(1)"] *)

val format : unit BatInnerIO.output -> pattern -> (unit BatInnerIO.output -> unit) array -> unit
  (** [format oc pattern directives] prints [pattern] on [oc], using
      [directives] to handle directives (as indices) in the pattern.

      For example:

      {[
        format BatIO.stdout "x=%(0), y=%(1)" [|fun oc -> BatIO.nwrite oc "foo";
                                            fun oc -> BatIO.nwrite oc "bar"|]
      ]}

      will produce the following output:

      {[
        "x = foo, y = bar"
      ]}

      Additionally, ["%%"] is printed as ["%"] and ["%!"] prints
      nothing and is interpreted as a flush on the output.

      If the pattern is incorrect or one of the index is not a valid
      index, it raises [Invalid_argument "Batteries.Print.format"].
      This may happen for example if you use i18n and a format
      translation is not correct. *)

(**
   The format to use for displaying the various arguments passed to the function.
   
   You probably won't ever need to manipulate values of this type directly.

   Syntactically, the format is a character string which contains two types
   of objects: plain characters, which are simply copied, and directives,
   each of which causes the conversion and printing of arguments.
*)
type ('a, 'b) format = {
  pattern : pattern;
  (** The pattern of the format. To translate a format (for i18n) you
      should replace it by its translation. *)

  printer : pattern -> ('a, 'b) directive;
  (** The printer of the format. It takes as argument the pattern and
      return a directive which will prints according to the pattern
      and the format.

      For example, with the syntax extension, the format:

      {[
         p"x = %d, y = %s"
      ]}

      will be expanded into the following expression:

      {[
        { pattern = "x = %(0), y = %(1)";
          printer = (fun pattern k ->
                       let printers = Array.create 2 (fun oc -> ()) in
                       printer_d (fun p ->
                                    printers.(0) <- p;
                                    printer_s (fun p ->
                                                 printers.(1) <- p;
                                                 k (fun oc -> format pattern printers)))) }
      ]}
  *)
}

(**/**)

(** {6 Formatting functions} *)

val printf : ('a, unit) format -> 'a
  (** [printf fmt args] formats the arguments in [args] as specified by [fmt]
      and prints the result on the standard output {!BatIO.stdout}, i.e. normally
      to the screen. If you are lost, this is probably the function you're looking for.*)

val eprintf : ('a, unit) format -> 'a
  (** [eprintf fmt args] behaves as [printf fmt args] but prints the result on
      the error out put {!BatIO.stderr} rather on the screen. This function is typically
      used to display warnings and errors, which may later be separated from "regular"
      printouts.*)

val sprintf : ('a, string) format -> 'a
  (** A function which doesn't print its result but returns it as a string. Useful
      for building messages, for translation purposes or for display in a window,
      for instance.

      While this function is quite convenient, don't abuse it to create very large
      strings such as files, that's not its role. For this kind of usage, prefer
      the more modular and usually faster {!fprintf}.*)


(** {6 Generic functions}*)

val fprintf : 'a BatInnerIO.output -> ('b, unit) format -> 'b
  (**General formatting function. 

     This function behaves mostly as {!printf} or {!eprintf} but,
     instead of printing to a predefined output, it takes as argument
     the output to which it should print.

     Typically, if you are attempting to build a large output such as a
     file, this is probably the function you are looking for. If you
     are writing a pretty-printer, this is probably the function you
     are looking for. If you are you are looking for a function to
     combine with directive [%a], this is also probably the function
     you are looking for.*)

val ifprintf: _        -> ('b, unit) format -> 'b
  (**As {!fprintf} but doesn't actually print anything.
     Sometimes useful for debugging.*)

val bprintf : Buffer.t -> ('a, unit) format -> 'a
  (** This function behaves as {!fprintf} but prints into a buffer rather than into an [output].*)

(** {6 Functions with continuations}*)

val kfprintf : ('a BatInnerIO.output -> 'b) -> 'a BatInnerIO.output -> ('c, 'b) format -> 'c
  (** [kfprintf k oc fmt] prints on [oc] then call [k] with [oc] as
      argument *)

val rprintf : ('a, BatRope.t) format -> 'a
  (** [rprintf fmt] returns the result as a rope *)

val krprintf : (BatRope.t -> 'b) -> ('a, 'b) format -> 'a
  (** [krprintf k fmt] creates a rope from the format and other
      arguments and pass it to [k] *)

val ksprintf : (string -> 'b) -> ('a, 'b) format -> 'a
  (** [ksprintf k fmt] creates a string from the format and other
      arguments and pass it to [k] *)

val kbprintf : (Buffer.t -> 'b) -> Buffer.t -> ('a, 'b) format -> 'a
  (** [bprintf k buf fmt] prints into the buffer [buf], then call [k]
      with [buf] as argument. *)

