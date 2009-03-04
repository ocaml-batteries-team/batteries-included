(*
 * ExtPrint - Functionnal unparsing
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
    {!ExtPrintf} but in a safer and extensible way.

    Classical printf implementation (in ocaml and other languages)
    deals with format strings as regular strings which are parsed at
    runtime. This approach is problematic considering typing and make
    it almost impossible to define new directives.

    This module attempts to handle the problem in a more functionnal
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
   screen ["(5, 10)\n"]. Similarly, if [foo] is an {!type:IO.output}
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

   {b TODO} List of directives?

   {7 Flags}
   {b TODO}

   {7 Extending formats}

   Format supports a number of default directives. However, it's also
   quite possible to use directives which haven't been defined yet.
   
   {b TODO}
*)

(** {6 Directives and formats} *)

type ('a, 'b, 'acc) directive = (('acc IO.output -> unit) -> 'b) -> 'a
  (** Type of a directive. Directives are the basic elements of
      formats. ['acc] is the type of the accumulator as in {!IO.output}.

      A directive takes as argument:

      - a continuation
      - any arguments they need

      its goal is to create a printer which prints the arguments of
      the directives and to pass it to the continuation.

      For example, considering ["%d"], the new directive have the
      following type:

      {[
        val pdir_d : (int -> 'a, 'a, _) directive
      ]}

      And here is a possible implementation:

      {[
        let pdir_d k x = k (fun oc -> IO.nwrite oc (string_of_int x))
      ]}

      Additionnally, directives can takes ``flags'' (like in ["%2d"]
      where [2] is here a width). Flags are passed to the
      directive as optionnal argument before the continuation, for
      example:

      {[
        val pdir_d : ?width : int -> (int -> 'a, 'a) directive
      ]}

      with:

      {[
        let pdir_d ?width k x =
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
          k (fun oc -> IO.nwrite oc str)
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

val literal : string -> ('a, 'a, 'acc) directive
  (** [literal str] create a directive which do not take any argument
      from a literal string *)

type pattern = string
    (** A pattern is a string where directives are replaced by index:

        For example the format string (with the syntax extension)
        [p"%s = %d"] will produce the pattern ["%(0) = %(1)"] *)

val format : 'a IO.output -> pattern -> ('a IO.output -> unit) array -> unit
  (** [format oc pattern directives] prints [pattern] on [oc], using
      [directives] to handle directives (as index) in the pattern.

      For example:

      {[
        format IO.stdout "x=%(0), y=%(1)" [|fun oc -> IO.nwrite oc "foo";
                                            fun oc -> IO.nwrite oc "bar"|]
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

(** Format. This is the replacement for classical formats (of type
    [Pervasives.format]). *)
type ('a, 'b, 'acc) format = {
  pattern : pattern;
  (** The pattern of the format. To translate a format (for i18n) you
      should replace it by its translation. *)

  printer : pattern -> ('a, 'b, 'acc) directive;
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
          printer = (fun patten k ->
                       let printers = Array.create 2 (fun oc -> ()) in
                       pdir_d (fun p ->
                                 printers.(0) <- p;
                                 pdir_s (fun p ->
                                           printers.(1) <- p;
                                           k (fun oc -> format pattern printers)))) }
      ]}
  *)
}

(** {6 Printf functions} *)

val printf : ('a, unit, unit) format -> 'a
  (** [printf fmt] prints on {!IO.stdout} *)

val eprintf : ('a, unit, unit) format -> 'a
  (** [printf fmt] prints on {!IO.stderr} *)

val fprintf : 'a IO.output -> ('b, unit, 'a) format -> 'b
  (** [fprintf oc fmt] prints on [oc] *)

val kfprintf : ('acc IO.output -> 'b) -> 'acc IO.output -> ('a, 'b, 'acc) format -> 'a
  (** [kfprintf k oc fmt] prints on [oc] then call [k] with [oc] as
      argument *)

val sprintf : ('a, string, string) format -> 'a
  (** [sprintf fmt] return the result as a string *)

val ksprintf : (string -> 'b) -> ('a, 'b, string) format -> 'a
  (** [ksprintf k fmt] create a string from the format and other
      arguments and pass it to [k] *)

val bprintf : Buffer.t -> ('a, unit, string) format -> 'a
  (** [bprintf buf fmt] prints into the buffer [buf] *)

val kbprintf : (Buffer.t -> 'b) -> Buffer.t -> ('a, 'b, string) format -> 'a
  (** [bprintf k buf fmt] prints into the buffer [buf], then call [k]
      with [buf] as argument. *)
