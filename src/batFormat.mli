(* 
 * ExtFormat - Extended Format module
 * Copyright (C) 1996 Pierre Weis
 *               2009 David Teller, LIFO, Universite d'Orleans
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

(** Pretty printing.

   This module implements a pretty-printing facility to format text
   within ``pretty-printing boxes''. The pretty-printer breaks lines
   at specified break hints, and indents lines according to the box
   structure.

   For a gentle introduction to the basics of pretty-printing using
   [Format], read
   {{:http://caml.inria.fr/resources/doc/guides/format.html}http://caml.inria.fr/resources/doc/guides/format.html}.

   You may consider this module as providing an extension to the
   [printf] facility to provide automatic line breaking. The addition of
   pretty-printing annotations to your regular [printf] formats gives you
   fancy indentation and line breaks.
   Pretty-printing annotations are described below in the documentation of
   the function {!Format.fprintf}.

   You may also use the explicit box management and printing functions
   provided by this module. This style is more basic but more verbose
   than the [fprintf] concise formats.

   For instance, the sequence
   [open_box 0; print_string "x ="; print_space (); print_int 1; close_box ()]
   that prints [x = 1] within a pretty-printing box, can be
   abbreviated as [printf "@[%s@ %i@]" "x =" 1], or even shorter
   [printf "@[x =@ %i@]" 1].

   Rule of thumb for casual users of this library:
 - use simple boxes (as obtained by [open_box 0]);
 - use simple break hints (as obtained by [print_cut ()] that outputs a
   simple break hint, or by [print_space ()] that outputs a space
   indicating a break hint);
 - once a box is opened, display its material with basic printing
   functions (e. g. [print_int] and [print_string]);
 - when the material for a box has been printed, call [close_box ()] to
   close the box;
 - at the end of your routine, evaluate [print_newline ()] to close
   all remaining boxes and flush the pretty-printer.

   The behaviour of pretty-printing commands is unspecified
   if there is no opened pretty-printing box. Each box opened via
   one of the [open_] functions below must be closed using [close_box]
   for proper formatting. Otherwise, some of the material printed in the
   boxes may not be output, or may be formatted incorrectly.

   In case of interactive use, the system closes all opened boxes and
   flushes all pending text (as with the [print_newline] function)
   after each phrase. Each phrase is therefore executed in the initial
   state of the pretty-printer.

   Warning: the material output by the following functions is delayed
   in the pretty-printer queue in order to compute the proper line
   breaking. Hence, you should not mix calls to the printing functions
   of the basic I/O system with calls to the functions of this module:
   this could result in some strange output seemingly unrelated with
   the evaluation order of printing commands.

    @author Pierre Weis (Base module)
    @author David Teller

    @documents Format
*)

  open IO
  open Format
(** {6 Boxes} *)


(** {6 Redirecting formatter output} *)

val set_formatter_output : 'a output -> unit;;


val formatter_of_output : _ output -> formatter;;
(** [formatter_of_output out] returns a new formatter that
   writes to the corresponding output [out]. *)


val std_formatter : formatter;;
(** The standard formatter used by the formatting functions
   above. It is defined as [formatter_of_output stdout]. *)

val err_formatter : formatter;;
(** A formatter to use with formatting functions below for
   output to standard error. It is defined as
   [formatter_of_output stderr]. *)

(** {6 Basic functions to use with formatters} *)

val pp_set_formatter_output      : formatter -> _ output -> unit;;
(** {6 Deprecated}*)

val set_formatter_out_channel : _ output -> unit;;
(** Redirect the pretty-printer output to the given channel. *)


val formatter_of_out_channel : _ output -> formatter;;
(** [formatter_of_out_channel oc] returns a new formatter that
   writes to the corresponding channel [oc]. *)


val pp_set_formatter_out_channel : formatter -> _ output -> unit;;
