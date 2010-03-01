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

    This module extends Stdlib's
    {{:http://caml.inria.fr/pub/docs/manual-ocaml/libref/Format.html}Format}
    module, go there for documentation on the rest of the functions
    and types.

    @author Pierre Weis (Base module)
    @author David Teller
*)

  open BatIO
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
