(* Batteries Included - Netdate
 *
 * Copyright (C) 2006 Gerd Stolpmann
 * Copyright (C) 2009 David Teller, LIFO, Universite d'Orleans
 * 
 * This library is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation; either version 2.1 of the
 * License, or (at your option) any later version, with the special
 * exception on linking described in file LICENSE.
 * 
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301
 * USA *)

open Sexplib
open Extlib
TYPE_CONV_PATH "" (*For Sexplib, Bin-prot...*)


module Netdate =
struct
  include Netdate

(**The default format, i.e. the format used to display dates in e-mails.*)
  let default_format = "%a, %d %b %Y %H:%M:%S %z"

(*We assume that any string involved is relatively short.
  Consequently, it should be at least as efficient to
  use [format] to extract a string and only then
  print that string to [out]*)
  let format_to out ?(fmt=default_format) t =
    IO.nwrite out (format ~fmt t)

  let format ?(fmt=default_format) t=
    format ~fmt t

  let print out t = IO.nwrite out (format t)

  let now () = create ~zone:localzone (Unix.time ())

    (*We replicate the type here to take advantage of sexplib*)
  type internal_t = t = {
    year     : int(**the complete year *);
    month    : int(**a month, between 1 and 12*);
    day      : int(**a day, between 1 and 31*);
    hour     : int(**the number of hours since midnight, between 0 and 23*);
    minute   : int(**the number of minutes since the beginning of the hour, between 0 and 59*);
    second   : int(**the number of seconds since the begining of the minute, between 0 and 59*);
    zone     : int(**the local zone offset, in minutes of advance wrt UTC. For instance, 60 = UTC+0100. *);
    week_day : int(**the number of days since sunday, between 0 and 6
		     {b Note} As a special exception, [week_day] may be [-1], if the day of the week is unknown*)
  } with sexp
  let sexp_of_t = sexp_of_internal_t
  let t_of_sexp = internal_t_of_sexp
end
