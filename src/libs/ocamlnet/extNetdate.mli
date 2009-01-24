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
 * USA
 *)

open Sexplib

(** 
    Support for dates, including parsing and formating.

    @documents Date

    @author Gerg Stolpmann (OCamlNet module)
    @author David Teller
 *)
module Netdate : sig

  (** {7 Note} Several functions of this module are defined with
      respect to a base date called the {i epoch}. While the
      definition epoch is may be system-dependent, most systems
      take as epoch 00:00:00 UTC, January 1, 1970.*)

  (**
     The representation of a date in time.
  *)
type t = {
  year     : int(**the complete year *);
  month    : int(**a month, between 1 and 12*);
  day      : int(**a day, between 1 and 31*);
  hour     : int(**the number of hours since midnight, between 0 and 23*);
  minute   : int(**the number of minutes since the beginning of the hour, between 0 and 59*);
  second   : int(**the number of seconds since the begining of the minute, between 0 and 59*);
  zone     : int(**the local zone offset, in minutes of advance wrt UTC. For instance, 60 = UTC+0100. *);
  week_day : int(**the number of days since sunday, between 0 and 6
    {b Note} As a special exception, [week_day] may be [-1], if the day of the week is unknown*);
}

val now : unit -> t
  (** [now ()] returns the current date, as expressed in the local zone.*)

val localzone : int
  (** The offset in minutes for the local time zone from the UTC *)

val create : ?zone:int -> float -> t
  (** Convert the time (seconds since the epoch) to a date/time record *)

val parse : string -> t
  (** Parse a string and return a date/time record *)

val since_epoch : t -> float
  (** Convert a date/time record into the time (seconds since the epoch) *)

val parse_epoch : string -> float
  (** Parse a string and return the time (seconds since the epoch *)

val format_to : _ Extlib.InnerIO.output -> ?fmt:string -> t -> unit
  (** Format a date/time record according to the format string and outputs
      the resulting string to the channel.
      
      The format string consists of zero or more conversion specifications
      and ordinary characters.  All ordinary characters are output directly
      to the channel.  A conversion specification consists of the '%'
      character and one other character.
      
      @param fmt An optional format stating how the date/time should be
      displayed. If unspecified, this will use the same format as
      {!mk_mail_date}.
      
      The conversion specifications are:
      
      - [%A]: full weekday name.
      - [%a]: abbreviated weekday name.
      - [%B]: full month name.
      - [%b]: abbreviated month name.
      - [%C]: (year / 100) as an integer; single digits are preceded by a zero.
      - [%c]: equivalent to ["%a %b %e %T %Y"].
      - [%D]: equivalent to ["%m/%d/%y"].
      - [%d]: day of the month as an integer (01-31); single digits are
      preceded by a zero.
      - [%e]: day of the month as an integer (1-31).
      - [%H]: hour (24-hour clock) as an integer (00-23).
      - [%h]: the same as %b.
      - [%I]: hour (12-hour clock) as an integer (01-12).
      - [%j]: day of the year as an integer (001-366).
      - [%k]: hour (24-hour clock) as an integer (0-23);
      single digits are preceded by a blank.
      - [%l]: hour (12-hour clock) as an integer (1-12);
      single digits are preceded by a blank.
      - [%M]: minute as an integer (00-59).
      - [%m]: month as an integer (01-12).
      - [%n]: a newline.
      - [%p]: either "AM" or "PM" as appropriate.
      - [%P]: either "am" or "pm" as appropriate.
      - [%R]: equivalent to ["%H:%M"].
      - [%r]: equivalent to ["%I:%M:%S %p"].
      - [%S]: second as an integer (00-60).
      - [%T]: equivalent to ["%H:%M:%S"].
      - [%t]: a tab.
      - [%U]: week number of the year (Sunday as the first day
      of the week) as an integer (00-53).
      - [%u]  weekday (Monday as the first day of the week) as
      an integer (1-7).
      - [%w]: weekday (Sunday as the first day of the week) as
      an integer (0-6).
      - [%X]: representation of the time.
      - [%x]: representation of the date.
      - [%Y]: year with century as an integer.
      - [%y]: year without century as an integer (00-99).
      - [%z]: time zone offset from UTC; a leading plus sign
      stands for east of UTC, a minus sign for west of UTC, hours and
      minutes follow with two digits each and no delimiter between them
      (common form for RFC 822 date headers).
      - [%%]: a `%' character.    
  *)
  
val format : ?fmt:string -> t -> string
  (** Format a date/time record as a string 
      
      @param fmt An optional format stating how the date/time should be
      displayed. If unspecified, this will use the same format as
      {!mk_mail_date}.
  *)

val mk_mail_date : ?zone:int -> float -> string
  (** Convert the time (seconds since the epoch) to a date string that
      conforms to RFC 1123 (which updates RFC 822).

      Example: ["Sun, 06 Nov 1994 08:49:37 -0500"].
*)

val mk_usenet_date : ?zone:int -> float -> string
  (** Convert the time (seconds since the epoch) to a date string that
      conforms to RFC 1036 (which obsoletes RFC 850).
      
      Example: ["Sunday, 06-Nov-94 08:49:37 -0500"].
      
      Note that this format has only two digits for the year.
*)

(** {6 Boilerplate code}*)
(** {7 S-Expressions}*)
  
val t_of_sexp : Sexplib.Sexp.t -> t
val sexp_of_t : t -> Sexplib.Sexp.t
  
(** {7 Printing}*)

val print : _ Extlib.InnerIO.output -> t -> unit
(** Print a date with a default format.

    The default format used is the same as {!mk_mail_date}*)

end
