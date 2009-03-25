(*
 * value_printer.mli
 * -----------------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *)

(** Value printers *)

(** This modules define utilities for printing values.

    @author Jeremie Dimino
*)

type ('a, 'b) t = bool -> 'b InnerIO.output -> 'a -> unit
  (** Type of a value-printer, the boolean argument tell whether
      composed expression must be parenthesed. *)

(** {6 Tuple printers} *)

val print_tuple0 : (unit, 'b) t
val print_tuple1 : ('a1, 'b) t -> ('a1, 'b) t
val print_tuple2 : ('a1, 'b) t -> ('a2, 'b) t -> ('a1 * 'a2, 'b) t
val print_tuple3 : ('a1, 'b) t -> ('a2, 'b) t -> ('a3, 'b) t -> ('a1 * 'a2 * 'a3, 'b) t
val print_tuple4 : ('a1, 'b) t -> ('a2, 'b) t -> ('a3, 'b) t -> ('a4, 'b) t -> ('a1 * 'a2 * 'a3 * 'a4, 'b) t
val print_tuple5 : ('a1, 'b) t -> ('a2, 'b) t -> ('a3, 'b) t -> ('a4, 'b) t -> ('a5, 'b) t -> ('a1 * 'a2 * 'a3 * 'a4 * 'a5, 'b) t
val print_tuple6 : ('a1, 'b) t -> ('a2, 'b) t -> ('a3, 'b) t -> ('a4, 'b) t -> ('a5, 'b) t -> ('a6, 'b) t -> ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6, 'b) t
val print_tuple7 : ('a1, 'b) t -> ('a2, 'b) t -> ('a3, 'b) t -> ('a4, 'b) t -> ('a5, 'b) t -> ('a6, 'b) t -> ('a7, 'b) t -> ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7, 'b) t
val print_tuple8 : ('a1, 'b) t -> ('a2, 'b) t -> ('a3, 'b) t -> ('a4, 'b) t -> ('a5, 'b) t -> ('a6, 'b) t -> ('a7, 'b) t -> ('a8, 'b) t -> ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8, 'b) t
val print_tuple9 : ('a1, 'b) t -> ('a2, 'b) t -> ('a3, 'b) t -> ('a4, 'b) t -> ('a5, 'b) t -> ('a6, 'b) t -> ('a7, 'b) t -> ('a8, 'b) t -> ('a9, 'b) t -> ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8 * 'a9, 'b) t
val print_tuple10 : ('a1, 'b) t -> ('a2, 'b) t -> ('a3, 'b) t -> ('a4, 'b) t -> ('a5, 'b) t -> ('a6, 'b) t -> ('a7, 'b) t -> ('a8, 'b) t -> ('a9, 'b) t -> ('a10, 'b) t -> ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8 * 'a9 * 'a10, 'b) t
