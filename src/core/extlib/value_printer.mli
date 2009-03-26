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

type 'a t = bool -> unit InnerIO.output -> 'a -> unit
  (** Type of a value-printer, the boolean argument tell whether
      composed expression must be parenthesed. *)

(** {6 Tuple printers} *)

val print_tuple0 : unit t
val print_tuple1 : 'a1 t -> 'a1 t
val print_tuple2 : 'a1 t -> 'a2 t -> ('a1 * 'a2) t
val print_tuple3 : 'a1 t -> 'a2 t -> 'a3 t -> ('a1 * 'a2 * 'a3) t
val print_tuple4 : 'a1 t -> 'a2 t -> 'a3 t -> 'a4 t -> ('a1 * 'a2 * 'a3 * 'a4) t
val print_tuple5 : 'a1 t -> 'a2 t -> 'a3 t -> 'a4 t -> 'a5 t -> ('a1 * 'a2 * 'a3 * 'a4 * 'a5) t
val print_tuple6 : 'a1 t -> 'a2 t -> 'a3 t -> 'a4 t -> 'a5 t -> 'a6 t -> ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6) t
val print_tuple7 : 'a1 t -> 'a2 t -> 'a3 t -> 'a4 t -> 'a5 t -> 'a6 t -> 'a7 t -> ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7) t
val print_tuple8 : 'a1 t -> 'a2 t -> 'a3 t -> 'a4 t -> 'a5 t -> 'a6 t -> 'a7 t -> 'a8 t -> ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8) t
val print_tuple9 : 'a1 t -> 'a2 t -> 'a3 t -> 'a4 t -> 'a5 t -> 'a6 t -> 'a7 t -> 'a8 t -> 'a9 t -> ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8 * 'a9) t
val print_tuple10 : 'a1 t -> 'a2 t -> 'a3 t -> 'a4 t -> 'a5 t -> 'a6 t -> 'a7 t -> 'a8 t -> 'a9 t -> 'a10 t -> ('a1 * 'a2 * 'a3 * 'a4 * 'a5 * 'a6 * 'a7 * 'a8 * 'a9 * 'a10) t
