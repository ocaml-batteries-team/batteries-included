(*
 * format_ast.ml
 * -------------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *)

type t =
  | Cst of string
  | Dir of string list * string
