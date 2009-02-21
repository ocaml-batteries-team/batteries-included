(*
 * format_ast.mli
 * --------------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *)

(** AST of format *)

type t =
  | Cst of string
      (** A constant *)
  | Dir of string list * string
      (** [Dir(labels, name)] a directive:

          - name is the name of the directive
          - labels are labels for directive arguments *)
