(*
 * Batteries_type_conv - Generating code from type specifications.
 * Copyright (C) 2004, 2005 Martin Sandin  <msandin@hotmail.com> (as "tywith")
 *               2005-?     Makus Mottl, Jane Street Holding, LLC
 *               2008       David Teller, LIFO, Universite d'Orleans
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

(** File is obsolete! *)

(**
   A tool for deriving functions from type definitions.

   This module contains the library side of an automatization tool
   used to generate automatically functions from type definitions
   (so-called "boilerplate code"). 

   Out-of-the-box, for any new type [<type>], it can generate a
   function [string_of_<type>], a function [map_<type>], a function
   [print_<type>] and a function [enum_<type>]. To create all these
   functions on a type you have defined, you need to annotate your
   type definition as follows: instead of writing [type foo = A of int
   | B of string | ... | Z], write [type foo = A of int | B of string
   | ... | Z with string_of, map, print].

   You may create new generators for similar use. These generators may
   serve to create new types, values, modules... Generators are simple
   functions from syntax to syntax, which you need to register using
   {!add_generator} (implementation part) and {!add_sig_generator}
   (interface part). For more information about generators, see the
   documentation of {{:#registration}the corresponding section}.

   {b Note} To run this tool on your code, you will need to activate
   syntax extension [pa_type_conv]. If you are using the default
   configuration for Batteries Included, this is done automatically
   for you. Otherwise, you will need to activate findlib package
   [batteries.pa_type_conv.syntax] by specifying options [ocamlfind
   ocamlc -syntax camlp4o -package batteries.pa_type_conv.syntax]
*)


open Camlp4.PreCast.Ast

(** {6:registration Adding/removing generators} 


*)

val add_generator : string -> (ctyp -> str_item) -> unit
  (** [add_generator name gen] adds the code generator [gen], which
      maps type declarations to structure items.  Note that the original
      type declaration get added automatically in any case. *)

val rem_generator : string -> unit
(** [rem_generator name] removes the code generator named [name]. *)

val add_sig_generator : string -> (ctyp -> sig_item) -> unit
(** [add_generator name gen] adds the code generator [gen], which
    maps type declarations to signature items.  Note that the original
    type declaration get added automatically in any case. *)

val rem_sig_generator : string -> unit
(** [rem_sig_generator name] removes the code signature generator named
    [name]. *)

val get_conv_path : unit -> string
(** [get_conv_path ()] @return the name to module containing a type
    as required for error messages. *)


(** {6 Utility functions} *)

val get_loc_err : Loc.t -> string -> string
(** [get_loc_err loc msg] generates a compile-time error message. *)

val hash_variant : string -> int
(** [hash_variant str] @return the integer encoding a variant tag with
    name [str]. *)

(** {6 General purpose code generation module} *)

module Gen : sig
  val ty_var_list_of_ctyp : ctyp -> string list -> string list
  (** [ty_var_list_of_ctyp tp acc] accumulates a list of type parameters
      contained in [tp] into [acc] as strings. *)

  val get_rev_id_path : ident -> string list -> string list
  (** [get_rev_id_path id acc] takes an identifier.  @return a reversed
      module path (list of strings) denoting this identifier, appending
      it to [acc]. *)

  val ident_of_rev_path : Loc.t -> string list -> ident
  (** [ident_of_rev_path loc path] takes a location [loc] and a reversed path
      [rev_path] to an identifier.  @return identifier denoting the
      bound value. *)

  val get_appl_path : Loc.t -> ctyp -> ident
  (** [get_appl_path loc tp] @return the identifier path associated with
      a polymorphic type. *)

  val abstract : Loc.t -> patt list -> expr -> expr
  (** [abstract loc patts body] takes a location [loc], a pattern list
      [patts], and an expression [body].  @return a function expression
      that takes the patterns as arguments, and binds them in [body]. *)

  val apply : Loc.t -> expr -> expr list -> expr
  (** [apply loc f_expr arg_exprs] takes a location [loc], an expression
      [f_expr] representing a function, and a list of argument expressions
      [arg_exprs].  @return an expression in which the function is
      applied to its arguments. *)

  val idp : Loc.t -> string -> patt
  (** [idp loc name] @return a pattern matching a lowercase identifier
      [name]. *)

  val ide : Loc.t -> string -> expr
  (** [ide loc name] @return an expression of a lowercase identifier
      [name]. *)

  val switch_tp_def :
    Loc.t ->
    alias : (Loc.t -> ctyp -> 'a) ->
    sum : (Loc.t -> ctyp -> 'a) ->
    record : (Loc.t -> ctyp -> 'a) ->
    variants : (Loc.t -> ctyp -> 'a) ->
    mani : (Loc.t -> ctyp -> ctyp -> 'a) ->
    nil : (Loc.t -> 'a) ->
    ctyp
    -> 'a
  (** [switch_tp_def loc ~alias ~sum ~record ~variants ~mani tp_def]
      takes a handler function for each kind of type definition and
      applies the appropriate handler when [tp_def] matches. *)

  val mk_expr_lst : Loc.t -> expr list -> expr
  (** [mk_expr_lst loc expr_list] takes a list of expressions.
      @return an expression representing a list of expressions. *)

  val mk_patt_lst : Loc.t -> patt list -> patt
  (** [mk_patt_lst _loc patt_list] takes a list of patterns.
      @return a pattern representing a list of patterns. *)

  val get_tparam_id : ctyp -> string
  (** [get_tparam_id tp]

      @return the string identifier associated with
      [tp] if it is a type parameter.

      @raise Failure otherwise. *)

  val type_is_recursive : Loc.t -> string -> ctyp -> bool
  (** [type_is_recursive _loc id tp]

      @return whether the type [tp]
      with name [id] refers to itself, assuming that it is not mutually
      recursive with another type. *)

  val drop_variance_annotations : Loc.t -> ctyp -> ctyp
  (** [drop_variance_annotations _loc tp] @return the type resulting
      from dropping all variance annotations in [tp]. *)
end

(**/**)
(** {6 For internal use}

    These functions are meant to be used only from the
    Camlp4 extension. You should not need to use them.
 *)
val get_conv_path_el : unit   -> string * string list
val set_conv_path    : string -> unit
val push_conv_path   : string -> unit
val pop_conv_path    : unit   -> unit
val gen_derived_defs : loc  -> ctyp -> string list -> str_item
val sig_generate     : ctyp -> string -> sig_item
val gen_derived_sigs : loc  -> ctyp -> string list -> sig_item
