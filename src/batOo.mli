(*
 * ExtOO - Extended operations on objects
 * Copyright (C) 1996 Jerome Vouillon, INRIA
 *               2008 David Teller, LIFO, Universite d'Orleans
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

(** Operations on objects

    @documents Oo
    @author Jerome Vouillon (Base module)
    @author David Teller (integration to Batteries)
*)
module Oo :
sig

val copy : (< .. > as 'a) -> 'a
  (** [Oo.copy o] returns a copy of object [o], that is a fresh
      object with the same methods and instance variables as [o]  *)

external id : < .. > -> int = "%field1"
  (** Return an integer identifying this object, unique for
      the current execution of the program. *)

(**/**)
(** For internal use (CamlIDL) *)

val new_method :          string -> CamlinternalOO.tag(**As {!Internal.public_method_label}*)
val public_method_label : string -> CamlinternalOO.tag(**As {!Internal.public_method_label}*)

(**/**)

module Internal:
sig

(** Run-time support for objects and classes.
    All functions in this module are for system use only, not for the
    casual user.

    @documents CamlinternalOO
*)

(** {6 Classes} *)

  type tag     = CamlinternalOO.tag
  type label   = CamlinternalOO.label
  type table   = CamlinternalOO.table  (**Internal representation of the vtable, i.e. the table of virtual methods.*)
  type meth    = CamlinternalOO.meth
  type t       = CamlinternalOO.t
  type obj     = CamlinternalOO.obj    (**Internal representation of an object.*)
  type closure = CamlinternalOO.closure(**Internal representation of a method.*)

  val public_method_label : string -> tag
  val new_method : table -> label
  val new_variable : table -> string -> int
  val new_methods_variables :
    table -> string array -> string array -> label array
  val get_variable : table -> string -> int
  val get_variables : table -> string array -> int array
  val get_method_label : table -> string -> label
  val get_method_labels : table -> string array -> label array
  val get_method : table -> label -> meth
  val set_method : table -> label -> meth -> unit
  val set_methods : table -> label array -> unit
  val narrow : table -> string array -> string array -> string array -> unit
  val widen : table -> unit
  val add_initializer : table -> (obj -> unit) -> unit
  val dummy_table : table
  val create_table : string array -> table
  val init_class : table -> unit
  val inherits :
    table -> string array -> string array -> string array ->
    (t * (table -> obj -> Obj.t) * t * obj) -> bool -> Obj.t array
  val make_class :
    string array -> (table -> Obj.t -> t) ->
    (t * (table -> Obj.t -> t) * (Obj.t -> t) * Obj.t)
  type init_table = CamlinternalOO.init_table
  val make_class_store :
    string array -> (table -> t) -> init_table -> unit
  val dummy_class :
    string * int * int ->
    (t * (table -> Obj.t -> t) * (Obj.t -> t) * Obj.t)

  (** {6 Objects} *)

  val copy : (< .. > as 'a) -> 'a
  val create_object : table -> obj
  val create_object_opt : obj -> table -> obj
  val run_initializers : obj -> table -> unit
  val run_initializers_opt : obj -> obj -> table -> obj
  val create_object_and_run_initializers : obj -> table -> obj
  external send : obj -> tag -> t = "%send"
  external sendcache : obj -> tag -> t -> int -> t = "%sendcache"
  external sendself : obj -> label -> t = "%sendself"
  external get_public_method : obj -> tag -> closure
    = "caml_get_public_method" "noalloc"

  (** {6 Table cache} *)

  type tables = CamlinternalOO.tables
  val lookup_tables : tables -> closure array -> tables

  (** {6 Builtins to reduce code size} *)

  (*
    val get_const : t -> closure
    val get_var : int -> closure
    val get_env : int -> int -> closure
    val get_meth : label -> closure
    val set_var : int -> closure
    val app_const : (t -> t) -> t -> closure
    val app_var : (t -> t) -> int -> closure
    val app_env : (t -> t) -> int -> int -> closure
    val app_meth : (t -> t) -> label -> closure
    val app_const_const : (t -> t -> t) -> t -> t -> closure
    val app_const_var : (t -> t -> t) -> t -> int -> closure
    val app_const_env : (t -> t -> t) -> t -> int -> int -> closure
    val app_const_meth : (t -> t -> t) -> t -> label -> closure
    val app_var_const : (t -> t -> t) -> int -> t -> closure
    val app_env_const : (t -> t -> t) -> int -> int -> t -> closure
    val app_meth_const : (t -> t -> t) -> label -> t -> closure
    val meth_app_const : label -> t -> closure
    val meth_app_var : label -> int -> closure
    val meth_app_env : label -> int -> int -> closure
    val meth_app_meth : label -> label -> closure
    val send_const : tag -> obj -> int -> closure
    val send_var : tag -> int -> int -> closure
    val send_env : tag -> int -> int -> int -> closure
    val send_meth : tag -> label -> int -> closure
  *)

  type impl  = CamlinternalOO.impl =
      GetConst
	       | GetVar
	       | GetEnv
	       | GetMeth
	       | SetVar
	       | AppConst
	       | AppVar
	       | AppEnv
	       | AppMeth
	       | AppConstConst
	       | AppConstVar
	       | AppConstEnv
	       | AppConstMeth
	       | AppVarConst
	       | AppEnvConst
	       | AppMethConst
	       | MethAppConst
	       | MethAppVar
	       | MethAppEnv
	       | MethAppMeth
	       | SendConst
	       | SendVar
	       | SendEnv
	       | SendMeth
	       | Closure of closure

  (** {6 Parameters} *)

  (** currently disabled *)
  type params  = CamlinternalOO.params =
      { mutable compact_table : bool;
	mutable copy_parent : bool;
	mutable clean_when_copying : bool;
	mutable retry_count : int;
	mutable bucket_small_size : int }

  val params : params

  (** {6 Statistics} *)

  type stats  = CamlinternalOO.stats =
      { classes : int;
	methods : int;
	inst_vars : int }
  val stats : unit -> stats

end

end
