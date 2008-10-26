(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*         Xavier Leroy, projet Cristal, INRIA Rocquencourt            *)
(*                                                                     *)
(*  Copyright 2004 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* $Id: camlinternalMod.mli,v 1.1 2004/08/12 12:57:00 xleroy Exp $ *)

(**
   Internals on modules.

   All functions in this module are for system use only, not for the
   casual user. 

   @documents CamlinternalMod
*)

type shape  = CamlinternalMod.shape =
  | Function
  | Lazy
  | Class
  | Module of shape array

val init_mod: string * int * int -> shape -> Obj.t
val update_mod: shape -> Obj.t -> Obj.t -> unit
