(*
 * Batlib_meta - Meta-level operations
 * Copyright (C) 2008 David Teller
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

(**
   Meta-level operations (marshalling, garbage-collection...)
*)

(** {1 Language}*)
module Marshal        = Batlib_Baselib_Marshal
module Oo             = Batlib_Baselib_Oo

(** {1 Interaction with other languages} *)
module Callback       = Batlib_Baselib_Callback

(** {1 Memory}*)
module Gc             = Batlib_Baselib_Gc
module Weak           = Batlib_Baselib_Weak

(** {1 Internals}
    Here Be Dragons*)
module Obj            = Batlib_Baselib_Obj
module CamlinternalMod= Batlib_Baselib_CamlinternalMod
module CamlinternalOO = Batlib_Baselib_CamlinternalOO
