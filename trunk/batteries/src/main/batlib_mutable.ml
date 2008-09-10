(*
 * Batlib.Data.Containers.Mutable - Mutable containers
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

(** Mutable containers (arrays, stacks...)*)

module Array                                 = Batlib_Extlib_Array         (*formerly Batlib_Baselib_Array*)
module ArrayLabels                           = Batlib_Baselib_ArrayLabels
module Bigarray                              = Batlib_Baselib_Bigarray     (*TODO:make enumerable*)
module BitSet                                = Batlib_Extlib_BitSet
module Dynarray                              = Batlib_Extlib_Dynarray
module Enum                                  = Batlib_Extlib_Enum
module Global                                = Batlib_Extlib_Global
module Hashtbl                               = Batlib_Extlib_Hashtbl       (*formerly Batlib_Baselib_Hashtbl*)
module HashtblLabels                         = Batlib_Baselib_HashtblLabels(*TODO:Bring to feature parity with {!Hashtbl}*)
module Queue                                 = Batlib_Baselib_Queue        (*TODO:build from enum?*)
module RefList                               = Batlib_Extlib_RefList
module Stack                                 = Batlib_Baselib_Stack        (*TODO:build from enum*)
module Stream                                = Batlib_Baselib_Stream       (*TODO:replace with latest version*)
