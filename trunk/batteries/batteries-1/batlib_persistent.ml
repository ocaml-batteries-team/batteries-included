(*
 * Batlib.Data.Containers.Persistent - Persistent containers
 * Copyright (C) 2008 David Teller, LIFO, Universite d'Orleans
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
   Persistent containers
*)


module Dllist                                = Batlib_Extlib_Dllist
module Lazy                                  = Batlib_Baselib_Lazy
module List                                  = Batlib_Extlib_List       (*formerly Batlib_Baselib_List*)
module ListLabels                            = Batlib_Baselib_ListLabels(*TODO:Bring to feature parity with {!List}*)
module Map                                   = Batlib_Baselib_Map       (*TODO:make enumerable*)
module MapLabels                             = Batlib_Baselib_MapLabels (*TODO:make enumerable*)
module Option                                = Batlib_Extlib_Option
module Set                                   = Batlib_Baselib_Set       (*TODO:make enumerable*)
module SetLabels                             = Batlib_Baselib_SetLabels (*TODO:make enumerable*)
