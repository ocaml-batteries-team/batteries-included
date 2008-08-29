(*
 * Batlib_containers - Data structures acting as containers
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
   Data containers (lists, sets, arrays...)
*)

(**{1 Container traversal}*)
module Enum                                  = Batlib_Extlib_Enum

(**{1 Generic persistent containers}*)
module Dllist                                = Batlib_Extlib_Dllist
module List                                  = Batlib_Extlib_List(*formerly Batlib_Baselib_List*)
module ListLabels                            = Batlib_Baselib_ListLabels
module Map                                   = Batlib_Baselib_Map
module MapLabels                             = Batlib_Baselib_MapLabels
module Set                                   = Batlib_Baselib_Set
module SetLabels                             = Batlib_Baselib_SetLabels

(**{1 Generic mutable containers}*)
module Array                                 = Batlib_Extlib_Array(*formerly Batlib_Baselib_Array*)
module Dynarray                              = Batlib_Extlib_Dynarray
module Global                                = Batlib_Extlib_Global
module Hashtbl                               = Batlib_Extlib_Hashtbl(*formerly Batlib_Baselib_Hashtbl*)
module HashtblLabels                         = Batlib_Baselib_HashtblLabels
module Queue                                 = Batlib_Baselib_Queue
module RefList                               = Batlib_Extlib_RefList
module Stack                                 = Batlib_Baselib_Stack
module Stream                                = Batlib_Baselib_Stream

(**{1 Specialized containers}*)
module Bitset                                = Batlib_Extlib_BitSet
module Buffer                                = Batlib_Baselib_Buffer
module Option                                = Batlib_Extlib_Option
module Lazy                                  = Batlib_Baselib_Lazy
