(*
 * Batlib_system - Interactions with the Operating System
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
   Interactions with the operating system (file manipulation, arguments...)
*)

(** {1 Environment I/O}*)

module Arg           = Batlib_Baselib_Arg
module OptParse      = Batlib_Extlib_OptParse

(** {1 Operations on streams}*)

module IO            = Batlib_Extlib_IO
module Unzip         = Batlib_Extlib_Unzip

(** {1 Actual operating system calls}*)

module File          = Batlib_Extlib_File
module Filename      = Batlib_Baselib_Filename
module Unix          = Batlib_Baselib_Unix
module UnixLabel     = Batlib_Baselib_UnixLabels
module Sys           = Batlib_Baselib_Sys

(** {1 Networking}*)

module Network       = Batlib_network
