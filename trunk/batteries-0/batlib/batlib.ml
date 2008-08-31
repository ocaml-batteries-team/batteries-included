(*
 * Batlib - Root of Batteries Included hierarchy
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
   Base of OCaml Batteries Included.
*)

(** Everything related to parallelism and concurrency*)
module Concurrency = Batlib_concurrency

(** Data containers (lists, sets, arrays...) *)
module Containers  = Batlib_containers

(** Control manipulation (monads, arrows...) *)
module Control     = Batlib_control

(** Numbers, operations and other mathematics *)
module Math        = Batlib_math

(** Metal-level operations (marshalling, garbage-collection, foreign function calls...)*)
module Meta        = Batlib_meta

(** Network access *)
module Network     = Batlib_network

(** Standard operations, should be opened automatically (not the case yet)*)
module Standard    = Batlib_standard

(** Interactions with the Operating System*)
module System      = Batlib_system

(** Tools for compiling OCaml, generating documentation, installing libraries...*)
module Text        = Batlib_text

(** Everything text related, from characters to parsing and printing.*)
module Toolchain   = Batlib_toolchain

(** Miscellaneous utilities*)
module Util        = Batlib_util

(**/**)
include Standard
