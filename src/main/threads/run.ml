(* 
 * Run - A launcher/dynamic loader for Batteries-compiled .cmo/.cmxs files
 * Copyright (C) 2009 David Rajchenbach-Teller, LIFO, Universite d'Orleans
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

open Batteries;;
open Standard;;


let arg = Sys.argv.(1) in
  Sys.argv.(0) <- Sys.argv.(1);(*Replace first argument, in case it is used to identify binary.*)
  incr invisible_args;         (*Hide this first argument from [args ()]*)
  incr Arg.current;            (*Hide this first argument from [Arg]*)
  Dynlink.loadfile arg

