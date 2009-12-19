(* 
 * ExtGC - Extended GC operations
 * Copyright (C) 1996 Damien Doligez
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


(** Memory management control and statistics; finalised values. 

    This module extends Stdlib's
    {{:http://caml.inria.fr/pub/docs/manual-ocaml/libref/Gc.html}Gc}
    module, go there for documentation on the rest of the functions
    and types.

    @author Damien Doligez (Base module)
    @author David Teller
*)


val print_stat : _ BatInnerIO.output -> unit
(** Print the current values of the memory management counters (in
   human-readable form) into the channel argument. *)
