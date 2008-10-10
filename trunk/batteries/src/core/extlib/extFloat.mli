(* 
 * ExtFloat - Extended floats
 * Copyright (C) 2007 Bluestorm <bluestorm dot dylc on-the-server gmail dot com>
 *               2008 David Teller
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



module Float :
  sig
    (**Operations on floating-point numbers*)

    type t = float
    val zero : float
    val one : float
    val neg : float -> float
    val succ : float -> float
    val pred : float -> float
    val abs : float -> float
    val add : float -> float -> float
    val sub : float -> float -> float
    val mul : float -> float -> float
    val div : float -> float -> float
    val modulo : float -> float -> float
    val pow : float -> float -> float
    val min_num : float
    val max_num : float
    val compare : 'a -> 'a -> int
    val of_int : int -> float
    val to_int : float -> int
    external of_float : float -> float = "%identity"
    external to_float : float -> float = "%identity"
    val of_string : string -> float
    val to_string : float -> string
    val ( + ) : t -> t -> t
    val ( - ) : t -> t -> t
    val ( * ) : t -> t -> t
    val ( / ) : t -> t -> t
    val ( ** ) : t -> t -> t
    val ( <> ) : t -> t -> bool
    val ( >= ) : t -> t -> bool
    val ( <= ) : t -> t -> bool
    val ( > ) : t -> t -> bool
    val ( < ) : t -> t -> bool
    val ( = ) : t -> t -> bool
    val operations : t Number.numeric

    (** {6 Boilerplate code}*)
    (** {7 S-Expressions}*)

    val t_of_sexp : Sexplib.Sexp.t -> t
    val sexp_of_t : t -> Sexplib.Sexp.t
end
