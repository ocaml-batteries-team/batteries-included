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
    val of_string : string -> float
    val to_string : float -> string
    module Numeric :
      sig
        type t = float
        val zero : t
        val one : t
        val neg : t -> t
        val succ : t -> t
        val pred : t -> t
        val abs : t -> t
        val add : t -> t -> t
        val sub : t -> t -> t
        val mul : t -> t -> t
        val div : t -> t -> t
        val modulo : t -> t -> t
        val pow : t -> t -> t
        val compare : t -> t -> int
        val of_int : int -> t
        val to_int : t -> int
        val of_string : string -> t
        val to_string : t -> string
        val ( +. ) : t -> t -> t
        val ( -. ) : t -> t -> t
        val ( *. ) : t -> t -> t
        val ( /. ) : t -> t -> t
        val ( ** ) : t -> t -> t
        val ( <>. ) : t -> t -> bool
        val ( >=. ) : t -> t -> bool
        val ( <=. ) : t -> t -> bool
        val ( >. ) : t -> t -> bool
        val ( <. ) : t -> t -> bool
        val ( =. ) : t -> t -> bool
        val operations : t Number.numeric
      end
  end
