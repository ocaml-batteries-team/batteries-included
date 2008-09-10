(* 
 * ExtBool - Extended booleans
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

module BaseBool :
  sig
    type t = bool
    val zero : bool
    val one : bool
    val neg : bool -> bool
    val succ : 'a -> bool
    val pred : 'a -> bool
    val abs : 'a -> 'a
    val add : bool -> bool -> bool
    val mul : bool -> bool -> bool
    val sub : 'a -> bool -> bool
    val div : 'a -> 'b -> 'c
    val modulo : 'a -> 'b -> 'c
    val pow : 'a -> 'b -> 'c
    val min_num : bool
    val max_num : bool
    val compare : 'a -> 'a -> int
    val of_int : int -> bool
    val to_int : bool -> int
    val of_string : string -> bool
    val to_string : bool -> string
  end
module Bool :
  sig
    type t = bool
    val zero : bool
    val one : bool
    val neg : bool -> bool
    val succ : 'a -> bool
    val pred : 'a -> bool
    val abs : 'a -> 'a
    val add : bool -> bool -> bool
    val mul : bool -> bool -> bool
    val sub : 'a -> bool -> bool
    val div : 'a -> 'b -> 'c
    val modulo : 'a -> 'b -> 'c
    val pow : 'a -> 'b -> 'c
    val min_num : bool
    val max_num : bool
    val compare : 'a -> 'a -> int
    val of_int : int -> bool
    val to_int : bool -> int
    val of_string : string -> bool
    val to_string : bool -> string
    module Numeric :
      sig
        type t = BaseBool.t
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
