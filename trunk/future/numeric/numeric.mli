(* Copyright (C) 2007-

      Author: Bluestorm
      email: bluestorm dot dylc on-the-server gmail dot com

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

As a special exception to the GNU Library General Public License, you
may link, statically or dynamically, a "work that uses the Library"
with a publicly distributed version of the Library to produce an
executable file containing portions of the Library, and distribute
that executable file under terms of your choice, without any of the
additional requirements listed in clause 6 of the GNU Library General
Public License.  By "a publicly distributed version of the Library",
we mean either the unmodified Library as distributed by INRIA, or a
modified version of the Library that is distributed under the
conditions defined in clause 2 of the GNU Library General Public
License.  This exception does not however invalidate any other reasons
why the executable file might be covered by the GNU Library General
Public License.

*)

module type NUMERIC_BASE =
  sig
    type t
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
    val min_num : t
    val max_num : t
    val compare : t -> t -> int
    val of_int : int -> t
    val to_int : t -> int
    val of_string : string -> t
    val to_string : t -> string
  end

module type NUMERIC =
  sig
    include NUMERIC_BASE
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
  end

module Numeric : functor (Base : NUMERIC_BASE) -> NUMERIC with type t = Base.t

module NumInt : NUMERIC with type t = int
module NumFloat : NUMERIC with type t = float
module NumInt64 : NUMERIC with type t = Int64.t
module NumNativeint : NUMERIC with type t = Nativeint.t
