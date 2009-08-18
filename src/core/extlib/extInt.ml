(* 
 * ExtInt - Extended integers
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


open Number

let enum () =
  let current_value   = ref min_int in
  let already_through = ref false   in
  let f  () =
    if  !current_value = max_int then
      if !already_through then raise Enum.No_more_elements
      else ( already_through := true; max_int )
    else Ref.post_incr current_value
  in Enum.from f

module BaseInt = struct
  
  type t = int
  
  let zero, one = 0, 1

  external neg : int -> int        = "%negint"
  external add : int -> int -> int = "%addint"
  external sub : int -> int -> int = "%subint"
  external mul : int -> int -> int = "%mulint"
  external div : int -> int -> int = "%divint"

  external ( + ) : int -> int -> int = "%addint"
  external ( - ) : int -> int -> int = "%subint"
  external ( * ) : int -> int -> int = "%mulint"
  external ( / ) : int -> int -> int = "%divint"

  external pred: int -> int        = "%predint"
  external succ: int -> int        = "%succint"
  let abs = abs

  external modulo : int -> int -> int = "%modint"
  let pow = generic_pow ~zero ~one ~div_two:(fun n -> n / 2) ~mod_two:(fun n -> n mod 2) ~mul

  let min_num, max_num = min_int, max_int
  let compare = ( - )

  external of_int : int -> int = "%identity"
  external to_int : int -> int = "%identity"


  let of_string x = 
    try int_of_string x
    with Failure "int_of_string" -> raise (Invalid_argument "int_of_string")
  let to_string = string_of_int

  let enum = enum

  let minus_one = ( - 1)

  external to_float : int -> float = "%floatofint"
  external of_float : float -> int = "%intoffloat"
      
  external of_string : string -> int = "caml_int_of_string"

  external rem : int -> int -> int = "%modint"

  let ( <> ) a b = a <> b
  let ( <= ) a b = a <= b
  let ( >= ) a b = a >= b
  let ( < )  a b = a < b
  let ( > )  a b = a > b
  let ( = )  a b = a = b

  let ( ** ) a b = pow a b

  let print out t = InnerIO.nwrite out (string_of_int t)
  let t_printer paren out t = print out t

  let ( -- )  x y = Enum.seq x (add one) ((>=) y)
  let ( --- ) x y = 
    if x <= y then x -- y 
    else Enum.seq x pred ((<=) y) 

end

module Int = struct
  include BaseInt
  let operations = let module N = Number.MakeNumeric(BaseInt) in N.operations



end

module BaseSafeInt = struct
  include BaseInt

  (**
     Open this module and [SafeInt] to replace traditional integer operators with
     their safe counterparts
  *)

  let add a b =
    let c = Pervasives.( + ) a b in
      if a < 0 && b < 0 && c >= 0 || a > 0 && b > 0 && c <= 0	then raise Overflow
      else c
  let ( + ) = add

  let sub a b =
    let c = Pervasives.( - ) a b in
      if a < 0 && b > 0 && c >= 0 || a > 0 && b < 0 && c <= 0	then raise Overflow
      else c
  let ( - ) a b = sub a b

  let neg x = 
    if x <> min_int then ~- x
    else raise Overflow

  let succ x =
    if x <> max_int then succ x
    else raise Overflow

  let pred x =
    if x <> min_int then pred x
    else raise Overflow
    
  let abs x =
    if x <> min_int then abs x
    else raise Overflow

  (*This function used to assume that in case of overflow the result would be
    different when computing in 31 bits (resp 63 bits) and in 32 bits (resp 64
    bits). This trick turned out to be *wrong* on 64-bit machines, where
    [Nativeint.mul 2432902008176640000n 21n] and [2432902008176640000 * 21]
    yield the same result, [-4249290049419214848]. *)
  let mul a b =
    if b = 0 then 0
    else if (abs a) > max_int / (abs b) then raise Overflow else a * b

  let ( * ) = mul

  let pow = Number.generic_pow ~zero ~one ~div_two:(fun n -> n/2) ~mod_two:(fun n -> n mod 2) ~mul
    
end

module Safe_int = struct
  include BaseSafeInt
  let operations = let module N = Number.MakeNumeric(BaseSafeInt) in N.operations
end

(*
module Int     = struct
  include BaseInt
  module Numeric = struct include Numeric(BaseInt) end
end

module SafeInt = struct
  include BaseSafeInt
  module Numeric = struct include Numeric(BaseSafeInt) end
end
*)
