(*
 * BatInt - Extended integers
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


open BatNumber

let enum () =
  let current_value   = ref min_int in
  let already_through = ref false   in
  let f  () =
    if  !current_value = max_int then
      if !already_through then raise BatEnum.No_more_elements
      else ( already_through := true; max_int )
    else BatRef.post_incr current_value
  in BatEnum.from f

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

  let pow a b =
    if b < 0
    then raise (Invalid_argument "Int.pow")
    else
      let div_two n = n / 2
      and mod_two n = n mod 2
      in generic_pow ~zero ~one ~div_two ~mod_two ~mul a b
  (*$Q pow
    Q.int     (fun a -> pow a 0 = 1)
    Q.int     (fun a -> pow a 1 = a)
    Q.int     (fun a -> pow a 2 = a * a)
    Q.pos_int (fun b -> b = 0 || pow 0 b = 0)
    Q.pos_int (fun b -> pow 1 b = 1)
    (Q.pair Q.int Q.neg_int) (fun (a,b) -> \
       b = 0 || Result.(catch2 pow a b |> is_exn (Invalid_argument "Int.pow")))
  *)
  (*$= pow
    (pow (-2) 3) (-8)
    (pow 0 0)    1
  *)


  let min_num, max_num = min_int, max_int

  (* this function is performance sensitive : it is heavily used by
     associative data structures using ordered keys (Set, Map). The
     current version, due to Mauricio "mfp" Fernandez, only uses
     a type annotation to benefit from the excellent compilation of
     statically-known integer comparisons. It outperforms the previous
     version calling directly the external primitive
     "caml_int_compare". *)
  let compare (x : int) y =
    if x > y then 1
    else if y > x then -1
    else 0

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

  let ( <> ) (a:int) b = a <> b
  let ( <= ) (a:int) b = a <= b
  let ( >= ) (a:int) b = a >= b
  let ( < )  (a:int) b = a < b
  let ( > )  (a:int) b = a > b
  let ( = )  (a:int) b = a = b

  let ( ** ) a b = pow a b

  let print out t = BatInnerIO.nwrite out (string_of_int t)
  let print_hex out t = BatPrintf.fprintf out "%X" t

  let ( -- )  x y = BatEnum.seq x (add one) ((>=) y)
  let ( --- ) x y =
    if x <= y then x -- y
    else BatEnum.seq x pred ((<=) y)

end

(* We want BaseInt versions of these function instead of MakeNumeric ones *)
module Compare = struct
  type bat__compare_t = int
  let ( <> ), ( >= ), ( <= ), ( > ), ( < ), ( = ) = BaseInt.(( <> ), ( >= ), ( <= ), ( > ), ( < ), ( = ))
end

include (BatNumber.MakeNumeric(BaseInt) : BatNumber.Numeric with type t := int and module Compare := Compare)
include BaseInt

let min a b = if a < b then a else b
let max a b = if a > b then a else b
(*$T min
   min 3 4 = 3
   min 4 4 = 4
   min (-3) 5 = -3
   min min_int max_int = min_int
*) (*$T max
     max 3 4 = 4
     max 4 4 = 4
     max (-3) 5 = 5
     max min_int max_int = max_int
     max max_int max_int = max_int
     max min_int min_int = min_int
   *)

let mid a b =
  a land b + ((a lxor b) asr 1)

(*$Q mid
  (Q.pair Q.int Q.int) (fun (a,b) -> \
    let m = mid a b in \
    a <= b ==> (a <= m && m <= b && abs ((m-a) - (b-m)) <= 1) && \
    b < a ==> (b <= m && m <= a && abs ((m-b) - (a-m)) <= 1))
  (Q.int) (fun a -> mid a a = a)
*)

let popcount =
  if Sys.word_size = 32 then
    let k1 = 0x55555555 in
    let k2 = 0x33333333 in
    let k3 = 0x0f0f0f0f in
    (fun x ->
      let x = x - (x lsr 1) land k1 in
      let x = ((x lsr 2) land k2) + (x land k2) in
      let x = (x + (x lsr 4)) land k3 in
      let x = x + x lsr 8 in
      (x + x lsr 16) land 0x3f
    )
  else (* word_size = 64 *)
    (* uses int_of_string to hide these constants from the 32-bit compiler *)
    let k1 = int_of_string "0x5555_5555_5555_5555" in
    let k2 = int_of_string "0x3333_3333_3333_3333" in
    let k4 = int_of_string "0x0f0f_0f0f_0f0f_0f0f" in
    (fun x ->
      let x = x - (x lsr 1) land k1 in
      let x = (x land k2) + ((x lsr 2) land k2) in
      let x = (x + x lsr 4) land k4 in
      let x = x + x asr 8 in
      let x = x + x asr 16 in
      let x = x + x asr 32 in
      x land 0x7f
    )

let popcount_sparse x =
  let rec loop n x = if x = 0 then n else loop (n+1) (x land (x-1)) in
  loop 0 x

(*$Q popcount
  (Q.int) (fun x -> popcount x = popcount_sparse x)
*)

module BaseSafeInt = struct
  include BaseInt

  (** Open this module and [SafeInt] to replace traditional integer
      operators with their safe counterparts *)

  let add a b =
    let c = Pervasives.( + ) a b in
    if a < 0 && b < 0 && c >= 0 || a > 0 && b > 0 && c <= 0	then raise Overflow
    else c

  let sub a b =
    let c = Pervasives.( - ) a b in
    if a < 0 && b > 0 && c >= 0 || a > 0 && b < 0 && c <= 0	then raise Overflow
    else c

  let neg x = if x <> min_int then ~- x else raise Overflow

  let succ x = if x <> max_int then succ x else raise Overflow

  let pred x = if x <> min_int then pred x else raise Overflow

  let abs x = if x <> min_int then abs x else raise Overflow

  (*This function used to assume that in case of overflow the result would be
    different when computing in 31 bits (resp 63 bits) and in 32 bits (resp 64
    bits). This trick turned out to be *wrong* on 64-bit machines, where
    [Nativeint.mul 2432902008176640000n 21n] and [2432902008176640000 * 21]
    yield the same result, [-4249290049419214848]. *)
  let shift_bits, mask =
    if Sys.word_size = 32 then 16,0xFFFF else 32, (1 lsl 32) - 1

  let mul a b =
    match a asr shift_bits, b asr shift_bits with
    |	0,0 -> a * b
    | 0,bh ->
      let al = a land mask in
      let cross = bh * al in
      if cross > mask then raise Overflow;
      let bl = b land mask in
      add (cross lsl shift_bits) (al*bl)
    | ah, 0 ->
      let bl = b land mask in
      let cross = ah * bl in
      if cross > mask then raise Overflow;
      let al = a land mask in
      add (cross lsl shift_bits) (al+bl)
    | _,_ -> raise Overflow

  let pow a b =
    if b < 0
    then raise (Invalid_argument "Safe_int.pow")
    else
      let div_two n = n / 2
      and mod_two n = n mod 2
      in BatNumber.generic_pow ~zero ~one ~div_two ~mod_two ~mul a b

end

module Safe_int = struct
  module Compare = struct
    type bat__compare_t = t
    let ( <> ), ( >= ), ( <= ), ( > ), ( < ), ( = ) = ( <> ), ( >= ), ( <= ), ( > ), ( < ), ( = )
  end
  include (BatNumber.MakeNumeric(BaseSafeInt) : BatNumber.Numeric with type t := int and module Compare := Compare)
  include BaseSafeInt  (* for performance, replace functor-values with direct values *)

end

(*$T &
  Result.(catch (Safe_int.add max_int) max_int |> is_exn Number.Overflow)
  Safe_int.neg max_int = -max_int
  Result.(catch Safe_int.neg min_int |> is_exn Number.Overflow)
  Result.(catch (List.reduce Safe_int.mul) \
    [1 lsl 18 * 21; 3*3*3*3*3*3*3*3; 5*5*5*5*7*7*11*13*17*19] \
      |> is_exn Number.Overflow)
  Result.(catch (Safe_int.Infix.(+) max_int) 1 |> is_exn Number.Overflow)
*)

(*$Q &
  (Q.pair Q.pos_int Q.pos_int) (fun (a,b) -> let (a,b) = max a b, min a b in \
    let b = max_int - a + b in try Safe_int.add a b |>ignore; false \
      with BatNumber.Overflow -> true)
  (Q.pair Q.pos_int Q.pos_int) (fun (a,b) -> let (a,b) = max a b, min a b in \
    let b = max_int - a + b in try Safe_int.sub (-a) b|>ignore; false \
      with BatNumber.Overflow -> true)
  (Q.pair Q.int Q.int) (fun (a,b) -> \
    let slow_mul a b = \
      if b = 0 then 0 \
      else if (abs a) > max_int / (abs b) then raise BatNumber.Overflow else a*b \
    in Pervasives.(=) \
      (Result.catch (Safe_int.mul a) b) (Result.catch (slow_mul a) b))
*)

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
