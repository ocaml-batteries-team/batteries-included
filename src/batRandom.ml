(*
 * ExtRandom - Additional randomization operations
 * Copyright (C) 1996 Damien Doligez
 *               2009 David Teller, LIFO, Universite d'Orleans
 *               2009 Pierre Chambart
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


let init      = Random.init
let full_init = Random.full_init
let self_init = Random.self_init
let bits      = Random.bits
let int       = Random.int
let int32     = Random.int32
let int64     = Random.int64
let nativeint = Random.nativeint
let float     = Random.float
let bool      = Random.bool
let char ()   = Char.chr (int 256)

let full_range_int =
  if Sys.word_size = 32 then (* need 31-bits of entropy, bits() gives 30 *)
    fun () -> if bool () then - (bits ())-1 else bits ()
  else (* 64-bit words *)
    fun () -> (* need 63 bits of entropy , bits + bits + bits land 0b11 *)
      let b = (bits ()) lor (bits () lsl 30) lor ((bits () land 0b11) lsl 60) in
      if bool () then b else -b - 1



module State =
struct
  include Random.State

  let char t   = Char.chr (int t 256)

  (**A constructor for enumerations of random numbers taking advantage
     of [State] to allow cloning.*)
  let random_enum state next =
    let rec aux state =
      let next  () = next state in
      let count () = raise BatEnum.Infinite_enum in
      let clone () = aux ( Random.State.copy state ) in
      BatEnum.make ~next ~count ~clone
    in aux state

  let enum_bits state () =
    let next state = bits state in
    random_enum state next

  let enum_int state bound =
    let next state = int state bound in
    random_enum state next

  let enum_int32 state bound =
    let next state = int32 state bound in
    random_enum state next

  let enum_int64 state bound =
    let next state = int64 state bound in
    random_enum state next

  let enum_float state bound =
    let next state = float state bound in
    random_enum state next

  let enum_nativeint state bound =
    let next state = nativeint state bound in
    random_enum state next

  let enum_bool state () =
    let next state = bool state in
    random_enum state next

  let enum_char state () =
    let next state = char state in
    random_enum state next

end

let random_enum clone next =
  let count () = raise BatEnum.Infinite_enum in
  let clone () = clone (Random.get_state ()) in
    BatEnum.make ~next ~count ~clone

let enum_bits () =
  let next = bits in
  let clone state = State.enum_bits state () in
    random_enum clone next

let enum_int bound =
  let next () = int bound in
  let clone state = State.enum_int state bound in
    random_enum clone next

let enum_int32 bound =
  let next () = int32 bound in
  let clone state = State.enum_int32 state bound in
    random_enum clone next

let enum_int64 bound =
  let next () = int64 bound in
  let clone state = State.enum_int64 state bound in
    random_enum clone next

let enum_float bound =
  let next () = float bound in
  let clone state = State.enum_float state bound in
    random_enum clone next

let enum_nativeint bound =
  let next () = nativeint bound in
  let clone state = State.enum_nativeint state bound in
    random_enum clone next

let enum_bool () =
  let next = bool in
  let clone state = State.enum_bool state () in
    random_enum clone next

let enum_char () =
  let next = char in
  let clone state = State.enum_char state () in
    random_enum clone next

let choice e = BatEnum.drop (int (BatEnum.count e)) e; BatEnum.get_exn e

(* Reservoir sampling algorithm (see for instance
   http://en.wikipedia.org/wiki/Reservoir_sampling)

   TODO: a more efficient algorithm when given enum length is known *)
let multi_choice n e =
  if BatEnum.is_empty e then
    BatEnum.empty ()
  else
    let next e = BatOption.get (BatEnum.get e) in
    (* Note: this assumes that Array.init will call the function for i
       = 0 to n-1 in that order *)
    let chosen = Array.init n (fun i -> next e, i) in
    BatEnum.iteri (fun i x ->
        let i = i + n + 1 in (* we've already chosen the n first items *)
        let r = Random.int i in
        if r < n then chosen.(r) <- x, i) e ;
    Array.sort (fun (_, i1) (_, i2) -> compare i1 i2) chosen ;
    BatArray.enum (Array.map fst chosen)

(*$T multi_choice
  BatEnum.is_empty (multi_choice 0 (BatEnum.empty ()))
  BatEnum.count (multi_choice 3 (BatList.enum [1;2;3;4;5])) = 3
  let l = [1;2;3;4;5] in let e = multi_choice 2 (BatList.enum l) in \
    let a = BatOption.get (BatEnum.get e) in a < BatOption.get (BatEnum.get e)
  let x = BatEnum.repeat ~times:99 [0;1] /@ (fun l -> \
    multi_choice 1 (BatList.enum l)) /@ \
    BatEnum.get_exn |> \
    reduce (+) in x > 0 && x < 99
*)
(* Note: this last test check that the first nor the last item is always chosen *)

let shuffle e =
  let a = BatArray.of_enum e in
  for n = Array.length a - 1 downto 1 do
    let k    = int ( n + 1 ) in
    if k <> n then
      let buf  = Array.get a n in
      Array.set a n (Array.get a k);
      Array.set a k buf
  done;
  a

let get_state = Random.get_state
let set_state = Random.set_state
