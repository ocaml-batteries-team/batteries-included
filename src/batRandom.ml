(*
 * BatRandom - Additional randomization operations
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


exception Empty
let init      = Random.init
let full_init = Random.full_init
let self_init = Random.self_init
let bits      = Random.bits
let int       = Random.int
##V>=4.13##let full_int = Random.full_int
##V>=5.2##let int_in_range = Random.int_in_range
let int32     = Random.int32
##V>=5.2##let int32_in_range = Random.int32_in_range
let int64     = Random.int64
##V>=5.2##let int64_in_range = Random.int64_in_range
let nativeint = Random.nativeint
##V>=5.2##let nativeint_in_range = Random.nativeint_in_range
let float     = Random.float
let bool      = Random.bool
let char ()   = Char.chr (int 256)

##V>=4.14##let bits32 = Random.bits32
##V>=4.14##let bits64 = Random.bits64
##V>=4.14##let nativebits = Random.nativebits

##V>=5##let split = Random.split

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

  (**A constructor for enumerations of random numbers. *)
  let enum_bits state () = BatEnum.from (fun () -> bits state)
  let enum_int state bound = BatEnum.from (fun () -> int state bound)
  let enum_int32 state bound = BatEnum.from (fun () -> int32 state bound)
  let enum_int64 state bound = BatEnum.from (fun () -> int64 state bound)
  let enum_float state bound = BatEnum.from (fun () -> float state bound)
  let enum_nativeint state bound =
    BatEnum.from (fun () -> nativeint state bound)
  let enum_bool state () = BatEnum.from (fun () -> bool state)
  let enum_char state () = BatEnum.from (fun () -> char state)

##V>=5##  external next: t -> (int64[@unboxed])
##V>=5##    = "caml_lxm_next" "caml_lxm_next_unboxed" [@@noalloc]

end

let enum_bits () = BatEnum.from bits
let enum_int bound = BatEnum.from (fun () -> int bound)
let enum_int32 bound = BatEnum.from (fun () -> int32 bound)
let enum_int64 bound = BatEnum.from (fun () -> int64 bound)
let enum_float bound = BatEnum.from (fun () -> float bound)
let enum_nativeint bound = BatEnum.from (fun () -> nativeint bound)
let enum_bool () = BatEnum.from bool
let enum_char () = BatEnum.from char

let choice e =
  if BatEnum.is_empty e then
    raise Empty
  else
    (BatEnum.drop (int (BatEnum.count e)) e;
     BatEnum.get_exn e)

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
  BatInnerShuffle.array_shuffle a;
  a

let get_state = Random.get_state
let set_state = Random.set_state

module Incubator = struct
  module Private_state_enums = struct
    module State = struct
      include State (* the state we defined up above *)

      let random_enum state next =
        let rec aux state =
          let next  () = next state in
          let count () = raise BatEnum.Infinite_enum in
          let clone () = aux ( copy state ) in
          BatEnum.make ~next ~count ~clone
        in aux (copy state)

      let enum_bits state () =
        random_enum state bits

      let enum_int state bound =
        random_enum state (fun state -> int state bound)

      let enum_int32 state bound =
        random_enum state (fun state -> int32 state bound)

      let enum_int64 state bound =
        random_enum state (fun state -> int64 state bound)

      let enum_float state bound =
        random_enum state (fun state -> float state bound)

      let enum_nativeint state bound =
        random_enum state (fun state -> nativeint state bound)

      let enum_bool state () =
        random_enum state bool

      let enum_char state () =
        random_enum state char

##V<5##  type implementation = { st: int array; mutable idx: int }
##V>=5## open Bigarray
##V>=5## type implementation = (int64, int64_elt, c_layout) Array1.t

      (*      external t_of_impl: implementation -> t = "%identity" *)
      external impl_of_t: t -> implementation = "%identity"

      let perturb state =
##V<5## let impl = impl_of_t state in
##V<5## make (Array.append impl.st [|impl.idx|])
##V>=5## let _ = State.next state in
##V>=5## state

    end

    (* bumps the existing global RNG state (reseeding on its current
       array) and returns the previous state *)
    let perturb_global () =
      let s_in = get_state () in
      set_state (State.perturb s_in);
      s_in

    let enum_bits () = State.enum_bits (perturb_global ()) ()
    let enum_bool () = State.enum_bool (perturb_global ()) ()
    let enum_char () = State.enum_char (perturb_global ()) ()

    let enum_int bound = State.enum_int (perturb_global ()) bound
    let enum_int32 bound = State.enum_int32 (perturb_global ()) bound
    let enum_int64 bound = State.enum_int64 (perturb_global ()) bound
    let enum_float bound = State.enum_float (perturb_global ()) bound
    let enum_nativeint bound = State.enum_nativeint (perturb_global ()) bound

##V>=4.14##let bits32 () = State.bits32 (perturb_global ())
##V>=4.14##let bits64 () = State.bits64 (perturb_global ())
##V>=4.14##let nativebits () = State.nativebits (perturb_global ())

  end
end
