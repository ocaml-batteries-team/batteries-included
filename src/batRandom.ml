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


  open Random

  let init      = init
  let full_init = full_init
  let self_init = self_init
  let bits      = bits
  let int       = int
  let int32     = int32
  let int64     = int64
  let nativeint = nativeint
  let float     = float
  let bool      = bool
  let full_range = 
    if Sys.word_size = 32 then (* need 31-bits of entropy, bits() gives 30 *)
      fun () -> if bool () then - (bits ())-1 else bits ()
    else (* 64-bit words *)
      fun () ->
	let b = (bits ()) lor (bits () lsl 30) lor ((bits () land 3) lsl 60) in
	if bool () then b else -b - 1

  let char ()   = Char.chr (int 256)


  module State =
  struct
    include State (*Note: here, we use [Marshal] to avoid breaking abstraction. So it's not portable.*)

    let char t   = Char.chr (int t 256)

    (**A constructor for enumerations of random numbers taking advantage
       of [State] to allow cloning.*)
    let random_enum state next =
      let rec aux state =
	let next  () = next state in
	let count () = raise BatEnum.Infinite_enum in
	let clone () = aux ( State.copy state ) in
	  BatEnum.make next count clone
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

  let random_enum next = State.random_enum ( State.make_self_init () ) next

  let enum_bits () =
    let next state = State.bits state in
      random_enum next

  let enum_int bound =
    let next state = State.int state bound in
      random_enum next

  let enum_int32 bound =
    let next state = State.int32 state bound in
      random_enum next
	
  let enum_int64 bound =
    let next state = State.int64 state bound in
      random_enum next
	
  let enum_float bound =
    let next state = State.float state bound in
      random_enum next
	
  let enum_nativeint bound =
    let next state = State.nativeint state bound in
      random_enum next
	
  let enum_bool () =
    let next state = State.bool state in
      random_enum next

  let enum_char () =
    let next state = State.char state in
      random_enum next

  let choice e =
    let a   = BatArray.of_enum e in
    let len = Array.length  a in
      Array.get a (int len)

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

  let get_state = get_state
  let set_state = set_state


