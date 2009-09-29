(* 
 * ExtInt32 - Extended 32-bit integers
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

let (|>) x f = f x
    

module BaseInt32 = struct
  include Int32
    
  let modulo = rem
  let pow = generic_pow ~zero ~one ~div_two:(fun n -> shift_right n 1) ~mod_two:(logand one) ~mul:mul
  let min_num, max_num = min_int, max_int
end

module Int32 = struct
  include BaseInt32

  let ( -- )  x y = Enum.seq x (add one) ((>=) y)
  let ( --- ) x y = 
    if x <= y then x -- y 
    else Enum.seq x pred ((<=) y) 

  let print out t = InnerIO.Printf.fprintf out "%lx" t
  let t_printer paren out t = print out t

  let to_byte n = Int32.logand 0xffl n |> Int32.to_int |> Char.chr
  let of_byte b = Char.code b |> Int32.of_int
      
  (* really need to just blit an int32 word into a string and vice versa *)
      
  let pack str pos item = 
    if String.length str > pos + 4 then failwith "Int32.pack: pos + 4 not within string";
    if pos < 0 then failwith "Int32.pack: pos negative";
    str.[pos] <- to_byte item;
    let item = shift_right item 8 in
    str.[pos+1] <- to_byte item;
    let item = shift_right item 8 in
    str.[pos+2] <- to_byte item;
    let item = shift_right item 8 in
    str.[pos+3] <- to_byte item (* optimize out last logand? *)
      
  let pack_big str pos item = 
    if String.length str > pos + 4 then failwith "Int32.pack_big: pos + 4 not within string";
    if pos < 0 then failwith "Int32.pack_big: pos negative";
    str.[pos+3] <- to_byte item;
    let item = Int32.shift_right item 8 in
    str.[pos+2] <- to_byte item;
    let item = Int32.shift_right item 8 in
    str.[pos+1] <- to_byte item;
    let item = Int32.shift_right item 8 in
    str.[pos] <- to_byte item (* optimize out last logand? *)
    
  let unpack str pos = 
    if String.length str > pos + 4 then failwith "Int32.unpack: pos + 4 not within string";
    if pos < 0 then failwith "Int32.unpack: pos negative";
    let shift n = Int32.shift_left n 8 
    and add b n = Int32.add (of_byte b) n in
    of_byte str.[pos+3] |> shift |> add str.[pos+2] |> shift 
      |> add str.[pos+1] |> shift |> add str.[pos]
      (* TODO: improve performance of bit twiddling?  will these curried functions get inlined? *)

  let unpack_big str pos = 
    if String.length str > pos + 4 then failwith "Int32.unpack: pos + 4 not within string";
    if pos < 0 then failwith "Int32.unpack: pos negative";
    let shift n = Int32.shift_left n 8 
    and add b n = Int32.add (of_byte b) n in
    of_byte str.[pos] |> shift |> add str.[pos+1] |> shift 
      |> add str.[pos+2] |> shift |> add str.[pos+3]

  include Number.MakeNumeric(BaseInt32)


end
