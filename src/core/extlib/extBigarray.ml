(*
 * ExtBigarray - additional and modified functions for big arrays.
 * Copyright (C) 2000 Michel Serrano
 *               2000 Xavier Leroy
 *               2009 David Teller, LIFO, Universite d'Orleans
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

TYPE_CONV_PATH "" (*For Sexplib, Bin-prot...*)

open ExtArray

module Bigarray =
struct
  type float32_elt = Bigarray.float32_elt
  type float64_elt = Bigarray.float64_elt
  type complex32_elt = Bigarray.complex32_elt
  type complex64_elt = Bigarray.complex64_elt
  type int8_signed_elt = Bigarray.int8_signed_elt
  type int8_unsigned_elt = Bigarray.int8_unsigned_elt
  type int16_signed_elt = Bigarray.int16_signed_elt
  type int16_unsigned_elt = Bigarray.int16_unsigned_elt
  type int_elt = Bigarray.int_elt
  type int32_elt = Bigarray.int32_elt
  type int64_elt = Bigarray.int64_elt
  type nativeint_elt = Bigarray.nativeint_elt
  type ('a, 'b) kind = ('a, 'b) Bigarray.kind
  type c_layout = Bigarray.c_layout
  type fortran_layout = Bigarray.fortran_layout
  type 'a layout = 'a Bigarray.layout

  let float32 = Bigarray.float32
  let float64 = Bigarray.float64
  let complex32 = Bigarray.complex32
  let complex64 = Bigarray.complex64
  let int8_signed = Bigarray.int8_signed
  let int8_unsigned = Bigarray.int8_unsigned
  let int16_signed = Bigarray.int16_signed
  let int16_unsigned = Bigarray.int16_unsigned
  let int = Bigarray.int
  let int32 = Bigarray.int32
  let int64 = Bigarray.int64
  let nativeint = Bigarray.nativeint
  let char = Bigarray.char
    
  let c_layout = Bigarray.c_layout
  let fortran_layout = Bigarray.fortran_layout


  module Genarray =
  struct
    include Bigarray.Genarray

    (**
       Emulate multi-dimensional coordinates.

       @param index  The index of the element.
       @param dims   The dimensions of the array.
       @param coor   A buffer in which to write the various coordinates
    *)
(*    let index_to_coor index ~dims ~coor =
      (*
	[| a; b; c; d |]
	0 -> 0 0 0 0
	1 -> 0 0 0 1
	2 -> 0 0 0 2
	3 -> 0 0 0 3
	d -> 0 0 1 0
	d+1->0 0 1 1
	d+2->0 0 1 2
	2*d->0 0 1 0
	c*d->0 1 0 0     ->  d' = index mod a * b * c * d
                             c' = index mod a * b * c
      *)

      let product = ref 1 in
	for i = 0 to Array.length dims - 1 do
	  indices.(i) <- 
	done*)

    (**
       Determine the coordinates of the item following this one.

       @param coor Coordinates to increment.
       @param dims The set of coordinates of the array.
       @return [true] if everything happened correctly, [false] if
       we've passed the last element.
    *)
    let inplace_next ~dims ~coor =
      let rec aux i =
	if i < 0 then false
	else
	  let new_value = coor.(i) + 1 in
	    if new_value = dims.(i) then  (*Propagate carry*)
	      begin
		coor.(i) <- 0;
		aux (i - 1)
	      end
	    else 
	      begin
		coor.(i) <- new_value;
		true
	      end
      in aux (Array.length dims - 1)

    let iter f e =
      let dims     = dims     e in
      let coor     = Array.Labels.create (num_dims e) ~init:0 in
	f (get e coor);
	while inplace_next ~dims ~coor do
	  f (get e coor)
	done
	  
    let enum e =
      let dims   = dims e
      and coor   = Array.Labels.create (num_dims e) ~init:0 
      and status = ref `ongoing in
	Enum.from (fun () ->
		     match !status with
		       | `ongoing ->
			   begin
			     try
			       let result = get e coor               in
			       let update = inplace_next ~dims ~coor in
				 if not update then status := `dry;
				 result
			     with _ -> 
			       status := `dry;
			       raise Enum.No_more_elements
			   end
 		        | `dry -> 
			    raise Enum.No_more_elements
		  )
			    
			    

			  


  end


  external genarray_of_array1: ('a, 'b, 'c) Bigarray.Array1.t -> ('a, 'b, 'c) Genarray.t
    = "%identity"
  external genarray_of_array2: ('a, 'b, 'c) Bigarray.Array2.t -> ('a, 'b, 'c) Genarray.t
    = "%identity"
  external genarray_of_array3: ('a, 'b, 'c) Bigarray.Array3.t -> ('a, 'b, 'c) Genarray.t
    = "%identity"
    external reshape:
   ('a, 'b, 'c) Genarray.t -> int array -> ('a, 'b, 'c) Genarray.t
   = "caml_ba_reshape"


  let reshape_3 = Bigarray.reshape_3
  let reshape_2 = Bigarray.reshape_2
  let reshape_1 = Bigarray.reshape_1

  let array3_of_genarray = Bigarray.array3_of_genarray
  let array2_of_genarray = Bigarray.array2_of_genarray
  let array1_of_genarray = Bigarray.array1_of_genarray

  module Array1 = struct
    include Bigarray.Array1
    let enum t = Genarray.enum (genarray_of_array1 t)
  end
  module Array2 = struct
    include Bigarray.Array2
    let enum t = Genarray.enum (genarray_of_array2 t)
  end
  module Array3 = struct
    include Bigarray.Array3
    let enum t = Genarray.enum (genarray_of_array3 t)
  end
end
