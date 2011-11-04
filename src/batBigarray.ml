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


module A = struct include BatArray include BatArray.Labels end

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

    let ofs e = match Obj.magic (layout e) with
      | 0 -> 0     (* keep these magic constants in sync with bigarray.ml *)
      | 0x100 -> 1 (* and transitively with caml_ba_layout in bigarray.h  *)
      | _ -> failwith "Unknown layout" (* note four copies of this function *)

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
    let inplace_next ~ofs ~dims ~coor =
      let rec aux i =
	if i < 0 then false
	else
	  let new_value = coor.(i) + 1 in
	    if new_value = dims.(i) + ofs then  (*Propagate carry*)
	      begin
		coor.(i) <- ofs;
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
      let offset   = ofs e in
      let coor     = A.create (num_dims e) ~init:offset in
	f (get e coor);
	while inplace_next ~ofs:offset ~dims ~coor do
	  f (get e coor)
	done

    let iteri f e =
      let dims     = dims     e in
      let offset   = ofs e in
      let coor     = A.create (num_dims e) ~init:offset in
	f (A.Cap.of_array coor) (get e coor);
	while inplace_next ~ofs:offset ~dims ~coor do
	  f (A.Cap.of_array coor) (get e coor)
	done

    let modify f e =
      let dims = dims e in
      let offset = ofs e in
      let change c = set e c (f (get e c)) in
      let coor = A.create (num_dims e) ~init:offset in
      change coor;
      while inplace_next ~ofs:offset ~dims ~coor do
        change coor
      done

    let modifyi f e =
      let dims = dims e in
      let offset = ofs e in
      let change c = set e c (f (A.Cap.of_array c) (get e c)) in
      let coor = A.create (num_dims e) ~init:offset in
      change coor;
      while inplace_next ~ofs:offset ~dims ~coor do
        change coor
      done

    let enum e =
      let dims   = dims e
      and offset = ofs e in
      let coor   = A.create (num_dims e) ~init:offset
      and status = ref `ongoing in
	BatEnum.from (fun () ->
		     match !status with
		       | `ongoing ->
			   begin
			     try
			       let result = get e coor               in
			       let update = inplace_next ~ofs:offset ~dims ~coor in
				 if not update then status := `dry;
				 result
			     with _ ->
			       status := `dry;
			       raise BatEnum.No_more_elements
			   end
		        | `dry ->
			    raise BatEnum.No_more_elements
		  )

    let map f b_kind a =
      let d = dims a in
      let b = create b_kind (layout a) d in
	iteri (fun i x -> set b (A.Cap.to_array i) (f x)) a;
	b

    let mapi f b_kind a =
      let d = dims a in
      let b = create b_kind (layout a) d in
	iteri (fun i x -> set b (A.Cap.to_array i) (f (A.Cap.read_only i) x)) a;
	b
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
    let ofs e = match Obj.magic (layout e) with
      | 0 -> 0     (* keep these magic constants in sync with bigarray.ml *)
      | 0x100 -> 1 (* and transitively with caml_ba_layout in bigarray.h  *)
      | _ -> failwith "Unknown layout" (* note four copies of this function *)
    let enum t = Genarray.enum (genarray_of_array1 t)

    let map f b_kind a =
      let b_dim = dim a in
      let b = create b_kind (layout a) b_dim in
      for i = ofs a to ofs a + b_dim - 1 do
        b.{i} <- f a.{i}
      done;
      b

    let mapi f b_kind a =
      let b_dim = dim a in
      let b = create b_kind (layout a) b_dim in
      for i = ofs a to ofs a + b_dim - 1 do
        b.{i} <- f i a.{i}
      done;
      b

    let modify f a =
      for i = ofs a to ofs a + dim a - 1 do
        unsafe_set a i (f (unsafe_get a i))
      done

    let modifyi f a =
      for i = ofs a to ofs a + dim a - 1 do
        unsafe_set a i (f i (unsafe_get a i))
      done

    let to_array a = Array.init (dim a) (fun i -> a.{i+(ofs a)})
  end
  module Array2 = struct
    include Bigarray.Array2
    let ofs e = match Obj.magic (layout e) with
      | 0 -> 0     (* keep these magic constants in sync with bigarray.ml *)
      | 0x100 -> 1 (* and transitively with caml_ba_layout in bigarray.h  *)
      | _ -> failwith "Unknown layout" (* note four copies of this function *)
    let enum t = Genarray.enum (genarray_of_array2 t)

    let map f b_kind a =
      let b_dim1 = dim1 a in
      let b_dim2 = dim2 a in
      let b = create b_kind (layout a) b_dim1 b_dim2 in
      for i = ofs a to ofs a + b_dim1 - 1 do
        for j = ofs a to ofs a + b_dim2 - 1 do
          b.{i, j} <- f a.{i, j}
        done
      done;
      b

    let mapij f b_kind a =
      let b_dim1 = dim1 a in
      let b_dim2 = dim2 a in
      let b = create b_kind (layout a) b_dim1 b_dim2 in
      for i = ofs a to ofs a + b_dim1 - 1 do
        for j = ofs a to ofs a + b_dim2 - 1 do
          b.{i, j} <- f i j a.{i, j}
        done
      done;
      b

    let modify f a =
      for i = ofs a to ofs a + dim1 a - 1 do
        for j = ofs a to ofs a + dim2 a - 1 do
          unsafe_set a i j (f (unsafe_get a i j))
        done
      done

    let modifyij f a =
      for i = ofs a to ofs a + dim1 a - 1 do
        for j = ofs a to ofs a + dim2 a - 1 do
          unsafe_set a i j (f i j (unsafe_get a i j))
        done
      done

    let to_array a =
      Array.init (dim1 a) (
        fun i ->
          Array.init (dim2 a) (
            fun j -> a.{i + ofs a, j + ofs a}
          )
      )
  end
  module Array3 = struct
    include Bigarray.Array3
    let ofs e = match Obj.magic (layout e) with
      | 0 -> 0     (* keep these magic constants in sync with bigarray.ml *)
      | 0x100 -> 1 (* and transitively with caml_ba_layout in bigarray.h  *)
      | _ -> failwith "Unknown layout" (* note four copies of this function *)
    let enum t = Genarray.enum (genarray_of_array3 t)

    let map f b_kind a =
      let b_dim1 = dim1 a in
      let b_dim2 = dim2 a in
      let b_dim3 = dim3 a in
      let b = create b_kind (layout a) b_dim1 b_dim2 b_dim3 in
      for i = 0 to b_dim1 - 1 do
        for j = 0 to b_dim2 - 1 do
          for k = 0 to b_dim3 - 1 do
            b.{i, j, k} <- f a.{i, j, k}
          done
        done
      done;
      b

    let mapijk f b_kind a =
      let b_dim1 = dim1 a in
      let b_dim2 = dim2 a in
      let b_dim3 = dim3 a in
      let b = create b_kind (layout a) b_dim1 b_dim2 b_dim3 in
      for i = 0 to b_dim1 - 1 do
        for j = 0 to b_dim2 - 1 do
          for k = 0 to b_dim3 - 1 do
            b.{i, j, k} <- f i j k a.{i, j, k}
          done
        done
      done;
      b

    let modify f a =
      for i = ofs a to ofs a + dim1 a - 1 do
        for j = ofs a to ofs a + dim2 a - 1 do
          for k = ofs a to ofs a + dim3 a - 1 do
            unsafe_set a i j k (f (unsafe_get a i j k))
          done
        done
      done

    let modifyijk f a =
      for i = ofs a to ofs a + dim1 a - 1 do
        for j = ofs a to ofs a + dim2 a - 1 do
          for k = ofs a to ofs a + dim3 a - 1 do
            unsafe_set a i j k (f i j k (unsafe_get a i j k))
          done
        done
      done

    let to_array a =
      Array.init (dim1 a) (
        fun i ->
          Array.init (dim2 a) (
            fun j ->
              Array.init (dim3 a) (
                fun k -> a.{i, j, k}
              )
          )
      )
  end
