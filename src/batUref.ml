(*
 * Uref -- unifiable references
 * Copyright (C) 2011  Batteries Included Development Team
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

(* Implements union-find with ranks and path-compression  *)

type 'a uref_contents =
  | Ranked of 'a * int
  | Ptr of 'a uref
and 'a uref = 'a uref_contents ref

type 'a t = 'a uref

let rec find ur =
  match !ur with
  | Ptr p ->
    let vr = find p in
    ur := Ptr vr ;
    vr
  | Ranked _ -> ur

let uref x = ref (Ranked (x, 0))

let uget ur =
  match !(find ur) with
  | Ptr _ -> assert false (*BISECT-VISIT*)
  | Ranked (x, _) -> x

let uset ur x =
  let ur = find ur in
  match !ur with
  | Ptr _ -> assert false (*BISECT-VISIT*)
  | Ranked (_, r) -> ur := Ranked (x, r)

let equal ur vr =
  find ur == find vr

let unite ?sel ur vr =
  (* we use ?sel instead of ?(sel=(fun x _y -> x)) because we want to be
     able to know whether a selection function was passed, for
     optimization purposes: when sel is the default (expected common
     case), we can take a short path in the (ur == vr) case. *)
  let ur = find ur in
  let vr = find vr in
  if ur == vr then begin
    match sel with
    | None -> ()
    | Some sel ->
      (* even when ur and vr are the same reference, we need to apply
         the selection function, as [sel x x] may be different from [x].

         For example, [unite ~sel:(fun _ _ -> v) r r] would fail
         to set the content of [r] to [v] otherwise. *)
      match !ur with
      | Ptr _ -> assert false (*BISECT-VISIT*)
      | Ranked (x, r) ->
        let x' = sel x x in
        ur := Ranked(x', r)
  end
  else
    match !ur, !vr with
    | _, Ptr _ | Ptr _, _ -> assert false (*BISECT-VISIT*)
    | Ranked (x, xr), Ranked (y, yr) ->
      let z = match sel with
        | None -> x (* in the default case, pick x *)
        | Some sel -> sel x y in
      if xr = yr then begin
        ur := Ranked (z, xr + 1) ;
        vr := Ptr ur
      end else if xr < yr then begin
        ur := Ranked (z, xr) ;
        vr := Ptr ur
      end else begin
        vr := Ranked (z, yr) ;
        ur := Ptr vr
      end

let print elepr out ur =
  match !(find ur) with
  | Ptr _ -> assert false (*BISECT-VISIT*)
  | Ranked (x, _) ->
    BatInnerIO.nwrite out "uref " ;
    elepr out x
    (*$T print
      let u1 = uref 2 and u2 = uref 3 in unite ~sel:(+) u1 u2; \
      BatIO.to_string (print BatInt.print) u1 = "uref 5" && \
      BatIO.to_string (print BatInt.print) u2 = "uref 5"
    *)
