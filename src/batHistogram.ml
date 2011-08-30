(*
 * BatHistogram
 * Copyright (C) 2011 Edgar Friendly <thelema314@gmail.com>
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

(* Inspired by Coda Hale's Metrics *)

type time = float
let now = Unix.gettimeofday

class type sample_t = object
  method size : int
  method clear : unit
  method values : int BatEnum.t
  method update : int -> unit
end

class uniformSample : int -> sample_t = fun size ->
object (self)
  val mutable count = 0
  val values = Array.make size 0
  method size = BatInt.min size count
  method clear = count <- 0; Array.fill values 0 size 0
  method values = BatArray.enum values
  method update x =
    count <- count + 1;
    if count < size then
      values.(count) <- x
    else
      let r = Random.int count in
      if r < size then
	values.(r) <- x
end

module Map = BatMap

class exponentiallyDecayingSample : int -> float -> sample_t = fun size alpha ->
  let rescale_threshold = float(60 * 60) in (* 1 Hour *)
  let weight t = exp (alpha *. t) in
object (self)
  val mutable count = 0
  val mutable start_time = now ()
  val mutable next_scale_time = now ()
  val mutable values = Map.empty
  method size = BatInt.min size count
  method clear =
    count <- 0;
    start_time <- now();
    next_scale_time <- now() +. rescale_threshold;
    values <- Map.empty
  method values = Map.values values
  method update x = self#update_time x (now ())

  method private update_time x time =  (* TODO: Make concurrent *)
    let prio = weight (time -. start_time) /. Random.float 1.0 in
    count <- count + 1;
    ( if count <= size then
	values <- Map.add prio x values
      else
	let first,_ = Map.min_binding values in
	if first < prio then
	  match Map.add_carry prio x values with
	    (* Still need to remove an element, remove first *)
	    | m, None -> values <- Map.remove first m
	    (* already removed the element with priority = prio *)
	    | m, Some _ -> values <- m
    );
    if now () > next_scale_time then self#rescale
  method private rescale =
    next_scale_time <- now() +. rescale_threshold;
    let old_start_time = start_time in
    start_time <- now();
    let key_mod (k,v) =
      k *. exp (~-. alpha *. (start_time -. old_start_time)),v in
    values <- Map.of_enum (BatEnum.map key_mod (Map.enum values))
end


type t = {mutable count: int; mutable min: float; mutable max: float;
	  mutable variance_m: float; mutable variance_s: float;
	  mutable sum: float; sample: sample_t; }

let make_sample s =
  { count=0; min=max_int; max=min_int; sum=0;
    variance_m = -1.; variance_s = 0.; sample = s}

let make = function
  | `UNIFORM -> make_sample (new uniformSample 1028 :> sample_t)
  | `BIASED -> make_sample (new exponentiallyDecayingSample 1028 0.015 :> sample_t)

let clear t =
  t.count <- 0; t.min <- max_int; t.max <- min_int; t.sum <- 0;
  t.variance_m <- -1.; t.variance_s <- 0.; t.sample#clear

let update_variance t x =
  let x = float x in
  if t.variance_m = -1. then (
    t.variance_m <- x;
    t.variance_s <- 0.;
  ) else (
    let new_m = t.variance_m +. ((x -. t.variance_m) /. float t.count) in
    t.variance_s <- t.variance_s +. ((x -. t.variance_m) *. (x -. new_m));
    t.variance_m <- new_m
  )

let update t x =
  t.count <- t.count + 1;
  if x < t.min then t.min <- x;
  if x > t.max then t.max <- x;
  t.sum <- t.sum + x;
  update_variance t x;
  t.sample#update x

let count t = t.count
let min t = if t.count = 0 then 0.0 else t.min
let max t = if t.count = 0 then 0.0 else t.max
let mean t = if t.count = 0 then 0.0 else t.sum /. float t.count
let variance t = if t.count <= 1 then 0.0 else t.variance_s /. float (t.count-1)
let std_dev t = if t.count = 0 then 0.0 else sqrt (variance t)

let values t = t.sample#values

(* pcts: input range 0-100 *)
let percentile t pcs =
  let values_ordered = t.sample#values |> BatArray.of_enum in
  Array.sort values_ordered;
  let array_size = float (Array.length values_ordered) in
  let get_pctile pct =
    if pct <= 0. | pct >= 100. then invalid_arg "Percentile: out of range";
    let pos, weight = (pct /. 100 *. array_size) |> modf in
    let pos = int_of_float pos in
    (* weighted average of v.(pos) and v.(pos+1) *)
    values_ordered.(pos) *. (1. -. weight) +.
      values_ordered.(pos+1) *. weight
  in
  List.map get_pctile pcs
