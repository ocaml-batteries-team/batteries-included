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

class type sample_t = object
  method size : int
  method clear : unit
  method values : int BatEnum.t
  method update : int -> unit
end

(** Size of sample -> sample_t *)
class uniformSample : int -> sample_t

(** Size of sample -> alpha (decay constant) -> sample_t *)
class exponentiallyDecayingSample : int -> float -> sample_t

(** type of histograms *)
type t

(** make a histogram from a sample object *)
val make_sample : sample_t -> t

(** make a histogram from some default sample settings

    `UNIFORM makes a histogram for uniformly distributed data, with
    sample size 1028, which gives a 99.9% confidence interval with a
    5% margin of error assuming a normal distribution

    `BIASED uses an exponentially decaying sample of 1028 elements,
    which offers a 99.9% confidence level with a 5% margin of error
    assuming a normal distribution, and an alpha factor of 0.015,
    which heavily biases the sample to the past 5 minutes of
    measurements.
 *)
val make : [`UNIFORM | `BIASED] -> t

val clear : t -> unit

val update : t -> int -> unit

val count : t -> int
val min : t -> int
val max : t -> int
val mean : t -> float
val std_dev : t -> float
val values : t -> int BatEnum.t
