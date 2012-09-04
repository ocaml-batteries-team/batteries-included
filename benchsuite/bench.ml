(*
 * Bench - Benchmarking functions
 * Copyright (C) 2011 Edgar Friendly
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

(** Benchmarking functions, based on haskell criterion

   @author Edgar Friendly <thelema314@gmail.com>
*)

open Printf

let (|>) x f = f x
let (/^) a b = (float a) /. (float b)
let rec repeat f x n = if n <= 0 then () else (ignore (f x); repeat f x (n-1))
let curry f (x,y) = f x y
let tap f x = f x; x
let rec (--.) (lo,step) hi = if lo <= hi then lo :: ((lo +. step),step) --. hi else []
let debug = false
let dtap f x = if debug then (f x; x) else x

module Measurement = struct
(* TODO: make customizable timer? *)
  (*external timer : unit -> float = "bat_clock_gettime"*)
  let timer = Unix.gettimeofday

  let time f x =
    let t0 = timer () in
    let ret = f x in
    timer () -. t0, ret

  let time_ f x =
    let t0 = timer () in
    f x;
    timer () -. t0

  let rec print oc t =
    if t < 0. then fprintf oc "-%a" print (-. t)
    else if t >= 1.   then fprintf oc "%.2f s" t
    else if t >= 1e-3 then fprintf oc "%.2f ms" (t*.1e3)
    else if t >= 1e-6 then fprintf oc "%.2f us" (t*.1e6)
    else if t >= 1e-9 then fprintf oc "%.2f ns" (t*.1e9)
    else if t >= 1e-12 then fprintf oc "%.2f ps" (t*.1e12)
    else fprintf oc "%g s" t
end
module M = Measurement

let mean a = (Array.fold_left (+.) 0. a) /. float (Array.length a)
let median a =
  let sorted = Array.copy a in
  Array.sort Pervasives.compare sorted;
  let len = Array.length a in
  if len land 1 = 1 then sorted.(len/2+1)
  else (sorted.(len/2) +. sorted.(len/2+1))/. 2.
let stdev ?mu a =
  let mu = match mu with None -> mean a | Some x -> x in
  let acc_dev acc a_i = let d = a_i -. mu in acc +. d *. d in
  let dev = Array.fold_left acc_dev 0. a in
  sqrt (dev /. float (Array.length a - 1))

let run_for_time t f seed0 =
  let t0 = M.timer () in
  let rec loop seed iters =
    let now = M.timer() in
    if now -. t0 > t *. 10. then
      failwith (sprintf "Took too long to run: seed %d iters %d" seed iters);
    let ti, ret = M.time f seed in
    if ti > t then (ti, seed, ret)
    else loop (2*seed) (iters+1)
  in
  loop seed0 0

module Normal_dist = struct
  let pi = 4. *. atan 1.
  let sqrt_2 = sqrt 2.0
  let sqrt_2_pi = pi *. sqrt_2
  let log_pi_over_2 = 0.572364942924700087071713675675 (*log_e(pi)/2*)

  (*reference - Haruhiko Okumura: C-gengo niyoru saishin algorithm jiten
    (New Algorithm handbook in C language) (Gijyutsu hyouron
    sha, Tokyo, 1991) p.227 [in Japanese] *)

  (* Incomplete gamma function
     1 / Gamma(a) * Int_0^x exp(-t) t^(a-1) dt  *)
  let rec p_gamma a x loggamma_a =
    if x >= 1. +. a then 1. -. q_gamma a x loggamma_a
    else if x = 0. then 0.
    else
      let rec pg_loop prev res term k =
        if k > 1000. then (eprintf "p_gamma could not converge."; res)
        else if prev = res then res
        else
          let term = term *. x /. (a +. k) in
          pg_loop res (res +. term) term (k +. 1.)
      in
      let r0 =  exp (a *. log x -. x -. loggamma_a) /. a in
      pg_loop min_float r0 r0 1.

  (* Incomplete gamma function
     1 / Gamma(a) * Int_x^inf exp(-t) t^(a-1) dt  *)
  and q_gamma a x loggamma_a =
    if x < 1. +. a then 1. -. p_gamma a x loggamma_a
    else
      let rec qg_loop prev res la lb w k =
        if k > 1000. then (eprintf "q_gamma could not converge."; res)
        else if prev = res then res
        else
          let la, lb =
            lb, ((k -. 1. -. a) *. (lb -. la) +. (k +. x) *. lb) /. k
          in
          let w = w *. (k -. 1. -. a) /. k in
          let prev, res = res, res +. w /. (la *. lb) in
          qg_loop prev res la lb w (k +. 1.)
      in
      let w = exp (a *. log x -. x -. loggamma_a) in
      let lb = (1. +. x -. a) in
      qg_loop min_float (w /. lb) 1. lb w 2.0

  let erf = function
    | x when classify_float x = FP_nan -> x
    | x when classify_float x = FP_infinite -> if x > 0. then 1. else -1.
    | x when x > 0. -> p_gamma 0.5 (x *. x) log_pi_over_2
    | x (* x < 0 *) -> -. p_gamma 0.5 (x *. x) log_pi_over_2
  let erfc = function
    | x when classify_float x = FP_nan -> x
    | x when classify_float x = FP_infinite -> if x > 0. then 0. else 2.
    | x when x >= 0. -> q_gamma 0.5 (x *. x) log_pi_over_2
    | x (* x < 0. *) -> 1. +. p_gamma 0.5 (x *. x) log_pi_over_2

  let standard_pdf x = exp (-. x *. x) /. sqrt_2_pi
  let standard_cdf x = erfc (-. x /. sqrt_2) /. 2.

  let find_root ?(accuracy=1e-15) ?(max_iters=150) cdf pdf y x0 x_min x_max =
    let rec fr_loop i dx x lo hi =
      if abs_float dx < accuracy || i > max_iters then x
      else
        let err = cdf x -. y in
        let lo,hi = if err < 0. then x, hi else lo, x in
        let pdf_x = pdf x in
        let dx,x' = if pdf_x = 0. then dx,x else err /. pdf_x, (x -. dx) in
        let dx,x' =
          if x' < lo || x' > hi || pdf_x = 0. then
            let y = (lo +. hi) /. 2. in (y -. x),y
          else dx, x'
        in
        fr_loop (i+1) dx x' lo hi
    in
    fr_loop 0 1. x0 x_min x_max

  let standard_quant =
    function
    | p when p < 0. || p > 1. -> nan
    | p when p = 0. -> neg_infinity
    | p when p = 1. -> infinity
    | p when p = 0.5 -> 0.
    | p -> find_root standard_cdf standard_pdf p 0. (-100.) 100.

end

module Bootstrap = struct
  type resample = Resample of float array

  let resample ests num_resamples samples =
    let num_samples = Array.length samples in
    let gen_sample () =
      Array.init num_samples (fun _ -> samples.(Random.int num_samples)) in
    let gen_estimations e =
      let est_outs = Array.init num_resamples (fun _ -> e(gen_sample ())) in
      Array.sort compare est_outs;
      Resample est_outs
    in
    List.map gen_estimations ests


  let drop_at i arr =
    let len = Array.length arr in
    let ret = Array.make (len-1) 0. in
    if i > 0 then Array.blit arr 0 ret 0 (i-1);
    if i < len-1 then Array.blit arr (i+1) ret i (len-(i+1));
    ret

  let jackknife est sample =
    Array.init (Array.length sample) (fun i -> est (drop_at i sample))

  type estimate = {point: float; lower: float; upper: float; confidence: float}
  let estimate p l u c = {point=p; lower=l; upper=u; confidence=c}
  let get {point;lower;upper} = (point,lower,upper)

  let est_scale s est = {est with point = s *. est.point; lower = s *. est.lower; upper = s *. est.upper}

  let e_print name oc e =
    fprintf oc "%s: %a, %2.0f%% CI: (%a, %a)\n" name
      M.print e.point (e.confidence *. 100.) M.print e.lower M.print e.upper
  let e_print_csv oc e = fprintf oc "%g,%g,%g" e.point e.lower e.upper


  let bootstrap_bca confidence sample estimators resamples =
    if confidence <= 0. || confidence >= 1. then failwith "bootstrap_bca: confidence must be between 0 and 1";
    let make_estimate est (Resample res) =
      let pt = est sample in
      if Array.length sample = 1 then
        estimate pt pt pt confidence
      else
        let n = Array.length res in
        let jack = jackknife est sample in
        let jackmean = mean jack in
        let sum_cubes = Array.fold_left (fun acc x -> let d = jackmean -. x in d *. d *. d) 0. jack in
        let sum_squares = Array.fold_left (fun acc x -> let d = jackmean -. x in d *. d) 0. jack in
        let accel = sum_cubes /. (6. *. (sum_squares ** 1.5)) in
        let cumn x = int_of_float ((Normal_dist.standard_cdf x) *. (float n)) in
        let probN = Array.fold_left (fun acc x -> if x < pt then acc+1 else acc) 0 res in
        let bias = Normal_dist.standard_quant (float probN /. float n) in
        let z1 = Normal_dist.standard_quant ((1. -. confidence) /. 2.) in
        let b1 = bias +. z1 in
        let b2 = bias -. z1 in
        let a1 = bias +. b1 /. (1. -. accel *. b1) in
        let a2 = bias +. b2 /. (1. -. accel *. b2) in
        let lo = max (cumn a1) 0 in
        let hi = min (cumn a2) (n-1) in
        estimate pt res.(lo) res.(hi) confidence
    in
    List.map2 make_estimate estimators resamples
end

module Outliers = struct
  type outliers = {
    data_count:  int;
    low_severe:  int; ls_limit: float;
    low_mild:    int; lm_limit: float;
    high_mild:   int; hm_limit: float;
    high_severe: int; hs_limit: float;
  }

  let print oc {data_count=dc; low_severe=ls; low_mild=lm; high_mild=hm; high_severe=hs} =
    let one_percent = dc / 100 in
    if ls>0 || lm > one_percent || hm > one_percent || hs > 0 then begin
      printf "Outliers: ";
      let print cat thr n =
        if n > thr then
          fprintf oc "%d (%.1f%%) %s, " n (n/^dc *. 100.) cat
      in
      print "Low Severe" 0 ls;
      print "Low Mild" one_percent lm;
      print "High Mild" one_percent hm;
      print "High Severe" 0 hs;
      print_newline();
    end

  (* Samples must be sorted in increasing order *)
  let quantile nth quantiles samples =
    assert (quantiles >= 2);
    assert (nth >= 0 && nth <= quantiles);
    let n = Array.length samples in
    if n = 0 then invalid_arg "Cannot quantile an empty array";
    if n = 1 then samples.(0)
    else (* weighted avg between idx and idx+1 *)
      let idx_float = ((n-1) * nth) /^ quantiles in
      let idx = int_of_float (idx_float) in
      let interp = idx_float -. (float idx) in
      (* printf "Quant: %d/%d of %d: %d\n" nth quantiles n idx; *)
      (* weighted average of idx'th and (idx+1)'th sample *)
      if idx >= n-1 then samples.(n-1)
      else samples.(idx) +. interp *. (samples.(idx+1) -. samples.(idx))

  (* searches a sorted array for the offset of the transition between
     elements less than elem and those greater than or equal to elem *)
  let find_transition (data:float array) elem =
    let rec iter a b =
      (* the bounds of the search includes data.(a) and excludes data.(b) *)
      if a = b then a
      else
        let mid = a + (b - a)/2 in
        match data.(mid) with
          | value when value = elem -> mid
          | value when value < elem -> iter (mid + 1) b
          | _                       -> iter a mid
    in
    iter 0 (Array.length data)

  let note_outliers oc a =
    let len = Array.length a in
    let sorted = Array.copy a in
    Array.sort compare sorted;
    let q1 = quantile 1 4 sorted in
    let q3 = quantile 3 4 sorted in
    let inter_quartile_range = q3 -. q1 in
    fprintf oc "N: %d Inter-quartile width:%a, Full range: (%a,%a)\n"
      len
      M.print inter_quartile_range
      M.print sorted.(0)
      M.print sorted.(len-1);
    if inter_quartile_range <> 0. then (
      let sevr_lo = q1 -. inter_quartile_range *. 3. in
      let mild_lo = q1 -. inter_quartile_range *. 1.5 in
      let mild_hi = q3 +. inter_quartile_range *. 1.5 in
      let sevr_hi = q3 +. inter_quartile_range *. 3. in
      let slo_pos = find_transition sorted sevr_lo in
      let mlo_pos = find_transition sorted mild_lo in
      let mhi_pos = find_transition sorted mild_hi in
      let shi_pos = find_transition sorted sevr_hi in
      print oc {
        data_count = len;
        low_severe = slo_pos; ls_limit = sevr_lo;
        low_mild = mlo_pos - slo_pos; lm_limit = mild_lo;
        high_mild = shi_pos - mhi_pos; hm_limit = mild_hi;
        high_severe = len-shi_pos; hs_limit = sevr_hi;
      }
    );
    ()

  let analyze_mean _i a =
    (*note_outliers IO.stdout a;*)
    mean a

  type effect =
    | Unaffected (* less then 1% effect *)
    | Slight     (* between 1% and 10% *)
    | Moderate   (* between 10% and 50% *)
    | Severe     (* more than 50% *)

  let effect_to_string = function | Unaffected -> "unaffected" | Slight -> "slightly affected" | Moderate -> "moderately affected" | Severe -> "severely affectedkil"

  let effect_of_var x =
    if x < 0.01 then Unaffected
    else if x < 0.1 then Slight
    else if x < 0.5 then Moderate
    else Severe

  let outlier_variance mu sigma n =
    let n_fl = float n in
    let sb = sigma.Bootstrap.point in
    let ua = mu.Bootstrap.point /. n_fl in
    let sb2 = sb *. sb in
    let sg = min (ua /. 8.) (sb /. sqrt n_fl) in
    let sg2 = sg *. sg in
    let cmax x =
      let d = 2. *. (ua -. x) in
      let ad = n_fl *. d in
      let k0 = -. n_fl *. ad in
      let k1 = sb2 -. n_fl *. sg2 +. ad in
      let det = k1 *. k1 -. 4. *. sg2 *. k0 in
      floor (-2. *. k0 /. (k1 +. sqrt det)) |> int_of_float
    in
    let var_out c = let ac = n-c in (ac /^ n) *. (sb2 -. float ac *. sg2) in
    let minby f x v = min (f x) (f v) in
    let var_out_min = minby var_out 1 (minby cmax 0. (ua /. 2.)) in
    var_out_min

  let print_effect oc ov =
    if ov > 0.00001 then (
      let effect = effect_of_var ov |> effect_to_string in
      printf "variance introduced by outliers: %.5f%%\n" (ov *. 100.);
      printf "variance is %s by outliers\n" effect;
    )

end

type results = {
  desc : string;
  times: float array;
  mean : Bootstrap.estimate;
  stdev: Bootstrap.estimate;
  ov : float; (* outlier variance *)
}

let analyze_sample desc ci samples num_resamples =
  let ests = [ mean; stdev ] in
  let resamples = Bootstrap.resample ests num_resamples samples in
  match Bootstrap.bootstrap_bca ci samples ests resamples with
    | [mu_hat; sigma_hat] ->
      let ov = Outliers.outlier_variance mu_hat sigma_hat (Array.length samples) in
      {desc=desc; times=samples; mean=mu_hat; stdev=sigma_hat; ov=ov}
    | _ -> assert false

(* scale the result values by s *)
let res_scale s res =
  { res with
    times = Array.map ( ( *. ) s ) res.times;
    mean = Bootstrap.est_scale s res.mean;
    stdev = Bootstrap.est_scale s res.stdev;
  }


(* Print a summary of the results, noting any outliers *)
let print_res ?(verbose=false) oc res =
  if verbose then Outliers.note_outliers oc res.times;
  Bootstrap.e_print "mean" oc res.mean;
  Bootstrap.e_print "std.dev." oc res.stdev;
  Outliers.print_effect oc res.ov;
  fprintf oc "\n";
  ()

let list_print ~first ~sep ~last to_string oc lst =
  let rec lp_aux = function
    | [] -> ()
    | [last] -> output_string oc (to_string last)
    | h::t -> output_string oc (to_string h); output_string oc sep; lp_aux t
  in
  output_string oc first;
  lp_aux lst;
  output_string oc last

(* print a list of results to a csv file *)
let print_csv resl oc =
  let print_csv_string l =
    list_print ~first:"\"" ~sep:"\",\"" ~last:"\"\n" (fun x -> x) oc l in
  let print_csv_float l =
    list_print ~first:"" ~sep:"," ~last:"\n" string_of_float oc l in
  print_csv_string (List.map (fun r -> r.desc) resl);
  for i = 0 to Array.length (List.hd resl).times - 1 do
    print_csv_float (List.map (fun r -> r.times.(i)) resl);
  done

let print_json resl oc =
  List.iter (fun res ->
    fprintf oc "{ name: \"%s\"; samples: [%a] }\n" res.desc
      (fun oc ts -> Array.iteri (fun i x ->  output_string oc (string_of_float x); if i <> Array.length ts then output_string oc ", ") ts ) res.times
  ) resl

let print_flat resl oc = match resl with [] -> () | res::_ ->
  fprintf oc "flat\n";
  Array.iter (fprintf oc "%g\n") res.times

let print_result oc res =
  let mp,ml,mu = Bootstrap.get res.mean in
  let sp,sl,su = Bootstrap.get res.stdev in
  fprintf oc "%s\n%g %g %g\n%g %g %g\n" res.desc mp ml mu sp sl su;
  Array.iter (fprintf oc "%g ") res.times;
  fprintf oc "\n"


let print_times filename =
  let handler =
    if Filename.check_suffix filename ".csv" then print_csv
    else if Filename.check_suffix filename ".json" then print_json
    else if Filename.check_suffix filename ".flat" then print_flat
    else failwith "Unknown output filename suffix"
  in
  (fun resl ->
    Printf.eprintf "Saving times to %s\n" filename;
    let oc = open_out filename in
    handler resl oc;
    close_out oc;
  )

let cmp_ci r1 r2 =
  let l1 = r1.mean.Bootstrap.lower in
  let u1 = r1.mean.Bootstrap.upper in
  let l2 = r2.mean.Bootstrap.lower in
  let u2 = r2.mean.Bootstrap.upper in
  if u1 < l2 then -1 else if u2 < l1 then 1 else 0
let cmp_point r1 r2 =
  Pervasives.compare r1.mean.Bootstrap.point r2.mean.Bootstrap.point
let change r1 r2 =
  let t1 = r1.mean.Bootstrap.point in
  let t2 = r2.mean.Bootstrap.point in
  (t2 -. t1) /. t2 *. 100. (* percent improvement *)

let test_unequal r1 r2 = (* t-test for difference in population means *)
  let u1 = r1.mean.Bootstrap.point in
  let u2 = r2.mean.Bootstrap.point in
  let s1 = r1.stdev.Bootstrap.point in
  let s2 = r2.stdev.Bootstrap.point in
  let s1m = s1 *. s1 /. float (Array.length r1.times) in
  let s2m = s2 *. s2 /. float (Array.length r2.times) in
  let t = (u2 -. u1) /. sqrt (s1m +. s2m) in
  if debug then Printf.printf "u1:%g u2:%g s1m:%g s2m:%g t-score: %g\n" u1 u2 s1m s2m t;
  (* Assumes large samples i.e. n>30 *)
  1. -. Normal_dist.standard_cdf t (* return p-value *)

(* print the given results in order from shortest time to longest
   time, with statistically indistinguishable values marked *)
let summarize ?(alpha=0.05) = function [] -> () | [_] -> () (* no functions - do nothing *)
  | res_list -> (* multiple functions tested - group and compare *)
    let rec print_changes ~pre = function
      | [] -> assert false
      | [r] -> printf "%s (%a)\n" r.desc M.print r.mean.Bootstrap.point
      | r1::(r2::_ as tl) ->
        let p_value = test_unequal r1 r2 in
        printf "%s (%a) %s" r1.desc M.print r1.mean.Bootstrap.point pre;
        if p_value > alpha then
          printf "is probably (alpha=%.2f%%) same speed as\n" (p_value *. 100.)
        else
          printf "is %.1f%% faster than\n" (change r1 r2);
        print_changes ~pre:"which " tl
    in
    print_changes ~pre:"" (List.sort cmp_point res_list)

type config = {
  mutable verbose : bool;
  mutable samples: int;
  mutable gc_between_tests: bool;
  mutable resamples: int;
  mutable confidence_interval: float;
  mutable output: (results list -> unit) list;
  mutable min_iters: int;
}

(* The module-global configuration for running benchmarks.

   TODO: this should be either parent or child of environment so it can
   be non-global
*)
let config = { verbose = true;
               samples=300;
               resamples = 1_000;
               confidence_interval = 0.95;
               gc_between_tests= false;
(*	       output = [summarize ~alpha:0.05];*)
               output = [print_times "times.flat"; summarize ~alpha:0.05];
               min_iters = 1;
             }

let vtap f x = if config.verbose then (f x; x) else x

type environment = {mutable clock_res: float; mutable clock_cost: float}
let env = {clock_res = min_float; clock_cost = max_float}

let is_positive x = x > 0.

(* produce an environment record appropriate for the current system by
   measuring the cost and resolution of the M.timer() function *)
let init_environment () =
  if env.clock_res = min_float then (* do nothing if already initialized *)
    let resolution i = (* measure the clock resolution *)
      let times = Array.init (i+1) (fun _ -> M.timer()) in
      let pos_diffs =
        Array.init i (fun i -> times.(i+1) -. times.(i))
	           |> Array.to_list |> List.filter is_positive |> Array.of_list
      in
      pos_diffs
    in
    let cost t t0 = (* compute clock cost *)
      (* put timer in closure to compensate for testing closure *)
      let f () = M.timer () in
      let tclock i = M.time_ (repeat f ()) i in
      ignore (tclock 100);
      let (_,iters,elapsed) = run_for_time t0 tclock 10_000 in
      let times = Array.init (ceil (t /. elapsed) |> int_of_float)
        (fun _ -> tclock iters)
      in
      Array.map (fun t -> t /. float iters) times
    in
    if config.verbose then print_endline "Measuring: System Clock";
    if config.verbose then print_endline "Warming up";
    let (_,seed,_) = run_for_time 0.1 resolution 10_000 in
    if config.verbose then print_string "Estimating clock resolution";
    let (_,i,clocks) = run_for_time 0.5 resolution seed in
    (* TODO: Do we want mean here?!? Look into better detection of clock resolution *)
    let clock_res = Outliers.analyze_mean i clocks in
    if config.verbose then printf " (%a)\nEstimating cost of timer call" M.print clock_res;
    let ts = cost (min (10_000. *. clock_res) 3.) (max 0.01 (5.*.clock_res)) in
    let clock_cost = Outliers.analyze_mean (Array.length ts) ts in
    if config.verbose then printf " (%a)\n" M.print clock_cost;
    env.clock_res <- clock_res;
    env.clock_cost <- clock_cost


let min_runtime = ref 0.1

(* benchmark a function appropriate for the current environment.

   The number of samples is given in config.sample

   The number of iterations of the benchmark to run per sample is
   computed based on the number of iterations that can be run in 0.1s
   so that each sample takes at most (clock_res * 1000) or 0.1
   seconds, unless it takes longer than that for a single repetition.
 *)
let run_benchmark (f: int -> 'a) =
  (* warm up clock function *)
  let tclock i = M.time_ (repeat M.timer ()) i in
  run_for_time 0.1 tclock 10_000 |> ignore;
  (* run for 0.1s per sample or 1000*clock resolution, whichever is shorter *)
  let min_time = min (env.clock_res *. 1_000.) !min_runtime in
  let (test_time, test_iters, _) = run_for_time min_time f 1 in
  if config.verbose then
    printf "Ran %d iterations in %a\n%!" test_iters M.print test_time;
  let iters = ceil (min_time *. float test_iters /. test_time) in
  let iters_int = max (int_of_float iters) config.min_iters in
  let est_time = float config.samples *. iters *. test_time /. float test_iters in
  if config.verbose then
    printf "Collecting %d samples, %d iterations each, estimated time: %a\n%!"
      config.samples iters_int M.print est_time;
  Array.init config.samples (fun _ ->
    if config.gc_between_tests then Gc.compact ();
    M.time_ f iters_int)
		   |> Array.map (fun t -> (t -. env.clock_cost) /. iters)

(** Run a benchmark and analyze the results, printing a simple summary to stdout *)
let run_and_analyze desc f =
  init_environment ();
  printf "Benchmarking: %s\n%!" desc;
  let times = run_benchmark f in
(*  printf " ... Analyzing with %d resamples\n%!" config.resamples;*)
  analyze_sample desc config.confidence_interval times config.resamples
  |> tap (print_res ~verbose:config.verbose stdout)

(* run the output functions on our results *)
let run_outputs res = List.iter (fun f -> f res) config.output

(** Functions to benchmark are (int -> unit).  Parameter is number of
    repetitions *)
let bench_n fs = List.map (curry run_and_analyze) fs

(* Benchmark unit functions with names *)
let bench fs =
  List.map (fun (d,f) -> run_and_analyze d (repeat f ())) fs |> run_outputs

(** This is the main function to benchmark and compare a number of
    functions.  Functions to benchmark have a value to apply them to.
    We will rewrite them to take int argument of # of reps to run. *)
let bench_arg fs =
  List.map (fun (d,f,x) -> run_and_analyze d (repeat f x)) fs

(** f argument is ('a -> unit), and we are given a [(string * 'a) list]
   to test across *)
let bench_args f dxs =
  List.map (fun (d,x) -> run_and_analyze d (repeat f x)) dxs

(** [bench_funs fs x] benchmarks a list of labeled functions on the
    same input, x *)
let bench_funs fs x =
  List.map (fun (d,f) -> run_and_analyze d (repeat f x)) fs

(** This function is similar to bench_args, but args are ints, and we
    rescale times.  This is useful for testing different block sizes
    of a function to see which work unit size leads to the highest
    throughput. *)
let bench_throughput f xs =
  let bench_one x =
    run_and_analyze (string_of_int x) (repeat f x)
  |> res_scale (1. /. float x)
  in
  List.map bench_one xs

(* generate points spaced nicely -
   exponential for really big ranges (lo/hi>10),
   if we can hit every int between lo and hi with between n/3 and n*3 points
   unit spacing for medium ranges,

   default 10 intervals *)
let rec gen_points ?(n=10) lo hi =
(*  printf "gp %g %g\n%!" lo hi;*)
  assert (hi >= lo);
  if hi = lo then [lo]
  else if lo > 0. && hi /. lo > 100. then gen_points ~n (log lo) (log hi) |> List.map exp
  else if hi -. lo < float (n*3) && hi -. lo > float (n/3) && floor (hi -. lo) = (hi -. lo) then
    (floor lo, 1.) --. ceil hi
  else
    let step = (hi -. lo) /. float n in
    (lo, step) --. hi

let rec uniq = function
  | x :: y :: t when x = y -> uniq (x :: t)
  | x :: t -> x :: uniq t
  | [] -> []

(* hide the float internals, give a nice int interface *)
let gen_points ?n lo hi =
  gen_points ?n (float lo) (float hi) |> List.map (fun x -> truncate (x+. 0.5)) |> uniq

let bench_range f ~input_gen ?n (lo,hi) =
  let points = gen_points ?n lo hi in
  let run_one i = run_and_analyze (string_of_int i) (repeat f (input_gen i)) in
  List.map run_one points

let rec transpose list = match list with
  | []             -> []
  | []   :: xss    -> transpose xss
  | (x::xs) :: xss ->
    (x :: List.map List.hd xss) :: transpose (xs :: List.map List.tl xss)

let bench_2d fs ~input_gen ?n (lo,hi) =
  let points = gen_points ?n lo hi in
  let run_one (df,f) i input =
    let d = df ^ "_" ^ string_of_int i in
    run_and_analyze d (repeat f input) |> res_scale (1. /. float i)
  in
  let run_all i = let inp = input_gen i in List.map (fun f -> run_one f i inp) fs in
  let results_by_input = List.map run_all points in
  let results_by_f = transpose results_by_input in
  points, List.map2 (fun (df,_) rs -> df, rs) fs results_by_f
(* returns list of (desc, result list); each sublist is all results
   for one function *)

let print_ranges oc (desc,resl) =
  fprintf oc "%s\n" desc;
  list_print ~first:"est " ~last:"\n" ~sep:" " (fun r -> string_of_float r.mean.Bootstrap.point) oc resl;
  list_print ~first:"lo " ~last:"\n" ~sep:" " (fun r -> string_of_float r.mean.Bootstrap.lower) oc resl;
  list_print ~first:"hi " ~last:"\n" ~sep:" " (fun r -> string_of_float r.mean.Bootstrap.upper) oc resl

let print_2d fn (points,rs) =
  let oc = open_out fn in
  output_string oc "multiplot\n";
  list_print ~first:"x-values " ~last:"\n" ~sep:" " string_of_int oc points;
  List.iter (print_ranges oc) rs;
  close_out oc

let print_1d fn resl =
  let oc = open_out fn in
  output_string oc "comparison\n";
  List.iter (print_result oc) resl;
  close_out oc
