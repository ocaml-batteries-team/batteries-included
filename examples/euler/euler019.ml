open Batteries

let daysmonth = [ 31; 28; 31; 30; 31; 30; 31; 31; 30; 31; 30; 31 ]
let daysleap = [ 31; 29; 31; 30; 31; 30; 31; 31; 30; 31; 30; 31 ]

let year_shift_f days = (List.reduce (+) days) mod 7

let year_shift     = year_shift_f daysmonth (* 1 *)
let year_shiftleap = year_shift_f daysleap  (* 2 *)

let count_shift days =
  let ret = Array.make 7 0 in
  let rec loop day = function
      h :: [] ->
	let sh = 6 - day in
	ret.(sh) <- ret.(sh) + 1 (* and done *)
    | h :: t  ->
	let sh = 6 - day in
	ret.(sh) <- ret.(sh) + 1;
	loop ((day + h) mod 7) t
  in
  loop 0 days;
  ret

let count_year = count_shift daysmonth
let count_leap = count_shift daysleap
(* val count_year : int array = [|2; 2; 1; 3; 1; 1; 2|] *)
(* val count_leap : int array = [|2; 1; 2; 2; 1; 1; 3|] *)

let is_leap yr =
  if yr mod 4 <> 0 then false
  else if yr mod 100 <> 0 then true
  else if yr mod 400 <> 0 then false
  else true

let rec count_sun (count, yr, endyr, dayone) =
  if yr >= endyr then count
  else
    let add, shift =
      if is_leap yr
      then count_leap.(dayone), year_shiftleap
      else count_year.(dayone), year_shift
    in
    count_sun ((count+add), (yr+1), endyr, ((dayone + shift) mod 7))

let () =
  let end_yr = 2001
  and start_yr = 1901
  and dayone = 1 (* monday *)
  in
  let count = count_sun (0, start_yr, end_yr, dayone) in
  print_int count; print_newline();;
