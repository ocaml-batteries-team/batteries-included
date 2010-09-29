open Batteries
let d n = Mathlib.sum_factors n

module ISet = Set.Make(Int)

let ret_amicable ~upto =
  let is_amic = ref ISet.empty
  and not_amic = ref ISet.empty
  and to_test = ref ((2--upto) |> ISet.of_enum)
  in
  let rec test n =  (* cleanup - ugly code *)
    if n >= upto then ()
    else if   ISet.mem n !is_amic
      || ISet.mem n !not_amic then
	()
    else
      let dn = d n in
      if dn >= upto || dn = n then
	not_amic := !not_amic |> ISet.add n
      else
	let ddn = d dn in
	if n = ddn then
	  is_amic := !is_amic |> ISet.add n |> ISet.add dn
	else
	  not_amic := !not_amic |> ISet.add n
  in
  while not (ISet.is_empty !to_test) do
    let n = ISet.choose !to_test in
    to_test := !to_test |> ISet.remove n;
    test n
  done;
  ISet.enum !is_amic

let print_int_enum e = Enum.print (fun stdout n -> IO.nwrite stdout (string_of_int n)) stdout e

let () =
  ret_amicable ~upto:10_000 |> Enum.reduce (+) |> print_int;
  print_newline();;
