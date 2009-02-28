open Rope

let (^^^) = append

let test_strings num =
  let x = ref ""
  and s = "a"
  in for i = 1 to num do
      x := !x ^ s
    done

let test_ropes num =
  let x = ref (r"")
  and s = r"a"
  in for i = 1 to num do
      x := !x ^^^ s
    done

let delta f x =
  let t0 = Sys.time () in
  let _  = f x         in
  let t1 = Sys.time () in
    t1 -. t0

let _ = 
  Printf.printf "Strings: %fms\n" (delta (fun () ->
	for i = 1 to 10 do
	  test_strings 10000
	done
     ) ());
  Printf.printf "Ropes: %fms\n" (delta (fun () ->
	for i = 1 to 10 do
	  test_ropes 1000000
	done
     ) ())


