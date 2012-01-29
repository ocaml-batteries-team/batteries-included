open OUnit;;

let ps,pl = print_string,print_endline
let va = Printf.sprintf
let pf = Printf.printf

let group cmp lst =
  let sorted = List.sort cmp lst in
  let fold first rest = List.fold_left
    (fun (acc, agr, last) elem ->
       if (cmp last elem) = 0 then (acc, elem::agr, elem)
        else (agr::acc, [elem], elem)
    )
    ([], [first], first)
    rest
   in
  match sorted with
   | [] -> []
   | hd::tl ->
   begin
    let groups, lastgr, _ = fold hd tl in
    List.rev_map List.rev (lastgr::groups)
   end



let run test =
  let _counter = ref (0,0,0) in (* Success, Failure, Other *)
  let total_tests = test_case_count test in
  let update = function
    | RSuccess _ -> let (s,f,o) = !_counter in _counter := (succ s,f,o)
    | RFailure _ -> let (s,f,o) = !_counter in _counter := (s,succ f,o)
    | _ -> let (s,f,o) = !_counter in _counter := (s,f, succ o)
  in
  let hdl_event = function
  | EStart p -> ps "\r";
    let (s,f,o) = !_counter in
    let cartouche = va " [%d+%d%s /%d] " s f (if o=0 then "" else va " %d" o) total_tests
    and path = string_of_path p in
    let line = cartouche ^ path in
    let remaining = 79 - String.length line in
    let cover = if remaining > 0 then String.make remaining ' ' else "" in
    pf "%s%s%!" line cover;
    Unix.sleep 1
  | EEnd p -> ()
  | EResult result -> update result
  in
  ps "Running tests...";
  let results = perform_test hdl_event test in
  ignore results