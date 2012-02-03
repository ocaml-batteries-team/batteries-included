open OUnit

module BS = BatBitSet

let bitset_of_iarray arr =
  let t = BS.create (Array.fold_right max arr 0) in
    Array.iter
      (BS.set t)
      arr;
    t


let test_of_array arr =
  let t = BS.create (Array.length arr) in
  let () =
    (* Create the table *)
    Array.iteri
      (fun idx vl ->
         try
           if vl then
             BS.set t idx
         with e ->
           assert_failure
             (Printf.sprintf
                "while setting bitset.(%d) got this exception: %s"
                idx (Printexc.to_string e)))
      arr
  in
  let () =
    (* Check the table *)
    Array.iteri
      (fun idx vl ->
         let res =
           try
             BS.mem t idx
           with e ->
             assert_failure
               (Printf.sprintf
                  "while getting bitset.(%d) got this exception: %s"
                  idx (Printexc.to_string e))
         in
           assert_equal
             ~msg:(Printf.sprintf "at idx %d" idx)
             ~printer:string_of_bool
             vl res)
      arr
  in
    ()

let assert_mem t lst =
  List.iter
    (fun (i, b) ->
       assert_equal
         ~msg:(Printf.sprintf "at idx %d" i)
         ~printer:string_of_bool
         b
         (BS.mem t i))
    lst

let arr1 = [|1; 4; 25; 27|]
let arr2 = [|1; 5; 26|]

let biop op lst () = 
  let t1 = bitset_of_iarray arr1 in 
  let t2 = bitset_of_iarray arr2 in 
  let tr = op t1 t2 in
    assert_mem tr (List.map (fun i -> i, true) lst);
    assert_equal
      ~msg:"number of element"
      ~printer:string_of_int
      (List.length lst)
      (BS.count tr)

let tests = "BitSet" >::: [
  "Check small array" >::
  (fun () ->
     test_of_array [|true; false; false; true; false; true;
                     true; false; false; false; true|]);

  "Check intermediate array" >::
  (fun () ->
     test_of_array
       (Array.init 100 (fun _ -> Random.bool ())));

  "Check huge array" >::
  (fun () ->
     test_of_array
       (Array.init 1000 (fun _ -> Random.bool ())));

  "empty" >::
  (fun () ->
     let t = BS.empty () in
       assert_mem t
         [1, false;
          2, false;
          100, false]);

  "create" >::
  (fun () ->
     let t = BS.create 10 in
       assert_mem t
         [1, false;
          2, false;
          9, false;
          10, false;
          100, false]);

  "full" >::
  (fun () ->
     let t = BS.create_full 10 in
       assert_mem t
         [0, true;
          1, true;
          2, true;
          9, true;
          10, false;
          100, false];
       (* TODO: this is not the case, eventhough mentioned into doc. *)
       assert_equal
         ~msg:"count"
         ~printer:string_of_int
         10
         (BS.count t));

  "copy" >::
  (fun () ->
     let t = bitset_of_iarray arr1 in
     let t' = BS.copy t in
       assert_bool 
         "Copy should be equals"
         (BS.equals t t'));

  "union" >::
   (biop BS.union [1; 4; 5; 26; 26; 27]);

  "diff" >::
  (biop BS.diff [4; 25; 27]);

  "sym_diff" >::
  (biop BS.sym_diff [4; 25; 27; 5; 26]);

  "inter" >::
  (biop BS.inter [1]);
]


