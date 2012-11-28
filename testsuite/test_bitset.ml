open OUnit

module BS = BatBitSet

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

let lst1 = [1; 4; 25; 27]
let lst2 = [1; 5; 26; 250]

let biop op ?(rev=false) lst () = 
  let t1 = BS.of_list lst1 in 
  let t2 = BS.of_list lst2 in 
  let t1, t2 = if rev then t2, t1 else t1, t2 in
  let tr = op t1 t2 in
    assert_mem tr (List.map (fun i -> i, true) lst);
    assert_equal
      ~msg:"number of element"
      ~printer:string_of_int
      (List.length lst)
      (BS.count tr)

module EInt = 
struct
  type t = int
  let compare = ( - )
  let pp_printer = Format.pp_print_int
  let pp_print_sep = OUnitDiff.pp_comma_separator
end

module ListInt = 
struct 
  include OUnitDiff.SetMake(EInt)

  let assert_equal ?msg lst1 lst2 = 
    assert_equal ?msg (of_list lst1) (of_list lst2)
end

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
     assert_equal
       ~msg:"count"
       ~printer:string_of_int
       10
       (BS.count t));

  "copy" >::
  (fun () ->
     let t = BS.of_list lst1 in
     let t' = BS.copy t in
     assert_bool 
       "Copy should be equals"
       (BS.equal t t'));

  "union" >::
   (biop BS.union [1; 4; 5; 25; 26; 27; 250]);

  "union2" >::
   (biop BS.union ~rev:true [1; 4; 5; 25; 26; 27; 250]);

  "diff1" >::
  (biop BS.diff [4; 25; 27]);

  "diff2" >::
  (biop BS.diff ~rev:true [5; 26; 250]);

  "sym_diff" >::
  (biop BS.sym_diff [4; 25; 27; 5; 26; 250]);

  "sym_diff2" >::
  (biop BS.sym_diff ~rev:true [4; 25; 27; 5; 26; 250]);

  "inter" >::
  (biop BS.inter [1]);

  "next_set_bit" >::
  (fun () ->
     let bs = BS.of_list lst1 in
     let string_of_int_opt = 
       function 
         | Some i -> string_of_int i
         | None -> "<none>"
     in
     let last = 
       List.fold_left 
         (fun prv cur ->
            assert_equal 
              ~printer:string_of_int_opt
              (Some cur)
              (BS.next_set_bit bs (prv + 1));
            cur)
         (-1)
         lst1
     in
       assert_equal
         ~printer:string_of_int_opt
         None
         (BS.next_set_bit bs (last + 1)));

  "enum" >::
  (fun () ->
     let t1 = BS.of_list lst1 in
     let t2 = BS.of_list lst2 in
     ListInt.assert_equal
       lst1
       (BatList.of_enum (BS.enum t1));
     ListInt.assert_equal
       lst2
       (BatList.of_enum (BS.enum t2)));

  "toggle" >::
  (fun () ->
     let t = BS.empty () in
     BS.toggle t 10;
     assert_bool "idx 10 is set" (BS.mem t 10);
     BS.toggle t 10;
     assert_bool "idx 10 is not set" (not (BS.mem t 10)));

  "compare" >::
  (fun () ->
     let t1 = BS.of_list lst1 in
     let t2 = BS.of_list lst2 in
     assert_bool "lst1 < lst2" (BS.compare t1 t2 < 0);
     assert_bool "lst2 > lst1" (BS.compare t2 t1 > 0);
     assert_bool "lst1 = lst1" (BS.compare t1 t1 = 0);
     assert_bool "lst2 = lst2" (BS.compare t2 t2 = 0))
]


