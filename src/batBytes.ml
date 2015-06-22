# 1 "src/batBytes.mlv"
include Bytes

(*$T init
   init 5 (fun i -> Char.chr (i + int_of_char '0')) |> to_string = "01234";
*)

(*$T mapi
   mapi (fun _ -> Char.uppercase) (of_string "Five") |> to_string = "FIVE"
   mapi (fun _ -> Char.uppercase) (of_string "") |> to_string = ""
   mapi (fun _ -> String.of_char %> failwith) (of_string "") |> to_string = ""
   mapi (fun i _c -> "0123456789".[9-i]) (of_string "0123456789") |> to_string = "9876543210"
   ignore (let last = ref (-1) in mapi (fun i _c -> assert (i > !last); last := i; '0') (of_string "012345")); true
*)

(* String.trim is @since 4.00 *)
# 17 "src/batBytes.mlv"

(*$T trim
   " \t foo\n  " |> of_string |> trim |> to_string |> (=) "foo"
   " foo bar " |> of_string |> trim |> to_string |> (=) "foo bar"
   "  \t " |> of_string |> trim |> to_string |> (=) ""
   "" |> of_string |> trim |> to_string |> (=) ""
*)

(* String.map is @since 4.00 *)
# 33 "src/batBytes.mlv"

(*$T map
   "Five" |> of_string |> map Char.uppercase |> to_string |> (=) "FIVE"
   "" |> of_string |> map Char.uppercase |> to_string |> (=) ""
   "" |> of_string |> map (String.of_char %> failwith) |> to_string |> (=) ""
*)

(* String.iteri is @since 4.00 *)
