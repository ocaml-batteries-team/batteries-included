include Bytes

(*$T init
   init 5 (fun i -> Char.chr (i + int_of_char '0')) |> to_string = "01234";
*)

(*$T mapi
   mapi (fun _ -> Char.uppercase_ascii) (of_string "Five") |> to_string = "FIVE"
   mapi (fun _ -> Char.uppercase_ascii) (of_string "") |> to_string = ""
   mapi (fun _ -> String.of_char %> failwith) (of_string "") |> to_string = ""
   mapi (fun i _c -> "0123456789".[9-i]) (of_string "0123456789") |> to_string = "9876543210"
   ignore (let last = ref (-1) in mapi (fun i _c -> assert (i > !last); last := i; '0') (of_string "012345")); true
*)

(* String.trim is @since 4.00 *)
##V<4.0##let trim b = Bytes.of_string (BatString.trim b)

(*$T trim
   " \t foo\n  " |> of_string |> trim |> to_string |> (=) "foo"
   " foo bar " |> of_string |> trim |> to_string |> (=) "foo bar"
   "  \t " |> of_string |> trim |> to_string |> (=) ""
   "" |> of_string |> trim |> to_string |> (=) ""
*)

(* String.map is @since 4.00 *)
##V<4.0##let map f s =
##V<4.0##  let len = length s in
##V<4.0##  let sc = create len in
##V<4.0##  for i = 0 to len - 1 do
##V<4.0##    unsafe_set sc i (f (unsafe_get s i))
##V<4.0##  done;
##V<4.0##  sc

(*$T map
   "Five" |> of_string |> map Char.uppercase_ascii |> to_string |> (=) "FIVE"
   "" |> of_string |> map Char.uppercase_ascii |> to_string |> (=) ""
   "" |> of_string |> map (String.of_char %> failwith) |> to_string |> (=) ""
*)

(* String.iteri is @since 4.00 *)
##V<4.0##let iteri f s =
##V<4.0##  for i = 0 to (Bytes.length s) - 1 do f i (Bytes.unsafe_get s i) done

##V<4.3##let equal b1 (b2 : Bytes.t) = (compare b1 b2 = 0)

##V<4.3##let uppercase_ascii s = map BatChar.uppercase_ascii s
##V<4.3##let lowercase_ascii s = map BatChar.lowercase_ascii s

(*$T uppercase_ascii
  String.equal ("five" |> of_string |> uppercase_ascii |> to_string) "FIVE"
  String.equal ("école" |> of_string |> uppercase_ascii |> to_string) "éCOLE"
 *)

(*$T lowercase_ascii
  String.equal ("FIVE" |> of_string |> lowercase_ascii |> to_string) "five"
  String.equal ("ÉCOLE" |> of_string |> lowercase_ascii |> to_string) "École"
 *)

##V<4.3##let map_first_char f s =
##V<4.3##  let r = copy s in
##V<4.3##  if length s > 0 then
##V<4.3##    unsafe_set r 0 (f(unsafe_get s 0));
##V<4.3##  r

##V<4.3##let capitalize_ascii s = map_first_char BatChar.uppercase_ascii s
##V<4.3##let uncapitalize_ascii s = map_first_char BatChar.lowercase_ascii s

(*$T capitalize_ascii
  String.equal ("five" |> of_string |> capitalize_ascii |> to_string) "Five"
  String.equal ("école" |> of_string |> capitalize_ascii |> to_string) "école"
 *)

(*$T uncapitalize_ascii
  String.equal ("Five" |> of_string |> uncapitalize_ascii |> to_string) "five"
  String.equal ("École" |> of_string |> uncapitalize_ascii |> to_string) "École"
 *)


##V<4.5##let index_opt b c = try Some (index b c) with _ -> None
##V<4.5##let rindex_opt b c = try Some (rindex b c) with _ -> None
##V<4.5##let index_from_opt b i c = try Some (index_from b i c) with _ -> None
##V<4.5##let rindex_from_opt b i c = try Some (rindex_from b i c) with _ -> None

##V>=4.07##let to_seq = to_seq
##V>=4.07##let to_seqi = to_seqi
##V>=4.07##let of_seq = of_seq

##V>=4.08##let get_uint8 = get_uint8
##V>=4.08##let get_int8 = get_int8
##V>=4.08##let get_uint16_ne = get_uint16_ne
##V>=4.08##let get_uint16_be = get_uint16_be
##V>=4.08##let get_uint16_le = get_uint16_le
##V>=4.08##let get_int16_ne = get_int16_ne
##V>=4.08##let get_int16_be = get_int16_be
##V>=4.08##let get_int16_le = get_int16_le
##V>=4.08##let get_int32_ne = get_int32_ne
##V>=4.08##let get_int32_be = get_int32_be
##V>=4.08##let get_int32_le = get_int32_le
##V>=4.08##let get_int64_ne = get_int64_ne
##V>=4.08##let get_int64_be = get_int64_be
##V>=4.08##let get_int64_le = get_int64_le
##V>=4.08##let set_uint8 = set_uint8
##V>=4.08##let set_int8 = set_int8
##V>=4.08##let set_uint16_ne = set_uint16_ne
##V>=4.08##let set_uint16_be = set_uint16_be
##V>=4.08##let set_uint16_le = set_uint16_le
##V>=4.08##let set_int16_ne = set_int16_ne
##V>=4.08##let set_int16_be = set_int16_be
##V>=4.08##let set_int16_le = set_int16_le
##V>=4.08##let set_int32_ne = set_int32_ne
##V>=4.08##let set_int32_be = set_int32_be
##V>=4.08##let set_int32_le = set_int32_le
##V>=4.08##let set_int64_ne = set_int64_ne
##V>=4.08##let set_int64_be = set_int64_be
##V>=4.08##let set_int64_le = set_int64_le

let fold_left f x a =
  let r = ref x in
  let n = length a in
  for i = 0 to n - 1 do
    r := f !r (unsafe_get a i)
  done;
  !r

let fold_right f a x =
  let r = ref x in
  let n = length a in
  for i = n - 1 downto 0 do
    r := f (unsafe_get a i) !r
  done;
  !r

let for_all p s =
  let n = length s in
  let rec loop i =
    if i = n then true
    else if p (unsafe_get s i) then loop (succ i)
    else false in
  loop 0

(*$T for_all
    for_all (fun c -> c <> '0') (of_string "123456789")
    false = for_all (fun c -> c <> '9') (of_string "123456789")
*)

let exists p s =
  let n = length s in
  let rec loop i =
    if i = n then false
    else if p (unsafe_get s i) then true
    else loop (succ i) in
  loop 0

(*$T exists
    exists (fun c -> c = '0') (of_string "1234567890")
*)

let starts_with ~prefix s =
  let len_s = length s
  and len_pre = length prefix in
  let rec aux i =
    if i = len_pre then true
    else if unsafe_get s i <> unsafe_get prefix i then false
    else aux (i + 1)
  in len_s >= len_pre && aux 0

(*$T starts_with
    starts_with ~prefix:(of_string "toto") (of_string "tototititata")
*)

let ends_with ~suffix s =
  let len_s = length s
  and len_suf = length suffix in
  let diff = len_s - len_suf in
  let rec aux i =
    if i = len_suf then true
    else if unsafe_get s (diff + i) <> unsafe_get suffix i then false
    else aux (i + 1)
  in diff >= 0 && aux 0

(*$T ends_with
    ends_with ~suffix:(of_string "tata") (of_string "tototititata")
*)

let split_on_char sep s =
  let r = ref [] in
  let n = length s in
  let j = ref n in
  for i = n - 1 downto 0 do
    if unsafe_get s i = sep then begin
      r := sub s (i + 1) (!j - i - 1) :: !r;
      j := i
    end
  done;
  sub s 0 !j :: !r

(*$T split_on_char
    split_on_char ';' (of_string "toto;titi;tata") = \
   [of_string "toto"; of_string "titi"; of_string "tata"]
*)
