(***
    Chapter 1 of PLEAC rewritten for OCaml Batteries Included.

    Note: to obtain Unicode version replace every occurrence of [String]
    with [Rope] and prepend [u] before any string literal.
*)

(** {3 Strings}*)
(** {4 Introduction}*)

open String

(*---------------------------*)
let string = "\\n"                  (* two characters, \ and an n*)
let string = "Jon 'Maddog' Orwant"  (* literal single quotes*)
(*---------------------------*)
let string = "\n"                     (* a "newline" character *)
let string = "Jon \"Maddog\" Orwant"  (* literal double quotes *)

let a = "
    This is a multiline here document
    terminated by one  double quote
    "

(** {4 Accessing Substrings}*)

let value offset count = sub string offset count
let value offset count = sub string offset (length string - offset)
(* or *)
let value offset = right string offset


(*-----------------------------*)
(* get a 5-byte string, skip 3, then grab 2 8-byte strings, then the rest*)
let foo s = open Enum in let e = enum s in
  [? List : of_enum (f e) | f <- List : [take 5; skip 3 |- take 5; take 5 ; identity]]

(* split at 'sz' byte boundaries *)
let split_every_n_chars sz s = open Enum in
  let e = enum s in
  from_while (fun _ -> if is_empty e then None else Some ( String.of_enum (take sz e) ))

let fivers = split_every_n_chars 5 string

(* chop string into individual characters *)
let chars = explode string

(*-----------------------------*)
let string = "This is what you have";;
(* Indexes are left to right. There is no possibility to index *)
(* directly from right to left *)
(* "T" *)
let first  = slice string ~first:0 ~last:1
(* "is" *)
let start  = slice string ~last:4 ~first:2
(* "you have" *)
let rest   = tail string 13
(* "e" *)
let last   = right string 1
(* "have" *)
let theend = right string 4
(* "you" *)
let piece  = slice string ~first:(-8) ~last:(-5)
(*-----------------------------*)
let string = "This is what you have";;
print_endline string
(*This is what you have*)

(* Change "is" to "wasn't"*)
let string = splice string 5 2 "wasn't"
(*This wasn't what you have *)

(*This wasn't wonderous *)
let string = splice string (-11) max_int "ondrous";;

(* delete first character *)
let string = lchop string
(*his wasn't wondrous*)

(* delete last 10 characters *)
let string = String.sub string 0 (String.length string - 10);;
(*his wasn'*)
(*-----------------------------*)

(**{4 Testing substrings}*)

(*

(**{4 Establishing a Default Value}*)


(* Because OCaml doesn't have the same notion of truth or definedness as Perl,
 * most of these examples just can't be done as they are in Perl.  Some can be
 * approximated via the use of options, but remember, unbound variables are not
 * automatically assigned the value of None -- the variable has to have been
 * explicitly bound to None (or Some x) beforehand.
*)

(* use b if b is not None, else use c *)
let a = match b with None -> c | _ -> b;;

(* set x to y if x is currently None *)
let x = match x with None -> y | _ -> x;;

(* Note that these are much closer to Perls notion of definedness than truth *)

(* We can set foo to either bar or "DEFAULT VALUE" in one of two ways *)
(* keep foo as a string option *)
let foo = match bar with Some x -> bar | _ -> Some "DEFAULT VALUE";;

(* Use foo as a string *)
let foo = match bar with Some x -> x | _ -> "DEFAULT VALUE";;

let dir = if Array.length Sys.argv > 1 then argv.(1) else "/tmp";;

(* None of the other examples really make sense in OCaml terms... *)

Exchanging Values Without Using Temporary Variables

(*-----------------------------*)
let var1, var2 = var2, var1
(*-----------------------------*)
let temp    = a
let a       = b
let b       = temp
(*-----------------------------*)
let a       = "alpha"
let b       = "omega"
let a, b = b, a      (* the first shall be last -- and versa vice *)
(*-----------------------------*)
let alpha, beta, production = "January", "March", "August"
(* move beta       to alpha,
 * move production to beta,
 * move alpha      to production *)
let alpha, beta, production = beta, production, alpha
(*-----------------------------*)

Converting Between ASCII Characters and Values

(*-----------------------------*)
let num  = Char.code char
let char = Char.chr num
(*-----------------------------*)
(* char and int are distinct datatypes in OCaml *)
printf "Number %d is character %c\n" num (Char.chr num)
(* Number 101 is character e *)
(*-----------------------------*)
(* convert string to list of chars *)
let explode s =
  let rec f acc = function
    | -1 -> acc
    | k -> f (s.[k] :: acc) (k - 1)
  in f [] (String.length s - 1)

(* convert list of chars to string *)
let implode l =
  let s = String.create (List.length l) in
  let rec f n = function
    | x :: xs -> s.[n] <- x; f (n + 1) xs
    | [] -> s
  in f 0 l

(* ascii is list of ints. *)
let ascii = List.map Char.code (explode string)
let string = implode (List.map Char.ord ascii)
(*-----------------------------*)
let ascii_value = Char.code 'e'    (* now 101 *)
let character   = Char.chr 101     (* now 'e' *)
(*-----------------------------*)
printf "Number %d is character %c\n" 101 (Char.chr 101)
(*-----------------------------*)
let ascii_character_numbers = List.map Char.code (explode "sample");;
List.iter (printf "%d ") ascii_character_numbers;
printf "\n"
115 97 109 112 108 101

let word = implode (List.map Char.chr ascii_character_numbers)
let word = implode (List.map Char.chr [115; 97; 109; 112; 108; 101]);; (* same *)
printf "%s\n" word
sample
(*-----------------------------*)
let hal = "HAL"
let ascii = List.map Char.code (explode hal)
let ascii = List.map (( + ) 1) ascii  (* add one to each ASCII value *)
let ibm = implode (List.map Char.chr ascii);;
printf "%s\n" ibm             (* prints "IBM" *)
(*-----------------------------*)

Processing a String One Character at a Time


(* One can split a string into an array of character, or corresponding ASCII
 * codes as follows, but this is not necessary to process the strings a
 * character at a time: *)

let array_of_chars = Array.init (String.length s) (fun i -> s.[i]);;
let array_of_codes = Array.init (String.length s) (fun i -> Char.code s.[i]);;

(* or one can just use String.iter *)
String.iter
  (fun i -> (*do something with s.[i], the ith char of the string*)) s;;

(* The following function can be used to return a list of all unique keys in a
 * hashtable *)

let keys h =
  let k = Hashtbl.fold (fun k v b -> k::b) h [] in
  (* filter out duplicates *)
  List.fold_left (fun b x -> if List.mem x b then b else x::b) [] k;;

(* and this function is a shorthand for adding a key,value pair to a hashtable
*)

let ( <<+ ) h (k,v) = Hashtbl.add h k v;;

let seen = Hashtbl.create 13;;
let s = "an apple a day";;
let array_of_chars = Array.init (String.length s) (fun i -> s.[i]);;
Array.iter (fun x -> seen <<+ (x,1)) array_of_chars;
print_string "unique chars are:\t";
List.iter print_char (List.sort compare (keys seen));
print_newline ();;

(* or, without the unnecessary and innefficient step of converting the string
 * into an array of chars *)
let seen = Hashtbl.create 13;;
let s = "an apple a day";;
String.iter (fun x -> seen <<+ (x,1)) s;
print_string "unique chars are:\t";
List.iter print_char (List.sort compare (keys seen));
print_newline ();;

(* To compute the simple 31-bit checksum of a string *)
let cksum s =
  let sum = ref 0 in
  String.iter (fun x -> sum := !sum + (Char.code x)) s;
  !sum;;
(*
# cksum "an apple a day";;
- : int = 1248
*)

(* to emulate the SysV 16-bit checksum, we will first write two routines sort of
 * similar to Perl's (<>), that will return the contents of a file either as a
 * list of strings or as a single string - not that the list of strings version
 * throws away the \n at the end of each line *)

let slurp_to_list filename =
  let ic = open_in filename and
  l = ref [] in
  let rec loop () =
    let line = input_line ic in
    l := line::!l;
    loop () in
  try loop () with End_of_file -> close_in ic; List.rev !l;;

let slurp_to_string filename =
  let ic = open_in filename and
  buf = Buffer.create 4096 in
  let rec loop () =
    let line = input_line ic in
    Buffer.add_string buf line;
    Buffer.add_string buf "\n";
    loop () in
  try loop () with End_of_file -> close_in ic; Buffer.contents buf;;

let cksum16 fn =
  let addString sum s =
    let sm = ref sum in
    String.iter (fun c -> sm := !sm + (Char.code c)) (s ^ "\n");
    !sm mod 65537 (* 2^16 - 1 *)in
  List.fold_left addString 0 (slurp_to_list fn);;

(* or *)
let cksum16 fn =
  let sum = ref 0
  and s = slurp_to_string fn in
  String.iter (fun c -> sum := (!sum + (Char.code c)) mod 65537) s;
  !sum;;



(* Note: slowcat as written is meant to be run from the command line, not in the
 * toplevel *)

#!/usr/local/bin/ocaml
(* slowcat - emulate a   s l o w  line printer *)
(* usage: slowcat [-DELAY] [files ...] *)
#load "unix.cma";;

(* make sure you have the code for the slurp_to_string function in this file as
 * well... *)

let _ =
  let delay,fs = try (float_of_string Sys.argv.(1)),2 with Failure _ -> 1.,1 in
  let files = Array.sub Sys.argv fs (Array.length Sys.argv - fs) in
  let print_file f =
    let s = slurp_to_string f in
    String.iter
      (fun c ->
        print_char c;
        ignore(Unix.select [] [] [] (0.005 *. delay))) s in
  Array.iter print_file files;;

Reversing a String by Word or Character


(* To flip the characters of a string, we can use a for loop.
 * Note that this version does not destructively update the string *)

let reverse s =
  let len = String.length s - 1 in
  let s' = String.create (len + 1) in
  for i = 0 to len do
    s'.[i] <- s.[len - i]
  done;
  s';;

(* to modify the string in place, we can use the following function *)
let reverse_in_place s =
  let len = String.length s - 1 in
  for i = 0 to (len + 1)/ 2 - 1 do
    let t = s.[i] in
    s.[i] <- s.[len - i];
    s.[len - i] <- t
  done;;

(* To reverse the words in a string, we can use String.concat, Str.split and
 * List.rev.  Note that this requires us to load in the Str module --
 * use `#load "str.cma"' in* the toplevel, or be sure to include str.cma in the
 * list of object files when compiling your code.  E.g.:
 *      ocamlc other options str.cma other files   -or-
 *      ocamlopt other options str.cmxa other files
*)

let reverse_words s =
  String.concat " " (List.rev (Str.split (Str.regexp " ") s));;

let is_palindrome s =
  s = reverse s;;

(* We do need to do a bit more work that Perl to find the big palindromes in
 * /usr/share/dict/words ... *)

let findBigPals () =
  let words = open_in "/usr/share/dict/words" in
  let rec loop () =
    let w = input_line words in
    if String.length w > 5 && w = reverse w then
      print_endline w;
    loop () in
  try loop () with End_of_file -> close_in words;;

Expanding and Compressing Tabs


let expand_tabs ?(spaces = 8) s =
  Str.global_replace (Str.regexp "\t") (String.make spaces ' ') s;;

let compress_tabs ?(spaces = 8) s =
  Str.global_replace (Str.regexp (String.make spaces ' ')) "\t" s;;

(*
# let st = "\tyo baby!\n\t\tWhat the shizzle?\t(Mack)";;
val st : string = "\tyo baby!\n\t\tWhat the shizzle?\t(Mack)"
# let etst = expand_tabs st;;
val etst : string =
  "        yo baby!\n                What the shizzle?        (Mack)"
# let etst = expand_tabs ~spaces:4 st;;
val etst : string = "    yo baby!\n        What the shizzle?    (Mack)"
# let etst = expand_tabs ~spaces:8 st;;
val etst : string =
  "        yo baby!\n                What the shizzle?        (Mack)"
# let rest = compress_tabs etst;;
val rest : string = "\tyo baby!\n\t\tWhat the shizzle?\t(Mack)"
# let rest = compress_tabs ~spaces:4 etst;;
val rest : string = "\t\tyo baby!\n\t\t\t\tWhat the shizzle?\t\t(Mack)"
# let rest = compress_tabs ~spaces:3 etst;;
val rest : string =
  "\t\t  yo baby!\n\t\t\t\t\t What the shizzle?\t\t  (Mack)"
*)

Expanding Variables in User Input

(* As far as I know there is no way to do this in OCaml due to
   type-safety contraints built into the OCaml compiler -- it may be
   feasible with *much* juju, but don't expect to see this anytime
   soon...

   If you don't mind supplying a data structure rather than capturing
   local variables, you can use Buffer.add_substitute to get a similar
   effect. *)

let buffer = Buffer.create 16
let vars = [("debt", "$700 billion")]
let () =
  Buffer.add_substitute buffer
    (fun name -> List.assoc name vars)
    "You owe $debt to me.";
  print_endline (Buffer.contents buffer)

Controlling Case


(* Just use the String module's uppercase, lowercase, capitalize and
 * uncapitalize *)

let big = String.uppercase little;;    (* "bo peep" -> "BO PEEP" *)
let little = String.lowercase big;;    (* "JOHN" -> "john" *)
let big = String.capitalize little;;   (* "bo" -> "Bo" *)
let little = String.uncapitalize big;; (* "BoPeep" -> "boPeep" *)

(* Capitalize each word's first character, downcase the rest *)
let text = "thIS is a loNG liNE";;
let text = String.capitalize (String.lowercase text);;
print_endline text;;

(*
This is a long line
*)

(* To do case insensitive comparisons *)
if String.uppercase a = String.uppercase b then
  print_endline "a and b are the same\n";;

let randcap fn =
  let s = slurp_to_string fn in
  for i = 0 to String.length s - 1 do
    if Random.int 100 < 20 then
      String.blit (String.capitalize (String.sub s i 1)) 0 s  i 1
  done;
  print_string s;;


(*
# randcap "/etc/passwd";;

##
# User DatAbAse
#
# Note That this fIle is consuLTed wHen the sysTeM Is runninG In single-user
# modE.  At other times this iNformAtion is handlEd by one or moRe oF:
# lOokupD DIrectorYServicEs
# By default, lOOkupd getS inFormaTion frOm NetInFo, so thiS fIle will
# not be cOnsultEd unless you hAvE cHaNged LOokupd's COnfiguratiOn.
# This fiLe is usEd while in siNgle UseR Mode.
#
# TO Use this file for noRmal aUthEnticatIon, you may eNable it With
# /ApPlicatiOns/Utilities/DiRectory AccEss.
##

< ... snip ... >
*)

Interpolating Functions and Expressions Within Strings


(* Again, because of OCaml's type-safe nature, actual interpolation cannot be
 * done inside of strings -- one must use either string concatenation or sprintf
 * to get the results we're looking for *)

let phrase = "I have " ^ (string_of_int (n+1)) ^ " guanacos.";;
let prhase = sprintf "I have %d guanacos." (n+1);;

Indenting Here Documents

#load "str.cma";;
let var =  Str.global_replace (Str.regexp "^[\t ]+") "" "\
    your text
    goes here
";;

Reformatting Paragraphs


(* We can emulate the Perl wrap function with the following function *)
let wrap width s =
  let l = Str.split (Str.regexp " ") s in
  Format.pp_set_margin Format.str_formatter width;
  Format.pp_open_box Format.str_formatter 0;
  List.iter
    (fun x ->
      Format.pp_print_string Format.str_formatter x;
      Format.pp_print_break Format.str_formatter 1 0;) l;
  Format.flush_str_formatter ();;

(*
# let st = "May I say how lovely you are looking today... this wrapping has done wonders for your figure!\n";;
val st : string =
  "May I say how lovely you are looking today... this wrapping has done wonders for your figure!\n"

# print_string (wrap 50 st);;
May I say how lovely you are looking today...
this wrapping has done wonders for your figure!

# print_string (wrap 30 st);;
May I say how lovely you are
looking today... this
wrapping has done wonders for
your figure!
*)

(* Note that this version doesn't allow you to specify an opening or standard
 * indentation (I am having trouble getting the Format module to behave as I
 * think it should...).  However, if one only wants to print spaces there
 * instead of arbitrary line leaders, we can use the following version *)

let wrap ?(lead=0) ?(indent=0) width s =
  let l = Str.split (Str.regexp " ") s in
  Format.pp_set_margin Format.str_formatter width;
  Format.pp_open_box Format.str_formatter 0;
  Format.pp_print_break Format.str_formatter lead indent;
  List.iter
    (fun x ->
      Format.pp_print_string Format.str_formatter x;
      Format.pp_print_break Format.str_formatter 1 indent;) l;
  Format.flush_str_formatter ();;

(*
# print_string (wrap 20 st);;
May I say how
lovely you are
looking today...
this wrapping has
done wonders for
your figure!
 - : unit = ()

# print_string (wrap ~lead:6 ~indent:2 20 st);;
      May I say how
  lovely you are
  looking today...
  this wrapping has
  done wonders for
  your figure!

# print_string (wrap ~lead:2 20 st);;
  May I say how
lovely you are
looking today...
this wrapping has
done wonders for
your figure!
*)

Escaping Characters

(*
** The Str module is deistributed with the standard Ocaml compiler
** suit but it is not automatically pulled in by the command line
** interpreter or the compilers.
**
** The "#load" line is only needed if you are running this in the
** command interpretter.
**
** If you are using either of the ocaml compilers, you will need
** to remove the "#load" line and link in str.cmxa in the final
** compile command.
*)

#load "str.cma" ;;

open Str

let escape charlist str =
        let rx = Str.regexp ("\\([" ^ charlist ^ "]\\)") in
        Str.global_replace rx "\\\\\\1" str

let text = "Mom said, \"Don't do that.\"" ;;
print_endline text ;;

let text = escape "'\"" text ;;
print_endline text ;;

Trimming Blanks from the Ends of a String


let trim s =
  let s' = Str.replace_first (Str.regexp "^[ \t\n]+") "" s in
  Str.replace_first (Str.regexp "[ \t\n]+$") "" s';;

let chop s =
  if s = "" then s else String.sub s 0 (String.length s - 1);;

let chomp ?(c='\n') s =
  if s = "" then s else
    let len = String.length s - 1 in
    if s.[len] = c then String.sub s 0 len else s;;

Parsing Comma-Separated Data


let parse_csv =
  let regexp = Str.regexp (String.concat "\\|" [
                             "\"\\([^\"\\\\]*\\(\\\\.[^\"\\\\]*\\)*\\)\",?";
                             "\\([^,]+\\),?";
                             ",";
                           ]) in
  fun text ->
    let rec loop start result =
      if Str.string_match regexp text start then
        let result =
          (try Str.matched_group 1 text with Not_found ->
             try Str.matched_group 3 text with Not_found ->
               "") :: result in
        loop (Str.match_end ()) result
      else
        result in
    List.rev ((if
                 try String.rindex text ',' = String.length text - 1
                 with Not_found -> false
               then [""] else [])
              @ loop 0 [])

let line = "XYZZY,\"\",\"O'Reilly, Inc\",\"Wall, Larry\",\"a \\\"glug\\\" bit,\",5,\"Error, Core Dumped\""
let () =
  Array.iteri
    (fun i x -> Printf.printf "%d : %s\n" i x)
    (Array.of_list (parse_csv line))

Soundex Matching


let soundex =
  let code_1 = Char.code '1' in
  let code_A = Char.code 'A' in
  let code_Z = Char.code 'Z' in

  let trans = Array.make (code_Z - code_A + 1) 0 in
  let add_letters number letters =
    let add letter =
      trans.(Char.code letter - code_A) <- (number + code_1) in
    String.iter add letters in
  Array.iteri add_letters [| "BFPV"; "CGJKQSXZ"; "DT"; "L"; "MN"; "R" |];

  fun ?(length=4) s ->
    let slength = String.length s in
    let soundex = String.make length '0' in
    let rec loop i j last =
      if i < slength && j < length then begin
        let code = Char.code (Char.uppercase s.[i]) in
        if code >= code_A && code <= code_Z
        then (if j = 0
              then (soundex.[j] <- Char.chr code;
                    loop (i + 1) (j + 1) trans.(code - code_A))
              else (match trans.(code - code_A) with
                      | 0 -> loop (i + 1) j 0
                      | code when code <> last ->
                          soundex.[j] <- Char.chr code;
                          loop (i + 1) (j + 1) code
                      | _ -> loop (i + 1) j last))
        else loop (i + 1) j last
      end in
    loop 0 0 0;
    soundex

(*-----------------------------*)

let code = soundex string;;
let codes = List.map soundex list;;

(*-----------------------------*)

#load "str.cma"
#load "unix.cma"

let () =
  print_string "Lookup user: ";
  let user = read_line () in
  if user <> "" then begin
    let name_code = soundex user in
    let regexp = Str.regexp ("\\([a-zA-Z_0-9]+\\)[^,]*[^a-zA-Z_0-9]+"
                             ^ "\\([a-zA-Z_0-9]+\\)") in
    let passwd = open_in "/etc/passwd" in
    try
      while true do
        let line = input_line passwd in
        let name = String.sub line 0 (String.index line ':') in
        let {Unix.pw_gecos=gecos} = Unix.getpwnam name in
        let (firstname, lastname) =
          if Str.string_match regexp gecos 0
          then (Str.matched_group 1 gecos, Str.matched_group 2 gecos)
          else ("", "") in
        if (name_code = soundex name
            || name_code = soundex lastname
            || name_code = soundex firstname)
        then Printf.printf "%s: %s %s\n" name firstname lastname
      done
    with End_of_file ->
      close_in passwd
  end

Program: fixstyle

(* fixstyle - switch first set of data strings to second set *)
#load "str.cma";;

let data = Hashtbl.create 0
let keys = ref []
let () =
  let ( => ) key value =
    keys := key :: !keys;
    Hashtbl.replace data key value in
  (
    "analysed"       => "analyzed";
    "built-in"       => "builtin";
    "chastized"      => "chastised";
    "commandline"    => "command-line";
    "de-allocate"    => "deallocate";
    "dropin"         => "drop-in";
    "hardcode"       => "hard-code";
    "meta-data"      => "metadata";
    "multicharacter" => "multi-character";
    "multiway"       => "multi-way";
    "non-empty"      => "nonempty";
    "non-profit"     => "nonprofit";
    "non-trappable"  => "nontrappable";
    "pre-define"     => "predefine";
    "preextend"      => "pre-extend";
    "re-compiling"   => "recompiling";
    "reenter"        => "re-enter";
    "turnkey"        => "turn-key";
  )

let pattern_text =
  "\\(" ^ (String.concat "\\|" (List.map Str.quote !keys)) ^ "\\)"
let pattern = Str.regexp pattern_text

let args = ref (List.tl (Array.to_list Sys.argv))

let verbose =
  match !args with
    | "-v" :: rest -> args := rest; true
    | _ -> false

let () =
  if !args = []
  then (Printf.eprintf "%s: reading from stdin\n" Sys.argv.(0);
        args := ["-"])

let replace_all text line file =
  String.concat ""
    (List.map
       (function
          | Str.Text s -> s
          | Str.Delim s ->
              if verbose
              then Printf.eprintf "%s => %s at %s line %d.\n"
                s (Hashtbl.find data s) file line;
              Hashtbl.find data s)
       (Str.full_split pattern text))

let () =
  List.iter
    (fun file ->
       let in_channel =
         if file = "-"
         then stdin
         else open_in file in
       let line = ref 0 in
       try
         while true do
           let text = input_line in_channel in
           incr line;
           print_endline (replace_all text !line file)
         done
       with End_of_file ->
         close_in in_channel)
    !args

Program: psgrep

#!/usr/bin/ocaml
(* psgrep - print selected lines of ps output by
            compiling user queries into code *)
#load "unix.cma";;

(* Warning: In order to closely approximate the original recipe, this
   example performs dynamic evaluation using the toplevel. This mechanism
   is undocumented and not type-safe. Use at your own risk.

   The "psgrep" utility, defined below, can be used to filter the results
   of the command-line "ps" program. Here are some examples:

   Processes whose command names start with "sh":

   % psgrep 'String.sub command 0 2 = "sh"'

   Processes running with a user ID below 10:

   % psgrep 'uid < 10'

   Login shells with active ttys:

   % psgrep "command.[0] = '-'" 'tty <> "?"'

   Processes running on pseudo-ttys:

   % psgrep 'String.contains "pqrst" tty.[0]'

   Non-superuser processes running detached:

   % psgrep 'uid > 0 && tty = "?"'

   Huge processes that aren't owned by the superuser:

   % psgrep 'vsz > 50000' 'uid <> 0'
*)

(* Eval recipe thanks to Clément Capel. *)
let () = Toploop.initialize_toplevel_env ()
let eval text = let lexbuf = (Lexing.from_string text) in
  let phrase = !Toploop.parse_toplevel_phrase lexbuf in
  ignore (Toploop.execute_phrase false Format.std_formatter phrase)
let get name = Obj.obj (Toploop.getvalue name)
let set name value = Toploop.setvalue name (Obj.repr value)

(* Type for "ps" results. *)
type ps =
    {f : int; uid : int; pid : int; ppid : int; pri : int; ni : string;
     vsz : int; rss : int; wchan : string; stat : string; tty : string;
     time : string; command : string}

(* Based on the GNU ps from Debian Linux. Other OSs will most likely
   require changes to this format. *)
let parse_ps_line line =
  Scanf.sscanf line "%d %d %d %d %d %s %d %d %6s %4s %10s %4s %s@\000"
    (fun f uid pid ppid pri ni vsz rss wchan stat tty time command ->
       {f=f; uid=uid; pid=pid; ppid=ppid; pri=pri; ni=ni;
        vsz=vsz; rss=rss; wchan=wchan; stat=stat; tty=tty;
        time=time; command=command})

let eval_predicate ps pred =
  (* Use "eval" to initialize each variable's name and type,
     then use "set" to set a value. *)
  eval "let f = 0;;";          set "f" ps.f;
  eval "let uid = 0;;";        set "uid" ps.uid;
  eval "let pid = 0;;";        set "pid" ps.pid;
  eval "let ppid = 0;;";       set "ppid" ps.ppid;
  eval "let pri = 0;;";        set "pri" ps.pri;
  eval "let ni = \"\";;";      set "ni" ps.ni;
  eval "let vsz = 0;;";        set "vsz" ps.vsz;
  eval "let rss = 0;;";        set "rss" ps.rss;
  eval "let wchan = \"\";;";   set "wchan" ps.wchan;
  eval "let stat = \"\";;";    set "stat" ps.stat;
  eval "let tty = \"\";;";     set "tty" ps.tty;
  eval "let time = \"\";;";    set "time" ps.time;
  eval "let command = \"\";;"; set "command" ps.command;
  (* Evaluate expression and return result as boolean. *)
  eval ("let result = (" ^ pred ^ ");;");
  (get "result" : bool)

exception TypeError of string
exception SyntaxError of string

let preds = List.tl (Array.to_list Sys.argv)
let () =
  if preds = []
  then (Printf.eprintf "usage: %s criterion ...
    Each criterion is an OCaml expression involving:
     f uid pid ppid pri ni vsz rss wchan stat tty time command
    All criteria must be met for a line to be printed.
" Sys.argv.(0); exit 0)

let () =
  let proc = Unix.open_process_in "ps wwaxl" in
  try
    print_endline (input_line proc);
    while true do
      let line = input_line proc in
      let ps = parse_ps_line line in
      if List.for_all
        (fun pred ->
           try eval_predicate ps pred
           with e ->
             (* Convert exceptions to strings to avoid depending on
                additional toplevel libraries. *)
             match Printexc.to_string e with
               | "Typecore.Error(_, _)" -> raise (TypeError pred)
               | "Syntaxerr.Error(_)"
               | "Lexer.Error(1, _)"
               | "Lexer.Error(_, _)" -> raise (SyntaxError pred)
               | "Misc.Fatal_error" -> failwith pred
               | _ -> raise e)
        preds
      then print_endline line
    done
  with
    | End_of_file ->
        ignore (Unix.close_process_in proc)
    | e ->
        ignore (Unix.close_process_in proc);
        raise e

*)
