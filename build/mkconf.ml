(* Program for substituting configuration data in various files.
 * Copyright (C) 2010 Michael Ekstrand
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:

 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.

 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 *)

let strip ?(chars=" \t\r\n") s =
	let p = ref 0 in
	let l = String.length s in
	while !p < l && String.contains chars (String.unsafe_get s !p) do
		incr p;
	done;
	let p = !p in
	let l = ref (l - 1) in
	while !l >= p && String.contains chars (String.unsafe_get s !l) do
		decr l;
	done;
	String.sub s p (!l - p + 1)

let version = (* get version string from _oasis *)
  try
    let chan = open_in (Filename.concat Filename.parent_dir_name "_oasis") in
    let v = ref (input_line chan) in
    while String.sub !v 0 8 <> "Version:" do v := input_line chan done;
    let _ = close_in chan in
    strip (String.sub !v 8 (String.length !v - 8))
  with x ->
    prerr_endline (Printexc.to_string x);
    exit 2
;;

let param_re = Str.regexp "@[A-Z0-9_]*@";;

let repl = function
    "@@" -> "@"
  | "@VERSION@" -> version
  | s ->
      let name = String.sub s 1 (String.length s - 2) in
        try Sys.getenv name
        with Not_found -> s
;;

let rec replace ?(pos=0) re f s =
  let p =
    try Str.search_forward re s pos
    with Not_found -> -1
  in
    if p < 0 then s
    else
      let param = Str.matched_string s in
      let e = Str.match_end () in
      let rep = f param in
        (* build new string *)
      let s' = Str.string_before s p ^ rep ^ Str.string_after s e in
        (* adjust position based on change in length *)
      let pos = e - String.length param + String.length rep in
        replace ~pos re f s'
;;

let rec loop_file inch outch =
  let line =
    try Some (input_line inch)
    with End_of_file -> None
  in
    match line with
        Some l ->
          output_string outch (replace param_re repl l);
          output_char outch '\n';
          loop_file inch outch
      | None -> ()
;;

if Array.length Sys.argv < 3 then begin
  prerr_endline "Not enough arguments";
  exit 2
end else begin
  let inch = open_in Sys.argv.(1) in
  let outch = open_out Sys.argv.(2) in
    loop_file inch outch;
    close_in inch;
    close_out outch
end;;
