(* 
 * BatInnerIO - Abstract input/output (inner module)
 * Copyright (C) 2003 Nicolas Cannasse
 *               2008 Philippe Strauss
 *               2008 David Teller
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version,
 * with the special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)


type 'a weak_set = ('a, unit) BatInnerWeaktbl.t
let weak_create size     = BatInnerWeaktbl.create size
let weak_add set element = BatInnerWeaktbl.add set element ()
let weak_iter f s        = BatInnerWeaktbl.iter (fun x _ -> f x) s

type input = {
  mutable in_read  : unit -> char;
  mutable in_input : string -> int -> int -> int;
  mutable in_close : unit -> unit;
  in_id: int;(**A unique identifier.*)
  in_upstream: input weak_set
}

type 'a output = {
  mutable out_write : char -> unit;
  mutable out_output: string -> int -> int -> int;
  mutable out_close : unit -> 'a;
  mutable out_flush : unit -> unit;
  out_id:    int;(**A unique identifier.*)
  out_upstream:unit output weak_set
    (**The set of outputs which have been created to write to this output.*)
}


module Input =
struct
  type t = input
  let compare x y = x.in_id - y.in_id
  let hash    x   = x.in_id
  let equal   x y = x.in_id = y.in_id
end

module Output =
struct
  type t = unit output
  let compare x y = x.out_id - y.out_id
  let hash    x   = x.out_id
  let equal   x y = x.out_id = y.out_id
end



(**All the currently opened outputs -- used to permit [flush_all] and [close_all].*)
(*module Inputs = Weaktbl.Make(Input)*)
module Outputs= Weak.Make(Output)



(** {6 Primitive operations}*)

external noop        : unit      -> unit        = "%ignore"
external cast_output : 'a output -> unit output = "%identity"
let lock = ref BatConcurrent.nolock


let outputs = Outputs.create 32
let outputs_add out =
  BatConcurrent.sync !lock (Outputs.add outputs) out

let outputs_remove out =
  BatConcurrent.sync !lock (Outputs.remove outputs) out


exception No_more_input
exception Input_closed
exception Output_closed




let post_incr r =
  let result = !r in
    incr r;
    result
let post r op =
  let result = !r in
    r := op !r;
    result

let uid = ref 0
let uid () = post_incr uid

let on_close_out out f =
  BatConcurrent.sync !lock (fun () -> 
			   let do_close = out.out_close in
			     out.out_close <- (fun () -> f out; do_close ())) ()

let on_close_in inp f =
  BatConcurrent.sync !lock (fun () -> 
			   let do_close = inp.in_close in
			     inp.in_close <- (fun () -> f inp; do_close ())) ()

let close_in i =
  let f _ = raise Input_closed in
    i.in_close();
    i.in_read <- f;
    i.in_input <- f;
    i.in_close <- noop (*Double closing is not a problem*)


let wrap_in ~read ~input ~close ~underlying =
  let result = 
  {
    in_read     = read;
    in_input    = input;
    in_close    = close;
    in_id       = uid ();
    in_upstream = weak_create 2
  }
in 
    BatConcurrent.sync !lock (List.iter (fun x -> weak_add x.in_upstream result)) underlying;
    Gc.finalise close_in result;
    result

let inherit_in ?read ?input ?close inp =
  let read  = match read  with None -> inp.in_read | Some f -> f
  and input = match input with None -> inp.in_input| Some f -> f
  and close = match close with None -> ignore      | Some f -> f
  in  wrap_in ~read ~input ~close ~underlying:[inp]


let create_in ~read ~input ~close =
  wrap_in ~read ~input ~close ~underlying:[]

(*For recursively closing outputs, we need either polymorphic
  recursion or a hack. Well, a hack it is.*)

(*Close a [unit output] -- note that this works for any kind of output,
  thanks to [cast_output], but this can't return a proper result.*)
let rec close_unit (o:unit output) : unit =
  let forbidden _ = raise Output_closed in
    o.out_flush ();
    weak_iter close_unit o.out_upstream;
    let r = o.out_close() in
      o.out_write  <- forbidden;
      o.out_output <- forbidden;
      o.out_close  <- (fun _ -> r) (*Closing again is not a problem*);
      o.out_flush  <- noop         (*Flushing again is not a problem*);
      ()

(*Close a ['a output] -- first close it as a [unit output] then
  recover the result.*)
let close_out o =
(*  Printf.eprintf "close_out\n%!";*)
  close_unit (cast_output o);
  o.out_close ()


let ignore_close_out out = ignore (close_out out)

let wrap_out ~write ~output ~flush ~close ~underlying  =
  let rec out = 
    {
      out_write  = write;
      out_output = output;
      out_close  = (fun () ->
		      outputs_remove (cast_output out);
		      close ());
      out_flush  = flush;
      out_id     = uid ();
      out_upstream = weak_create 2
    }
  in 
  let o = cast_output out in
    BatConcurrent.sync !lock (List.iter (fun x -> weak_add x.out_upstream o)) underlying;
    outputs_add (cast_output out); 
    Gc.finalise ignore_close_out out;
    out

let inherit_out ?write ?output ?flush ?close out =
  let write = match write  with None -> out.out_write | Some f -> f
  and output= match output with None -> out.out_output| Some f -> f
  and flush = match flush  with None -> out.out_flush | Some f -> f
  and close = match close  with None -> ignore        | Some f -> f
  in wrap_out ~write ~output ~flush ~close ~underlying:[out]

let create_out ~write ~output ~flush ~close =
  wrap_out ~write ~output ~flush ~close ~underlying:[]

let read i = i.in_read()

let nread i n =
	if n < 0 then invalid_arg "BatIO.nread";
	if n = 0 then
		""
	else
	let s = String.create n in
	let l = ref n in
	let p = ref 0 in
	try
		while !l > 0 do
			let r = i.in_input s !p !l in
			if r = 0 then raise No_more_input;
			p := !p + r;
			l := !l - r;
		done;
		s
	with
		No_more_input as e ->
			if !p = 0 then raise e;
			String.sub s 0 !p

let really_output o s p l' =
	let sl = String.length s in
	if p + l' > sl || p < 0 || l' < 0 then invalid_arg "BatIO.really_output";
   	let l = ref l' in
	let p = ref p in
	while !l > 0 do 
		let w = o.out_output s !p !l in
		if w = 0 then raise Sys_blocked_io;
		p := !p + w;
		l := !l - w;
	done;
	l'

let input i s p l =
	let sl = String.length s in
	if p + l > sl || p < 0 || l < 0 then invalid_arg "BatIO.input";
	if l = 0 then
		0
	else
		i.in_input s p l

let really_input i s p l' =
	let sl = String.length s in
	if p + l' > sl || p < 0 || l' < 0 then invalid_arg "BatIO.really_input";
	let l = ref l' in
	let p = ref p in
	while !l > 0 do
		let r = i.in_input s !p !l in
		if r = 0 then raise Sys_blocked_io;
		p := !p + r;
		l := !l - r;
	done;
	l'

let really_nread i n =
	if n < 0 then invalid_arg "BatIO.really_nread";
	if n = 0 then ""
	else
	let s = String.create n 
	in
	ignore(really_input i s 0 n);
	s


let write o x = o.out_write x

let nwrite o s =
	let p = ref 0 in
	let l = ref (String.length s) in
	while !l > 0 do
		let w = o.out_output s !p !l in
		if w = 0 then raise Sys_blocked_io;
		p := !p + w;
		l := !l - w;
	done

let write_buf o b = nwrite o (Buffer.contents b)

let output o s p l =
	let sl = String.length s in
	if p + l > sl || p < 0 || l < 0 then invalid_arg "BatIO.output";
	o.out_output s p l

let flush o = o.out_flush()

let flush_all () =
  BatConcurrent.sync !lock ( Outputs.iter (fun o -> try flush o with _ -> ())) outputs

let close_all () =
  let outs =
    BatConcurrent.sync !lock (Outputs.fold (fun o os -> o :: os) outputs) []
  in
    List.iter (fun o -> try close_out o with _ -> ()) outs

let read_all i =
  let maxlen = 1024 in
  let str    = ref [] in
  let pos    = ref 0 in
  let rec loop() =
    let s = nread i maxlen in
      str := (s,!pos) :: !str;
      pos := !pos + String.length s;
      loop()
  in
    try
      loop()
    with
	No_more_input
      | Input_closed ->
	  let buf = String.create !pos in
	    List.iter (fun (s,p) ->
			 String.unsafe_blit s 0 buf p (String.length s)
		      ) !str;
	    buf

let input_string s =
  let pos = ref 0 in
  let len = String.length s in
    create_in
      ~read:(fun () ->
	       if !pos >= len then raise No_more_input
	       else String.unsafe_get s (post_incr pos))
      ~input:(fun sout p l ->
		if !pos >= len then raise No_more_input;
		let n = (if !pos + l > len then len - !pos else l) in
		  String.unsafe_blit s (post pos ( (+) n ) ) sout p n;
		  n
	     )
      ~close:noop



(**
   {6 Standard BatIO}
*)




let default_buffer_size = 16 (*Arbitrary number. If you replace it, just
			       don't put something too small, i.e. anything
			       smaller than 10 is probably a bad idea.*)

let output_string() =
  let b = Buffer.create default_buffer_size in
    create_out
      ~write:  (fun c -> Buffer.add_char b c )
      ~output: (fun s p l -> Buffer.add_substring b s p l;  l  )
      ~close:  (fun () -> Buffer.contents b)
      ~flush:  noop



let output_buffer buf =
  create_out
    ~write: (Buffer.add_char buf)
    ~output:(fun s p l -> Buffer.add_substring buf s p l; l)
    ~close: (fun () -> Buffer.contents buf)
    ~flush: noop

(** A placeholder used to allow recursive use of [self]
    in an [input_channel]*)
let placeholder_in = 
  { in_read  = (fun () -> ' ');
    in_input = (fun _ _ _ -> 0);
    in_close = noop;
    in_id    = (-1);
    in_upstream= weak_create 0 }
let input_channel ?(autoclose=true) ?(cleanup=false) ch =
  let me = ref placeholder_in (*placeholder*)
  in let result = 
  create_in
    ~read:(fun () -> try input_char ch
	   with End_of_file -> 
	     if autoclose then close_in !me;
	     raise No_more_input)
    ~input:(fun s p l ->
	      let n = Pervasives.input ch s p l in
		if n = 0 then 
		  begin
                    if autoclose then close_in !me else ();
		    raise No_more_input
		  end
		else n)
    ~close:(if cleanup then fun () -> Pervasives.close_in ch else ignore)
  in
    me := result;
    result

let output_channel ?(cleanup=false) ch =
    create_out
      ~write: (fun c     -> output_char ch c)
      ~output:(fun s p l -> Pervasives.output ch s p l; l)
      ~close: (if cleanup then fun () -> 
		 begin
(*		   Printf.eprintf "Cleaning up\n%!";*)
		   Pervasives.close_out ch 
		 end
	       else fun () -> 
		 begin
(*		   Printf.eprintf "Not cleaning up\n%!";*)
		   Pervasives.flush ch
		 end)
      ~flush: (fun ()    -> Pervasives.flush ch)


let pipe() =
  let input = ref "" in
  let inpos = ref 0 in
  let output = Buffer.create default_buffer_size in
  let flush() =
    input := Buffer.contents output;
    inpos := 0;
    Buffer.reset output;
    if String.length !input = 0 then raise No_more_input
  in
  let read() =
    if !inpos = String.length !input then flush();
    String.unsafe_get !input (post_incr inpos)
  in
  let input s p l =
    if !inpos = String.length !input then flush();
    let r = (if !inpos + l > String.length !input then String.length !input - !inpos else l) in
    String.unsafe_blit !input !inpos s p r;
    inpos := !inpos + r;
    r
  in
  let write c =
    Buffer.add_char output c
  in
  let output s p l =
    Buffer.add_substring output s p l;
    l
  in
  let input  = create_in ~read ~input  ~close:noop
  and output = create_out ~write ~output ~close:noop ~flush:noop
  in
    input , output



(*let to_input_channel inp =
  let (fin, fout) = Unix.pipe () in
    let outp = out_channel fout  in
    (*connect [inp] to [outp]*)
    in_channel_of_descr fin*)
(**
   {6 Binary APIs}
*)


exception Overflow of string

let read_byte i = int_of_char (i.in_read())

let read_signed_byte i =
	let c = int_of_char (i.in_read()) in
	if c land 128 <> 0 then
		c - 256
	else
		c
  
let read_string i =
	let b = Buffer.create 8 in
	let rec loop() =
		let c = i.in_read() in
		if c <> '\000' then begin
			Buffer.add_char b c;
			loop();
		end;
	in
	loop();
	Buffer.contents b

let read_line i =
	let b = Buffer.create 8 in
	let cr = ref false in
	let rec loop() =
		let c = i.in_read() in
		match c with
		| '\n' ->
			()
		| '\r' ->
			cr := true;
			loop()
		| _ when !cr ->
			cr := false;
			Buffer.add_char b '\r';
			Buffer.add_char b c;
			loop();
		| _ ->
			Buffer.add_char b c;
			loop();
	in
	try
		loop();
		Buffer.contents b
	with
		No_more_input ->
			if !cr then Buffer.add_char b '\r';
			if Buffer.length b > 0 then
				Buffer.contents b
			else
				raise No_more_input

let read_ui16 i =
	let ch1 = read_byte i in
	let ch2 = read_byte i in
	ch1 lor (ch2 lsl 8)

let read_i16 i =
	let ch1 = read_byte i in
	let ch2 = read_byte i in
	let n = ch1 lor (ch2 lsl 8) in
	if ch2 land 128 <> 0 then
		n - 65536
	else
		n

let fix = lnot 0x7FFFFFFF (* -:) *)

let read_i32 ch =
	let ch1 = read_byte ch in
	let ch2 = read_byte ch in
	let ch3 = read_byte ch in
	let ch4 = read_byte ch in
	if ch4 land 128 <> 0 then begin
		if ch4 land 64 = 0 then raise (Overflow "read_i32");
	        (ch1 lor (ch2 lsl 8) lor (ch3 lsl 16) lor ((ch4 land 127) lsl 24)) lor fix (* FIX HERE *)
	end else begin
		if ch4 land 64 <> 0 then raise (Overflow "read_i32");
		ch1 lor (ch2 lsl 8) lor (ch3 lsl 16) lor (ch4 lsl 24)
	end

let read_real_i32 ch =
	let ch1 = read_byte ch in
	let ch2 = read_byte ch in
	let ch3 = read_byte ch in
	let base = Int32.of_int (ch1 lor (ch2 lsl 8) lor (ch3 lsl 16)) in
	let big = Int32.shift_left (Int32.of_int (read_byte ch)) 24 in
	Int32.logor base big

let read_i64 ch =
	let ch1 = read_byte ch in
	let ch2 = read_byte ch in
	let ch3 = read_byte ch in
	let ch4 = read_byte ch in
	let base = Int64.of_int (ch1 lor (ch2 lsl 8) lor (ch3 lsl 16)) in
	let small = Int64.logor base (Int64.shift_left (Int64.of_int ch4) 24) in
	let big = Int64.of_int32 (read_real_i32 ch) in
	Int64.logor (Int64.shift_left big 32) small

let read_double ch =
	Int64.float_of_bits (read_i64 ch)

let read_float ch =
	Int32.float_of_bits (read_real_i32 ch)

let write_byte o n =
	(* doesn't test bounds of n in order to keep semantics of Pervasives.output_byte *)
	write o (Char.unsafe_chr (n land 0xFF))

let write_string o s =
	nwrite o s;
	write o '\000'

let write_line o s =
	nwrite o s;
	write o '\n'

let write_ui16 ch n =
	if n < 0 || n > 0xFFFF then raise (Overflow "write_ui16");
	write_byte ch n;
	write_byte ch (n lsr 8)

let write_i16 ch n =
	if n < -0x8000 || n > 0x7FFF then raise (Overflow "write_i16");
	if n < 0 then
		write_ui16 ch (65536 + n)
	else
		write_ui16 ch n

let write_i32 ch n =
	write_byte ch n;
	write_byte ch (n lsr 8);
	write_byte ch (n lsr 16);
	write_byte ch (n asr 24)

let write_real_i32 ch n =
	let base = Int32.to_int n in
	let big = Int32.to_int (Int32.shift_right_logical n 24) in
	write_byte ch base;
	write_byte ch (base lsr 8);
	write_byte ch (base lsr 16);
	write_byte ch big

let write_i64 ch n =
	write_real_i32 ch (Int64.to_int32 n);
	write_real_i32 ch (Int64.to_int32 (Int64.shift_right_logical n 32))

let write_double ch f =
	write_i64 ch (Int64.bits_of_float f)

let write_float ch f =
        write_real_i32 ch (Int32.bits_of_float f)

let stdin  = input_channel Pervasives.stdin
let stdout = output_channel Pervasives.stdout
let stderr = output_channel Pervasives.stderr
let stdnull= create_out
  ~write:ignore 
  ~output:(fun _ _ l -> l)
  ~flush:ignore
  ~close:ignore


(**
   {6 Printf}

   A reimplementation of Printf (with a few additional functions) based
   on [output].    We provide an internal signature to limit the dangers
   of {!Obj.magic}.

   {b Note} this module is inlined because of circular dependencies (themselves
   caused by the legacy definition of a function {!printf} in module {!BatIO}).
*)
module Printf :
sig
  type ('a, 'b, 'c) t = ('a, 'b, 'c) Pervasives.format

  val printf: ('b, 'a output, unit) format -> 'b
    (**The usual [printf] function, prints to
       [stdout].*)

  val eprintf: ('b, 'a output, unit) format -> 'b
    (**The usual [eprintf] function, prints to
       [stderr].*)

  val sprintf:  ('a, unit, string) format -> 'a
    (** As [fprintf] but outputs are replaced with
	strings. In particular, any function called with 
	[%a] should have type [unit -> string].*)

  val sprintf2: ('a, 'b output, unit, string) format4 -> 'a
    (**As [printf] but produces a string instead
       of printing to the output. By opposition to
       [sprintf], only the result is changed with
       respect to [printf], not the inner workings.*)


  val fprintf: 'a output -> ('b, 'a output, unit) format -> 'b
    (**General printf, prints to any output.*)

  val ifprintf: _        -> ('b, 'a output, unit) format -> 'b
    (**As [fprintf] but doesn't actually print anything.
       Sometimes useful for debugging.*)

  val bprintf: Buffer.t  -> ('a, Buffer.t, unit) format -> 'a
    (**As [fprintf], but with buffers instead of outputs.*)

  val bprintf2: Buffer.t  -> ('b, 'a output, unit) format -> 'b
    (**As [printf] but writes to a buffer instead
       of printing to the output. By opposition to
       [bprintf], only the result is changed with
       respect to [printf], not the inner workings.*)

  val kfprintf : ('a output -> 'b) -> 'a output -> ('c, 'a output, unit, 'b) format4 -> 'c
    (**Same as [fprintf], but instead of returning immediately, passes the [output] to its first
       argument at the end of printing.*)

  val ksprintf: (string -> 'a) -> ('b, unit, string, 'a) format4 -> 'b
    (** Same as [sprintf] above, but instead of returning the string,
	passes it to the first argument. *)
  val ksprintf2: (string -> 'b) -> ('c, 'a output, unit, 'b) format4 -> 'c
    (** Same as [sprintf2] above, but instead of returning the string,
	passes it to the first argument. *)

  val kbprintf : (Buffer.t -> 'a) ->
       Buffer.t -> ('b, Buffer.t, unit, 'a) format4 -> 'b
    (** Same as [bprintf], but instead of returning immediately,
	passes the buffer to its first argument at the end of printing. *)
  val kbprintf2 : (Buffer.t -> 'b) ->  Buffer.t -> ('c, 'a output, unit, 'b) format4 -> 'c
    (** Same as [bprintf2], but instead of returning immediately,
	passes the buffer to its first argument at the end of printing.*)

  val kprintf : (string -> 'a) -> ('b, unit, string, 'a) format4 -> 'b

  val mkprintf: ('a output -> 'b) -> 'a output -> ('c, 'a output, unit, 'b) format4 -> 'c
end
=
struct


external format_float: string -> float -> string
  = "caml_format_float"
external format_int: string -> int -> string
  = "caml_format_int"
external format_int32: string -> int32 -> string
  = "caml_int32_format"
external format_nativeint: string -> nativeint -> string
  = "caml_nativeint_format"
external format_int64: string -> int64 -> string
  = "caml_int64_format"

module Sformat = struct

  type index;;

  external unsafe_index_of_int : int -> index = "%identity";;
  let index_of_int i =
    if i >= 0 then unsafe_index_of_int i
    else failwith ("index_of_int: negative argument " ^ string_of_int i);;
  external int_of_index : index -> int = "%identity";;

  let add_int_index i idx = index_of_int (i + int_of_index idx);;
  let succ_index = add_int_index 1;;

  external length : ('a, 'b, 'c, 'd, 'e, 'f) format6 -> int
    = "%string_length";;
  external get : ('a, 'b, 'c, 'd, 'e, 'f) format6 -> int -> char
    = "%string_safe_get";;
  external unsafe_get : ('a, 'b, 'c, 'd, 'e, 'f) format6 -> int -> char
    = "%string_unsafe_get";;
  external unsafe_to_string : ('a, 'b, 'c, 'd, 'e, 'f) format6 -> string
    = "%identity";;
  let sub fmt idx len =
    String.sub (unsafe_to_string fmt) (int_of_index idx) len;;
  let to_string fmt = sub fmt (unsafe_index_of_int 0) (length fmt);;

end;;

let bad_conversion sfmt i c =
  invalid_arg
    ("printf: bad conversion %" ^ String.make 1 c ^ ", at char number " ^
     string_of_int i ^ " in format string ``" ^ sfmt ^ "''");;

let bad_conversion_format fmt i c =
  bad_conversion (Sformat.to_string fmt) i c;;

let incomplete_format fmt =
  invalid_arg
    ("printf: premature end of format string ``" ^
     Sformat.to_string fmt ^ "''");;

(* Parses a string conversion to return the specified length and the padding direction. *)
let parse_string_conversion sfmt =
  let rec parse neg i =
    if i >= String.length sfmt then (0, neg) else
    match String.unsafe_get sfmt i with
    | '1'..'9' ->
      (int_of_string
         (String.sub sfmt i (String.length sfmt - i - 1)),
       neg)
    | '-' -> parse true (succ i)
    | _   -> parse neg  (succ i) in
  try parse false 1 with Failure _ -> bad_conversion sfmt 0 's'

(* Pad a (sub) string into a blank string of length [p],
   on the right if [neg] is true, on the left otherwise. *)
let pad_string pad_char p neg s i len =
  if p = len && i = 0 then s else
  if p <= len then String.sub s i len else
  let res = String.make p pad_char in
  if neg
  then String.blit s i res 0 len
  else String.blit s i res (p - len) len;
  res

(* Format a string given a %s format, e.g. %40s or %-20s.
   To do: ignore other flags (#, +, etc)? *)
let format_string sfmt s =
  let (p, neg) = parse_string_conversion sfmt in
  pad_string ' ' p neg s 0 (String.length s);;

(* Extract a format string out of [fmt] between [start] and [stop] inclusive.
   '*' in the format are replaced by integers taken from the [widths] list.
   extract_format returns a string. *)
let extract_format fmt start stop widths =
  let start = succ start in
  let b = Buffer.create (stop - start + 10) in
  Buffer.add_char b '%';
  let rec fill_format i widths =
    if i <= stop then
      match (Sformat.unsafe_get fmt i, widths) with
      | ('*', h :: t) ->
        Buffer.add_string b (string_of_int h);
        let i = succ i in
        fill_format i t
      | ('*', []) ->
        assert false (* should not happen *)
      | (c, _) ->
        Buffer.add_char b c; fill_format (succ i) widths in
  fill_format start (List.rev widths);
  Buffer.contents b;;

let extract_format_int conv fmt start stop widths =
   let sfmt = extract_format fmt start stop widths in
   match conv with
   | 'n' | 'N' ->
     sfmt.[String.length sfmt - 1] <- 'u';
     sfmt
   | _ -> sfmt;;

(* Returns the position of the next character following the meta format
   string, starting from position [i], inside a given format [fmt].
   According to the character [conv], the meta format string is
   enclosed by the delimitors %{ and %} (when [conv = '{']) or %( and
   %) (when [conv = '(']). Hence, [sub_format] returns the index of
   the character following the [')'] or ['}'] that ends the meta format,
   according to the character [conv]. *)
let sub_format incomplete_format bad_conversion_format conv fmt i =
  let len = Sformat.length fmt in
  let rec sub_fmt c i =
    let close = if c = '(' then ')' else (* '{' *) '}' in
    let rec sub j =
       if j >= len then incomplete_format fmt else
       match Sformat.get fmt j with
       | '%' -> sub_sub (succ j)
       | _ -> sub (succ j)
    and sub_sub j =
       if j >= len then incomplete_format fmt else
       match Sformat.get fmt j with
       | '(' | '{' as c ->
         let j = sub_fmt c (succ j) in sub (succ j)
       | '}' | ')' as c ->
         if c = close then succ j else bad_conversion_format fmt i c
       | _ -> sub (succ j) in
    sub i in
  sub_fmt conv i;;

let sub_format_for_printf conv =
  sub_format incomplete_format bad_conversion_format conv;;

let iter_on_format_args fmt add_conv add_char =

  let lim = Sformat.length fmt - 1 in

  let rec scan_flags skip i =
    if i > lim then incomplete_format fmt else
    match Sformat.unsafe_get fmt i with
    | '*' -> scan_flags skip (add_conv skip i 'i')
    | '#' | '-' | ' ' | '+' -> scan_flags skip (succ i)
    | '_' -> scan_flags true (succ i)
    | '0'..'9'
    | '.' -> scan_flags skip (succ i)
    | _ -> scan_conv skip i
  and scan_conv skip i =
    if i > lim then incomplete_format fmt else
    match Sformat.unsafe_get fmt i with
    | '%' | '!' | ',' -> succ i
    | 's' | 'S' | '[' -> add_conv skip i 's'
    | 'c' | 'C' -> add_conv skip i 'c'
    | 'd' | 'i' |'o' | 'u' | 'x' | 'X' | 'N' -> add_conv skip i 'i'
    | 'f' | 'e' | 'E' | 'g' | 'G' | 'F' -> add_conv skip i 'f'
    | 'B' | 'b' -> add_conv skip i 'B'
    | 'a' | 'r' | 't' as conv -> add_conv skip i conv
    | 'l' | 'n' | 'L' as conv ->
      let j = succ i in
      if j > lim then add_conv skip i 'i' else begin
        match Sformat.get fmt j with
        | 'd' | 'i' | 'o' | 'u' | 'x' | 'X' ->
          add_char (add_conv skip i conv) 'i'
        | c -> add_conv skip i 'i' end
    | '{' as conv ->
      (* Just get a regular argument, skipping the specification. *)
      let i = add_conv skip i conv in
      (* To go on, find the index of the next char after the meta format. *)
      let j = sub_format_for_printf conv fmt i in
      (* Add the meta specification to the summary anyway. *)
      let rec loop i =
        if i < j - 2 then loop (add_char i (Sformat.get fmt i)) in
      loop i;
      (* Go on, starting at the closing brace to properly close the meta
         specification in the summary. *)
      scan_conv skip (j - 1)
    | '(' as conv ->
      (* Use the static format argument specification instead of
         the runtime format argument value: they must have the same type
         anyway. *)
      scan_fmt (add_conv skip i conv)
    | '}' | ')' as conv -> add_conv skip i conv
    | conv -> bad_conversion_format fmt i conv

  and scan_fmt i =
    if i < lim then
     if Sformat.get fmt i = '%'
     then scan_fmt (scan_flags false (succ i))
     else scan_fmt (succ i)
    else i in

  ignore (scan_fmt 0);;

(* Returns a string that summarizes the typing information that a given
   format string contains.
   For instance, [summarize_format_type "A number %d\n"] is "%i".
   It also checks the well-formedness of the format string. *)
let summarize_format_type fmt =
  let len = Sformat.length fmt in
  let b = Buffer.create len in
  let add_char i c = Buffer.add_char b c; succ i in
  let add_conv skip i c =
    if skip then Buffer.add_string b "%_" else Buffer.add_char b '%';
    add_char i c in
  iter_on_format_args fmt add_conv add_char;
  Buffer.contents b;;

module Ac = struct
  type ac = {
    mutable ac_rglr : int;
    mutable ac_skip : int;
    mutable ac_rdrs : int;
  }
end;;

open Ac;;

(* Computes the number of arguments of a format (including flag
   arguments if any). *)
let ac_of_format fmt =
  let ac = { ac_rglr = 0; ac_skip = 0; ac_rdrs = 0; } in
  let incr_ac skip c =
    let inc = if c = 'a' then 2 else 1 in
    if c = 'r' then ac.ac_rdrs <- ac.ac_rdrs + 1;
    if skip
    then ac.ac_skip <- ac.ac_skip + inc
    else ac.ac_rglr <- ac.ac_rglr + inc in
  let add_conv skip i c =
    (* Just finishing a meta format: no additional argument to record. *)
    if c <> ')' && c <> '}' then incr_ac skip c;
    succ i
  and add_char i c = succ i in

  iter_on_format_args fmt add_conv add_char;
  ac;;

let count_arguments_of_format fmt =
  let ac = ac_of_format fmt in
  ac.ac_rglr + ac.ac_skip + ac.ac_rdrs;;

let list_iter_i f l =
  let rec loop i = function
  | [] -> ()
  | [x] -> f i x (* Tail calling [f] *)
  | x :: xs -> f i x; loop (succ i) xs in
  loop 0 l;;

(* ``Abstracting'' version of kprintf: returns a (curried) function that
   will print when totally applied.
   Note: in the following, we are careful not to be badly caught
   by the compiler optimizations on the representation of arrays. *)
let kapr kpr fmt =
  match count_arguments_of_format fmt with
  | 0 -> kpr fmt [||]
  | 1 -> Obj.magic (fun x ->
      let a = Array.make 1 (Obj.repr 0) in
      a.(0) <- x;
      kpr fmt a)
  | 2 -> Obj.magic (fun x y ->
      let a = Array.make 2 (Obj.repr 0) in
      a.(0) <- x; a.(1) <- y;
      kpr fmt a)
  | 3 -> Obj.magic (fun x y z ->
      let a = Array.make 3 (Obj.repr 0) in
      a.(0) <- x; a.(1) <- y; a.(2) <- z;
      kpr fmt a)
  | 4 -> Obj.magic (fun x y z t ->
      let a = Array.make 4 (Obj.repr 0) in
      a.(0) <- x; a.(1) <- y; a.(2) <- z;
      a.(3) <- t;
      kpr fmt a)
  | 5 -> Obj.magic (fun x y z t u ->
      let a = Array.make 5 (Obj.repr 0) in
      a.(0) <- x; a.(1) <- y; a.(2) <- z;
      a.(3) <- t; a.(4) <- u;
      kpr fmt a)
  | 6 -> Obj.magic (fun x y z t u v ->
      let a = Array.make 6 (Obj.repr 0) in
      a.(0) <- x; a.(1) <- y; a.(2) <- z;
      a.(3) <- t; a.(4) <- u; a.(5) <- v;
      kpr fmt a)
  | nargs ->
    let rec loop i args =
      if i >= nargs then
        let a = Array.make nargs (Obj.repr 0) in
        list_iter_i (fun i arg -> a.(nargs - i - 1) <- arg) args;
        kpr fmt a
      else Obj.magic (fun x -> loop (succ i) (x :: args)) in
    loop 0 [];;

(* Get the index of the next argument to printf. *)
let next_index n = Sformat.succ_index n;;

(* Decode a format string and act on it.
   [fmt] is the printf format string, and [pos] points to a [%] character.
   After consuming the appropriate number of arguments and formatting
   them, one of the five continuations is called:
   [cont_s] for outputting a string (args: arg num, string, next pos)
   [cont_a] for performing a %a action (args: arg num, fn, arg, next pos)
   [cont_t] for performing a %t action (args: arg num, fn, next pos)
   [cont_f] for performing a flush action (args: arg num, next pos)
   [cont_m] for performing a %( action (args: arg num, sfmt, next pos)

   "arg num" is the index in array args of the next argument to printf.
   "next pos" is the position in [fmt] of the first character following
   the %conversion specification in [fmt]. *)

(* Note: here, rather than test explicitly against [Sformat.length fmt]
   to detect the end of the format, we use [Sformat.unsafe_get] and
   rely on the fact that we'll get a "nul" character if we access
   one past the end of the string.  These "nul" characters are then
   caught by the [_ -> bad_conversion] clauses below.
   Don't do this at home, kids. *)
let scan_format fmt args n pos cont_s cont_a cont_t cont_f cont_m =

  let get_arg n =
    Obj.magic (args.(Sformat.int_of_index n)) in

  let rec scan_flags n widths i =
    match Sformat.unsafe_get fmt i with
    | '*' ->
      let (width : int) = get_arg n in
      scan_flags (next_index n) (width :: widths) (succ i)
    | '0'..'9'
    | '.' | '#' | '-' | ' ' | '+' -> scan_flags n widths (succ i)
    | _ -> scan_conv n widths i

  and scan_conv n widths i =
    match Sformat.unsafe_get fmt i with
    | '%' ->
      cont_s n "%" (succ i)
    | 's' | 'S' as conv ->
      let (x : string) = get_arg n in
      let x = if conv = 's' then x else "\"" ^ String.escaped x ^ "\"" in
      let s =
        (* optimize for common case %s *)
        if i = succ pos then x else
        format_string (extract_format fmt pos i widths) x in
      cont_s (next_index n) s (succ i)
    | 'c' | 'C' as conv ->
      let (x : char) = get_arg n in
      let s =
        if conv = 'c' then String.make 1 x else "'" ^ Char.escaped x ^ "'" in
      cont_s (next_index n) s (succ i)
    | 'd' | 'i' | 'o' | 'u' | 'x' | 'X' | 'N' as conv ->
      let (x : int) = get_arg n in
      let s =
        format_int (extract_format_int conv fmt pos i widths) x in
      cont_s (next_index n) s (succ i)
    | 'f' | 'e' | 'E' | 'g' | 'G' ->
      let (x : float) = get_arg n in
      let s = format_float (extract_format fmt pos i widths) x in
      cont_s (next_index n) s (succ i)
    | 'F' ->
      let (x : float) = get_arg n in
      cont_s (next_index n) (string_of_float x) (succ i)
    | 'B' | 'b' ->
      let (x : bool) = get_arg n in
      cont_s (next_index n) (string_of_bool x) (succ i)
    | 'a' ->
      let printer = get_arg n in
      let n = Sformat.succ_index n in
      let arg = get_arg n in
      cont_a (next_index n) printer arg (succ i)
    | 't' ->
      let printer = get_arg n in
      cont_t (next_index n) printer (succ i)
    | 'l' | 'n' | 'L' as conv ->
      begin match Sformat.unsafe_get fmt (succ i) with
      | 'd' | 'i' | 'o' | 'u' | 'x' | 'X' ->
        let i = succ i in
        let s =
          match conv with
          | 'l' ->
            let (x : int32) = get_arg n in
            format_int32 (extract_format fmt pos i widths) x
          | 'n' ->
            let (x : nativeint) = get_arg n in
            format_nativeint (extract_format fmt pos i widths) x
          | _ ->
            let (x : int64) = get_arg n in
            format_int64 (extract_format fmt pos i widths) x in
        cont_s (next_index n) s (succ i)
      | _ ->
        let (x : int) = get_arg n in
        let s = format_int (extract_format_int 'n' fmt pos i widths) x in
        cont_s (next_index n) s (succ i)
      end
    | ',' -> cont_s n "" (succ i)
    | '!' -> cont_f n (succ i)
    | '{' | '(' as conv (* ')' '}' *) ->
      let (xf : ('a, 'b, 'c, 'd, 'e, 'f) format6) = get_arg n in
      let i = succ i in
      let j = sub_format_for_printf conv fmt i in
      if conv = '{' (* '}' *) then
        (* Just print the format argument as a specification. *)
        cont_s
          (next_index n)
          (summarize_format_type xf)
          j else
        (* Use the format argument instead of the format specification. *)
        cont_m (next_index n) xf j
    | (* '(' *) ')' ->
      cont_s n "" (succ i)
    | conv ->
      bad_conversion_format fmt i conv in

  scan_flags n [] (succ pos);;

(*Trimmed-down version of the legacy lib's [mkprintf]. Most of the generality
  is lifted to [output] rather than [mkprintf] itself.*)
let mkprintf k out fmt =

  let rec pr k n fmt v =

    let len = Sformat.length fmt in

    let rec doprn n i =
       if i >= len then Obj.magic (k out)
       else             match Sformat.unsafe_get fmt i with
	 | '%' -> scan_format fmt v n i cont_s cont_a cont_t cont_f cont_m
	 |  c  -> write out c; doprn n (succ i)
    and cont_s n s i =
      nwrite out s; 
      doprn n i
    and cont_a n printer arg i =
      printer out arg;
      doprn n i
    and cont_t n printer i =
      printer out;
      doprn n i
    and cont_f n i =
      flush out; 
      doprn n i
    and cont_m n xf i =
      let m = Sformat.add_int_index (count_arguments_of_format xf) n in
	pr (Obj.magic (fun _ -> doprn m i)) n xf v

    in doprn n 0 
  in let kpr = pr k (Sformat.index_of_int 0) in
    kapr kpr fmt;;

external identity : 'a -> 'a = "%identity"(*Inlined from [Std] to avoid cyclic dependencies*)
let fprintf out fmt = mkprintf ignore out fmt
let printf      fmt = fprintf stdout fmt
let eprintf     fmt = fprintf stderr fmt
let ifprintf _  fmt = fprintf stdnull fmt
let ksprintf2 k fmt =
  let out = output_string () in
    mkprintf (fun out -> k (close_out out)) out fmt
let kbprintf2 k buf fmt =
  let out = output_buffer buf in
    mkprintf (fun out -> k buf) out fmt
let sprintf2 fmt = ksprintf2 (identity) fmt
let bprintf2 buf fmt = kbprintf2 ignore buf fmt
(**
   Other possible implementation of [sprintf2],
   left as example:

[
let sprintf2    fmt = 
  let out = output_string () in
    mkprintf (fun out -> close_out out) out fmt
]
*)
(**
   Other possible implementation of [bprintf2],
   left as example:
[
let bprintf2 buf fmt = 
  let out = output_buffer buf in
    mkprintf ignore out fmt
]*)

type ('a, 'b, 'c) t = ('a, 'b, 'c) Pervasives.format

let kfprintf        = mkprintf
let bprintf         = Printf.bprintf
let sprintf         = Printf.sprintf
let ksprintf        = Printf.ksprintf
let kbprintf        = Printf.kbprintf
let kprintf         = Printf.kprintf
end

let get_output out = out.out_output
let get_flush  out = out.out_flush

let get_output_id out = out.out_id
let get_input_id  inp = inp.in_id
