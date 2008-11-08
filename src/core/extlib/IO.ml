(*
 * IO - Abstract input/output
 * Copyright (C) 2003 Nicolas Cannasse
 *               2008 David Teller (contributor)
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

TYPE_CONV_PATH "Batteries.System.IO" (*For Sexplib, Bin-prot...*)

open ExtChar
include InnerIO

(**
   {6 API}
*)

external noop :          unit -> unit = "%ignore"
external default_close : unit -> unit = "%ignore"

let pos_in i =
  let p = ref 0 in
    (create_in 
      ~read:(fun () ->
	       let c = read i in
		 incr p;
		 c
	    )
      ~input:(fun s sp l ->
		let n = input i s sp l in
		  p := !p + n;
		  n
	     )
      ~close:(fun () -> close_in i)
      , (fun () -> !p))

let pos_out o =
  let p = ref 0 in
    (wrap_out
       ~write:(fun c ->
		 write o c;
		 incr p
	      )
       ~output:(fun s sp l ->
		  let n = output o s sp l in
		    p := !p + n;
		    n
	       )
       ~close:noop
       ~flush:(fun () -> flush o)
       ~underlying:[o]
     , fun () -> !p)

(**
   {6 Support for enumerations}
*)

(*Function inlined here to avoid circular dependencies between [IO]
  and [ExtString].*)
let string_enum s =
  let l = String.length s in
  let rec make i =
    Enum.make 
      ~next:(fun () ->
	       if !i = l then
		 raise Enum.No_more_elements
	       else
		 let p = !i in
		   incr i;
		   String.unsafe_get s p
	    )
      ~count:(fun () -> l - !i)
      ~clone:(fun () -> make (ref !i))
  in
    make (ref 0)


let input_enum e =
  let pos = ref 0 in
    create_in
      ~read:(fun () ->
	       match Enum.get e with
		 | None -> raise No_more_input
		 | Some c ->
		     incr pos;
		     c
	    )
      ~input:(fun s p l ->
		let rec loop p l =
		  if l = 0 then
		    0
		  else
		    match Enum.get e with
		      | None -> l
		      | Some c ->
			  String.unsafe_set s p c;
			  loop (p + 1) (l - 1)
		in
		let k = loop p l in
		  if k = l then raise No_more_input;
		  l - k
	     )
      ~close:default_close

let output_enum() =
  let b = Buffer.create default_buffer_size in
    create_out
      ~write:(fun x ->
		Buffer.add_char b x
	     )
      ~output:(fun s p l ->
		 Buffer.add_substring b s p l;
		 l
	      )
      ~close:(fun () ->
		let s = Buffer.contents b in
		  string_enum s
	     )
      ~flush:default_close
    

(** [apply_enum f x] applies [f] to [x] and converts exceptions
    [No_more_input] and [Input_closed] to [Enum.No_more_elements]*)
let apply_enum f x =
  try f x
  with No_more_input 
    |  InnerIO.Input_closed  -> raise Enum.No_more_elements

(** [close_at_end input e] returns an enumeration which behaves as [e]
    and has the secondary effect of closing [input] once everything has
    been read.*)
let close_at_end (input:input) e=
  Enum.suffix_action (fun () -> close_in input) e

let make_enum f input =
  close_at_end input (Enum.from (fun () -> apply_enum f input))


let combine (a,b) =
  wrap_out ~write:(fun c -> 
		       write a c;
		       write b c)
    ~output:(fun s i j ->
	       let _ = output a s i j in
		 output b s i j)
    ~flush:(fun () ->
	      flush a;
	      flush b)
    ~close:(fun () ->
	      (close_out a, close_out b))
    ~underlying:[cast_output a; cast_output b]


let write_enum out f enum =
  Enum.iter f enum
(*;
  flush out*)
      
(**
   {6 Big Endians}
*)

module BigEndian = struct

let read_ui16 i =
	let ch2 = read_byte i in
	let ch1 = read_byte i in
	ch1 lor (ch2 lsl 8)

let read_i16 i =
	let ch2 = read_byte i in
	let ch1 = read_byte i in
	let n = ch1 lor (ch2 lsl 8) in
	if ch2 land 128 <> 0 then
		n - 65536
	else
		n

let read_i32 ch =
	let ch4 = read_byte ch in
	let ch3 = read_byte ch in
	let ch2 = read_byte ch in
	let ch1 = read_byte ch in
	if ch4 land 128 <> 0 then begin
		if ch4 land 64 = 0 then raise (Overflow "read_i32");
		ch1 lor (ch2 lsl 8) lor (ch3 lsl 16) lor ((ch4 land 127) lsl 24)
	end else begin
		if ch4 land 64 <> 0 then raise (Overflow "read_i32");
		ch1 lor (ch2 lsl 8) lor (ch3 lsl 16) lor (ch4 lsl 24)
	end

let read_real_i32 ch =
	let big = Int32.shift_left (Int32.of_int (read_byte ch)) 24 in
	let ch3 = read_byte ch in
	let ch2 = read_byte ch in
	let ch1 = read_byte ch in
	let base = Int32.of_int (ch1 lor (ch2 lsl 8) lor (ch3 lsl 16)) in
	Int32.logor base big

let read_i64 ch =
	let big = Int64.of_int32 (read_real_i32 ch) in
	let ch4 = read_byte ch in
	let ch3 = read_byte ch in
	let ch2 = read_byte ch in
	let ch1 = read_byte ch in
	let base = Int64.of_int (ch1 lor (ch2 lsl 8) lor (ch3 lsl 16)) in
	let small = Int64.logor base (Int64.shift_left (Int64.of_int ch4) 24) in
	Int64.logor (Int64.shift_left big 32) small

let read_double ch =
	Int64.float_of_bits (read_i64 ch)

let read_float ch =
	Int32.float_of_bits (read_real_i32 ch)

let write_ui16 ch n =
	if n < 0 || n > 0xFFFF then raise (Overflow "write_ui16");
	write_byte ch (n lsr 8);
	write_byte ch n

let write_i16 ch n =
	if n < -0x8000 || n > 0x7FFF then raise (Overflow "write_i16");
	if n < 0 then
		write_ui16 ch (65536 + n)
	else
		write_ui16 ch n

let write_i32 ch n =
	write_byte ch (n asr 24);
	write_byte ch (n lsr 16);
	write_byte ch (n lsr 8);
	write_byte ch n

let write_real_i32 ch n =
	let base = Int32.to_int n in
	let big = Int32.to_int (Int32.shift_right_logical n 24) in
	write_byte ch big;
	write_byte ch (base lsr 16);
	write_byte ch (base lsr 8);
	write_byte ch base

let write_i64 ch n =
	write_real_i32 ch (Int64.to_int32 (Int64.shift_right_logical n 32));
	write_real_i32 ch (Int64.to_int32 n)

let write_double ch f =
	write_i64 ch (Int64.bits_of_float f)

let write_float ch f =
	write_real_i32 ch (Int32.bits_of_float f)

let ui16s_of input     = make_enum read_ui16 input

let i16s_of input      = make_enum read_i16 input

let i32s_of input      = make_enum read_i32 input

let real_i32s_of input = make_enum read_real_i32 input

let i64s_of input      = make_enum read_i64 input

let doubles_of input   = make_enum read_double input

let floats_of input    = make_enum read_float input

let write_bytes output enum =
  write_enum output (write_byte output) enum

let write_ui16s output enum =
  write_enum output (write_ui16 output) enum

let write_i16s output enum =
  write_enum output (write_i16 output) enum

let write_i32s output enum =
  write_enum output (write_i32 output) enum

let write_real_i32s output enum =
  write_enum output (write_real_i32 output) enum

let write_i64s output enum =
  write_enum output (write_i64 output) enum

let write_doubles output enum =
  write_enum output (write_double output) enum

let write_floats output enum =
  write_enum output (write_float output) enum

let write_strings output enum =
  write_enum output (write_string output) enum

let write_lines output enum =
  write_enum output (write_line output) enum


end

(**
   {6 Bits API}
*)

type 'a bc = {
	ch : 'a;
	mutable nbits : int;
	mutable bits : int;
}

type in_bits = input bc
type out_bits = unit output bc

exception Bits_error

let input_bits ch =
	{
		ch = ch;
		nbits = 0;
		bits = 0;
	}

let output_bits ch =
	{
		ch = cast_output ch;
		nbits = 0;
		bits = 0;
	}

let rec read_bits b n =
	if b.nbits >= n then begin
		let c = b.nbits - n in
		let k = (b.bits asr c) land ((1 lsl n) - 1) in
		b.nbits <- c;
		k
	end else begin
		let k = read_byte b.ch in
		if b.nbits >= 24 then begin
			if n >= 31 then raise Bits_error;
			let c = 8 + b.nbits - n in
			let d = b.bits land ((1 lsl b.nbits) - 1) in
			let d = (d lsl (8 - c)) lor (k lsr c) in
			b.bits <- k;
			b.nbits <- c;
			d
		end else begin			
			b.bits <- (b.bits lsl 8) lor k;
			b.nbits <- b.nbits + 8;
			read_bits b n;
		end
	end

let drop_bits b =
	b.nbits <- 0

let rec write_bits b ~nbits x =
	let n = nbits in
	if n + b.nbits >= 32 then begin
		if n > 31 then raise Bits_error;
		let n2 = 32 - b.nbits - 1 in
		let n3 = n - n2 in
		write_bits b ~nbits:n2 (x asr n3);
		write_bits b ~nbits:n3 (x land ((1 lsl n3) - 1));
	end else begin
		if n < 0 then raise Bits_error;
		if (x < 0 || x > (1 lsl n - 1)) && n <> 31 then raise Bits_error;
		b.bits <- (b.bits lsl n) lor x;
		b.nbits <- b.nbits + n;
		while b.nbits >= 8 do
			b.nbits <- b.nbits - 8;
			write_byte b.ch (b.bits asr b.nbits)
		done
	end

let flush_bits b =
	if b.nbits > 0 then write_bits b (8 - b.nbits) 0

(**
   {6 Generic IO}
*)


class in_channel ch =
  object
	method input s pos len = input ch s pos len
	method close_in() = close_in ch
  end

class out_channel ch =
  object
	method output s pos len = output ch s pos len
	method flush() = flush ch
	method close_out() = ignore(close_out ch)
  end

class in_chars ch =
  object
	method get() = try read ch with No_more_input -> raise End_of_file
	method close_in() = close_in ch
  end

class out_chars ch =
  object
	method put t = write ch t
	method flush() = flush ch
	method close_out() = ignore(close_out ch)
  end

let from_in_channel ch =
	let cbuf = String.create 1 in
	let read() =
		try
			if ch#input cbuf 0 1 = 0 then raise Sys_blocked_io;
			String.unsafe_get cbuf 0
		with
			End_of_file -> raise No_more_input
	in
	let input s p l =
		ch#input s p l
	in
	create_in
		~read
		~input
		~close:ch#close_in

let from_out_channel ch =
	let cbuf = String.create 1 in
	let write c =
		String.unsafe_set cbuf 0 c;
		if ch#output cbuf 0 1 = 0 then raise Sys_blocked_io;
	in
	let output s p l =
		ch#output s p l
	in
	create_out
	  ~write
	  ~output
	  ~flush:ch#flush
	  ~close:ch#close_out

let from_in_chars ch =
	let input s p l =
		let i = ref 0 in
		try
			while !i < l do
				String.unsafe_set s (p + !i) (ch#get());
				incr i
			done;
			l
		with
			End_of_file when !i > 0 ->
				!i
	in
	create_in
		~read:ch#get
		~input
		~close:ch#close_in

let from_out_chars ch =
	let output s p l =
		for i = p to p + l - 1 do
			ch#put (String.unsafe_get s i)
		done;
		l
	in
	create_out
	  ~write:ch#put
	  ~output
	  ~flush:ch#flush
	  ~close:ch#close_out

(**
   {6 Enumerations}
*)

let bytes_of input        = make_enum read_byte input

let signed_bytes_of input = make_enum read_signed_byte input

let ui16s_of input        = make_enum read_ui16 input

let i16s_of input         = make_enum read_i16 input

let i32s_of input         = make_enum read_i32 input

let real_i32s_of input    = make_enum read_real_i32 input

let i64s_of input         = make_enum read_i64 input

let doubles_of input      = make_enum read_double input

let floats_of input       = make_enum read_float input

let strings_of input      = make_enum read_string input

let lines_of input        = make_enum read_line input

let chunks_of n input     = make_enum (fun input -> nread input n) input

(**The number of chars to read at once*)
let buffer_size = 1024 (*Arbitrary size.*)

let chars_of input = close_at_end input (Enum.concat (Enum.from (fun () -> 
   apply_enum (fun source -> string_enum (nread source buffer_size)) input)))

let bits_of input = close_at_end input.ch (Enum.from (fun () -> apply_enum read_bits input 1))

let write_bytes output enum =
  write_enum output (write_byte output) enum

let write_chars output enum =
  write_enum output (write output) enum

let write_ui16s output enum =
  write_enum output (write_ui16 output) enum

let write_i16s output enum =
  write_enum output (write_i16 output) enum

let write_i32s output enum =
  write_enum output (write_i32 output) enum

let write_real_i32s output enum =
  write_enum output (write_real_i32 output) enum

let write_i64s output enum =
  write_enum output (write_i64 output) enum

let write_doubles output enum =
  write_enum output (write_double output) enum

let write_floats output enum =
  write_enum output (write_float output) enum

let write_strings output enum =
  write_enum output (write_string output) enum

let write_lines output enum =
  write_enum output (write_line output) enum

let write_chunks output enum =
  write_enum output (nwrite output) enum

let write_bitss ~nbits output enum =
  Enum.iter (write_bits ~nbits output) enum
(*;
  flush output.ch*)

(**
   {6 Standard IO}
*)






let printf = Printf.fprintf

(**
   {6 Utilities}
*)

let make_list_printer (p:('a output -> 'b -> unit)) 
                      (start:   string)
		      (finish:  string)
		      (separate:string)
		      (out:'a output)
		      (l:  'b list  ) = 
  let rec aux out l = match l with
  | []    -> ()
  | [h]   -> p out h
  | h::t  -> printf out "%a%s%a" p h separate aux t
  in printf out "%s%a%s" start aux l finish



let tab_out ?(tab=' ') n out =
  let spaces   = String.make n tab in
  wrap_out 
    ~write: (fun c     -> 
	       write out  c;
	       if Char.is_newline c then (
		 nwrite out spaces)
	    )
    ~output:(fun s p l -> (*Replace each newline within the segment with newline^spaces*)
	       let length = String.length s                 in
	       let buffer = Buffer.create (String.length s) in
		 for i = p to min (length - 1) l do
		   let c = String.unsafe_get s i in
		     Buffer.add_char buffer c;
		     if Char.is_newline c then
		       Buffer.add_string buffer spaces
		 done;
		 let s' = Buffer.contents buffer                  in
		 output out s' 0 (String.length s'))
    ~flush:noop
    ~close:noop
    ~underlying:[out]

(*
let lmargin n (p:_ output -> 'a -> unit) out x =
  p (tab_out n (cast_output out)) x
*)

let comb (a,b) =
  create_out ~write:(fun c -> 
		       write a c;
		       write b c)
    ~output:(fun s i j ->
	       let _ = output a s i j in
		 output b s i j)
    ~flush:(fun () ->
	      flush a;
	      flush b)
    ~close:(fun () ->
	      ignore (close_out a); close_out b)




(** {6 Unicode}*)
open ExtUTF8
 
(** {7 Reading unicode}
 
All these functions assume that the input is UTF-8 encoded.
*)
 
(*val read_uchar: input -> UChar.t*)
(** read one UChar from a UTF-8 encoded input*)
let read_uchar_as_string i =
  let n0  = read i in
  let len = ExtUTF8.UTF8.length0 (Char.code n0) in
  let s   = String.create len in
    String.set s 0 n0;
    ignore(really_input i s 1 ( len - 1));
    s
 
let read_uchar i =
  ExtUTF8.UTF8.get (ExtUTF8.UTF8.of_string_unsafe (read_uchar_as_string i)) 0
 
(*val read_rope: input -> int -> Rope.t*)
(** read up to n uchars from a UTF-8 encoded input*)
let read_rope i n =
  let rec loop r j =
    if j = 0 then r
    else loop (Rope.append_char (read_uchar i) r) (j-1) (* TODO: make efficient by appending a string of Rope.leaf_size (256) chars at a time *)
  in
  if n <= 0 then Rope.empty
  else loop Rope.empty n
    
(*val read_uline: input -> Rope.t*)
(** read a line of UTF-8*)
 
let read_uline i =
  let line = read_line i in
  UTF8.validate line;
  Rope.of_ustring (UTF8.of_string_unsafe line)
 
(*val read_uall : input -> Rope.t*)
(** read the whole contents of a UTF-8 encoded input*)
 
let read_uall i = 
  let all = read_all i in
  UTF8.validate all;
  Rope.of_ustring (UTF8.of_string_unsafe all) 
(* TODO: make efficient - possibly similar to above - buffering leaf_size chars at a time *)
 
 
(*val uchars_of : input -> UChar.t Enum.t*)
(** offer the characters of an UTF-8 encoded input as an enumeration*)
 
let uchars_of i = make_enum read_uchar i
 
 
(*val ulines_of : input -> Rope.t Enum.t*)
(** offer the lines of a UTF-8 encoded input as an enumeration*)
let ulines_of i = make_enum read_uline i
 
(** {7 Writing unicode}
 
All these functions assume that the output is UTF-8 encoded.*)

let write_ustring o c = write_string o (ExtUTF8.UTF8.to_string_unsafe c) 

(*val write_uchar: _ output -> UChar.t -> unit*)
let write_uchar o c = write_ustring o (ExtUTF8.UTF8.of_char c)
 
(*val write_rope : _ output -> Rope.t -> unit*)
let write_rope = Rope.print
 
(*val write_uline: _ output -> Rope.t -> unit*)
let write_uline o r = write_rope o r; write o '\n'
  
(*val write_ulines : _ output -> Rope.t Enum.t -> unit*)
let write_ulines o re = write_enum o (write_uline o) re
 
(*val write_ropes : _ output -> Rope.t Enum.t -> unit*)
let write_ropes o re = write_enum o (write_rope o) re

(*val write_uchars : _ output -> UChar.t Enum.t -> unit*)
let write_uchars o uce = write_enum o (write_uchar o) uce

(*let copy input output = write_chunks output (chunks_of default_buffer_size input)*)
(*let copy input output = write_chars output (chars_of input)*)

let copy inp out =
  let n = default_buffer_size in
  let buffer = String.create n in
    try
      while true do
	let len = input inp buffer 0 n in
	  if len = 0 then raise No_more_input
	  else            ignore (really_output out buffer 0 len)
      done
    with No_more_input -> ()

(*let fast_chunks_of n inp  =
  let buffer = String.create n in
    make_enum (fun inp -> input inp buffer 0 n) input*)



(*
(** {6 Test} *)
let in_channel_of_input i =
  let (cin, cout) = Unix.pipe () in

  let latest_pos_in  = ref 0     in
  let rec aux ()  =
    let new_pos_in = pos_in  cin in
      if new_pos_in > !latest_pos_in then (*Something has been read, we can write a little bit more*)
	let size = new_pos_in - !latest_pos_in in
	let buf  = String.create size          in
	  input i buf

(*    UnixLabels.select
      ~read:?
      ~write:*)
      
(*  let (fin, fout) = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM in
  let cin         = open_in  fin
  and cout        = open_out fout in
  let rec aux () =
    let c = read i in
      Pervasives.output_char cout c;
      aux ()*)
*)
