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

open ExtString

type input = {
	mutable in_read : unit -> char;
	mutable in_input : string -> int -> int -> int;
	mutable in_close : unit -> unit;
}

type 'a output = {
	mutable out_write : char -> unit;
	mutable out_output : string -> int -> int -> int;
	mutable out_close : unit -> 'a;
	mutable out_flush : unit -> unit;
}

exception No_more_input
exception Input_closed
exception Output_closed

(** [apply_enum f x] applies [f] to [x] and converts exceptions
    [No_more_input] and [Input_closed] to [Enum.No_more_elements]*)
let apply_enum f x =
  try f x
  with No_more_input 
    |  Input_closed  -> raise Enum.No_more_elements




(* -------------------------------------------------------------- *)
(* API *)

let default_close = (fun () -> ())

let create_in ~read ~input ~close =
	{
		in_read = read;
		in_input = input;
		in_close = close;
	}

let create_out ~write ~output ~flush ~close =
	{
		out_write = write;
		out_output = output;
		out_close = close;
		out_flush = flush;
	}

let read i = i.in_read()

let nread i n =
	if n < 0 then invalid_arg "IO.nread";
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
	if p + l' > sl || p < 0 || l' < 0 then invalid_arg "IO.really_output";
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
	if p + l > sl || p < 0 || l < 0 then invalid_arg "IO.input";
	if l = 0 then
		0
	else
		i.in_input s p l

let really_input i s p l' =
	let sl = String.length s in
	if p + l' > sl || p < 0 || l' < 0 then invalid_arg "IO.really_input";
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
	if n < 0 then invalid_arg "IO.really_nread";
	if n = 0 then ""
	else
	let s = String.create n 
	in
	ignore(really_input i s 0 n);
	s

let close_in i =
	let f _ = raise Input_closed in
	i.in_close();
	i.in_read <- f;
	i.in_input <- f;
	i.in_close <- f

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

let output o s p l =
	let sl = String.length s in
	if p + l > sl || p < 0 || l < 0 then invalid_arg "IO.output";
	o.out_output s p l

let flush o = o.out_flush()

let close_out o =
	let f _ = raise Output_closed in
	let r = o.out_close() in
	o.out_write <- f;
	o.out_output <- f;
	o.out_close <- f;
	o.out_flush <- f;
	r

let read_all i =
	let maxlen = 1024 in
	let str = ref [] in
	let pos = ref 0 in
	let rec loop() =
		let s = nread i maxlen in
		str := (s,!pos) :: !str;
		pos := !pos + String.length s;
		loop()
	in
	try
		loop()
	with
		No_more_input ->
			let buf = String.create !pos in
			List.iter (fun (s,p) ->
				String.unsafe_blit s 0 buf p (String.length s)
			) !str;
			buf

let pos_in i =
	let p = ref 0 in
	{
		in_read = (fun () ->
			let c = i.in_read() in
			incr p;
			c
		);
		in_input = (fun s sp l ->
			let n = i.in_input s sp l in
			p := !p + n;
			n
		);
		in_close = i.in_close
	} , (fun () -> !p)

let pos_out o =
	let p = ref 0 in
	{
		out_write = (fun c ->
			o.out_write c;
			incr p
		);
		out_output = (fun s sp l ->
			let n = o.out_output s sp l in
			p := !p + n;
			n
		);
		out_close = o.out_close;
		out_flush = o.out_flush;
	} , (fun () -> !p)

(* -------------------------------------------------------------- *)
(* Standard IO *)

let input_string s =
	let pos = ref 0 in
	let len = String.length s in
	{
		in_read = (fun () ->
			if !pos >= len then raise No_more_input;
			let c = String.unsafe_get s !pos in
			incr pos;
			c
		);
		in_input = (fun sout p l ->
			if !pos >= len then raise No_more_input;
			let n = (if !pos + l > len then len - !pos else l) in
			String.unsafe_blit s !pos sout p n;
			pos := !pos + n;
			n
		);
		in_close = (fun () -> ());
	}

let output_string() =
	let b = Buffer.create 0 in
	{
		out_write = (fun c ->
			Buffer.add_char b c
		);
		out_output = (fun s p l ->
			Buffer.add_substring b s p l;
			l
		);
		out_close = (fun () -> Buffer.contents b);
		out_flush = (fun () -> ());
	}

let input_channel ch =
	{
		in_read = (fun () ->
			try
				input_char ch
			with
				End_of_file -> raise No_more_input
		);
		in_input = (fun s p l ->
			let n = Pervasives.input ch s p l in
			if n = 0 then raise No_more_input;
			n
		);
		in_close = (fun () -> Pervasives.close_in ch);
	}

let output_channel ch =
	{
		out_write = (fun c -> output_char ch c);
		out_output = (fun s p l -> Pervasives.output ch s p l; l);
		out_close = (fun () -> Pervasives.close_out ch);
		out_flush = (fun () -> Pervasives.flush ch);
	}

let input_enum e =
	let pos = ref 0 in
	{
		in_read = (fun () ->
			match Enum.get e with
			| None -> raise No_more_input
			| Some c ->
				incr pos;
				c
		);
		in_input = (fun s p l ->
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
		);
		in_close = (fun () -> ());
	}

let output_enum() =
	let b = Buffer.create 0 in
	{
		out_write = (fun x ->
			Buffer.add_char b x
		);
		out_output = (fun s p l ->
			Buffer.add_substring b s p l;
			l
		);
		out_close = (fun () ->
			let s = Buffer.contents b in
			ExtString.String.enum s
		);
		out_flush = (fun () -> ());
	}
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
	      let _ = close_out a in
	      close_out b)

let pipe() =
	let input = ref "" in
	let inpos = ref 0 in
	let output = Buffer.create 0 in
	let flush() =
		input := Buffer.contents output;
		inpos := 0;
		Buffer.reset output;
		if String.length !input = 0 then raise No_more_input
	in
	let read() =
		if !inpos = String.length !input then flush();
		let c = String.unsafe_get !input !inpos in
		incr inpos;
		c
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
	let input = {
		in_read = read;
		in_input = input;
		in_close = (fun () -> ());
	} in
	let output = {
		out_write = write;
		out_output = output;
		out_close = (fun () -> ());
		out_flush = (fun () -> ());
	} in
	input , output

external cast_output : 'a output -> unit output = "%identity"

(* -------------------------------------------------------------- *)
(* BINARY APIs *)

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

let read_i32 ch =
	let ch1 = read_byte ch in
	let ch2 = read_byte ch in
	let ch3 = read_byte ch in
	let ch4 = read_byte ch in
	if ch4 land 128 <> 0 then begin
		if ch4 land 64 = 0 then raise (Overflow "read_i32");
		ch1 lor (ch2 lsl 8) lor (ch3 lsl 16) lor ((ch4 land 127) lsl 24)
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

(* -------------------------------------------------------------- *)
(* Big Endians *)

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

let enum_ui16 input = Enum.from (fun () -> apply_enum read_ui16 input)

let enum_i16 input = Enum.from (fun () -> apply_enum read_i16 input)

let enum_i32 input = Enum.from (fun () -> apply_enum read_i32 input)

let enum_real_i32 input = Enum.from (fun () -> apply_enum read_real_i32 input)

let enum_i64 input = Enum.from (fun () -> apply_enum read_i64 input)

let enum_double input = Enum.from (fun () -> apply_enum read_double input)

let write_byte_enum output enum =
  Enum.iter (write_byte output) enum

let write_ui16_enum output enum =
  Enum.iter (write_ui16 output) enum

let write_i16_enum output enum =
  Enum.iter (write_i16 output) enum

let write_i32_enum output enum =
  Enum.iter (write_i32 output) enum

let write_real_i32_enum output enum =
  Enum.iter (write_real_i32 output) enum

let write_i64_enum output enum =
  Enum.iter (write_i64 output) enum

let write_double_enum output enum =
  Enum.iter (write_double output) enum

let write_string_enum output enum =
  Enum.iter (write_string output) enum

let write_line_enum output enum =
  Enum.iter (write_line output) enum


end

(* -------------------------------------------------------------- *)
(* Bits API *)

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

(* -------------------------------------------------------------- *)
(* Generic IO *)

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
(* ------*)
(* Enumeration API *)

let enum_byte input = Enum.from (fun () -> apply_enum read_byte input)

let enum_signed_byte input = Enum.from (fun () -> apply_enum read_signed_byte input)

let enum_ui16 input = Enum.from (fun () -> apply_enum read_ui16 input)

let enum_i16 input = Enum.from (fun () -> apply_enum read_i16 input)

let enum_i32 input = Enum.from (fun () -> apply_enum read_i32 input)

let enum_real_i32 input = Enum.from (fun () -> apply_enum read_real_i32 input)

let enum_i64 input = Enum.from (fun () -> apply_enum read_i64 input)

let enum_double input = Enum.from (fun () -> apply_enum read_double input)

let enum_string input = Enum.from (fun () -> apply_enum read_string input)

let enum_line input = Enum.from (fun () -> apply_enum read_line input)

let enum_char input = Enum.concat (Enum.from (fun () -> apply_enum (fun x -> String.enum (read_line x)) input))

let enum_bits in_bits = Enum.from (fun () -> apply_enum read_bits in_bits 1)

let write_byte_enum output enum =
  Enum.iter (write_byte output) enum

let write_ui16_enum output enum =
  Enum.iter (write_ui16 output) enum

let write_i16_enum output enum =
  Enum.iter (write_i16 output) enum

let write_i32_enum output enum =
  Enum.iter (write_i32 output) enum

let write_real_i32_enum output enum =
  Enum.iter (write_real_i32 output) enum

let write_i64_enum output enum =
  Enum.iter (write_i64 output) enum

let write_double_enum output enum =
  Enum.iter (write_double output) enum

let write_string_enum output enum =
  Enum.iter (write_string output) enum

let write_line_enum output enum =
  Enum.iter (write_line output) enum

let write_bits_enum ~nbits output enum =
  Enum.iter (write_bits ~nbits output) enum


(* -----------
   Standard IO
*)

let stdin = input_channel Pervasives.stdin
let stdout= output_channel Pervasives.stdout
let stderr= output_channel Pervasives.stderr

(* -----------
   Printf
*)
module Printf =
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
    | '-' ->
      parse true (succ i)
    | _ ->
      parse neg (succ i) in
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
    | '%' | '!' -> succ i
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

(*Trimmed-down version of the legacy lib's [mkprintf] -- in this version,
  the generality is not in function [mkprintf] but rather in the definition
  of [out]*)
let printf out fmt =

  let rec pr k n fmt v =

    let len = Sformat.length fmt in

    let rec doprn n i =
       if i >= len then Obj.magic (k out) else
       match Sformat.unsafe_get fmt i with
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
      pr (Obj.magic (fun _ -> doprn m i)) n xf v in

    doprn n 0 in

  let kpr = pr ignore (Sformat.index_of_int 0) in

  kapr kpr fmt;;

end

let printf = Printf.printf

(*let sprintf fmt = 
  let out = output_string in
    printf out fmt*)
