(*
 * IOThread - Abstract input/output, threaded version
 * Copyright (C) 1996 Xavier Leroy
 *               2003 Nicolas Cannasse
 *               2007 Zheng Li
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

include IO

open BatMutex;;
lock         := RMutex.make ();;
lock_factory := RMutex.make;;
(*
lock         := Mutex.make ();;
lock_factory := Mutex.make;;
*)
(*
lock         := DebugMutex.make ();;
lock_factory := DebugMutex.make;;
*)
(*
open Unix

(**
   Convert a
*)
let in_channel_of_input inp =
  let (pin, pout) = pipe () in
  let (cin, cout) = (in_channel_of_descr pin, out_channel_of_descr pout) in
    ignore (Thread.create (
      fun () -> 
	let buf = String.create 4096 in
	try
	  while true do
	    let read = IO.input inp buf 0 buffer in
	      if read = 0 then
		raise No_more_input;
	      Pervasives.output cout buf 0 read
	  done
	with e -> 
	  Pervasives.flush cout;
	  Pervasives.close_out cout
	    ) ()); cin

(********Not finished yet*************)
(*** We'll need async I/O before we can advance.*)
(*
open Unix
let out_channel_for_output out =
  let (pin, pout) = pipe ()                     in
  let (cin, cout) = (in_channel_of_descr pin, 
		     out_channel_of_descr pout) in
  let inp = create_in 
    ~read:  (let rec aux () =
	       try Pervasives.input_char cin
	       with Sys_blocked_io ->
		 Thread.yield ();
		 aux ()
	     in aux)
    ~close: (fun () -> Pervasives.close_in cin)
    ~input:(fun buf o l ->
	      let rec aux () =
		try Pervasives.input cin buf o l
		with Sys_blocked_io ->
		  Thread.yield()
		    aux ()
	    in aux ())
  in
  Thread.create (IO.copy inp) out;
  cout

*)
*)
