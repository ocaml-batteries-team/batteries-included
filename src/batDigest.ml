(*
 * ExtDigest - Additional functions for message digests
 * Copyright (C) 1996 Xavier Leroy, INRIA Rocquencourt
 * Copyright (C) 2009 David Teller, LIFO, Universite d'Orleans
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

open BatIO

(*Imported from [Digest.input] -- the functions used take advantage of
  [BatIO.input] rather than [in_channel]*)
let input inp = 
  let digest = String.create 16 in
  let _      = really_input inp digest 0 16 in
    digest


let output = BatIO.nwrite


let channel inp len = (*TODO: Make efficient*)
  if len >= 0 then 
    let buf = String.create len             in
    let _   = BatIO.really_input inp buf 0 len in
      Digest.string buf
  else Digest.channel (BatIO.to_input_channel inp) len

(*** batdigest

(*1. Compute the digest of this file using Legacy.Digest*)

let legacy_result () =
  let inp    = Pervasives.open_in_bin Sys.argv.(0) in
  let result = Legacy.Digest.channel inp (-1) in
    Pervasives.close_in inp;
    result
in
(*2. Compute the digest of this file using Batteries.Digest*)

let batteries_result () =
  let inp    = BatFile.open_in Sys.argv.(0) in
  let result = BatDigest.channel inp (-1)   in
    BatIO.close_in inp;
    result
in
(*3. Compare*)
  assert_equal ~printer:(Printf.sprintf "%S")
    (legacy_result ()) (batteries_result ())
 **)
