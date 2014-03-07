(*
 * BatBencode - interface to Bencode
 * Copyright (C) 2014 Simon Cruanes
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

type t = Bencode.t

let source =
  let module U = BatConv.UniversalSource in
  let rec visit : type b. b BatConv.Sink.t -> t -> b =
  fun sink x -> match x, BatConv.Sink.expected sink with
    | Bencode.String s, BatConv.Sink.ExpectSum -> U.sum ~src sink s []
    | Bencode.List (Bencode.String name :: l), BatConv.Sink.ExpectSum ->
        U.sum ~src sink name l
    | Bencode.Dict l, _ -> U.record ~src sink l
    | Bencode.String s, _ -> U.string_ sink s
    | Bencode.List [], BatConv.Sink.ExpectUnit -> U.unit_ sink
    | Bencode.List l, _ -> U.list_ ~src sink l
    | Bencode.Integer 0, BatConv.Sink.ExpectUnit -> U.unit_ sink
    | Bencode.Integer i, _ -> U.int_ sink i
  and src = { U.visit=visit; } in
  src

let sink =
  let open BatConv.UniversalSink in
  { unit_ = Bencode.Integer 0;
    bool_ = (fun b -> Bencode.Integer (if b then 1 else 0));
    float_ = (fun f -> Bencode.String (string_of_float f));
    int_ = (fun i -> Bencode.Integer i);
    string_ = (fun s -> Bencode.String s);
    list_ = (fun l -> Bencode.List l);
    record = (fun l -> Bencode.Dict l);
    tuple = (fun l -> Bencode.List l);
    sum = (fun name l -> match l with
      | [] -> Bencode.String name
      | _::_ -> Bencode.List (Bencode.String name :: l));
  }

let bencode_to_string = Bencode.encode_to_string

let into src x = BatConv.into src sink x

let from sink x = BatConv.from source sink x

let from_opt sink x =
  try Some (from sink x)
  with BatConv.ConversionFailure _ -> None

let to_string src x =
  bencode_to_string (into src x)

let of_string sink s =
  try
    let sexp = Bencode.decode (`String s) in
    from_opt sink sexp
  with Failure _ -> None
