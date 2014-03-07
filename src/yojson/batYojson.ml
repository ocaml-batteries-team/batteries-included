(*
 * BatYojson - interface to Yojson
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


type t = Yojson.Basic.json

let source =
  let module U = BatConv.UniversalSource in
  let rec visit : type b. b BatConv.Sink.t -> t -> b =
  fun sink x -> match x with
    | `Int i -> U.int_ sink i
    | `Float f -> U.float_ sink f
    | `Bool b -> U.bool_ sink b
    | `Null -> U.unit_ sink
    | `String s ->
        begin match BatConv.Sink.expected sink with
        | BatConv.Sink.ExpectSum -> U.sum ~src sink s []
        | _ -> U.string_ sink s
        end
    | `List ((`String name :: l) as l') ->
        begin match BatConv.Sink.expected sink with
        | BatConv.Sink.ExpectSum -> U.sum ~src sink name l
        | _ -> U.list_ ~src sink l'
        end
    | `List l -> U.list_ ~src sink l
    | `Assoc l -> U.record ~src sink l
  and src = { U.visit=visit; } in
  src

let sink : t BatConv.UniversalSink.t =
  let open BatConv.UniversalSink in
  { unit_ = `Null;
    bool_ = (fun b -> `Bool b);
    float_ = (fun f -> `Float f);
    int_ = (fun i -> `Int i);
    string_ = (fun s -> `String s);
    list_ = (fun l -> `List l);
    record = (fun l -> `Assoc l);
    tuple = (fun l -> `List l);
    sum = (fun name l -> match l with
      | [] -> `String name
      | _::_ -> `List (`String name :: l));
  }

let json_to_string s = Yojson.Basic.to_string ~std:true s

let into src x = BatConv.into src sink x

let from sink x = BatConv.from source sink x

let from_opt sink x =
  try Some (from sink x)
  with BatConv.ConversionFailure _ -> None

let to_string src x =
  json_to_string (into src x)

let of_string sink s =
  try
    let sexp = Yojson.Basic.from_string s in
    from_opt sink sexp
  with Yojson.Json_error _ -> None
