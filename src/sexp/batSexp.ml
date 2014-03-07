(*
 * BatSexp - interface to Sexplib
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

type t = Sexplib.Sexp.t =
  | Atom of string
  | List of t list

let source =
  let module U = BatConv.UniversalSource in
  let rec visit : type b. b BatConv.Sink.t -> t -> b =
  fun sink x -> match x, BatConv.Sink.expected sink with
    | Atom s, BatConv.Sink.ExpectSum -> U.sum ~src sink s []
    | List (Atom name :: l), BatConv.Sink.ExpectSum -> U.sum ~src sink name l
    | List l, BatConv.Sink.ExpectRecord ->
        let l' = List.map (function
          | List [Atom name; x] -> name, x
          | _ -> BatConv.report_error "get List, but expected Record") l
        in U.record ~src sink l'
    | Atom s, _ -> U.string_ sink s
    | List [], BatConv.Sink.ExpectUnit -> U.unit_ sink
    | List l, _ -> U.list_ ~src sink l
  and src = { U.visit=visit; } in
  src

let sink =
  let open BatConv.UniversalSink in
  { unit_ = List [];
    bool_ = (fun b -> Atom (string_of_bool b));
    float_ = (fun f -> Atom (string_of_float f));
    int_ = (fun i -> Atom (string_of_int i));
    string_ = (fun s -> Atom (String.escaped s));
    list_ = (fun l -> List l);
    record = (fun l -> List (List.map (fun (a,b) -> List [Atom a; b]) l));
    tuple = (fun l -> List l);
    sum = (fun name l -> match l with
      | [] -> Atom name
      | _::_ -> List (Atom name :: l));
  }

let sexp_to_string = Sexplib.Sexp.to_string

let into src x = BatConv.into src sink x

let from sink x = BatConv.from source sink x

let from_opt sink x =
  try Some (from sink x)
  with BatConv.ConversionFailure _ -> None

let to_string src x =
  sexp_to_string (into src x)

let of_string sink s =
  try
    let sexp = Sexplib.Sexp.of_string s in
    from_opt sink sexp
  with Failure _ -> None
