(*
 * BatList - additional and modified functions for lists.
 * Copyright (C) 2003 Brian Hurt
 * Copyright (C) 2003 Nicolas Cannasse
 * Copyright (C) 2008 Red Hat Inc.
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


module SMap = Map.Make(String)

type t =
  | I of int
  | S of string
  | L of t list
  | D of t SMap.t

let rec equal t1 t2 = match t1, t2 with
  | I i1, I i2 -> i1 = i2
  | S s1, S s2 -> s1 = s2
  | L l1, L l2 ->
    (try List.for_all2 equal l1 l2 with Invalid_argument _ -> false)
  | D d1, D d2 ->
    SMap.equal equal d1 d2
  | _ -> false

let hash t = Hashtbl.hash t

let dict_of_list l =
  let d = List.fold_left
    (fun d (k, v) -> SMap.add k v d)
    SMap.empty l
  in
  D d

(** {6 Serialization} *)

module Encoder = struct
  type string_encoder = {
    buf : string;
    mutable offset : int;
  }

  type out_encoder = {
    write_string : string -> int -> int -> unit;
    write_char : char -> unit;
  }

  type t =
    | String of string_encoder
    | Out of out_encoder

  let of_output o =
    Out {
      write_string = (fun s i len -> ignore (BatInnerIO.really_output o s i len));
      write_char = BatInnerIO.write o;
    }

  let of_buffer b =
    Out {
      write_string = (fun s i len -> Buffer.add_substring b s i len);
      write_char = Buffer.add_char b;
    }

  (* create string of given size, return encoder + string *)
  let make size =
    let result = String.make size ' ' in
    String { buf=result; offset = 0; }, result

  let write_char e c = match e with
    | Out o -> o.write_char c
    | String se -> se.buf.[se.offset] <- c; se.offset <- se.offset + 1

  let write e s = match e with
    | String se ->
      String.blit s 0 se.buf se.offset (String.length s);
      se.offset <- se.offset + String.length s
    | Out o ->
      o.write_string s 0 (String.length s)

  let write_int e i =
    let s = string_of_int i in
    write e s

  let write_str e s =
    write_int e (String.length s);
    write_char e ':';
    write e s
end

(* length of an encoded int, in bytes *)
let _len_int i =
  match i with
  | 0 -> 1
  | _ when i < 0 -> 2 + int_of_float (log10 (float_of_int ~-i))
  | _ -> 1 + int_of_float (log10 (float_of_int i))

(* length of an encoded string, in bytes *)
let _len_str s =
  _len_int (String.length s) + 1 + String.length s

let rec byte_size t = match t with
  | I i -> 2 + _len_int i
  | S s -> _len_str s
  | L l -> List.fold_left (fun acc i -> acc + byte_size i) 2 l
  | D map -> SMap.fold (fun k v acc -> acc + _len_str k + byte_size v) map 2

module E = Encoder

let rec encode enc t = match t with
  | I i -> E.write enc "i"; E.write_int enc i; E.write enc "e"
  | S s -> E.write_str enc s
  | L l ->
    E.write_char enc 'l';
    List.iter (encode enc) l;
    E.write_char enc 'e';
  | D m ->
    E.write_char enc 'd';
    SMap.iter (fun key t' -> E.write_str enc key; encode enc t') m;
    E.write_char enc 'e'

let to_string v =
  let enc, result = E.make (byte_size v) in
  encode enc v;
  assert (match enc with
            | E.String se -> se.E.offset = String.length result 
            | E.Out _  -> false);
  result

(*$T to_string
  to_string (L[I 1; I 2; L[S "foo"; S"hello world"]]) = \
    "li1ei2el3:foo11:hello worldee"
  to_string (L[S "a\t\n b"; dict_of_list ["foo", I 0]]) = \
    "l5:a\t\n bd3:fooi0eee"
 *)

let print out v =
  let enc = E.of_output out in
  encode enc v

let rec pretty_print out v =
  match v with
  | I i -> BatInnerIO.nwrite out (string_of_int i)
  | S s ->
    BatInnerIO.write out '"';
    BatInnerIO.nwrite out s;
    BatInnerIO.write out '"'
  | L l ->
    BatInnerIO.write out '[';
    List.iteri
      (fun i x ->
         if i > 0 then BatInnerIO.nwrite out "; "; pretty_print out x)
      l;
    BatInnerIO.write out ']'
  | D d ->
    BatInnerIO.write out '{';
    let first = ref true in
    SMap.iter
      (fun k v ->
        (if !first then first:=false else BatInnerIO.nwrite out "; ");
        BatInnerIO.nwrite out k;
        BatInnerIO.nwrite out " -> ";
        pretty_print out v)
      d;
    BatInnerIO.write out '}'

let pretty_to_string v =
  let out = BatInnerIO.output_string () in
  pretty_print out v;
  BatInnerIO.close_out out

(** {6 Deserialization (decoding)} *)

type parse_result =
  | ParseOk of t
  | ParseError of string
  | ParsePartial

module Decoder = struct
  type int_state =
    | Start
    | Negative
    | Positive

  (** Partial state of the parser *)
  type partial_state =
  | PS_I of int_state * int (* integer *)
  | PS_S of int ref * string  (* index in string, plus string *)
  | PS_L of t list
  | PS_D of t SMap.t  (* in dictionary *)
  | PS_D_key of string * t SMap.t  (* parsed key, wait for value *) 
  | PS_return of t  (* bottom of stack *)
  | PS_error of string (* error *)

  type t = {
    mutable buf : string;  (* buffer *)
    mutable i : int;  (* index in buf *)
    mutable len : int;  (* length of substring to read *)
    mutable c : int;  (* line *)
    mutable l : int;  (* column *)
    mutable state : parse_result;
    mutable stack : partial_state list;
  }

  let create () =
    let dec = {
      buf = "";
      i = 0;
      len = 0;
      c = 0;
      l = 0;
      state = ParsePartial;
      stack = [];
    } in
    dec

  let reset dec =
    dec.l <- 0;
    dec.c <- 0;
    dec.i <- 0;
    dec.len <- 0;
    dec.state <- ParsePartial;
    dec.stack <- [];
    ()

  let state dec = dec.state

  let rest dec =
    String.sub dec.buf dec.i dec.len

  let rest_size dec =
    dec.len

  let is_empty dec = dec.len = 0
  let cur dec = dec.buf.[dec.i]

  let junk dec =
    (* update line/column *)
    (if cur dec = '\n'
      then (dec.c <- 0; dec.l <- dec.l + 1)
      else dec.c <- dec.c + 1);
    dec.i <- dec.i + 1;
    dec.len <- dec.len - 1

  let next dec =
    let c = cur dec in
    junk dec;
    c

  (* parse value *)
  let rec parse_rec dec =
    match dec.stack with
    | [PS_return v] ->  (* return value *)
      dec.stack <- [];
      dec.state <- ParseOk v;
      dec.state
    | [PS_error s] -> (* failure *)
      dec.stack <- [];
      dec.state <- ParseError s;
      dec.state
    | _ when is_empty dec -> ParsePartial (* wait *)
    | _ ->
      let c = next dec in
      begin match dec.stack, c with
      | (PS_I (istate, 0)) :: stack, '0' ->
        begin match istate with
          | Start ->
            dec.stack <- PS_I (Positive, 0) :: stack
          | Positive|Negative ->
            error dec "invalid prefix 0"
        end
      | (PS_I (Start, 0)) :: stack, '-' ->
        dec.stack <- PS_I (Negative, 0) :: stack  (* negative number *)
      | (PS_I (Start, i)) :: stack, '0' .. '9' ->
        dec.stack <- PS_I (Positive, (Char.code c - Char.code '0') + 10 * i) :: stack;
      | (PS_I (istate, i)) :: stack, '0' .. '9' ->
        dec.stack <- PS_I (istate, (Char.code c - Char.code '0') + 10 * i) :: stack;
      | (PS_I (Negative, i)) :: stack, 'e' ->
        dec.stack <- stack;
        push_value dec (I ~- i)
      | (PS_I (Positive, i)) :: stack, 'e' ->
        dec.stack <- stack;
        push_value dec (I i)
      | ((PS_D _ | PS_D_key _ | PS_L _) :: _ | []), '0' .. '9' ->
        (* initial length of string *)
        dec.stack <- (PS_I (Positive, Char.code c - Char.code '0')) :: dec.stack
      | (PS_I (Positive, i)) :: stack, ':' ->
        assert (i >= 0);
        if i = 0 then  (* empty string *)
          let _ = dec.stack <- stack in
          push_value dec (S "")
        else (* prepare to parse a string *)
          dec.stack <- (PS_S (ref 0, String.create i)) :: stack;
      | (PS_S (n, s)) :: stack, _ ->
        s.[!n] <- c;
        incr n;
        (* value completed *)
        (if !n = String.length s
          then
            let _ = dec.stack <- stack in
            push_value dec (S s));
      | stack, 'i' ->
        (* expect an integer, maybe 0 *)
        dec.stack <- (PS_I (Start, 0)) :: stack
      | stack, 'l' ->
        dec.stack <- PS_L [] :: stack;
      | stack, 'd' ->
        dec.stack <- PS_D SMap.empty :: stack
      | (PS_L l) :: stack, 'e' -> (* end of list *)
        dec.stack <- stack;
        push_value dec (L (List.rev l))
      | (PS_D d) :: stack, 'e' -> (* end of dict *)
        dec.stack <- stack;
        push_value dec (D d)
      | (PS_D_key _) :: _, 'e' -> (* error *)
        error dec "missing value in dict"
      | _ -> (* generic error *)
        error dec (Printf.sprintf "expected value, got %c" c)
      end;
      parse_rec dec
  (* When a value is parsed, push it on the stack (possibly collapsing it) *)
  and push_value dec v =
    match v, dec.stack with
    | _, [] ->
      dec.stack <- [PS_return v] (* finished *)
    | _, (PS_L l) :: stack ->
      (* add to list *)
      dec.stack <- (PS_L (v :: l)) :: stack;
    | S key, ((PS_D d) :: stack) ->
      (* new key for the map *)
      dec.stack <- (PS_D_key (key, d)) :: stack;
    | _, ((PS_D d) :: _) ->
      (* error: key must be string *)
      error dec "dict keys must be strings"
    | _, (PS_D_key (key, d)) :: stack ->
      (* new binding for the map *)
      dec.stack <- (PS_D (SMap.add key v d)) :: stack;
    | _ -> assert false
  (* signal error *)
  and error dec msg =
    let msg = Printf.sprintf "Bencode: error at line %d, column %d: %s"
      dec.l dec.c msg in
    dec.stack <- [PS_error msg]

  (* exported parse function *)
  let parse dec s i len =
    (if i < 0 || i+len > String.length s
      then invalid_arg "Bencode.parse: not a valid substring");
    (* add the input to [dec] *)
    if dec.len = 0
      then begin
        dec.buf <- String.copy s;
        dec.i <- i;
        dec.len <- len;
      end else begin
        (* use a buffer to merge the stored input and the new input *)
        let buf' = String.create (dec.len + len - dec.i) in
        String.blit dec.buf dec.i buf' 0 dec.len;
        String.blit s i buf' dec.len len;
        dec.buf <- buf';
        dec.i <- 0;
        dec.len <- dec.len + len - dec.i;
      end;
    (* enter the state machine *)
    parse_rec dec
end

let decode = Decoder.parse

let resume_parsing d = Decoder.parse_rec d

(** {6 Utils} *)

let of_string s =
  let dec = Decoder.create () in
  match decode dec s 0 (String.length s) with
    | ParsePartial -> BatResult.Bad "unexpected end of string"
    | ParseOk v -> BatResult.Ok v
    | ParseError e -> BatResult.Bad e

(*$T of_string
  of_string "li1ei2e4:yeahd3:fooi42e3:barleee" = \
    BatResult.Ok(BatBencode.(L[I 1; I 2; S "yeah"; \
      dict_of_list ["foo", I 42; "bar", L []]]))
  of_string "li1ei2ei3ei4ee" = \
    BatResult.Ok(BatBencode.(L[I 1; I 2; I 3; I 4]))
  of_string "li-1ei0ei34567ei40001ee" = \
    BatResult.Ok(BatBencode.(L[I ~-1; I 0; I 34567; I 40001]))
  match of_string "li000ee" with \
    | BatResult.Ok _ -> false \
    | BatResult.Bad _ -> true
*)

(* TODO quickcheck
  module B = BatBencode;;
  let rec _rand_bencode_val depth st =
    if depth = 0 then B.I (Q.small_int st)
     else match Random.State.int st 4 with
      | 0 -> B.I (Random.State.int 200 - 100)
      | 1 -> B.S (Q.string st)
      | 2 -> B.L (Q.list (_rand_val (depth-1)) st)
      | 3 -> B.dict_of_list (Q.list\
        (Q.pair Q.printable_string (_rand_bencode_val (depth-1°))) st)
      | _ -> assert false;;
*) 

let decode_enum ?(dec=Decoder.create()) e =
  let stop = ref false in
  (* read next string *)
  let rec read_chunk () =
    if !stop then raise BatEnum.No_more_elements;
    match BatEnum.get e with
    | None ->
        begin match dec.Decoder.stack with
          | [] ->
            (* at value boundary *)
            raise BatEnum.No_more_elements
          | _::_ -> fail "unexpected end of input"
        end
    | Some str ->
      match decode dec str 0 (String.length str) with
      | ParseOk v ->
        BatResult.Ok v  (* yield result *)
      | ParseError e -> fail e
      | ParsePartial -> read_chunk () (* try again *)
  and resume () =
    if !stop then raise BatEnum.No_more_elements;
    match resume_parsing dec with
    | ParseOk v -> BatResult.Ok v
    | ParseError e -> fail e
    | ParsePartial -> read_chunk ()
  and fail msg =
    stop := true;
    BatResult.Bad msg
  in
  BatEnum.from resume

let decode_input ?dec i =
  decode_enum ?dec (BatIO.chunks_of 256 i)

let decode_string ?dec s =
  decode_enum ?dec (BatEnum.singleton s)
