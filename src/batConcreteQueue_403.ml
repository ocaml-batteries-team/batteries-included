type 'a cell =
  | Nil
  | Cons of { content: 'a; mutable next: 'a cell }
[@@warning "-37"]
(* Disable warning 37 (Unused constructor):
   Cons is never used to build values,
   but it is used implicitly in [of_abstr] *)

type 'a t = {
  mutable length: int;
  mutable first: 'a cell;
  mutable last: 'a cell
}

external of_abstr : 'a Queue.t -> 'a t = "%identity"
external to_abstr : 'a t -> 'a Queue.t = "%identity"

let filter_inplace f queue =
  (* find_next returns the next 'true' cell, or Nil *)
  let rec find_next = function
    | Nil -> Nil
    | (Cons cell) as cons ->
       if f cell.content then cons
       else find_next cell.next
  in
  (* last is the last known 'true' Cons cell
     (may be Nil if no true cell has be found yet)
     next is the next candidate true cell
     (may be Nil if there is no next cell) *)
  let rec loop length last next = match next with
    | Nil -> (length, last)
    | (Cons cell) as cons ->
       let next = find_next cell.next in
       cell.next <- next;
       loop (length + 1) cons next
  in
  let first = find_next queue.first in
  (* returning a pair is unnecessary, the writes could be made at the
     end of 'loop', but the present style makes it obvious that all
     three writes are performed atomically, without allocation,
     function call or return (yield points) in between, guaranteeing
     some form of state consistency in the face of signals, threading
     or what not. *)
  let (length, last) = loop 0 Nil first in
  queue.length <- length;
  queue.first <- first;
  queue.last <- last;
  ()
