

type 'a t = {
  mutable count : unit -> int; (** Return the number of remaining elements in the enumeration. *)
  mutable next  : unit -> 'a;  (** Return the next element of the enumeration or raise [No_more_elements].*)
  mutable clone : unit -> 'a t;(** Return a copy of the enumeration. *)
  mutable fast  : bool;        (** [true] if [count] can be done without reading all elements, [false] otherwise.*)
}

exception No_more_elements 

let make ~next ~count ~clone =
  {
    count = count;
    next  = next;
    clone = clone;
    fast  = true;
  }
