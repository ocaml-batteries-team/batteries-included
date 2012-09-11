module O = BatOrd

let ( |? ) = BatOption.( |? )

exception Invalid_bounds

type 'a bound_t = [ `o of 'a | `c of 'a ]

type 'a bounding_f = ?min:'a bound_t -> ?max:'a bound_t -> 'a -> 'a option

let bounding_of_ord ?default_low ?default_high ord = 
  fun ?(min : 'a bound_t option) ?(max : 'a bound_t option) ->
    match min, max with
    | Some (`c l), Some (`c u) -> begin
      if ord l u = O.Gt then raise Invalid_bounds;
      fun x ->
        match ord x l, ord x u with
        | O.Lt, _ -> default_low
        | _, O.Gt -> default_high
        | O.Eq, _
        | _, O.Eq
        | O.Gt, _ -> Some x
    end
    | None, Some (`c u) -> begin
      fun x ->
        match ord x u with
        | O.Gt -> default_high
        | O.Eq
        | O.Lt -> Some x
    end
    | Some (`c l), None -> begin
      fun x ->
        match ord x l with
        | O.Lt -> default_low
        | O.Gt
        | O.Eq -> Some x
    end
    | None, None -> BatOption.Monad.return
    | Some (`o l), Some (`o u) -> begin
      if ord l u = O.Gt then raise Invalid_bounds;
      fun x ->
        match ord x l, ord x u with
        | O.Lt, _
        | O.Eq, _ -> default_low
        | _, O.Gt
        | _, O.Eq -> default_high
        | O.Gt, _ -> Some x
    end
    | None, Some (`o u) -> begin
      fun x ->
        match ord x u with
        | O.Gt
        | O.Eq -> default_high
        | O.Lt -> Some x
    end
    | Some (`o l), None -> begin
      fun x ->
        match ord x l with
        | O.Lt
        | O.Eq -> default_low
        | O.Gt -> Some x
    end
    | Some (`c l), Some (`o u) -> begin
      if ord l u = O.Gt then raise Invalid_bounds;
      fun x ->
        match ord x l, ord x u with
        | O.Lt, _ -> default_low
        | _, O.Gt
        | _, O.Eq -> default_high
        | O.Eq, _
        | O.Gt, _ -> Some x
    end
    | Some (`o l), Some (`c u) -> begin
      if ord l u = O.Gt then raise Invalid_bounds;
      fun x ->
        match ord x l, ord x u with
        | O.Lt, _
        | O.Eq, _ -> default_low
        | _, O.Gt -> default_high
        | _, O.Eq
        | O.Gt, _ -> Some x
    end

let ret _ = None

let bounding_of_ord_chain ?low ?high ord = 
  let low = low |? ret in
  let high = high |? ret in
  fun ?(min : 'a bound_t option) ?(max : 'a bound_t option) ->
    match min, max with
    (* Closed bounds (inclusive) *)
    | Some (`c l), Some (`c u) -> begin
      if ord l u = O.Gt then raise Invalid_bounds;
      fun x ->
        match ord x l, ord x u with
        | O.Lt, _ -> low x
        | _, O.Gt -> high x
        | O.Eq, _
        | _, O.Eq
        | O.Gt, _ -> Some x
    end
    | None, Some (`c u) -> begin
      fun x ->
        match ord x u with
        | O.Gt -> high x
        | O.Eq
        | O.Lt -> Some x
    end
    | Some (`c l), None -> begin
      fun x ->
        match ord x l with
        | O.Lt -> low x
        | O.Gt
        | O.Eq -> Some x
    end
    (* Open bounds (exclusive) *)
    | Some (`o l), Some (`o u) -> begin
      if ord l u = O.Gt then raise Invalid_bounds;
      fun x ->
        match ord x l, ord x u with
        | O.Lt, _
        | O.Eq, _ -> low x
        | _, O.Gt
        | _, O.Eq -> high x
        | O.Gt, _ -> Some x
    end
    | None, Some (`o u) -> begin
      fun x ->
        match ord x u with
        | O.Gt
        | O.Eq -> high x
        | O.Lt -> Some x
    end
    | Some (`o l), None -> begin
      fun x ->
        match ord x l with
        | O.Lt
        | O.Eq -> low x
        | O.Gt -> Some x
    end
    (* Mixed open and closed bounds *)
    | Some (`c l), Some (`o u) -> begin
      if ord l u = O.Gt then raise Invalid_bounds;
      fun x ->
        match ord x l, ord x u with
        | O.Lt, _ -> low x
        | _, O.Gt
        | _, O.Eq -> high x
        | O.Eq, _
        | O.Gt, _ -> Some x
    end
    | Some (`o l), Some (`c u) -> begin
      if ord l u = O.Gt then raise Invalid_bounds;
      fun x ->
        match ord x l, ord x u with
        | O.Lt, _
        | O.Eq, _ -> low x
        | _, O.Gt -> high x
        | _, O.Eq
        | O.Gt, _ -> Some x
    end
    | None, None -> ret

module type BoundedType = sig
  type t
  val min : t bound_t option
  val max : t bound_t option
  val bounded : t bounding_f
end

module type S = sig
  type u
  type t = private u
  exception Out_of_range
  val min : t bound_t option
  val max : t bound_t option
  val make : u -> t option
  val make_exn : u -> t
end

module Make(M : BoundedType) : (S with type u = M.t) = struct
  include M
  type u = t
  exception Out_of_range
  let make = bounded ?min ?max
  let make_exn x = BatOption.get_exn (make x) Out_of_range
end

module Int10_base = struct
  type t = int
  let min = Some (`c 1)
  let max = Some (`c 10)
  let default_low = None
  let default_high = None
  let bounded = bounding_of_ord ?default_low ?default_high BatInt.ord
end

(** Only accept integers between 1 and 10 *)
module Int10 = Make(Int10_base)

