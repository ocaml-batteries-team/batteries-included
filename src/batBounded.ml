module O = BatOrd

exception Invalid_bounds

type 'a bound_t = [ `o of 'a | `c of 'a | `u]

type ('a, 'b) bounding_f = bounds:('a bound_t * 'a bound_t) -> 'a -> 'b

let ret_some x = Some x
let ret_none _ = None
let const a _ = a
external identity : 'a -> 'a = "%identity"

let bounding_of_ord ~default_low ~default_high conv ord =
  fun ~(bounds : 'a bound_t * 'a bound_t) ->
    match bounds with
    | `c l, `c u -> begin
        if ord l u = O.Gt then raise Invalid_bounds;
        fun x ->
          match ord x l, ord x u with
          | O.Lt, _ -> default_low
          | _, O.Gt -> default_high
          | O.Eq, _
          | _, O.Eq
          | O.Gt, _ -> conv x
      end
    | `u, `c u -> begin
        fun x ->
          match ord x u with
          | O.Gt -> default_high
          | O.Eq
          | O.Lt -> conv x
      end
    | `c l, `u -> begin
        fun x ->
          match ord x l with
          | O.Lt -> default_low
          | O.Gt
          | O.Eq -> conv x
      end
    | `u, `u -> conv
    | `o l, `o u -> begin
        if ord l u = O.Gt then raise Invalid_bounds;
        fun x ->
          match ord x l, ord x u with
          | O.Lt, _
          | O.Eq, _ -> default_low
          | _, O.Gt
          | _, O.Eq -> default_high
          | O.Gt, _ -> conv x
      end
    | `u, `o u -> begin
        fun x ->
          match ord x u with
          | O.Gt
          | O.Eq -> default_high
          | O.Lt -> conv x
      end
    | `o l, `u -> begin
        fun x ->
          match ord x l with
          | O.Lt
          | O.Eq -> default_low
          | O.Gt -> conv x
      end
    | `c l, `o u -> begin
        if ord l u = O.Gt then raise Invalid_bounds;
        fun x ->
          match ord x l, ord x u with
          | O.Lt, _ -> default_low
          | _, O.Gt
          | _, O.Eq -> default_high
          | O.Eq, _
          | O.Gt, _ -> conv x
      end
    | `o l, `c u -> begin
        if ord l u = O.Gt then raise Invalid_bounds;
        fun x ->
          match ord x l, ord x u with
          | O.Lt, _
          | O.Eq, _ -> default_low
          | _, O.Gt -> default_high
          | _, O.Eq
          | O.Gt, _ -> conv x
      end

(*$T bounding_of_ord
   bounding_of_ord ~default_low:None ~default_high:None (fun x -> Some x) BatInt.ord ~bounds:(`u, `u) 0 = Some 0
   bounding_of_ord ~default_low:None ~default_high:None (fun x -> Some x) BatInt.ord ~bounds:(`c 0, `u) 0 = Some 0
   bounding_of_ord ~default_low:None ~default_high:None (fun x -> Some x) BatInt.ord ~bounds:(`o 0, `u) 0 = None
   bounding_of_ord ~default_low:None ~default_high:None (fun x -> Some x) BatInt.ord ~bounds:(`u, `c 0) 0 = Some 0
   bounding_of_ord ~default_low:None ~default_high:None (fun x -> Some x) BatInt.ord ~bounds:(`u, `o 0) 0 = None

   bounding_of_ord ~default_low:(Some ~-10) ~default_high:(Some 10) (fun x -> Some x) BatInt.ord ~bounds:(`u, `u) 0 = Some 0
   bounding_of_ord ~default_low:(Some ~-10) ~default_high:(Some 10) (fun x -> Some x) BatInt.ord ~bounds:(`c 0, `u) 0 = Some 0
   bounding_of_ord ~default_low:(Some ~-10) ~default_high:(Some 10) (fun x -> Some x) BatInt.ord ~bounds:(`o 0, `u) 0 = Some ~-10
   bounding_of_ord ~default_low:(Some ~-10) ~default_high:(Some 10) (fun x -> Some x) BatInt.ord ~bounds:(`u, `c 0) 0 = Some 0
   bounding_of_ord ~default_low:(Some ~-10) ~default_high:(Some 10) (fun x -> Some x) BatInt.ord ~bounds:(`u, `o 0) 0 = Some 10
*)

let bounding_of_ord_chain ~low ~high conv ord =
  fun ~(bounds : 'a bound_t * 'a bound_t) ->
    match bounds with
    (* Closed bounds (inclusive) *)
    | `c l, `c u -> begin
        if ord l u = O.Gt then raise Invalid_bounds;
        fun x ->
          match ord x l, ord x u with
          | O.Lt, _ -> low x
          | _, O.Gt -> high x
          | O.Eq, _
          | _, O.Eq
          | O.Gt, _ -> conv x
      end
    | `u, `c u -> begin
        fun x ->
          match ord x u with
          | O.Gt -> high x
          | O.Eq
          | O.Lt -> conv x
      end
    | `c l, `u -> begin
        fun x ->
          match ord x l with
          | O.Lt -> low x
          | O.Gt
          | O.Eq -> conv x
      end
    (* Open bounds (exclusive) *)
    | `o l, `o u -> begin
        if ord l u = O.Gt then raise Invalid_bounds;
        fun x ->
          match ord x l, ord x u with
          | O.Lt, _
          | O.Eq, _ -> low x
          | _, O.Gt
          | _, O.Eq -> high x
          | O.Gt, _ -> conv x
      end
    | `u, `o u -> begin
        fun x ->
          match ord x u with
          | O.Gt
          | O.Eq -> high x
          | O.Lt -> conv x
      end
    | `o l, `u -> begin
        fun x ->
          match ord x l with
          | O.Lt
          | O.Eq -> low x
          | O.Gt -> conv x
      end
    (* Mixed open and closed bounds *)
    | `c l, `o u -> begin
        if ord l u = O.Gt then raise Invalid_bounds;
        fun x ->
          match ord x l, ord x u with
          | O.Lt, _ -> low x
          | _, O.Gt
          | _, O.Eq -> high x
          | O.Eq, _
          | O.Gt, _ -> conv x
      end
    | `o l, `c u -> begin
        if ord l u = O.Gt then raise Invalid_bounds;
        fun x ->
          match ord x l, ord x u with
          | O.Lt, _
          | O.Eq, _ -> low x
          | _, O.Gt -> high x
          | _, O.Eq
          | O.Gt, _ -> conv x
      end
    | `u, `u -> conv

(*$T bounding_of_ord_chain as f
   f (fun x -> Some x) BatInt.ord ~low:(const None) ~high:(const None) ~bounds:(`u, `u) 0 = Some 0
   f (fun x -> Some x) BatInt.ord ~low:(const None) ~high:(const None) ~bounds:(`c 0, `u) 0 = Some 0
   f (fun x -> Some x) BatInt.ord ~low:(const None) ~high:(const None) ~bounds:(`o 0, `u) 0 = None
   f (fun x -> Some x) BatInt.ord ~low:(const None) ~high:(const None) ~bounds:(`u, `c 0) 0 = Some 0
   f (fun x -> Some x) BatInt.ord ~low:(const None) ~high:(const None) ~bounds:(`u, `o 0) 0 = None

   f (fun x -> Some x) ~low:(fun _x -> Some ~-10) ~high:(fun _x -> Some 10) BatInt.ord ~bounds:(`u, `u) 0 = Some 0
   f (fun x -> Some x) ~low:(fun _x -> Some ~-10) ~high:(fun _x -> Some 10) BatInt.ord ~bounds:(`c 0, `u) 0 = Some 0
   f (fun x -> Some x) ~low:(fun _x -> Some ~-10) ~high:(fun _x -> Some 10) BatInt.ord ~bounds:(`o 0, `u) 0 = Some ~-10
   f (fun x -> Some x) ~low:(fun _x -> Some ~-10) ~high:(fun _x -> Some 10) BatInt.ord ~bounds:(`u, `c 0) 0 = Some 0
   f (fun x -> Some x) ~low:(fun _x -> Some ~-10) ~high:(fun _x -> Some 10) BatInt.ord ~bounds:(`u, `o 0) 0 = Some 10
*)

let saturate_of_ord ~(bounds : 'a bound_t * 'a bound_t) ord =
  match bounds with
  | `o l, `o h
  | `c l, `c h
  | `o l, `c h
  | `c l, `o h ->
    bounding_of_ord_chain
      ~low:(const l) ~high:(const h) identity ord ~bounds
  | `u, `o h
  | `u, `c h ->
    bounding_of_ord_chain ~low:identity ~high:(const h) identity ord ~bounds
  | `o l, `u
  | `c l, `u ->
    bounding_of_ord_chain ~low:(const l) ~high:identity identity ord ~bounds
  | `u, `u ->
    bounding_of_ord_chain ~low:identity ~high:identity identity ord ~bounds

let opt_of_ord ~(bounds : 'a bound_t * 'a bound_t) ord =
  bounding_of_ord_chain ~low:ret_none ~high:ret_none ret_some ord ~bounds

module type BoundedType = sig
  type base_t
  type t
  val bounds : base_t bound_t * base_t bound_t
  val bounded : (base_t, t) bounding_f
  val base_of_t : t -> base_t option
  val base_of_t_exn : t -> base_t
end

module type BoundedNumericType = sig
  include BoundedType
  module Infix : BatNumber.Infix with type bat__infix_t := base_t
end

module type S = sig
  type base_u
  type u
  type t = private u
  val bounds : base_u bound_t * base_u bound_t
  val make : base_u -> t
  external extract : t -> u = "%identity"
  val map : (base_u -> base_u) -> t -> t option
  val map2 : (base_u -> base_u -> base_u) -> t -> t -> t option
  val map_exn : (base_u -> base_u) -> t -> t
  val map2_exn : (base_u -> base_u -> base_u) -> t -> t -> t
end

module type NumericSig = sig
  include S

  val ( + ) : t -> base_u -> t
  val ( - ) : t -> base_u -> t
  val ( * ) : t -> base_u -> t
  val ( / ) : t -> base_u -> t
  val ( +: ) : t -> t -> t
  val ( -: ) : t -> t -> t
  val ( *: ) : t -> t -> t
  val ( /: ) : t -> t -> t
end

module Make(M : BoundedType) : (
  S with type base_u = M.base_t with type u = M.t with type t = private M.t
) = struct
  include M
  type base_u = base_t
  type u = t
  let make = bounded ~bounds
  external extract : t -> u = "%identity"
  let map f x =
    BatOption.map make (BatOption.map f (base_of_t x))
  let map2 f x y =
    match base_of_t x, base_of_t y with
    | Some bx, Some by ->
      Some (make (f bx by))
    | None, Some _
    | Some _, None
    | None, None ->
      None
  let map_exn f x =
    make (f (base_of_t_exn x))
  let map2_exn f x y =
    let bx = base_of_t_exn x in
    let by = base_of_t_exn y in
    make (f bx by)
end

module MakeNumeric(M : BoundedNumericType) = struct
  include Make(M)
  module I = M.Infix

  let ( + ) a b = map_exn (I.( + ) b) a
  let ( - ) a b = map_exn (I.( - ) b) a
  let ( * ) a b = map_exn (I.( * ) b) a
  let ( / ) a b = map_exn (I.( / ) b) a
  let ( +: ) = map2_exn I.( + )
  let ( -: ) = map2_exn I.( - )
  let ( *: ) = map2_exn I.( * )
  let ( /: ) = map2_exn I.( / )
end
