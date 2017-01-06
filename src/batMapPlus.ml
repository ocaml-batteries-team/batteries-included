open BatPervasives
module Enum = BatEnum
module List = BatList
module Opt = BatOption

module type ORD = BatInterfaces.OrderedType

(** adds some missing functions to BatMap.Make *)
module Make (Key : ORD) = struct

  include BatMap.Make (Key)

  let diff : 'a t -> 'b t -> 'a t = fun m1 m2 ->
    let by = fun _ x y -> match x, y with Some x, None -> Some x | _ -> None
    in merge by m1 m2

  let union : 'a. ?zero:'a -> ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t =
    fun ?zero f -> merge @@ match zero with
      | Some zero -> let d = Opt.default zero in fun _ l r -> Some (f (d l) (d r))
      | None -> fun _ l r -> match l, r with
        | Some l, Some r -> Some (f l r)
        | None, r -> r
        | l, None -> l

  let union_by : 'a 'b. zero:'a -> ('a -> 'a -> 'b) -> 'a t -> 'a t -> 'b t =
    fun ~zero f -> merge @@ let d = Opt.default zero in fun _ l r -> Some (f (d l) (d r))

  let unions : 'a. ?zero:'a -> ('a -> 'a -> 'a) -> 'a t Enum.t -> 'a t =
    fun ?zero f -> BatPervasives.fold (union ?zero f) empty

  let of_enum_by : 'a. ?zero:'a -> ('a -> 'a -> 'a) -> (Key.t * 'a) Enum.t -> 'a t =
    fun ?zero f -> unions ?zero f % Enum.map (uncurry singleton)

  let intersecti : (Key.t -> 'a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t =
    fun f m1 m2 ->
      let merge_fun k a b = match a, b with
        | Some v1, Some v2 -> Some (f k v1 v2)
        | None, _ | _, None -> None
      in merge merge_fun m1 m2

  let intersect : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t =
    fun f m1 m2 -> intersecti (const f) m1 m2

  let intersect_left : 'a t -> 'b t -> 'a t = fun m1 m2 -> intersect const m1 m2

  let of_list : 'a. (Key.t * 'a) list -> 'a t = fun l -> of_enum @@ List.enum l

  let list : 'a. 'a t -> (Key.t * 'a) list = fun m -> List.of_enum @@ enum m

  let dump : 'a. 'a t -> string = fun v -> enum v |>
    Enum.fold (fun acc str -> acc ^ "  " ^ str) "" %
    Enum.map (fun (dim, v) -> dump dim ^ ":" ^ dump v)

end
