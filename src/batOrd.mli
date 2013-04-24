
type order = Lt | Eq | Gt
(** An algebraic datatype for ordering.

    Traditional OCaml code, under the influence of C comparison
    functions, has used int-returning comparisons (< 0, 0 or >
    0). Using an algebraic datatype instead is actually nicer, both
    for comparison producers (no arbitrary choice of a positive and
    negative value) and consumers (nice pattern-matching elimination).
*)

type 'a ord = 'a -> 'a -> order
(** The type of ordering functions returning an [order] variant. *)

type 'a comp = 'a -> 'a -> int
(** The legacy int-returning comparisons :
    - compare a b < 0 means a < b
    - compare a b = 0 means a = b
    - compare a b > 0 means a > b
*)

module type Comp = sig type t val compare : t comp end
(** We use [compare] as member name instead of [comp], so that the
    Comp modules can be used as the legacy OrderedType interface. *)

module type Ord = sig type t val ord : t ord end

val ord0 : int -> order
val ord : 'a comp -> 'a ord
(** Returns a variant ordering from a legacy comparison *)
module Ord : functor (Comp : Comp) -> Ord with type t = Comp.t

val comp0 : order -> int
val comp : 'a ord -> 'a comp
(** Returns an legacy comparison from a variant ordering *)
module Comp : functor (Ord : Ord) -> Comp with type t = Ord.t

val poly_comp : 'a comp
val poly_ord : 'a ord
val poly : 'a ord
(** Polymorphic comparison functions, based on the
    [Pervasives.compare] function from inria's stdlib, have
    polymorphic types: they claim to be able to compare values of any
    type. In practice, they work for only some types, may fail on
    function types and may not terminate on cyclic values.

    They work by runtime magic, inspecting the values in an untyped
    way. While being an useful hack for base types and simple
    composite types (say [(int * float) list], they do not play well
    with functions, type abstractions, and structures that would need
    a finer notion of equality/comparison. For example, if one
    represent sets as balanced binary tree, one may want set with
    equal elements but different balancings to be equal, which would
    not be the case using the polymorphic equality function.

    When possible, you should therefore avoid relying on these
    polymorphic comparison functions. You should be especially careful
    if your data structure may later evolve to allow cyclic data
    structures or functions.
*)

val rev_ord0 : order -> order
val rev_comp0 : int -> int
val rev_ord : 'a ord -> 'a ord
val rev_comp : 'a comp -> 'a comp
val rev : 'a ord -> 'a ord
(** Reverse a given ordering. If [Int.ord] sorts integer by increasing
    order, [rev Int.ord] will sort them by decreasing order. *)

module RevOrd (Ord : Ord) : Ord with type t = Ord.t
module RevComp (Comp : Comp) : Comp with type t = Comp.t
module Rev (Ord : Ord) : Ord with type t = Ord.t

type 'a eq = 'a -> 'a -> bool
(** The type for equality function.

    All ordered types also support equality, as equality can be
    derived from ordering. However, there are also cases where
    elements may be compared for equality, but have no natural
    ordering. It is therefore useful to provide equality as an
    independent notion.
*)

val eq_ord0 : order -> bool
val eq_comp0 : int -> bool
val eq_ord : 'a ord -> 'a eq
val eq_comp : 'a comp -> 'a eq
val eq : 'a ord -> 'a eq
(** Derives an equality function from an ordering function. *)


module type Eq = sig type t val eq : t eq end
module EqOrd (Ord : Ord) : Eq with type t = Ord.t
module EqComp (Comp : Comp) : Eq with type t = Comp.t
module Eq (Ord : Ord) : Eq with type t = Ord.t

type 'a choice = 'a -> 'a -> 'a
(** choice functions, see [min] and [max]. *)

val min_ord : 'a ord -> 'a choice
val max_ord : 'a ord -> 'a choice
val min_comp : 'a comp -> 'a choice
val max_comp : 'a comp -> 'a choice
val min : 'a ord -> 'a choice
(** [min ord] will choose the smallest element, according to [ord].
    For example, [min Int.ord 1 2] will return [1].

    {[
      (* the minimum element of a list *)
      let list_min ord = List.reduce (min ord)
    ]}
*)

val max : 'a ord -> 'a choice
(** [max ord] will choose the biggest element according to [ord]. *)

val bin_comp : 'a comp -> 'a -> 'a -> 'b comp -> 'b -> 'b -> int
val bin_ord : 'a ord -> 'a -> 'a -> 'b ord -> 'b -> 'b -> order
(** binary lifting of the comparison function, using lexicographic order:
    [bin_ord ord1 v1 v1' ord2 v2 v2'] is [ord2 v2 v2'] if [ord1 v1 v1' = Eq],
    and [ord1 v1 v1'] otherwhise.
*)
val bin_eq : 'a eq -> 'a -> 'a -> 'b eq -> 'b -> 'b -> bool

val map_eq : ('a -> 'b) -> 'b eq -> 'a eq
val map_comp : ('a -> 'b) -> 'b comp -> 'a comp
val map_ord : ('a -> 'b) -> 'b ord -> 'a ord
(** These functions extend an existing equality/comparison/ordering to
    a new domain through a mapping function.  For example, to order
    sets by their cardinality, use [map_ord Set.cardinal Int.ord].
    The input of the mapping function is the type you want to compare,
    so this is the reverse of [List.map]. *)

module Incubator : sig
  val eq_by : ('a -> 'b) -> 'a eq
  val comp_by : ('a -> 'b) -> 'a comp
  val ord_by : ('a -> 'b) -> 'a ord
    (** Build a [eq], [cmp] or [ord] function from a projection function.
        For example, if you wanted to compare integers based on their
        lowest 4 bits, you could write [let cmp_bot4 = cmp_by (fun x
        -> x land 0xf)] and use cmp_bot4 as the desired integer
        comparator. *)
end
