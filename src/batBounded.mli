(** Bounded values

    This module implements values which must fall within given bounds.

    @author Hezekiah M. Carty

    @since 2.0
*)

type 'a bound_t = [ `o of 'a | `c of 'a | `u]
(** [`o]pen or [`c]losed or [`u]nbounded bounds *)

type 'a bounding_f = bounds:('a bound_t * 'a bound_t) -> 'a -> 'a option
(** The type of a bounding function with limits of [min] and [max] *)

val bounding_of_ord :
  ?default_low:'a ->
  ?default_high:'a ->
  ('a -> 'a -> BatOrd.order) -> 'a bounding_f
(** [bounding_of_ord ?default_low ?default_high ord] will returning a bounding
    function using [ord] for value comparison and [default_low] and
    [default_high] for values which fall outside of the requested range.  If
    no default out of range values are provided, the resulting function will
    return [None] for out of range inputs. *)

val bounding_of_ord_chain :
  ?low:('a -> 'a option) ->
  ?high:('a -> 'a option) ->
  ('a -> 'a -> BatOrd.order) -> 'a bounding_f
(** [bounding_oF_ord_chain ?low ?high ord] is like {!bounding_of_ord} except
    that functions are used to handle out of range values rather than single
    default values.

    @param low defaults to returning [None] for out of range values
    @param high defaults to returning [None] for out of range values *)

module type BoundedType = sig
  type t
  (** The type that makes up the bounded range *)

  val bounds : t bound_t * t bound_t
  (** [bounds] defines the [(min, max)] bounds for the bounded range *)

  val bounded : t bounding_f
  (** [bounded ~bounds x] returns a bounded {!t} value if [x] falls into the
      given range *)
end

module type S = sig
  type u
  (** Raw unbounded type *)

  type t = private u
  (** Private version of {!u} to avoid construction of {!t} values without
      using [make] or [make_exn] below. *)

  exception Out_of_range
  (** Exception to indicate that a requested value falls outside of the
      defined boundaries *)

  val bounds : t bound_t * t bound_t
  (** [bounds] defines the [(min, max)] bounds for the bounded range *)

  val make : u -> t option
  (** [make x] will return [Some x] if [x] falls within the bounds defined by
      {!bounds}. *)

  val make_exn : u -> t
  (** [make x] will [x] if [x] falls within the bounds defined by {!bounds}.

      @raise Out_of_range if [x] is outside of the range defined by {!bounds}
      *)
end

module Make : functor (M : BoundedType) -> S with type u = M.t
(** Functor to build an implementation of a bounded type given the bounded
    values definition [M] *)

