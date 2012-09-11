(** Bounded values

    This module implements values which must fall within given bounds.  All
    bounds are considered to be inclusive.

    @author Hezekiah M. Carty

    @since 2.0
*)

type 'a bound_t = [ `o of 'a | `c of 'a ]
(** [`o]pen or [`c]losed bounds *)

type 'a bounding_f = ?min:'a bound_t -> ?max:'a bound_t -> 'a -> 'a option
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

  val min : t bound_t option
  (** [min] is the minimum value in the bounded range *)

  val max : t bound_t option
  (** [max] is the maximum value in the bounded range *)

  val bounded : t bounding_f
  (** [bounded ?min ?max x] returns a bounded {!t} value if [x] falls into the
      given range *)
end

module type S = sig
  type u
  (** Raw bounded type *)

  type t = private u
  (** Private version of {!u} to avoid construction of {!t} values without
      using [make] or [make_exn] below. *)

  exception Out_of_range
  (** Exception to indicate that a requested value falls outside of the
      defined boundaries *)

  val min : t bound_t option
  (** [min] is the minimum value in the bounded range *)

  val max : t bound_t option
  (** [max] is the maximum value in the bounded range *)

  val make : u -> t option
  (** [make x] will return [Some x] if [x] falls within the bounds defined by
      [min] and [max]. *)

  val make_exn : u -> t
  (** [make x] will [x] if [x] falls within the bounds defined by [min] and
      [max].

      @raise Out_of_range if [x] is outside of the range defined by {!min} and
             {!max} *)
end

module Make : functor (M : BoundedType) -> S with type u = M.t
(** Functor to build an implementation of a bounded type given the bounded
    values definition [M] *)

