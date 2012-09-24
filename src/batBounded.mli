(** Bounded values

    This module implements values which must fall within given bounds.

    @author Hezekiah M. Carty

    @since 2.0
*)

type 'a bound_t = [ `o of 'a | `c of 'a | `u]
(** [`o]pen or [`c]losed or [`u]nbounded bounds *)

type ('a, 'b) bounding_f = bounds:('a bound_t * 'a bound_t) -> 'a -> 'b
(** The type of a bounding function with limits specified by [bounds] *)

val bounding_of_ord :
  ?default_low:'a ->
  ?default_high:'a ->
  ('a -> 'a -> BatOrd.order) -> ('a, 'a option) bounding_f
(** [bounding_of_ord ?default_low ?default_high ord] will returning a bounding
    function using [ord] for value comparison and [default_low] and
    [default_high] for values which fall outside of the requested range.  If
    no default out of range values are provided, the resulting function will
    return [None] for out of range inputs. *)

val bounding_of_ord_chain :
  ?low:('a -> 'a option) ->
  ?high:('a -> 'a option) ->
  ('a -> 'a -> BatOrd.order) -> ('a, 'a option) bounding_f
(** [bounding_oF_ord_chain ?low ?high ord] is like {!bounding_of_ord} except
    that functions are used to handle out of range values rather than single
    default values.

    @param low defaults to returning [None] for out of range values
    @param high defaults to returning [None] for out of range values *)

val saturate_of_ord :
  bounds:('a bound_t * 'a bound_t) -> 
  ('a -> 'a -> BatOrd.order) -> 'a -> 'a
(** [saturate_of_ord ~bounds:(low, high) ord] will returning a bounding
    function using [ord] for value com parison and [low] and [high] for values
    which fall outside of the requested range. *)

val opt_of_ord :
  bounds:('a bound_t * 'a bound_t) -> 
  ('a -> 'a -> BatOrd.order) -> 'a -> 'a option
(** [opt_of_ord ~bounds:(low, high) ord] will returning a bounding function
    using [ord] for value comparison a nd [None] for values which fall outside
    of the requested range. *)

module type BoundedType = sig
  type base_t
  (** The base/raw type *)

  type t 
  (** The type that makes up the bounded range *)

  val bounds : base_t bound_t * base_t bound_t
  (** [bounds] defines the [(min, max)] bounds for the bounded range *)

  val bounded : (base_t, t) bounding_f
  (** [bounded ~bounds x] returns a bounded {!t} value derived from [x]. *)

  val base_of_t : t -> base_t option
  val base_of_t_exn : t -> base_t
  val map : (base_t -> base_t) -> t -> t
  val map2 : (base_t -> base_t -> base_t) -> t -> t -> t
end

module type S = sig
  type base_u
  (** Raw unbounded type *)

  type u
  (** {!base_u} after bounding constraints have been applied *)

  type t = private u
  (** Private version of {!u} to avoid construction of {!t} values without
      using [make] below. *)

  val bounds : base_u bound_t * base_u bound_t
  (** [bounds] defines the [(min, max)] bounds for the bounded range *)

  val make : base_u -> t
  (** [make x] will return a value of type {!t} derived from [x]. *)

  external extract : t -> u = "%identity"
  (** [extract x] will return [x] as a value of type {!u}.  A similar result
      could be achieved with [(x :> u)] *)

  val map : (base_u -> base_u) -> t -> t option
  val map2 : (base_u -> base_u -> base_u) -> t -> t -> t option
  val map_exn : (base_u -> base_u) -> t -> t
  val map2_exn : (base_u -> base_u -> base_u) -> t -> t -> t
end

module Make : functor (M : BoundedType) ->
  S with type base_u = M.base_t
    with type u = M.t
    with type t = private M.t
(** Functor to build an implementation of a bounded type given the bounded
    values definition [M] *)

