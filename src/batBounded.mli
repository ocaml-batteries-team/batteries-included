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
  default_low:'b ->
  default_high:'b ->
  ('a -> 'b) ->
  ('a -> 'a -> BatOrd.order) -> ('a, 'b) bounding_f
(** [bounding_of_ord ~default_low ~default_high conv ord] will returning a
    bounding function using [ord] for value comparison and [default_low] and
    [default_high] for values which fall outside of the requested range.
    [conv] is used to convert values which are in-range to the result type. *)

val bounding_of_ord_chain :
  low:('a -> 'b) ->
  high:('a -> 'b) ->
  ('a -> 'b) ->
  ('a -> 'a -> BatOrd.order) -> ('a, 'b) bounding_f
(** [bounding_oF_ord_chain ?low ?high ord] is like {!bounding_of_ord} except
    that functions are used to handle out of range values rather than single
    default values. *)

val saturate_of_ord :
  bounds:('a bound_t * 'a bound_t) -> 
  ('a -> 'a -> BatOrd.order) -> 'a -> 'a
(** [saturate_of_ord ~bounds:(low, high) ord] will returning a bounding
    function using [ord] for value comparison and [low] and [high] for values
    which fall outside of the requested range. *)

val opt_of_ord :
  bounds:('a bound_t * 'a bound_t) -> 
  ('a -> 'a -> BatOrd.order) -> 'a -> 'a option
(** [opt_of_ord ~bounds:(low, high) ord] will returning a bounding function
    using [ord] for value comparison and [None] for values which fall outside
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
  (** [base_of_t x] converts a value of type {!t} back to a {!base_t} if
      possible. *)

  val base_of_t_exn : t -> base_t
    (** [base_of_t_exn x] converts a value of type {!t} back to a {!base_t}.  If
        a conversion is not possible then an exception will be raised. *)
end

module type BoundedNumericType = sig
  include BoundedType
  module Infix : BatNumber.Infix with type bat__infix_t := base_t
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
  (** [map f x] applies [f] to [x].  Returns [Some y] if [x] can be converted
      back to type {!base_u}, otherwise returns [None]. *)

  val map2 : (base_u -> base_u -> base_u) -> t -> t -> t option
  (** [map f x y] applies [f] to [x] and [y].  Returns [Some z] if [x] and [y]
      can be converted back to type {!base_u}, otherwise returns [None]. *)

  val map_exn : (base_u -> base_u) -> t -> t
  (** [map_exn f x] applies [f] to [x].  Returns [y] if [x] can be converted
      back to type {!base_u}, otherwise raise an exception. *)

  val map2_exn : (base_u -> base_u -> base_u) -> t -> t -> t
    (** [map f x y] applies [f] to [x] and [y].  Returns [z] if [x] and [y]
        can be converted back to type {!base_u}, otherwise raise an exception. *)
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

module Make : functor (M : BoundedType) ->
  S with type base_u = M.base_t
  with type u = M.t
  with type t = private M.t
(** Functor to build an implementation of a bounded type given the bounded
    values definition [M] *)

module MakeNumeric : functor (M : BoundedNumericType) ->
  NumericSig with type base_u = M.base_t
  with type u = M.t
  with type t = private M.t

