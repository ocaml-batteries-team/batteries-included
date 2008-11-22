(** A signature for data structures which may be converted to [enum].

    If you create a new data structure, you should make it compatible
    with [Enumerable].
*)
module type Enumerable = sig
  type container (**The data structure, e.g. [string]*)
  type contents  (**The contents of the data structure, e.g. [char]*)
  val enum : container -> contents Enum.t
end

(** A signature for data structures which may be converted to [enum].

    If you create a new data structure, if possible, you should make
    it compatible with [Of_enum].
*)
module type Of_enum = sig
  type container (**The data structure, e.g. [string]*)
  type contents  (**The contents of the data structure, e.g. [char]*)
  val of_enum : contents Enum.t -> container
end

module type OrderedType =
sig
  type t
    (** The type of the set elements. *)
  val compare : t -> t -> int
    (** A total ordering function over the set elements.
        This is a two-argument function [f] such that
        [f e1 e2] is zero if the elements [e1] and [e2] are equal,
        [f e1 e2] is strictly negative if [e1] is smaller than [e2],
        and [f e1 e2] is strictly positive if [e1] is greater than [e2].
        Example: a suitable ordering function is the generic structural
        comparison function {!Pervasives.compare}. *)
end
