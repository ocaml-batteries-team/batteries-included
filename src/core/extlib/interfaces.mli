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
