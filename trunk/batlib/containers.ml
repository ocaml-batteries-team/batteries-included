(** The containers library *)

(** {1 Generic containers} *)

(** {2 Persistent containers} *)
module Map            = Map
module MapLabels      = MoreLabels.Map
module PMap           = PMap
module LazyList       = LazyList
module LazyListLabels = LazyListLabels
module List           = ExtList.List
(*TODO: ListLabels*)

module Set            = Set
(*TODO: ExtSet.Set*)
(*TODO: ExtSet.SetLabels*)
module Vect           = Vect
(*TODO: VectLabels*)

(** {2 Mutable containers} *)
module Array          = ExtArray.Array
(*TODO: ArrayLabels*)
(*TODO: Bigarray*)
module Dllist         = Dllist
module DynArray       = DynArray
(*TODO: DynArrayLabels*)
module Enum           = Enum
module EnumLabels     = EnumLabels
module Hashtbl        = ExtHashtbl.Hashtbl
(*TODO: HashtblLabels*)

module Queue          = Queue
(*TODO: ExtQueue.Queue*)
(*TODO: ExtQueue.QueueLabels*)
module RefList        = RefList
module Stack          = Stack
(*TODO: ExtStack.Stack*)
(*TODO: ExtStack.StackLabels*)
module Stream         = ExtStream.Stream
module StreamLabels   = ExtStream.StreamLabels



(** {1 Specialized containers} *)

module Buffer         = Buffer
module Option         = Option
module Result         = Result

module Global         = Global
module Ref            = Ref
module Lazy           = Lazy


