(** The containers library *)

(** {1 Generic containers} *)

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
module Map            = Map
module MapLabels      = MoreLabels.Map
module PMap           = PMap
module LazyList       = LazyList
module LazyListLabels = LazyListLabels
module List           = ExtList.List
(*TODO: ListLabels*)
module Queue          = Queue
(*TODO: ExtQueue.Queue*)
(*TODO: ExtQueue.QueueLabels*)
module RefList        = RefList
module Set            = Set
(*TODO: ExtSet.Set*)
(*TODO: ExtSet.SetLabels*)
module Stack          = Stack
(*TODO: ExtStack.Stack*)
(*TODO: ExtStack.StackLabels*)
module Stream         = ExtStream.Stream
module StreamLabels   = ExtStream.StreamLabels
module Vect           = Vect
(*TODO: VectLabels*)


(** {1 Specialized containers} *)

module Buffer         = Buffer
module Option         = Option
module Result         = Result

module Global         = Global
module Ref            = Ref
module Lazy           = Lazy


