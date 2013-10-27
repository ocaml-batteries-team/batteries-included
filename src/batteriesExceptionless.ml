(* open this to extend all Foo with BatFoo and BatFoo.Exceptionless if available *)

include (Batteries :
           module type of Batteries
         with module Array := Batteries.Array
          and module Hashtbl := Batteries.Hashtbl
          and module List := Batteries.List
          and module Map := Batteries.Map
          and module Queue := Batteries.Queue
          and module Stack := Batteries.Stack
          and module String := Batteries.String
          and module Enum := Batteries.Enum
          and module LazyList := Batteries.LazyList
          and module Seq := Batteries.Seq
          and module Splay := Batteries.Splay
        )

module Array = struct
  include (BatArray :
             module type of BatArray
           with module Labels := BatArray.Labels
            and module Cap := BatArray.Cap
          )
  include BatArray.Exceptionless
  module Labels = struct
    include BatArray.Labels
    include BatArray.Labels.LExceptionless
  end
  module Cap = struct
    include BatArray.Cap
    include BatArray.Cap.Exceptionless
  end
end
module Hashtbl = struct
  include BatHashtbl
  include BatHashtbl.Exceptionless
  (* TODO *)
end
module List = struct
  include (BatList :
             module type of BatList
           with module Labels := BatList.Labels
          )
  include BatList.Exceptionless
  module Labels = struct
    include BatList.Labels
    include BatList.Labels.LExceptionless
  end
end
module Map = struct
  include (BatMap :
             module type of BatMap
           with module PMap := BatMap.PMap
          )
  include Exceptionless
  module PMap = struct
    include BatMap.PMap
    include BatMap.PMap.Exceptionless
  end
  (* TODO *)
end
module Queue = struct
  include BatQueue
  include BatQueue.Exceptionless
end
(* module Set = BatSet (* TODO *) *)
module Stack = struct
  include BatStack
  include BatStack.Exceptionless
end
module String = struct
  include (BatString :
             module type of BatString
           with module Cap := BatString.Cap
          )
  include BatString.Exceptionless
  module Cap = struct
    include BatString.Cap
    include BatString.Cap.Exceptionless
  end
end

(* Extlib modules not replacing stdlib *)
module Enum = struct
  include (BatEnum :
             module type of Batteries.Enum
           with module Labels := Batteries.Enum.Labels
          )
  include BatEnum.Exceptionless
  module Labels = struct
    include BatEnum.Labels
    include BatEnum.Labels.LExceptionless
  end
end
module LazyList = struct
  include (BatLazyList :
             module type of Batteries.LazyList
           with module Labels := Batteries.LazyList.Labels
          )
  include BatLazyList.Exceptionless
  module Labels = struct
    include BatLazyList.Labels
    include BatLazyList.Labels.Exceptionless
  end
end

(* Batteries specific modules *)
module Seq = struct
  include BatSeq
  include BatSeq.Exceptionless
end
module Splay = struct
  include (BatSplay :
             module type of BatSplay
           with module Map := BatSplay.Map
          )
  module Map (Ord : BatInterfaces.OrderedType) = struct
    include BatSplay.Map(Ord)
    include Exceptionless
  end
end
