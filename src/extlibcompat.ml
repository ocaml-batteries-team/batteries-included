module ExtArray = struct 
  module Array = struct include Array include BatArray end 
end
module ExtHashtbl = struct 
  module Hashtbl = BatHashtbl 
end
module ExtList = struct
  module List = struct include List include BatList end
end
module ExtString = struct
  module String = struct include String include BatString end
end
