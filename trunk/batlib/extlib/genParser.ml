module ExceptionBased = struct
  exception Rejected

  type ('a, 'b) t = 'a Enum.t -> 'b

  let singleton x e =
    match Enum.get e with
      | Some v when v = x -> v
      | _ -> raise Rejected

	  
  val exact_sequence e e' =
    Enum.fold2
end
