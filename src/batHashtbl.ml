(* 
 * ExtHashtbl, extra functions over hashtables.
 * Copyright (C) 1996 Xavier Leroy
 *               2003 Nicolas Cannasse
 *               2005 Damien Doligez
 *               2009 David Rajchenbach-Teller, LIFO, Universite d'Orleans
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version,
 * with the special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)
 

    (** {6 Import the contents of {!Hashtbl}}

	Note: We can't directly [include Hashtbl] as this
	would cause a collision on [Make]*)
    type ('a, 'b) t = ('a, 'b) Hashtbl.t
    let create s  = Hashtbl.create s
    let clear    = Hashtbl.clear
    let add      = Hashtbl.add
    let copy     = Hashtbl.copy
    let find     = Hashtbl.find
    let find_all = Hashtbl.find_all
    let mem      = Hashtbl.mem
    let remove   = Hashtbl.remove
    let replace  = Hashtbl.replace
    let iter     = Hashtbl.iter
    let fold     = Hashtbl.fold
    let length   = Hashtbl.length
    let hash     = Hashtbl.hash
    external hash_param : int -> int -> 'a -> int = "caml_hash_univ_param" "noalloc"

    type ('a, 'b) h_bucketlist =
      | Empty
      | Cons of 'a * 'b * ('a, 'b) h_bucketlist
	  
    type ('a, 'b) h_t = {
      mutable size: int;
      mutable data: ('a, 'b) h_bucketlist array
    }

    external h_conv : ('a, 'b) t -> ('a, 'b) h_t = "%identity"
    external h_make : ('a, 'b) h_t -> ('a, 'b) t = "%identity"

    let resize hashfun tbl =
      let odata = tbl.data in
      let osize = Array.length odata in
      let nsize = min (2 * osize + 1) Sys.max_array_length in
	if nsize <> osize then begin
	  let ndata = Array.create nsize Empty in
	  let rec insert_bucket = function
              Empty -> ()
	    | Cons(key, data, rest) ->
		insert_bucket rest; (* preserve original order of elements *)
		let nidx = (hashfun key) mod nsize in
		  ndata.(nidx) <- Cons(key, data, ndata.(nidx)) in
	    for i = 0 to osize - 1 do
	      insert_bucket odata.(i)
	    done;
	    tbl.data <- ndata;
	end
      
    let enum h =
      let rec make ipos ibuck idata icount =
	let pos = ref ipos in
	let buck = ref ibuck in
	let hdata = ref idata in
	let hcount = ref icount in
	let force() =
	  (** this is a hack in order to keep an O(1) enum constructor **)
	  if !hcount = -1 then begin
	    hcount := (h_conv h).size;
	    hdata := Array.copy (h_conv h).data;
	  end;
	in
	let rec next() =
	  force();
	  match !buck with
	    | Empty ->					
		if !hcount = 0 then raise BatEnum.No_more_elements;
		incr pos;
		buck := Array.unsafe_get !hdata !pos;
		next()
	    | Cons (k,i,next_buck) ->
		buck := next_buck;
		decr hcount;
		(k,i)
	in
	let count() =
	  if !hcount = -1 then (h_conv h).size else !hcount
	in
	let clone() =
	  force();
	  make !pos !buck !hdata !hcount
	in
	  BatEnum.make ~next ~count ~clone
      in		
	make (-1) Empty (Obj.magic()) (-1)
	  
    let keys h =
      BatEnum.map (fun (k,_) -> k) (enum h)
	
    let values h =
      BatEnum.map (fun (_,v) -> v) (enum h)
	
    let map f h =
      let rec loop = function
	| Empty -> Empty
	| Cons (k,v,next) -> Cons (k,f k v,loop next)
      in
	h_make {
	  size = (h_conv h).size;
	  data = Array.map loop (h_conv h).data; 
	}

    let remove_all h key =
      let hc = h_conv h in
      let rec loop = function
	| Empty -> Empty
	| Cons(k,v,next) ->
	    if k = key then begin
	      hc.size <- pred hc.size;
	      loop next
	    end else
	      Cons(k,v,loop next)
      in
      let pos = (hash key) mod (Array.length hc.data) in
	Array.unsafe_set hc.data pos (loop (Array.unsafe_get hc.data pos))
	  
    let find_default h key defval =
      let rec loop = function
	| Empty -> defval
	| Cons (k,v,next) ->
	    if k = key then v else loop next
      in
      let pos = (hash key) mod (Array.length (h_conv h).data) in
	loop (Array.unsafe_get (h_conv h).data pos)
	  
    let find_option h key =
      let rec loop = function
	| Empty -> None
	| Cons (k,v,next) ->
	    if k = key then Some v else loop next
      in
      let pos = (hash key) mod (Array.length (h_conv h).data) in
	loop (Array.unsafe_get (h_conv h).data pos)
	  
    let of_enum e =
      let h = create (if BatEnum.fast_count e then BatEnum.count e else 0) in
	BatEnum.iter (fun (k,v) -> add h k v) e;
	h
	  
    let length h =
      (h_conv h).size
	
    let is_empty h = length h = 0

    let print ?(first="{\n") ?(last="\n}") ?(sep=",\n") print_k print_v out t =
      BatEnum.print ~first ~last ~sep (fun out (k,v) -> BatPrintf.fprintf out "%a: %a" print_k k print_v v) out (enum t)

    let filteri (f:'key -> 'a -> bool) (t:('key, 'a) t) =
      let result = create 16 in
	iter (fun k a -> if f k a then add result k a) t;
	result

    let filter  f t = filteri (fun k a -> f a) t

    let filter_map f t =
      let result = create 16 in
	iter (fun k a -> match f k a with
		| None   -> ()
		| Some v -> add result k v) t;
	result


    module Exceptionless =
    struct
      let find = find_option
    end

    module Infix =
    struct
      let (-->) h k = find h k
      let (<--) h (k,v) = add h k v
    end

    module Labels =
    struct

      let label f               = fun key data -> f ~key ~data
      let add e ~key ~data      = add e key data
      let replace e ~key ~data  = replace e key data
      let iter       ~f e       = iter (label f) e
      let map        ~f e       = map (label f) e
      let filter     ~f e       = filter f e
      let filteri    ~f e       = filteri (label f) e
      let filter_map ~f e       = filter_map (label f) e
      let fold       ~f e ~init = fold (label f) e init

    end

    module type HashedType = Hashtbl.HashedType

    module type S =
    sig
      type key
      type 'a t
      val create : int -> 'a t
      val length : 'a t -> int
      val is_empty : 'a t -> bool
      val clear : 'a t -> unit
      val copy : 'a t -> 'a t
      val add : 'a t -> key -> 'a -> unit
      val remove : 'a t -> key -> unit
      val remove_all : 'a t -> key -> unit
      val find : 'a t -> key -> 'a
      val find_all : 'a t -> key -> 'a list
      val find_default : 'a t -> key ->  'a -> 'a
      val find_option : 'a t -> key -> 'a option
      val replace : 'a t -> key -> 'a -> unit
      val mem : 'a t -> key -> bool
      val iter : (key -> 'a -> unit) -> 'a t -> unit
      val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
      val map : (key -> 'b -> 'c) -> 'b t -> 'c t
      val filter: ('a -> bool) -> 'a t -> 'a t
      val filteri: (key -> 'a -> bool) -> 'a t -> 'a t
      val filter_map: (key -> 'a -> 'b option) -> 'a t -> 'b t
      val keys : 'a t -> key BatEnum.t
      val values : 'a t -> 'a BatEnum.t
      val enum : 'a t -> (key * 'a) BatEnum.t
      val of_enum : (key * 'a) BatEnum.t -> 'a t
      val print :  ?first:string -> ?last:string -> ?sep:string -> 
	('a BatInnerIO.output -> key -> unit) -> 
	('a BatInnerIO.output -> 'b -> unit) -> 
	'a BatInnerIO.output -> 'b t -> unit

      (** Operations on {!Hashtbl} without exceptions.*)
      module Exceptionless :
      sig
	val find : 'a t -> key -> 'a option
      end
      
      (** Infix operators over a {!BatHashtbl} *)
      module Infix :
      sig
        val (-->) : 'a t -> key -> 'a
        (** [tbl-->x] returns the current binding of [x] in [tbl],
            or raises [Not_found] if no such binding exists.
            Equivalent to [Hashtbl.find tbl x]*)

        val (<--) : 'a t -> key * 'a -> unit
        (** [tbl<--(x, y)] adds a binding of [x] to [y] in table [tbl].
            Previous bindings for [x] are not removed, but simply
            hidden. That is, after performing {!Hashtbl.remove}[ tbl x],
            the previous binding for [x], if any, is restored.
            (Same behavior as with association lists.)
            Equivalent to [Hashtbl.add tbl x y]*)
      end

      (** Operations on {!Hashtbl} with labels.
	  
	  This module overrides a number of functions of {!Hashtbl} by
	  functions in which some arguments require labels. These labels are
	  there to improve readability and safety and to let you change the
	  order of arguments to functions. In every case, the behavior of the
	  function is identical to that of the corresponding function of {!Hashtbl}.
      *)
      module Labels :
      sig
	val add : 'a t -> key:key -> data:'a -> unit
	val replace : 'a t -> key:key -> data:'a -> unit
	val iter : f:(key:key -> data:'a -> unit) -> 'a t -> unit
	val map : f:(key:key -> data:'a -> 'b) -> 'a t -> 'b t
	val filter: f:('a -> bool) -> 'a t -> 'a t
	val filteri:f:(key:key -> data:'a -> bool) -> 'a t -> 'a t
	val filter_map:f:(key:key -> data:'a -> 'b option) -> 'a t -> 'b t
	val fold :
	  f:(key:key -> data:'a -> 'b -> 'b) ->
	  'a t -> init:'b -> 'b
      end

    end
      

    module Make(H: HashedType): (S with type key = H.t) =
    struct
      include Hashtbl.Make(H)
      external to_hash : 'a t -> (key, 'a) Hashtbl.t = "%identity"
      external of_hash : (key, 'a) Hashtbl.t -> 'a t = "%identity"

(*      type key = H.t
      type 'a hashtbl = (key, 'a) t
      type 'a t = 'a hashtbl
      let create = create
      let clear = clear
      let copy = copy
	
      let safehash key = (H.hash key) land max_int
	
      let add h key info =
	let h = h_conv h in
	let i = (safehash key) mod (Array.length h.data) in
	let bucket = Cons(key, info, h.data.(i)) in
	  h.data.(i) <- bucket;
	  h.size <- succ h.size;
	  if h.size > Array.length h.data lsl 1 then resize safehash h
	    
      let remove h key =
	let h = h_conv h in 
	let rec remove_bucket = function
            Empty ->
              Empty
          | Cons(k, i, next) ->
              if H.equal k key
              then begin h.size <- pred h.size; next end
              else Cons(k, i, remove_bucket next) in
	let i = (safehash key) mod (Array.length h.data) in
	  h.data.(i) <- remove_bucket h.data.(i)
	    
      let rec find_rec key = function
          Empty ->
            raise Not_found
	| Cons(k, d, rest) ->
            if H.equal key k then d else find_rec key rest
	      
      let find h key =
	let h = h_conv h in
	match h.data.((safehash key) mod (Array.length h.data)) with
            Empty -> raise Not_found
	  | Cons(k1, d1, rest1) ->
              if H.equal key k1 then d1 else
		match rest1 with
		    Empty -> raise Not_found
		  | Cons(k2, d2, rest2) ->
		      if H.equal key k2 then d2 else
			match rest2 with
			    Empty -> raise Not_found
			  | Cons(k3, d3, rest3) ->
			      if H.equal key k3 then d3 else find_rec key rest3
				
      let find_all h key =
	let rec find_in_bucket = function
            Empty ->
              []
	  | Cons(k, d, rest) ->
              if H.equal k key
              then d :: find_in_bucket rest
              else find_in_bucket rest in
	  find_in_bucket h.data.((safehash key) mod (Array.length h.data))
	    
      let replace h key info =
	let rec replace_bucket = function
            Empty ->
              raise Not_found
          | Cons(k, i, next) ->
              if H.equal k key
              then Cons(k, info, next)
              else Cons(k, i, replace_bucket next) in
	let i = (safehash key) mod (Array.length h.data) in
	let l = h.data.(i) in
	  try
            h.data.(i) <- replace_bucket l
	  with Not_found ->
            h.data.(i) <- Cons(key, info, l);
            h.size <- succ h.size;
            if h.size > Array.length h.data lsl 1 then resize safehash h

      let mem h key =
	let rec mem_in_bucket = function
	  | Empty ->
              false
	  | Cons(k, d, rest) ->
              H.equal k key || mem_in_bucket rest in
	  mem_in_bucket h.data.((safehash key) mod (Array.length h.data))*)
	    
      let iter = iter
      let fold = fold
      let length = length

      let enum h               = enum (to_hash h)
      let of_enum e            = of_hash (of_enum e)
      let values  h            = values (to_hash h)
      let keys h               = keys (to_hash h)
      let map (f:key -> 'a -> 'b) h              = of_hash (map f (to_hash h))
      let find_option h key    = find_option (to_hash h) key
      let find_default h key v = find_default (to_hash h) key v
      let remove_all h key     = remove_all (to_hash h) key
      let is_empty h           = length h = 0
      let print ?first ?last ?sep print_k print_v out t =
	print ?first ?last ?sep print_k print_v out (to_hash t)

      let filteri f t =
	let result = create 16 in
	  iter (fun k a -> if f k a then add result k a) t;
	  result

      let filter  f t = filteri (fun k a -> f a) t
	
      let filter_map f t =
	let result = create 16 in
	  iter (fun k a -> match f k a with
		  | None   -> ()
		  | Some v -> add result k v) t;
	  result

      module Labels =
      struct
	let label f              = fun key data -> f ~key ~data
	let add e ~key ~data     = add e key data
	let replace e ~key ~data = replace e key data
	let iter  ~f e           = iter (label f) e
	let map   ~f e           = map (label f) e
	let filter     ~f e      = filter f e
	let filteri    ~f e      = filteri (label f) e
	let filter_map ~f e      = filter_map (label f) e
	let fold  ~f e ~init     = fold (label f) e init
      end

      module Exceptionless =
      struct
	let find = find_option
      end

      module Infix =
      struct
        let (-->) h k = find h k
        let (<--) h (k,v) = add h k v
      end
    end

    module Cap =
    struct
      type ('a, 'b, 'c) t = ('a, 'b) Hashtbl.t constraint 'c = [< `Read | `Write ]
	
      let create = create
      external of_table  : ('a, 'b) Hashtbl.t -> ('a, 'b, _ ) t = "%identity"
      external to_table  : ('a, 'b, [`Read | `Write]) t -> ('a, 'b) Hashtbl.t = "%identity"
      external read_only :  ('a, 'b, [>`Read])  t -> ('a, 'b, [`Read])  t = "%identity"
      external write_only : ('a, 'b, [>`Write]) t -> ('a, 'b, [`Write]) t = "%identity"

      let length      = length
      let is_empty    = is_empty
      let add         = add
      let remove      = remove
      let remove_all  = remove_all
      let replace     = replace
      let copy        = copy
      let clear       = clear
      let find        = find
      let find_all    = find_all
      let find_default= find_default
      let find_option = find_option
      let mem         = mem
      let iter        = iter
      let fold        = fold
      let map         = map
      let filter      = filter
      let filteri     = filteri
      let filter_map  = filter_map
      let keys        = keys
      let values      = values
      let enum        = enum
      let of_enum     = of_enum
      let print       = print
      let filter      = filter
      let filteri     = filteri
      let filter_map  = filter_map
      module Labels =
      struct
	let label f              = fun key data -> f ~key ~data
	let add e ~key ~data     = add e key data
	let replace e ~key ~data = replace e key data
	let iter  ~f e           = iter (label f) e
	let map   ~f e           = map (label f) e
	let filter     ~f e      = filter f e
	let filteri    ~f e      = filteri (label f) e
	let filter_map ~f e      = filter_map (label f) e
	let fold  ~f e ~init     = fold (label f) e init
      end

      module Exceptionless =
      struct
	let find = find_option
      end
    end
