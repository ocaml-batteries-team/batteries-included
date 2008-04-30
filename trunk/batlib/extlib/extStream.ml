(* 
 * Stream - streams and stream parsers
 * Copyright (C) 1997 Daniel de Rauglaudre
 *               2007 Zheng Li
 *               2008 David Teller
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

module OldStream = Stream

module Stream = struct

include Stream
exception End_of_flow = Failure

let (|>) x f = f x
let (@.) f x = f x
let (|-) f g x = g (f x)
let (-|) f g x = f (g x)
let (//) f g = fun (x,y) -> (f x, g y)
let curry f x y =  f (x,y)
let uncurry f (x,y) = f x y
let id x = x



let rec of_fun f = 
  [< try let h = f () in [<'h; of_fun f>] with End_of_flow -> [< >] >]

let to_fun fl = fun () -> next fl
  
let to_list fl = 
  let buf = ref [] in
    iter (fun x -> buf := x::!buf) fl;
    List.rev !buf
      
let to_string fl = 
  let sl = to_list fl in
  let len = List.length sl in
  let s = String.create len in
    List.iter (let i = ref 0 in fun x -> s.[!i] <- x; incr i) sl;
    s

let to_channel ch = iter (output_char ch)

let to_output o   = iter (IO.write o)

let rec of_input i=  [<try let h = IO.read i in [<'h; of_input i>] with IO.No_more_input -> [< >] >]

let rec cycle times x = match times with 
  | None -> [<x; cycle None x>]
  | Some 1 -> [<x>] (* in case of destriction *)
  | Some n when n<= 0 -> [< >]
  | Some n -> [<x; cycle (Some (n-1)) x>]

let rec repeat times x = cycle times [<'x>]

let rec seq init step cont = 
  if cont init then [<'init; seq (step init) step cont>] else [<>]

let rec range n until = 
  let step x = (x+1) land max_int in
  let cont = match until with None -> fun x -> true | Some x -> (>=) x in
    seq n step cont

let (--) p q = range p (Some q)



let next = parser [<'h>] -> h | [<>] -> raise End_of_flow 


let rec foldl f init s = match peek s with
  | Some h ->
      (match f init h with
	 | accu, None -> junk s; foldl f accu s
	 | accu, Some true -> junk s; accu
	 | _, Some false -> init)
  | None -> init
      
let rec foldr f init s = match s with parser
  | [<'h>] -> f h (lazy (foldr f init s))
  | [<>] -> init

let fold f s =  match s with parser
  | [<'h>] -> foldl f h s | [<>] -> raise End_of_flow


let cons x s = [<'x; s>]

let apnd s1 s2 = [<s1; s2>]
    
let is_empty s = match peek s with None -> true | _ -> false
  
let rec concat ss = 
  [< match ss with parser [<'p>] -> [<p; concat ss>] | [<>] -> [<>]>]
      
let rec filter f s = 
  [< match s with parser 
       | [<'h>] -> if f h then [<'h; filter f s>] else [<filter f s>]
       | [<>] -> [<>] >]

let take n fl = 
  let i = ref n in
    of_fun (fun () -> if !i <= 0 then raise End_of_flow else decr i; next fl)

let drop n fl = 
  let i = ref n in
  let rec f () = if !i <= 0 then next fl else (ignore (next fl); decr i; f ()) in
    of_fun f
	
let rec take_while f s =
  [< match peek s with
       | Some h -> if f h then (junk s; [<'h; take_while f s>]) else [<>]
       | None -> [<>] >]
    
let rec drop_while f s = 
  [< match s with parser 
       | [<'h>] -> if f h then [<drop_while f s>] else [<'h;s>]
       | [<>] -> [<>] >]
      
let span p s =
  let q = Queue.create () and sr = ref None in
  let rec get_head () = 
    [< if not (Queue.is_empty q) then [<'(Queue.take q); get_head () >]
       else match s with parser
	 | [<'h>] -> if p h then [<'h; get_head ()>] else (sr := Some h; [<>])
	 | [<>] -> [<>] >] in
  let rec get_tail () = match !sr with 
    | Some v -> [<'v; s>] 
    | None -> [< match s with parser
                   | [<'h>] -> 
                       if p h then Queue.add h q else sr := Some h; get_tail ()
                   | [<>] -> [<>] >] in
    (get_head (), [<get_tail ()>])
      
let break p s = span (p|-not) s
  
let rec group p s = 
  [< match peek s with 
       | None -> [<>] 
       | Some v -> if p v then group_aux p s else group_aux (p |- not) s >]
and group_aux p s = match peek s with 
  | None -> [<>] 
  | Some _ ->
      let h = next s in 
      let s1, s2 = span p s in
	[< '([<'h; s1>]); group_aux (p |- not) s2 >]
	  
	  
let rec map f s = 
  [< match s with parser [<'h>] -> [<'f h; map f s>] | [<>] -> [<>]>]
    
let dup s =
  let rec gen s qa qb = 
    [< if not (Queue.is_empty qa) then [<'(Queue.take qa); gen s qa qb>]
       else match s with parser
	 | [<'h>] -> Queue.add h qb;[<'h; gen s qa qb>] | [<>] -> [<>]
    >] in
  let q1 = Queue.create () and q2 = Queue.create () in
    (gen s q1 q2, gen s q2 q1)
      
let rec combn sa = 
  [< if Array.fold_left (fun b s -> b || is_empty s) false sa then [<>] 
     else [<'(Array.map next sa); combn sa>] >]
    
let rec comb (s1,s2) = 
  [< match peek s1 with
       | Some h1 -> (match peek s2 with
		       | Some h2 -> (junk s1; junk s2; [<'(h1,h2); comb (s1, s2)>]) 
		       | None -> [<>])
       | None -> [<>] >]
    
      
let dupn n s =
  let qa = Array.init n (fun _ -> Queue.create ()) in
  let rec gen i =
    [< if not (Queue.is_empty qa.(i)) then [<'(Queue.take qa.(i)); gen i>]
       else match s with parser
	 | [<'h>] -> for i = 0 to n-1 do Queue.add h qa.(i) done; gen i
	 | [<>] -> [<>] >] in
    Array.init n gen
	
	
let splitn n s = 
  let qa = Array.init n (fun _ -> Queue.create ()) in
  let rec gen i =
    [< if not (Queue.is_empty qa.(i)) then [<'(Queue.take qa.(i)); gen i>]
       else match s with parser
	 | [<'h>] -> for i = 0 to n-1 do Queue.add h.(i) qa.(i) done; gen i
	 | [<>] -> [<>] >] in
    Array.init n gen
      
let split s = (dup |- map fst // map snd) s
  
let mergen f sa = 
  let n = Array.length sa in
  let pt = Array.init n id in
  let rec alt x i = (i < n) && 
    if pt.((x+i) mod n) = pt.(x) then alt x (i+1) else 
      (for j = 0 to i-1 do pt.((x+j) mod n) <- pt.((x+i) mod n) done; true) in
  let rec aux i = 
    [< match sa.(pt.(i)) with parser
	 | [<'h>] -> let i' = pt.(i) in [<'h; aux pt.((f i' h) mod n)>]
	 | [<>] -> if alt i 1 then aux i else [< >] >] in
    aux 0
      
let merge f (s1,s2) = 
  let i2b = function 0 -> true | 1 -> false | _ -> assert false
  and b2i = function true -> 0 | false -> 1 in
    mergen (fun i x -> b2i (f (i2b i) x)) [|s1;s2|]
      
let switchn n f s = 
  let qa = Array.init n (fun _ -> Queue.create ()) in
  let rec gen i =
    [< if not (Queue.is_empty qa.(i)) then [<'(Queue.take qa.(i)); gen i>]
       else match s with parser
	 | [<'h>] ->
             let i' = (f h) mod n in
	       if i' = i then [<'h; gen i>]
	       else (Queue.add h qa.(i'); [<gen i>])
	 | [<>] -> [<>] >] in
    Array.init n gen
      
let switch f s = 
  let sa = switchn 2 (fun x -> if f x then 0 else 1) s in
    (sa.(0), sa.(1))
	
	
let rec scanl f init s =
  [< match s with parser 
	 [<'h>] -> [<'init; scanl f (f init h) s>] | [<>] -> [<'init>] >]
      
let scan f s =
  [< match s with parser [<'h>] -> [<scanl f h s>] | [<>] -> [<>] >]
    
let map2 f = comb |- map (uncurry f) |> curry;;

let rec map_fold f s = 
  [< match peek s with
       | None -> [<>]
       | Some _ -> [<'(fold f s); map_fold f s>] >]
    
    
let feed stf vf delay exp =
  let s_in' = ref [<>] in
  let out = exp [<delay; !s_in'>] in
  let s_in = stf out and s_out = vf out in
    s_in' := s_in;
    s_out
      
let feedl delay exp = feed fst snd delay exp
  
let feedr delay exp = feed snd fst delay exp
  
let circ delay exp = feedl delay (exp |- dup)
  
let while_do size test exp = 
  let size = match size with
    | Some n when n>=1  -> n
    | _    -> 1
  in
  let inside = ref 0 in
  let judge x = if test x then (incr inside; true) else false in
  let choose b _ = if not b then decr inside; !inside < size in
    merge choose |- switch judge |- exp // id |> curry |- feedl [<>]
	
let do_while size test exp =
  let size = match size with
    | Some n when n>=1  -> n
    | _    -> 1
  in
  let inside = ref 0  in
  let judge x = if test x then (incr inside; true) else false in
  let choose b _ = if not b then decr inside; !inside < size in
    merge choose |- exp |- switch judge |> curry |- feedl [<>]
	
let farm par size path exp_gen s =
  let par = match par with
    | None   -> 1
    | Some p -> p in
  let size= match size with
    | None   -> (fun _ -> 1)
    | Some f -> f in
  let path= match path with
    | None   -> ignore |- to_fun @. cycle None ( 0 -- (par - 1) )
    | Some f -> f
  in
  let par = if par < 1 then 1 else par in
  let count = Array.make par 0 in
  let size x = let s = size x in if s < 1 then 1 else s in
  let path x = let i = path x in count.(i) <- succ count.(i); i in
  let choose = 
    let rec find_next cond last i =
      if i < par then 
        let j = (last + i) mod par in
	  if cond j then Some j else find_next cond last (i+1)
      else None in
      fun last _ -> 
	count.(last) <- count.(last) - 1; 
	let nth = match find_next (fun i -> count.(i) >= (size i)) last 1 with
	  | Some j -> j
	  | None -> match find_next (fun i -> count.(i) > 0) last 1 with
	      | Some j -> j
	      | None -> last + 1 mod par in
	  nth in
  let sa_in = switchn par path s in
  let sa_out = Array.mapi exp_gen sa_in in
    mergen choose sa_out
      
let (|||) exp1 exp2 = exp1 |- exp2
      
    
let enum x =
  Enum.from (
    fun () ->
      try   next x
      with  End_of_flow -> raise Enum.No_more_elements
  )
    
    
let rec of_enum e =
  [< match Enum.get e with | Some h ->  [<'h; of_enum e>] | None -> [< >] >]
end

module StreamLabels = struct
include Stream

let iter   ~f x      = iter f x
  
let switch ~f x      = switch f x
    
let foldl ~f ~init   = foldl f init
  
let foldr ~f ~init   = foldr f init
  
let fold ~f ~init    = fold f init
  
let filter ~f        = filter f
  
let map ~f           = map f
  
let map2 ~f          = map2 f
  
let scanl ~f         = scanl f
  
let scan ~f          = scan f
  
let while_do ?size ~f= while_do size f
  
let do_while ?size ~f= do_while size f
  
let range ?until p   = range p until
  
let repeat ?times    = repeat times
  
let cycle ?times     = cycle times
  
let take_while ~f    = take_while f
  
let drop_while ~f    = drop_while f
  
let span ~f          = span f
  
let break ~f         = break f
  
let group ~f         = group f
  
let merge ~f         = merge f
  
let mergen ~f        = mergen f
  
let switchn x ~f     = switchn x f
  
let farm ?par ?size ?path = farm par size path

end
