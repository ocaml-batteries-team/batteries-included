open OUnit
open Batteries

(* Here we check the various modify_* functions.
   We start by creating some Modifiable signatures with which
   all concerned modules should comply.
*)

(* Polymorphic, mutable containers *)

module type MODIFIABLE_MUTABLE =
sig
    type ('a, 'b) t

    val modify : 'a -> ('b -> 'b) -> ('a, 'b) t -> unit
    val modify_def : 'b -> 'a -> ('b -> 'b) -> ('a, 'b) t -> unit
    val modify_opt : 'a -> ('b option -> 'b option) -> ('a, 'b) t -> unit

    (* for testing we need to be able to inspect the container: *)
    val of_enum : ('a * 'b) Enum.t -> ('a, 'b) t
    val enum : ('a, 'b) t -> ('a * 'b) Enum.t
end

let none _ = None

module TestModifiable_mutable (M : MODIFIABLE_MUTABLE) =
struct
    let test () =
        let m = M.of_enum (Enum.combine (1 -- 5, 1 -- 5)) in
        M.modify 2 succ m ;
        let e = M.enum m /@ snd |> List.of_enum |> List.sort Int.compare in
        assert_equal ~printer:(BatIO.to_string (List.print Int.print))
            e [1;3;3;4;5] ;
        (* Add an entry using modify_def *)
        M.modify_def 0 0 identity m ;
        (* Empty everything using modify_opt *)
        for i = 0 to 5 do M.modify_opt i none m done ;
        assert_bool "couldn't empty the map" (Enum.is_empty (M.enum m))
end

let test_hashtbl_modifiable () =
    let module T = TestModifiable_mutable(Hashtbl) in
    T.test ()

(* Immutable containers *)

module type MODIFIABLE_IMMUTABLE =
sig
    type key = int
    type 'a t

    val modify : key -> ('a -> 'a) -> 'a t -> 'a t
    val modify_def : 'a -> key -> ('a -> 'a) -> 'a t -> 'a t
    val modify_opt : key -> ('a option -> 'a option) -> 'a t -> 'a t

    (* for testing we need to be able to inspect the container: *)
    val of_enum : (key * 'a) Enum.t -> 'a t
    val enum : 'a t -> (key * 'a) Enum.t
end

let rec reapply_i mi ma f m =
    if mi > ma then m else reapply_i (mi+1) ma f (f mi m)

module TestModifiable_immutable (M : MODIFIABLE_IMMUTABLE) =
struct
    let test () =
        let m = M.of_enum (Enum.combine (1 -- 5, 1 -- 5)) in
        let m = M.modify 2 succ m in
        let e = M.enum m /@ snd |> List.of_enum |> List.sort Int.compare in
        assert_equal ~printer:(BatIO.to_string (List.print Int.print))
            e [1;3;3;4;5] ;
        (* Add an entry using modify_def *)
        let m = M.modify_def 0 0 identity m in
        (* Empty everything using modify_opt *)
        let m = reapply_i 0 5 (fun i m -> M.modify_opt i none m) m in
        assert_bool "couldn't empty the map" (Enum.is_empty (M.enum m))
end

let test_map_modifiable () =
    let module T = TestModifiable_immutable(Map.Make (Int)) in
    T.test ()

let test_splay_modifiable () =
    let module T = TestModifiable_immutable(Splay.Map (Int)) in
    T.test ()

let test_imap_modifiable () =
    let module MyIMap =
    struct
        include IMap
        let of_enum e =
            let e' = e /@ fun (k, v) -> (k, k, v) in
            of_enum ~eq:(=) e'
        let enum t =
            enum t /@
            (fun (n1, n2, v) -> ((n1 -- n2) /@ fun n -> n, v)) |>
            Enum.flatten
    end in
    let module T = TestModifiable_immutable(MyIMap) in
    T.test ()


(* And polymorphic, immutable containers *)

module type MODIFIABLE_POLY_IMMUTABLE =
sig
    type ('a, 'b) t

    val modify : 'a -> ('b -> 'b) -> ('a, 'b) t -> ('a, 'b) t
    val modify_def : 'b -> 'a -> ('b -> 'b) -> ('a, 'b) t -> ('a, 'b) t
    val modify_opt : 'a -> ('b option -> 'b option) -> ('a, 'b) t -> ('a, 'b) t

    (* for testing we need to be able to inspect the container: *)
    val of_enum : ('a * 'b) Enum.t -> ('a, 'b) t
    val enum : ('a, 'b) t -> ('a * 'b) Enum.t
end

module TestModifiable_poly_immutable (M : MODIFIABLE_POLY_IMMUTABLE) =
struct
    let test () =
        let m = M.of_enum (Enum.combine (1 -- 5, 1 -- 5)) in
        let m = M.modify 2 succ m in
        let e = M.enum m /@ snd |> List.of_enum |> List.sort Int.compare in
        assert_equal ~printer:(BatIO.to_string (List.print Int.print))
            e [1;3;3;4;5] ;
        (* Add an entry using modify_def *)
        let m = M.modify_def 0 0 identity m in
        (* Empty everything using modify_opt *)
        let m = reapply_i 0 5 (fun i m -> M.modify_opt i none m) m in
        assert_bool "couldn't empty the map" (Enum.is_empty (M.enum m))
end

let test_list_modifiable () =
    let module AssocList =
    struct
        type ('a, 'b) t = ('a * 'b) list
        let modify = List.modify
        let modify_def = List.modify_def
        let modify_opt = List.modify_opt
        let of_enum = List.of_enum
        let enum = List.enum
    end in
    let module T = TestModifiable_poly_immutable(AssocList) in
    T.test ()

let test_pmap_modifiable () =
    let module T = TestModifiable_poly_immutable(Map) in
    T.test ()

(* And...? Polymorphic, immutable Multi containers! *)

module type MODIFIABLE_POLY_MULTI_IMMUTABLE =
sig
    type ('a, 'b) t

    val modify : 'a -> ('b BatSet.PSet.t -> 'b BatSet.PSet.t) -> ('a, 'b) t -> ('a, 'b) t
    val modify_def : 'b BatSet.PSet.t -> 'a -> ('b BatSet.PSet.t -> 'b BatSet.PSet.t) -> ('a, 'b) t -> ('a, 'b) t
    val modify_opt : 'a -> ('b BatSet.PSet.t option -> 'b BatSet.PSet.t option) -> ('a, 'b) t -> ('a, 'b) t

    (* for testing we need to be able to inspect the container: *)
    val of_enum : ('a * 'b) Enum.t -> ('a, 'b) t
    val enum : ('a, 'b) t -> ('a * 'b) Enum.t
end

module TestModifiable_poly_multi_immutable (M : MODIFIABLE_POLY_MULTI_IMMUTABLE) =
struct
    let test () =
        let m = M.of_enum (Enum.combine (1 -- 5, 1 -- 5)) in
        let m = M.modify 2 (BatSet.PSet.map succ) m in
        let e = M.enum m /@ snd |> List.of_enum |> List.sort Int.compare in
        assert_equal ~printer:(BatIO.to_string (List.print Int.print))
            e [1;3;3;4;5] ;
        (* Add an entry using modify_def *)
        let m = M.modify_def (BatSet.PSet.singleton 0) 0 identity m in
        (* Empty everything using modify_opt *)
        let m = reapply_i 0 5 (fun i m -> M.modify_opt i none m) m in
        assert_bool "couldn't empty the map" (Enum.is_empty (M.enum m))
end

let test_multipmap_modifiable () =
    let module MyMultiPMap =
    struct
        include MultiPMap
        let of_enum e = of_enum ~keys:compare ~data:compare e
    end in
    let module T = TestModifiable_poly_multi_immutable(MyMultiPMap) in
    T.test ()

(* Wait! Non-polymorphic, immutable Multi containers *)

module type MODIFIABLE_MULTI_IMMUTABLE =
sig
    type ('a, 'b) t

    val modify : 'a -> ('b BatSet.t -> 'b BatSet.t) -> ('a, 'b) t -> ('a, 'b) t
    val modify_def : 'b BatSet.t -> 'a -> ('b BatSet.t -> 'b BatSet.t) -> ('a, 'b) t -> ('a, 'b) t
    val modify_opt : 'a -> ('b BatSet.t option -> 'b BatSet.t option) -> ('a, 'b) t -> ('a, 'b) t

    (* for testing we need to be able to inspect the container: *)
    val of_enum : ('a * 'b) Enum.t -> ('a, 'b) t
    val enum : ('a, 'b) t -> ('a * 'b) Enum.t
end

module TestModifiable_multi_immutable (M : MODIFIABLE_MULTI_IMMUTABLE) =
struct
    let test () =
        let m = M.of_enum (Enum.combine (1 -- 5, 1 -- 5)) in
        let m = M.modify 2 (BatSet.map succ) m in
        let e = M.enum m /@ snd |> List.of_enum |> List.sort Int.compare in
        assert_equal ~printer:(BatIO.to_string (List.print Int.print))
            e [1;3;3;4;5] ;
        (* Add an entry using modify_def *)
        let m = M.modify_def (BatSet.singleton 0) 0 identity m in
        (* Empty everything using modify_opt *)
        let m = reapply_i 0 5 (fun i m -> M.modify_opt i none m) m in
        assert_bool "couldn't empty the map" (Enum.is_empty (M.enum m))
end

let test_multimap_modifiable () =
    let module T = TestModifiable_multi_immutable(MultiMap) in
    T.test ()


(* -- *)

let tests = "Modifiable" >::: [
  "Hashtbl" >:: test_hashtbl_modifiable;
  "List" >:: test_list_modifiable;
  "Map" >:: test_map_modifiable;
  "PMap" >:: test_pmap_modifiable;
  "Splay" >:: test_splay_modifiable;
  "IMap" >:: test_imap_modifiable;
  "MultiPMap" >:: test_multipmap_modifiable;
  "MultiMap" >:: test_multimap_modifiable;
]
