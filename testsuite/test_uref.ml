module M = BatUref
module U = OUnit

let (>:), (>::), (>:::) = U.(>:), U.(>::), U.(>:::)
let (@?) = U.(@?)
let (@!) msg (exn, f) = U.assert_raises ~msg exn f

let test_uref_uget_uset () =
  let v, v' = 1, 2 in
  "uget (uref v) = v" @?
    (M.uget (M.uref v) = v);
  "let r = uref v in uset r v'; uget r = v'" @?
    (let r = M.uref v in M.uset r v'; M.uget r = v');
  ()

let test_unite () =
  let v, v' = 1, 2 in
  "let r = unref v in unite r r; uget r = v" @?
    (let r = M.uref v in M.unite r r; M.uget r = v);
  "let r, r' = unref v, unref v' in unite r r'; uget r = v' && uget r' = v'" @?
    (let r, r' = M.uref v, M.uref v' in
     M.unite r r'; M.uget r = v && M.uget r' = v);
  "let r, r' = unref v, unref v' in
   unite ~sel:(fun _x y -> y) r r';
   uget r = v && uget r' = v" @?
    (let r, r' = M.uref v, M.uref v' in
     M.unite ~sel:(fun _x y -> y) r r'; M.uget r = v' && M.uget r' = v');
  "let r = uref v in
   unite ~sel:(fun _ _ -> v') r r;
   uget r = v'" @?
    (let r = M.uref v in
     M.unite ~sel:(fun _ _ -> v') r r;
     M.uget r = v');
  "let r, r' = uref v, uref v in
   unite ~sel:(fun _ _ -> v') r r'; uget r = v'" @?
    (let r, r' = M.uref v, M.uref v' in
     M.unite ~sel:(fun _ _ -> v') r r';
     M.uget r = v');
  "let r, r' = uref (ref v), uref (ref v) in
   uget r != uget r' && (unite r r'; uget r == uget r')" @?
    (let r, r' = M.uref (ref v), M.uref (ref v) in
     M.uget r != M.uget r' && (M.unite r r'; M.uget r == M.uget r'));
  "let r, r' = uref v, uref v in
   unite r r';
   unite r' r;
   unite r' r; equal r r'" @?
    (let r, r' = M.uref v, M.uref v' in
     List.iter (fun (x, y) -> M.unite x y) [r,r'; r',r; r',r];
     M.equal r r');
  ()

let test_equal () =
  let v, v' = 1, 2 in
  "let r = uref v in equal r r" @?
    (let r = M.uref v in M.equal r r);
  "let r, r' = uref v, uref v in not (equal r r')" @?
    (let r, r' = M.uref v, M.uref v in not (M.equal r r'));
  let inequal_then_equal r r' =
    not (M.equal r r') && (M.unite r r'; M.equal r r') in
  "let r, r' = uref v, uref v' in
   not (equal r r') && (unite r r'; equal r r')" @?
    (inequal_then_equal (M.uref v) (M.uref v'));
  "let ra, ra' = uref v, uref v in
   let rb, rb' = uref v, uref v in
   unite ra ra'; unite rb rb';
   not (equal ra' rb') && (unite ra rb; equal ra' rb')" @?
   (let ra, ra' = M.uref v, M.uref v in
    let rb, rb' = M.uref v, M.uref v in
    M.unite ra ra'; M.unite rb rb';
    inequal_then_equal ra' rb');
  ()

let test_unite_shuffle () =
  (* testing the unification in all possible orders of n urefs
     unfornatunaly, since this is an imperative structure where
     you can't undo operations, this is slighlty complicated *)

  let pick_one n l f =
    assert (n <> 0);
    for i = 0 to n - 1 do
      let elt_l () =
        let l, rest = l () in
        let elt = BatList.nth l i in
        let l =
          let l1, l2 = BatList.split_at i l in
          l1 @ BatList.tl l2 in
        elt, l, rest in
      f elt_l
    done in

  let rec pick_two n l check =
    pick_one n l (fun elt_l ->
      if n = 1 then
        let elt, l, orig = elt_l () in
        assert (l = []);
        check elt orig
      else
        pick_one (n - 1)
          (fun () -> let elt, l, orig = elt_l () in l, (elt, orig))
          (fun elt2_l_elt ->
            pick_two (n - 1) (fun () ->
              let elt, l, (elt2, orig) = elt2_l_elt () in
              M.unite ~sel:(+) elt elt2;
              elt :: l, orig
            ) check;
            pick_two (n - 1) (fun () ->
              let elt, l, (elt2, orig) = elt2_l_elt () in
              M.unite ~sel:(+) elt elt2;
              elt2 :: l, orig
            ) check;
          )
    ) in

  let n = 4 in
  let urefs () = let l = BatList.init n (fun i -> M.uref i) in l, l in
  pick_two n urefs (fun elt urefs ->
    U.assert_equal ~printer:string_of_int (n * (n - 1) / 2) (M.uget elt);
    BatList.iter (fun uref -> U.assert_equal true (M.equal elt uref)) urefs
  )

let tests = "Uref" >::: [
  "uref, uget, uset" >:: test_uref_uget_uset;
  "unite" >:: test_unite;
  "equal" >:: test_equal;
  "unite_shuffle" >:: test_unite_shuffle;
]
