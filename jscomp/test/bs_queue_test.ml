(* TEST
*)

module Q = Bs.Queue

let does_raise f q =
  try
    ignore (f q : int);
    false
  with _ ->
    true

let () =
  let q = Q.create () in
  ();                     assert (Q.toArray q = [|          |] && Q.length q = 0);
  Q.push 1 q;             assert (Q.toArray q = [|1         |] && Q.length q = 1);
  Q.push 2 q;             assert (Q.toArray q = [|1; 2      |] && Q.length q = 2);
  Q.push 3 q;             assert (Q.toArray q = [|1; 2; 3   |] && Q.length q = 3);
  Q.push 4 q;             assert (Q.toArray q = [|1; 2; 3; 4|] && Q.length q = 4);
  assert (Q.popAssert q = 1); assert (Q.toArray q = [|   2; 3; 4|] && Q.length q = 3);
  assert (Q.popAssert q = 2); assert (Q.toArray q = [|      3; 4|] && Q.length q = 2);
  assert (Q.popAssert q = 3); assert (Q.toArray q = [|         4|] && Q.length q = 1);
  assert (Q.popAssert q = 4); assert (Q.toArray q = [|          |] && Q.length q = 0);
  assert (does_raise Q.popAssert q);
;;

let () =
  let q = Q.create () in
  Q.push 1 q; assert (Q.popAssert q = 1); assert (does_raise Q.popAssert q);
  Q.push 2 q; assert (Q.popAssert q = 2); assert (does_raise Q.popAssert q);
  assert (Q.length q = 0);
;;

let () =
  let q = Q.create () in
  Q.push 1 q; assert (Q.peekAssert q = 1);
  Q.push 2 q; assert (Q.peekAssert q = 1);
  Q.push 3 q; assert (Q.peekAssert q = 1);
  assert (Q.peekAssert q = 1); assert (Q.popAssert q = 1);
  assert (Q.peekAssert q = 2); assert (Q.popAssert q = 2);
  assert (Q.peekAssert q = 3); assert (Q.popAssert q = 3);
  assert (does_raise Q.peekAssert q);
  assert (does_raise Q.peekAssert q);
;;

let () =
  let q = Q.create () in
  for i = 1 to 10 do Q.push i q done;
  Q.clear q;
  assert (Q.length q = 0);
  assert (does_raise Q.popAssert q);
  assert (q = Q.create ());
  Q.push 42 q;
  assert (Q.popAssert q = 42);
;;

let () =
  let q1 = Q.create () in
  for i = 1 to 10 do Q.push i q1 done;
  let q2 = Q.copy q1 in
  assert (Q.toArray q1 = [|1; 2; 3; 4; 5; 6; 7; 8; 9; 10|]);
  assert (Q.toArray q2 = [|1; 2; 3; 4; 5; 6; 7; 8; 9; 10|]);
  assert (Q.length q1 = 10);
  assert (Q.length q2 = 10);
  for i = 1 to 10 do
    assert (Q.popAssert q1 = i);
  done;
  for i = 1 to 10 do
    assert (Q.popAssert q2 = i);
  done;
;;

let () =
  let q = Q.create () in
  assert (Q.isEmpty q);
  for i = 1 to 10 do
    Q.push i q;
    assert (Q.length q = i);
    assert (not (Q.isEmpty q));
  done;
  for i = 10 downto 1 do
    assert (Q.length q = i);
    assert (not (Q.isEmpty q));
    ignore (Q.popAssert q : int);
  done;
  assert (Q.length q = 0);
  assert (Q.isEmpty q);
;;

let () =
  let q = Q.create () in
  for i = 1 to 10 do Q.push i q done;
  let i = ref 1 in
  Q.iter (fun[@bs] j -> assert (!i = j); incr i) q;
;;

let () =
  let q1 = Q.create () and q2 = Q.create () in
  assert (Q.length q1 = 0); assert (Q.toArray q1 = [||]);
  assert (Q.length q2 = 0); assert (Q.toArray q2 = [||]);
  Q.transfer q1 q2;
  assert (Q.length q1 = 0); assert (Q.toArray q1 = [||]);
  assert (Q.length q2 = 0); assert (Q.toArray q2 = [||]);
;;

let () =
  let q1 = Q.create () and q2 = Q.create () in
  for i = 1 to 4 do Q.push i q1 done;
  assert (Q.length q1 = 4); assert (Q.toArray q1 = [|1; 2; 3; 4|]);
  assert (Q.length q2 = 0); assert (Q.toArray q2 = [|          |]);
  Q.transfer q1 q2;
  assert (Q.length q1 = 0); assert (Q.toArray q1 = [|          |]);
  assert (Q.length q2 = 4); assert (Q.toArray q2 = [|1; 2; 3; 4|]);
;;

let () =
  let q1 = Q.create () and q2 = Q.create () in
  for i = 5 to 8 do Q.push i q2 done;
  assert (Q.length q1 = 0); assert (Q.toArray q1 = [|          |]);
  assert (Q.length q2 = 4); assert (Q.toArray q2 = [|5; 6; 7; 8|]);
  Q.transfer q1 q2;
  assert (Q.length q1 = 0); assert (Q.toArray q1 = [|          |]);
  assert (Q.length q2 = 4); assert (Q.toArray q2 = [|5; 6; 7; 8|]);
;;

let () =
  let q1 = Q.create () and q2 = Q.create () in
  for i = 1 to 4 do Q.push i q1 done;
  for i = 5 to 8 do Q.push i q2 done;
  assert (Q.length q1 = 4); assert (Q.toArray q1 = [|1; 2; 3; 4|]);
  assert (Q.length q2 = 4); assert (Q.toArray q2 = [|5; 6; 7; 8|]);
  Q.transfer q1 q2;
  assert (Q.length q1 = 0); assert (Q.toArray q1 = [|                      |]);
  let v = [|5; 6; 7; 8; 1; 2; 3; 4|] in 
  assert (Q.length q2 = 8); assert (Q.toArray q2 = v );

  assert (Q.fold (fun[@bs] x y -> x - y ) 0 q2 = 
          Bs.Array.foldLeft v 0 (fun [@bs] x y -> x - y) )

;;


let () = Js.log "OK"