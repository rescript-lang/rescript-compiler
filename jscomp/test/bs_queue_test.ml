(* TEST
*)

module Q = Bs.Queue

let does_raise f q =
  try
    ignore (f q : int);
    false
  with _ ->
    true
let (++) = Q.add 

let () =
  let q = Q.create () in
  assert (Q.toArray q = [|          |] && Q.length q = 0);
  assert (Q.toArray (q ++ 1) = [|1         |] && Q.length q = 1);
  assert (Q.toArray (q ++ 2) = [|1; 2      |] && Q.length q = 2);
  assert (Q.toArray (q ++ 3) = [|1; 2; 3   |] && Q.length q = 3);
  assert (Q.toArray (q ++ 4) = [|1; 2; 3; 4|] && Q.length q = 4);
  assert (Q.popExn q = 1); assert (Q.toArray q = [|   2; 3; 4|] && Q.length q = 3);
  assert (Q.popExn q = 2); assert (Q.toArray q = [|      3; 4|] && Q.length q = 2);
  assert (Q.popExn q = 3); assert (Q.toArray q = [|         4|] && Q.length q = 1);
  assert (Q.popExn q = 4); assert (Q.toArray q = [|          |] && Q.length q = 0);
  assert (does_raise Q.popExn q);
;;

let () =
  let q = Q.create () in
  assert (Q.popExn (q ++ 1) = 1); assert (does_raise Q.popExn q);
  assert (Q.popExn (q ++ 2) = 2); assert (does_raise Q.popExn q);
  assert (Q.length q = 0);
;;

let () =
  let q = Q.create () in
  assert (Q.peekExn (q ++ 1) = 1);
  assert (Q.peekExn (q ++ 2) = 1);
  assert (Q.peekExn (q ++ 3) = 1);
  assert (Q.peekExn q = 1); assert (Q.popExn q = 1);
  assert (Q.peekExn q = 2); assert (Q.popExn q = 2);
  assert (Q.peekExn q = 3); assert (Q.popExn q = 3);
  assert (does_raise Q.peekExn q);
  assert (does_raise Q.peekExn q);
;;

let () =
  let q = Q.create () in
  for i = 1 to 10 do Q.addDone q i  done;
  Q.clear q;
  assert (Q.length q = 0);
  assert (does_raise Q.popExn q);
  assert (q = Q.create ());
  Q.addDone q 42;
  assert (Q.popExn q = 42);
;;

let () =
  let q1 = Q.create () in
  for i = 1 to 10 do Q.addDone q1 i done;
  let q2 = Q.copy q1 in
  assert (Q.toArray q1 = [|1; 2; 3; 4; 5; 6; 7; 8; 9; 10|]);
  assert (Q.toArray q2 = [|1; 2; 3; 4; 5; 6; 7; 8; 9; 10|]);
  assert (Q.length q1 = 10);
  assert (Q.length q2 = 10);
  for i = 1 to 10 do
    assert (Q.popExn q1 = i);
  done;
  for i = 1 to 10 do
    assert (Q.popExn q2 = i);
  done;
;;

let () =
  let q = Q.create () in
  assert (Q.isEmpty q);
  for i = 1 to 10 do
    Q.addDone q i;
    assert (Q.length q = i);
    assert (not (Q.isEmpty q));
  done;
  for i = 10 downto 1 do
    assert (Q.length q = i);
    assert (not (Q.isEmpty q));
    ignore (Q.popExn q : int);
  done;
  assert (Q.length q = 0);
  assert (Q.isEmpty q);
;;

let () =
  let q = Q.create () in
  for i = 1 to 10 do Q.addDone q i done;
  let i = ref 1 in
  Q.forEach q (fun[@bs] j -> assert (!i = j); incr i);
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
  for i = 1 to 4 do Q.addDone q1 i done;
  assert (Q.length q1 = 4); assert (Q.toArray q1 = [|1; 2; 3; 4|]);
  assert (Q.length q2 = 0); assert (Q.toArray q2 = [|          |]);
  Q.transfer q1 q2;
  assert (Q.length q1 = 0); assert (Q.toArray q1 = [|          |]);
  assert (Q.length q2 = 4); assert (Q.toArray q2 = [|1; 2; 3; 4|]);
;;

let () =
  let q1 = Q.create () and q2 = Q.create () in
  for i = 5 to 8 do Q.addDone q2 i done;
  assert (Q.length q1 = 0); assert (Q.toArray q1 = [|          |]);
  assert (Q.length q2 = 4); assert (Q.toArray q2 = [|5; 6; 7; 8|]);
  Q.transfer q1 q2;
  assert (Q.length q1 = 0); assert (Q.toArray q1 = [|          |]);
  assert (Q.length q2 = 4); assert (Q.toArray q2 = [|5; 6; 7; 8|]);
;;

let () =
  let q1 = Q.create () and q2 = Q.create () in
  for i = 1 to 4 do Q.addDone q1 i  done;
  for i = 5 to 8 do Q.addDone q2 i  done;
  assert (Q.length q1 = 4); assert (Q.toArray q1 = [|1; 2; 3; 4|]);
  assert (Q.length q2 = 4); assert (Q.toArray q2 = [|5; 6; 7; 8|]);
  Q.transfer q1 q2;
  assert (Q.length q1 = 0); assert (Q.toArray q1 = [|                      |]);
  let v = [|5; 6; 7; 8; 1; 2; 3; 4|] in 
  assert (Q.length q2 = 8); assert (Q.toArray q2 = v );

  assert (Q.reduce q2 0 (fun[@bs] x y -> x - y ) = 
          Bs.Array.reduce v 0 (fun [@bs] x y -> x - y) )

;;


let () = Js.log "OK"