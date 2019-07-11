(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                 Jeremie Dimino, Jane Street Europe                  *)
(*                                                                     *)
(*  Copyright 2015 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

module Q = struct
  include Queue

  let to_list q = fold (fun l x -> x :: l) [] q |> List.rev
end

let does_raise f q =
  try
    ignore (f q : int) ;
    false
  with Q.Empty -> true

let () =
  let q = Q.create () in
  () ;
  assert (Q.to_list q = [] && Q.length q = 0) ;
  Q.add 1 q ;
  assert (Q.to_list q = [1] && Q.length q = 1) ;
  Q.add 2 q ;
  assert (Q.to_list q = [1; 2] && Q.length q = 2) ;
  Q.add 3 q ;
  assert (Q.to_list q = [1; 2; 3] && Q.length q = 3) ;
  Q.add 4 q ;
  assert (Q.to_list q = [1; 2; 3; 4] && Q.length q = 4) ;
  assert (Q.take q = 1) ;
  assert (Q.to_list q = [2; 3; 4] && Q.length q = 3) ;
  assert (Q.take q = 2) ;
  assert (Q.to_list q = [3; 4] && Q.length q = 2) ;
  assert (Q.take q = 3) ;
  assert (Q.to_list q = [4] && Q.length q = 1) ;
  assert (Q.take q = 4) ;
  assert (Q.to_list q = [] && Q.length q = 0) ;
  assert (does_raise Q.take q)

let () =
  let q = Q.create () in
  Q.add 1 q ;
  assert (Q.take q = 1) ;
  assert (does_raise Q.take q) ;
  Q.add 2 q ;
  assert (Q.take q = 2) ;
  assert (does_raise Q.take q) ;
  assert (Q.length q = 0)

let () =
  let q = Q.create () in
  Q.add 1 q ;
  assert (Q.peek q = 1) ;
  Q.add 2 q ;
  assert (Q.peek q = 1) ;
  Q.add 3 q ;
  assert (Q.peek q = 1) ;
  assert (Q.peek q = 1) ;
  assert (Q.take q = 1) ;
  assert (Q.peek q = 2) ;
  assert (Q.take q = 2) ;
  assert (Q.peek q = 3) ;
  assert (Q.take q = 3) ;
  assert (does_raise Q.peek q) ;
  assert (does_raise Q.peek q)

let () =
  let q = Q.create () in
  for i = 1 to 10 do
    Q.add i q
  done ;
  Q.clear q ;
  assert (Q.length q = 0) ;
  assert (does_raise Q.take q) ;
  assert (q = Q.create ()) ;
  Q.add 42 q ;
  assert (Q.take q = 42)

let () =
  let q1 = Q.create () in
  for i = 1 to 10 do
    Q.add i q1
  done ;
  let q2 = Q.copy q1 in
  assert (Q.to_list q1 = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10]) ;
  assert (Q.to_list q2 = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10]) ;
  assert (Q.length q1 = 10) ;
  assert (Q.length q2 = 10) ;
  for i = 1 to 10 do
    assert (Q.take q1 = i)
  done ;
  for i = 1 to 10 do
    assert (Q.take q2 = i)
  done

let () =
  let q = Q.create () in
  assert (Q.is_empty q) ;
  for i = 1 to 10 do
    Q.add i q ;
    assert (Q.length q = i) ;
    assert (not (Q.is_empty q))
  done ;
  for i = 10 downto 1 do
    assert (Q.length q = i) ;
    assert (not (Q.is_empty q)) ;
    ignore (Q.take q : int)
  done ;
  assert (Q.length q = 0) ;
  assert (Q.is_empty q)

let () =
  let q = Q.create () in
  for i = 1 to 10 do
    Q.add i q
  done ;
  let i = ref 1 in
  Q.iter
    (fun j ->
      assert (!i = j) ;
      incr i)
    q

let () =
  let q1 = Q.create () and q2 = Q.create () in
  assert (Q.length q1 = 0) ;
  assert (Q.to_list q1 = []) ;
  assert (Q.length q2 = 0) ;
  assert (Q.to_list q2 = []) ;
  Q.transfer q1 q2 ;
  assert (Q.length q1 = 0) ;
  assert (Q.to_list q1 = []) ;
  assert (Q.length q2 = 0) ;
  assert (Q.to_list q2 = [])

let () =
  let q1 = Q.create () and q2 = Q.create () in
  for i = 1 to 4 do
    Q.add i q1
  done ;
  assert (Q.length q1 = 4) ;
  assert (Q.to_list q1 = [1; 2; 3; 4]) ;
  assert (Q.length q2 = 0) ;
  assert (Q.to_list q2 = []) ;
  Q.transfer q1 q2 ;
  assert (Q.length q1 = 0) ;
  assert (Q.to_list q1 = []) ;
  assert (Q.length q2 = 4) ;
  assert (Q.to_list q2 = [1; 2; 3; 4])

let () =
  let q1 = Q.create () and q2 = Q.create () in
  for i = 5 to 8 do
    Q.add i q2
  done ;
  assert (Q.length q1 = 0) ;
  assert (Q.to_list q1 = []) ;
  assert (Q.length q2 = 4) ;
  assert (Q.to_list q2 = [5; 6; 7; 8]) ;
  Q.transfer q1 q2 ;
  assert (Q.length q1 = 0) ;
  assert (Q.to_list q1 = []) ;
  assert (Q.length q2 = 4) ;
  assert (Q.to_list q2 = [5; 6; 7; 8])

let () =
  let q1 = Q.create () and q2 = Q.create () in
  for i = 1 to 4 do
    Q.add i q1
  done ;
  for i = 5 to 8 do
    Q.add i q2
  done ;
  assert (Q.length q1 = 4) ;
  assert (Q.to_list q1 = [1; 2; 3; 4]) ;
  assert (Q.length q2 = 4) ;
  assert (Q.to_list q2 = [5; 6; 7; 8]) ;
  Q.transfer q1 q2 ;
  assert (Q.length q1 = 0) ;
  assert (Q.to_list q1 = []) ;
  assert (Q.length q2 = 8) ;
  assert (Q.to_list q2 = [5; 6; 7; 8; 1; 2; 3; 4])

let () = print_endline "OK"
