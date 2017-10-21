
(* Test file for topological sort *)

open Format
open Graph
open Pack.Digraph

let test ?(check=true) iter n edges =
  let v = Array.init n V.create in
  let g = create () in
  let () = Array.iter (add_vertex g) v in
  let build (s,t) = add_edge g v.(s) v.(t) in
  List.iter build edges;
  (* run top sort *)
  let num = Array.make n 0 in
  let i = ref 0 in
  iter (fun v -> incr i; num.(V.label v) <- !i) g;
  let r = Array.init n (fun i -> i) in
  Array.sort (fun i j -> num.(i) - num.(j)) r;
  if check then for v = 0 to n-1 do printf "%d " r.(v) done; printf "@.";
  (* check *)
  let path = PathCheck.check_path (PathCheck.create g) in
  let check_edge (x,y) =
    let vx = v.(x) and vy = v.(y) in
    printf "x=%d y=%d num(x)=%d num(y)=%d@." x y num.(x) num.(y);
    printf "x-->y=%b  y-->x=%b@." (path vx vy) (path vy vx);
    assert (num.(x) > 0 && num.(y) > 0);
    assert (num.(x) >= num.(y) || path vx vy || not (path vy vx)) in
  if check then
    for x = 0 to n-1 do for y = 0 to n-1 do check_edge (x,y) done done;
  (* display_with_gv g; *)
  ()

let tests iter =
  let test = test iter in
  test 3 [0,1; 1,2];
  test 3 [];
  (* 1-cycle *)
  test 1 [0,0];
  (* 2-cycle *)
  test 2 [0,1; 1,0];
  (* 2-cycle with out edge *)
  test 3 [0,1; 1,0; 1,2];
  test 3 [2,0; 0,2; 0,1];
  test 3 [1,2; 2,1; 2,0];
  (* 2 loops *)
  test 5 [1,2; 2,1; 2,0; 3,4; 4,3];
  (* 2-cycle with in edge *)
  test 3 [1,2; 2,1; 0,2];
  test 3 [1,2; 2,1; 0,1];
  (* 2 cycles connected *)
  test 4 [0,1; 1,0; 2,3; 3,2; 2,1];
  test 4 [0,1; 1,0; 2,3; 3,2; 1,2];
  test 4 [0,1; 1,0; 2,3; 3,2; 1,2; 2,1];
  (* 3-cycle with in and out edges *)
  test 5 [0,1; 1,2; 2,0; 3,0; 2,4];
  (* 3 cycles in a row *)
  test 7 [0,1; 1,0; 1,2; 2,3; 3,2; 3,4; 4,5; 5,6; 6,4];
  (* 3 cycles with 2 cycles in a cycle *)
  test 7 [0,1; 1,0; 1,2; 2,3; 3,2; 3,4; 4,5; 5,6; 6,4; 5,2];
  printf "All tests succeeded.@."

let () = tests Topological.iter
(* let () = tests Topological.iter_stable *)

let n = int_of_string Sys.argv.(1)

let () =
  let el = ref [] in
  (* linear graph *)
  (* for i = 0 to n-2 do el := (i,i+1) :: !el done; *)
  (* for i = 0 to n-2 do el := (i+1,i) :: !el done; *)
  el := [n-1,0]; for i = 0 to n-2 do el := (i,i+1) :: !el done;
  test ~check:false Topological.iter n !el

