(* Check the effectiveness of structured constant propagation and
   static allocation.

   Ref: http://caml.inria.fr/mantis/view.php?id=5779
*)

let () =
  let x0 = Gc.allocated_bytes () in
  let x1 = Gc.allocated_bytes () in
  let pair x y = (x, y) in
  let a = pair 1 2 in
  let b = pair a ["x";"y"] in
  let g () = (a, fst b) in
  assert (g () == ((1,2), (1,2)));
  assert (fst (pair a a) == (1, 2));
  assert (snd b != ["x"; "y"] || Config.safe_string);  (* mutable "constant", cannot be shared *)
  let x2 = Gc.allocated_bytes () in
  assert(x1 -. x0 = x2 -. x1)
     (* check that we did not allocated anything between x1 and x2 *)
