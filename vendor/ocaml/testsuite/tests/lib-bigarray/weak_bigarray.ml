

(** check that custom block are not copied by Weak.get_copy *)

open Bigarray
open Bigarray.Array1

let () =
  let a = ref (create float64 c_layout 10) in
  Gc.compact ();
  set !a 0 42.;

  let w = Weak.create 1 in
  Weak.set w 0 (Some !a);

  let b =
    match Weak.get_copy w 0 with
    | None -> assert false
    | Some b -> b
  in
  Printf.printf "a.(0) = %f\n" (get !a 0);
  Printf.printf "b.(0) = %f\n" (get b 0);
  a := create float64 c_layout 10;
  Gc.compact ();

  let c = create float64 c_layout 10 in
  set c 0 33.;
  Printf.printf "b.(0) = %f\n" (get b 0);
