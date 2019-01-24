(* Mantis 7201 *)

let f () = 0 [@@inline never]

let () =
  try
    ignore ((0 / f ()) : int);
    assert false
  with Division_by_zero -> ()

(* Not in Mantis 7201, but related: *)

let () =
  try
    ignore ((0 mod f ()) : int);
    assert false
  with Division_by_zero -> ()
