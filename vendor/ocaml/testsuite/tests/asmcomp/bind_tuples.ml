(* Check the effectiveness of optimized compilation of tuple binding

   Ref: http://caml.inria.fr/mantis/view.php?id=4800
*)

let f () =
  let x0 = Gc.allocated_bytes () in
  let x1 = Gc.allocated_bytes () in

  let r = ref 0 in
  for i = 1 to 20 do
    let (x, y) =
      try
        if i mod 2 = 0 then (1, i * 2)
        else if i mod 5 = 0 then raise Exit
        else (-1, i * 3)
      with Exit ->
        (1, -1)
    in
    r := !r * x + y
  done;
  let x2 = Gc.allocated_bytes () in
  print_int !r;
  assert (!r = 82);
  assert(x1 -. x0 = x2 -. x1) (* check no allocation between x1 and x2 *)
  [@@inline never]

let () = f ()
