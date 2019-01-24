type t = Leaf of int | Branch of t * t

type floatref = { mutable f : float }

let a = { f = 0.0 }

let rec allocate_lots m = function
  | 0 -> Leaf m
  | n -> Branch (allocate_lots m (n-1), allocate_lots (m+1) (n-1))

let measure f =
  let a = Gc.minor_words () in
  f ();
  let c = Gc.minor_words () in
  c -. a

let () =
  let n = measure (fun () -> a.f <- Gc.minor_words ()) in
  (* Gc.minor_words should not allocate, although bytecode
     generally boxes the floats *)
  assert (n < 10.);
  if Sys.backend_type = Sys.Native then assert (n = 0.);
  let n = measure (fun () -> Sys.opaque_identity (allocate_lots 42 10)) in
  (* This should allocate > 3k words (varying slightly by unboxing) *)
  assert (n > 3000.);
  print_endline "ok"
