(* PR#7533 *)

exception Foo

let f x =
  if x > 42 then 1
  else raise Foo

let () =
  let f = Sys.opaque_identity f in
  match (f 0) / (List.hd (Sys.opaque_identity [0])) with
  | exception Foo -> ()
  | _ -> assert false

let () =
  let f = Sys.opaque_identity f in
  match (f 0) mod (List.hd (Sys.opaque_identity [0])) with
  | exception Foo -> ()
  | _ -> assert false
