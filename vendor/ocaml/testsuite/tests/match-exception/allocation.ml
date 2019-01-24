(** Test that matching multiple values doesn't allocate a block. *)

let f x y =
  match x, y with
  | Some x, None
  | None, Some x -> x + 1
  | None, None -> 0
  | Some x, Some y -> x + y
  | exception _ -> -1

let test_multiple_match_does_not_allocate =
  let allocated_bytes = Gc.allocated_bytes () in
  let allocated_bytes' = Gc.allocated_bytes () in
  let a = Some 3 and b = None in
  let allocated_bytes'' = Gc.allocated_bytes () in
  let _ = f a b in
  let allocated_bytes''' = Gc.allocated_bytes () in
  if allocated_bytes' -. allocated_bytes
     = allocated_bytes''' -. allocated_bytes''
  then
    Printf.printf "no allocations for multiple-value match\n"
  else
    Printf.printf "multiple-value match allocated %f bytes\n"
      ((allocated_bytes''' -. allocated_bytes'') -.
       (allocated_bytes' -. allocated_bytes))
