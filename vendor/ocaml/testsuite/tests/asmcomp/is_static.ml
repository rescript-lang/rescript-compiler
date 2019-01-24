(* Data that should be statically allocated by the compiler (all versions) *)

external is_in_static_data : 'a -> bool = "caml_is_in_static_data"

(* Basic constant blocks should be static *)
let block1 = (1,2)
let () = assert(is_in_static_data block1)

(* as pattern shouldn't prevent it *)
let (a, b) as block2 = (1,2)
let () = assert(is_in_static_data block2)

(* Also in functions *)
let f () =
  let block = (1,2) in
  assert(is_in_static_data block)

let () = (f [@inlined never]) ()

(* Closed functions should be static *)
let closed_function x = x + 1 (* + is a primitive, it cannot be in the closure*)
let () = assert(is_in_static_data closed_function)

(* And functions using closed functions *)
let almost_closed_function x =
  (closed_function [@inlined never]) x
let () = assert(is_in_static_data almost_closed_function)

(* Recursive constant functions should be static *)
let rec f1 a = g1 a
and g1 a = f1 a
let () =
  assert(is_in_static_data f1);
  assert(is_in_static_data g1)
