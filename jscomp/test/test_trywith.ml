let ff g x =
  (try g x with Not_found -> ()) ;
  (try g x with Out_of_memory -> ()) ;
  (try g x with Sys_error _ -> ()) ;
  (try g x with Invalid_argument _ -> ()) ;
  (try g x with End_of_file -> ()) ;
  (try g x with Match_failure _ -> ()) ;
  (try g x with Stack_overflow -> ()) ;
  (try g x with Sys_blocked_io -> ()) ;
  (try g x with Assert_failure _ -> ()) ;
  try g x with Undefined_recursive_module _ -> ()

[@@@warning "-21"]

let u () =
  raise Not_found ;
  let f = 3 + 3 in
  f + f

let u1 = "bad character decimal encoding \\"
let v = "bad character decimal encoding \\%c%c%c"

(** test default branch *)
type u = A | B | C | D of int | E of char

let f (x : u) = match x with D _ -> 1 | A | B | C -> 2 | _ -> assert false
