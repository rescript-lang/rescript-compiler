(* This will test the parsing of the smallest integer on 32-bit architectures.
   It doesn't do much on 64-bit but at least it doesn't crash.
 *)

let min_int = -1073741824
let () = match min_int with
| -1073741824 as i ->
  assert (string_of_int i = "-1073741824");
  print_endline "OK"
| _ -> assert false
