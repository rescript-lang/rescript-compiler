
let () =
  print_endline (Opaque_intf.choose "Opaque_intf: First" "Opaque_intf: Second")

let () =
  print_endline (Opaque_impl.choose "Opaque_impl: First" "Opaque_impl: Second")

let () =
  print_endline (Regular.choose "Regular: First" "Regular: Second")
