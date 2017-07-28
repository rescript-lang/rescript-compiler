(* %n, %l, %N and %L have a scanf-specific semantics, but are supposed
   to be interpreted by Printf and Format as %u, despite this
   interpretation being mildly deprecated *)

let test format = (Printf.sprintf format (-3) : string)
;;

let () = Printf.printf "%%n: %B\n"
  (test "%n" = test "%u")
;;

let () = Printf.printf "%%l: %B\n"
  (test "%l" = test "%u")
;;

let () = Printf.printf "%%N: %B\n"
  (test "%N" = test "%u")
;;

let () = Printf.printf "%%L: %B\n"
  (test "%L" = test "%u")
;;
