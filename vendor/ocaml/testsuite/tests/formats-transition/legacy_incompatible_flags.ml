(* the legacy parser ignores flags on formatters on which they make no
   sense *)

let () = Printf.printf "%+s\n" "toto"
;;
let () = Printf.printf "%#s\n" "toto"
;;
let () = Printf.printf "% s\n" "toto"
;;
let () = Printf.printf "%03s\n" "toto"
;;
let () = Printf.printf "%03S\n" "toto"
;;
let () = Printf.printf "%.3s\n" "toto"
;;

(* it still fails on flags used with ignored formats (%_d, etc.),
   but it's unclear how to test that in a backward-compatible way,
   if we accept that the error message may have changed
*)
