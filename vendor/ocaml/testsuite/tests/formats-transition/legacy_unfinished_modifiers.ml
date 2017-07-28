(* test whether padding modifiers are accepted without any padding
   size

   the precision modifier is accepted without precision setting, but it
   defaults to 0, which is not the same thing as not having precision:
     %.0f 3.5 => 3
     %.f 3.5 => 3
     %f  3.5 => 3.5
*)

let () = Printf.printf "%0d\n" 3
;;
let () = Printf.printf "%-d\n" 3
;;
let () = Printf.printf "%.d\n" 3
;;
let () = Printf.printf "%.f\n" 3.
;;
