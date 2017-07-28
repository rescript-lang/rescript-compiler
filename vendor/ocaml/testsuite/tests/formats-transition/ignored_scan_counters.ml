(* Benoît's patch did not support %_[nlNL]; test their behavior *)

(* not supported by Printf or Format: fails at runtime *)
let () = Printf.printf "%_n"
;;
let () = Printf.printf "%_N"
;;
let () = Printf.printf "%_l"
;;
let () = Printf.printf "%_L"
;;

let () = Format.printf "%_n"
;;
let () = Format.printf "%_N"
;;
let () = Format.printf "%_l"
;;
let () = Format.printf "%_L"
;;

(* identity for Scanf *)
let () = print_endline (Scanf.sscanf "" "%_n" "Hello World!")
;;
let () = print_endline (Scanf.sscanf "" "%_N" "Hello World!")
;;
let () = print_endline (Scanf.sscanf "" "%_l" "Hello World!")
;;
let () = print_endline (Scanf.sscanf "" "%_L" "Hello World!")
;;
