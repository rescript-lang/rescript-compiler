open Scanf
let suites :  Mt.pair_suites ref  = ref []
let test_id = ref 0
let eq f (a,b) = Mt_global.collect_eq test_id suites f a b 


let rec scan_elems ib accu =
  bscanf ib "%1[];]" (function
  | "]" -> accu
  | ";" -> scan_rest ib accu
  | _ ->
    failwith
      (Printf.sprintf "scan_int_list" (*
        "scan_int_list: char %i waiting for ']' or ';' but found %c"
        (Scanning.char_count ib) (Scanning.peek_char ib)*)))

(* and scan_elem ib accu = *)
(*   bscanf ib " %i " (fun i -> scan_elems ib (i :: accu)) *)

and scan_rest ib accu =
  bscanf ib "%[]]" (function
  | "]" -> accu
  | _ -> scan_elem ib accu)

and scan_elem ib accu =
  bscanf ib " %i " (fun i -> scan_elems ib (i :: accu))
;;

let scan_int_list ib =
  bscanf ib " [ " ();
  List.rev (scan_rest ib [])
;;


let _ =
  eq __LOC__ (scan_int_list (Scanning.from_string "[]"), [])

let () = 
  Mt.from_pair_suites __MODULE__ !suites
