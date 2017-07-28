(*************************************************************************)
(*                                                                       *)
(*                                 OCaml                                 *)
(*                                                                       *)
(*            Pierre Weis, projet Cristal, INRIA Rocquencourt            *)
(*                                                                       *)
(*   Copyright 2002 Institut National de Recherche en Informatique et    *)
(*   en Automatique.  All rights reserved.  This file is distributed     *)
(*   under the terms of the Q Public License version 1.0.                *)
(*                                                                       *)
(*************************************************************************)

(*

A testbed file for the module Scanf.

*)

open Testing;;

open Scanf;;

(* The ``continuation'' that returns the scanned value. *)
let id x = x;;

(* Testing space scanning. *)
let test0 () =
  (sscanf "" "" id) 1 +
  (sscanf "" " " id) 2 +
  (sscanf " " " " id) 3 +
  (sscanf "\t" " " id) 4 +
  (sscanf "\n" " " id) 5 +
  (sscanf "\n\t 6" " %d" id)
;;

test (test0 () = 21)
;;

(* Testing integer scanning %i and %d. *)
let test1 () =
  sscanf "1" "%d" id +
  sscanf " 2" " %d" id +
  sscanf " -2" " %d" id +
  sscanf " +2" " %d" id +
  sscanf " 2a " " %da" id
;;

test (test1 () = 5)
;;

let test2 () =
  sscanf "123" "%2i" id +
  sscanf "245" "%d" id +
  sscanf " 2a " " %1da" id
;;

test (test2 () = 259)
;;

let test3 () =
  sscanf "0xff" "%3i" id +
  sscanf "0XEF" "%3i" id +
  sscanf "x=-245" " x = %d" id +
  sscanf " 2a " " %1da" id
;;

test (test3 () = -214)
;;

(* Testing float scanning. *)
(* f style. *)
let test4 () =
  bscanf (Scanning.from_string "1")
    "%f" (fun b0 -> b0 = 1.0) &&
  bscanf (Scanning.from_string "-1")
    "%f" (fun b0 -> b0 = -1.0) &&
  bscanf (Scanning.from_string "+1")
    "%f" (fun b0 -> b0 = 1.0) &&
  bscanf (Scanning.from_string "1.")
    "%f" (fun b0 -> b0 = 1.0) &&
  bscanf (Scanning.from_string ".1")
    "%f" (fun b0 -> b0 = 0.1) &&
  bscanf (Scanning.from_string "-.1")
    "%f" (fun b0 -> b0 = -0.1) &&
  bscanf (Scanning.from_string "+.1")
    "%f" (fun b0 -> b0 = 0.1) &&
  bscanf (Scanning.from_string "+1.")
    "%f" (fun b0 -> b0 = 1.0) &&
  bscanf (Scanning.from_string "-1.")
    "%f" (fun b0 -> b0 = -1.0) &&
  bscanf (Scanning.from_string "0 1. 1.3")
    "%f %f %f" (fun b0 b1 b2 -> b0 = 0.0 && b1 = 1.0 && b2 = 1.3) &&
  bscanf (Scanning.from_string "0.113")
    "%4f" (fun b0 -> b0 = 0.11) &&
  bscanf (Scanning.from_string "0.113")
    "%5f" (fun b0 -> b0 = 0.113) &&
  bscanf (Scanning.from_string "000.113")
    "%15f" (fun b0 -> b0 = 0.113) &&
  bscanf (Scanning.from_string "+000.113")
    "%15f" (fun b0 -> b0 = 0.113) &&
  bscanf (Scanning.from_string "-000.113")
    "%15f" (fun b0 -> b0 = -0.113)
;;
test (test4 ())
;;

(* e style. *)
let test5 () =
  bscanf (Scanning.from_string "1e1")
    "%e" (fun b -> b = 10.0) &&
  bscanf (Scanning.from_string "1e+1")
    "%e" (fun b -> b = 10.0) &&
  bscanf (Scanning.from_string "10e-1")
    "%e" (fun b -> b = 1.0) &&
  bscanf (Scanning.from_string "10.e-1")
    "%e" (fun b -> b = 1.0) &&
  bscanf (Scanning.from_string "1e1 1.e+1 1.3e-1")
    "%e %e %e" (fun b1 b2 b3 -> b1 = 10.0 && b2 = b1 && b3 = 0.13) &&

(* g style. *)
  bscanf (Scanning.from_string "1 1.1 0e+1 1.3e-1")
    "%g %g %g %g"
    (fun b1 b2 b3 b4 ->
     b1 = 1.0 && b2 = 1.1 && b3 = 0.0 && b4 = 0.13)
;;

test (test5 ())
;;

(* Testing boolean scanning. *)
let test6 () =
  bscanf (Scanning.from_string "truetrue") "%B%B"
         (fun b1 b2 -> (b1, b2) = (true, true))  &&
  bscanf (Scanning.from_string "truefalse") "%B%B"
         (fun b1 b2 -> (b1, b2) = (true, false)) &&
  bscanf (Scanning.from_string "falsetrue") "%B%B"
         (fun b1 b2 -> (b1, b2) = (false, true)) &&
  bscanf (Scanning.from_string "falsefalse") "%B%B"
         (fun b1 b2 -> (b1, b2) = (false, false)) &&
  bscanf (Scanning.from_string "true false") "%B %B"
         (fun b1 b2 -> (b1, b2) = (true, false))
;;

test (test6 ())
;;

(* Testing char scanning. *)

let test7 () =
  bscanf (Scanning.from_string "'a' '\n' '\t' '\000' '\032'")
         "%C %C %C %C %C"
    (fun c1 c2 c3 c4 c5 ->
       c1 = 'a' && c2 = '\n' && c3 = '\t' && c4 = '\000' && c5 = '\032') &&

(* Here \n, \t, and \032 are skipped due to the space semantics of scanf. *)
  bscanf (Scanning.from_string "a \n \t \000 \032b")
         "%c %c %c "
    (fun c1 c2 c3 ->
       c1 = 'a' && c2 = '\000' && c3 = 'b')
;;

test (test7 ())
;;

let verify_read c =
  let s = Printf.sprintf "%C" c in
  let ib = Scanning.from_string s in
  assert (bscanf ib "%C" id = c)
;;

let verify_scan_Chars () =
  for i = 0 to 255 do verify_read (char_of_int i) done
;;

let test8 () = verify_scan_Chars () = ();;

test (test8 ())
;;

(* Testing string scanning. *)

(* %S and %s styles. *)
let unit fmt s =
  let ib = Scanning.from_string (Printf.sprintf "%S" s) in
  Scanf.bscanf ib fmt id
;;

let test_fmt fmt s = unit fmt s = s;;

let test9_string = "\239\187\191";;

let test_S = test_fmt "%S";;
let test9 () =
  test_S "poi" &&
  test_S "a\"b" &&
  test_S "a\nb" &&
  test_S "a\010b" &&
  test_S "a\\\n\
          b \\\n\
          c\010\\\n\
          b" &&
  test_S "a\\\n\
          \\\n\
          \\\n\
          b \\\n\
          c\010\\\n\
          b" &&
  test_S "\xef" &&
  test_S "\\xef" &&
  Scanf.sscanf "\"\\xef\"" "%S" (fun s -> s) =
                  "\xef" &&
  Scanf.sscanf "\"\\xef\\xbb\\xbf\"" "%S" (fun s -> s) =
                  test9_string &&
  Scanf.sscanf "\"\\xef\\xbb\\xbf\"" "%S" (fun s -> s) =
                  "\239\187\191" &&
  Scanf.sscanf "\"\xef\xbb\xbf\"" "%S" (fun s -> s) =
                  test9_string &&
  Scanf.sscanf "\"\\\\xef\\\\xbb\\\\xbf\"" "%S" (fun s -> s) =
                  "\\xef\\xbb\\xbf" &&
  Scanf.sscanf "\"\ \"" "%S" (fun s -> s) =
                  "\ "
;;

test (test9 ())
;;

let test10 () =
  let unit s =
    let ib = Scanning.from_string s in
  Scanf.bscanf ib "%S" id in

  let res =
    sscanf "Une chaine: \"celle-ci\" et \"celle-la\"!"
           "%s %s %S %s %S %s"
           (fun s1 s2 s3 s4 s5 s6 -> s1 ^ s2 ^ s3 ^ s4 ^ s5 ^ s6) in
  res = "Unechaine:celle-cietcelle-la!" &&
  (* Testing the result of reading a %S string. *)
  unit "\"a\\\n  b\"" = "ab" &&
  unit "\"\\\n  ab\"" = "ab" &&
  unit "\"\n\\\n  ab\"" = "\nab" &&
  unit "\"\n\\\n  a\nb\"" = "\na\nb" &&
  unit "\"\n\\\n  \\\n  a\nb\"" = "\na\nb" &&
  unit "\"\n\\\n  a\n\\\nb\\\n\"" = "\na\nb" &&
  unit "\"a\\\n  \"" = "a" &&
  true
;;

test (test10 ())
;;

(* %[] style *)
let test11 () =
  sscanf "Pierre\tWeis\t70" "%s %s %s"
    (fun prenom nom poids ->
     prenom = "Pierre" && nom = "Weis" && int_of_string poids = 70)
  &&
  sscanf "Jean-Luc\tde Leage\t68" "%[^\t] %[^\t] %d"
    (fun prenom nom poids ->
     prenom = "Jean-Luc" && nom = "de Leage" && poids = 68)
  &&
  sscanf "Daniel\tde Rauglaudre\t66" "%s@\t %s@\t %d"
    (fun prenom nom poids ->
     prenom = "Daniel" && nom = "de Rauglaudre" && poids = 66)
;;

(* Empty string (end of input) testing. *)
let test110 () =
  sscanf "" " " (fun x -> x) "" = "" &&
  sscanf "" "%s" (fun x -> x = "") &&
  sscanf "" "%s%s" (fun x y -> x = "" && y = "") &&
  sscanf "" "%s " (fun x -> x = "") &&
  sscanf "" " %s" (fun x -> x = "") &&
  sscanf "" " %s " (fun x -> x = "") &&
  sscanf "" "%[^\n]" (fun x -> x = "") &&
  sscanf "" "%[^\n] " (fun x -> x = "") &&
  sscanf " " "%s" (fun x -> x = "") &&
  sscanf " " "%s%s" (fun x y -> x = "" && y = "") &&
  sscanf " " " %s " (fun x -> x = "") &&
  sscanf " " " %s %s" (fun x y -> x = "" && x = y) &&
  sscanf " " " %s@ %s" (fun x y -> x = "" && x = y) &&
  sscanf " poi !" " %s@ %s@." (fun x y -> x = "poi" && y = "!") &&
  sscanf " poi !" "%s@ %s@." (fun x y -> x = "" && y = "poi !")
;;

let test111 () = sscanf "" "%[^\n]@\n" (fun x -> x = "");;

test (test11 () && test110 () && test111 ())
;;

(* Scanning lists. *)
let ib () = Scanning.from_string "[1;2;3;4; ]";;

(* Statically known lists can be scanned directly. *)
let f ib =
  bscanf ib " [" ();
  bscanf ib " %i;" (fun i ->
  bscanf ib " %i;" (fun j ->
  bscanf ib " %i;" (fun k ->
  bscanf ib " %i;" (fun l ->
  bscanf ib " ]" ();
  [i; j; k; l]))));;

let test12 () = f (ib ()) = [1; 2; 3; 4];;

test (test12 ())
;;

(* A general list scanner that always fails to succeed. *)
let rec scan_elems ib accu =
  try bscanf ib " %i;" (fun i -> scan_elems ib (i :: accu)) with
  | _ -> accu
;;

let g ib = bscanf ib "[ " (); List.rev (scan_elems ib []);;

let test13 () = g (ib ()) = [1; 2; 3; 4];;

test (test13 ())
;;

(* A general int list scanner. *)
let rec scan_int_list ib =
  bscanf ib "[ " ();
  let accu = scan_elems ib [] in
  bscanf ib " ]" ();
  List.rev accu
;;

let test14 () = scan_int_list (ib ()) = [1; 2; 3; 4];;

test (test14 ())
;;

(* A general list scanner that always succeeds. *)
let rec scan_elems ib accu =
  bscanf ib " %i %c"
    (fun i -> function
     | ';' -> scan_elems ib (i :: accu)
     | ']' -> List.rev (i :: accu)
     | c -> failwith "scan_elems")
;;

let rec scan_int_list ib =
  bscanf ib "[ " ();
  scan_elems ib []
;;

let test15 () =
  scan_int_list (Scanning.from_string "[1;2;3;4]") = [1; 2; 3; 4];;

test (test15 ())
;;

let rec scan_elems ib accu =
  try
  bscanf ib "%c %i"
    (fun c i ->
     match c with
     | ';' -> scan_elems ib (i :: accu)
     | ']' -> List.rev (i :: accu)
     | '[' when accu = [] -> scan_elems ib (i :: accu)
     | c -> print_endline (String.make 1 c); failwith "scan_elems")
  with
  | Scan_failure _ -> bscanf ib "]" (); accu
  | End_of_file -> accu
;;

let scan_int_list ib = scan_elems ib [];;

let test16 () =
  scan_int_list (Scanning.from_string "[]") = List.rev [] &&
  scan_int_list (Scanning.from_string "[1;2;3;4]") = List.rev [1;2;3;4] &&
  scan_int_list (Scanning.from_string "[1;2;3;4; ]") = List.rev [1;2;3;4] &&
  (* Should fail but succeeds! *)
  scan_int_list (Scanning.from_string "[1;2;3;4") = List.rev [1;2;3;4];;

test (test16 ())
;;

let rec scan_elems ib accu =
  bscanf ib " %i%[]; \t\n\r]"
    (fun i s ->
     match s with
     | ";" -> scan_elems ib (i :: accu)
     | "]" -> List.rev (i :: accu)
     | s -> List.rev (i :: accu))
;;

let scan_int_list ib =
  bscanf ib " [" ();
  scan_elems ib []
;;

let test17 () =
  scan_int_list (Scanning.from_string "[1;2;3;4]") = [1;2;3;4] &&
  scan_int_list (Scanning.from_string "[1;2;3;4; ]") = [1;2;3;4] &&
  (* Should fail but succeeds! *)
  scan_int_list (Scanning.from_string "[1;2;3;4 5]") = [1;2;3;4];;

test (test17 ())
;;

let rec scan_elems ib accu =
  bscanf ib " %c " (fun c ->
    match c with
    | '[' when accu = [] ->
        (* begginning of list: could find either
           - an int, if the list is not empty,
           - the char ], if the list is empty. *)
        bscanf ib "%[]]"
          (function
           | "]" -> accu
           | _ ->
             bscanf ib " %i " (fun i ->
               scan_rest ib (i :: accu)))
    | _ -> failwith "scan_elems")

and scan_rest ib accu =
  bscanf ib " %c " (fun c ->
    match c with
    | ';' ->
        bscanf ib "%[]]"
          (function
           | "]" -> accu
           | _ ->
             bscanf ib " %i " (fun i ->
             scan_rest ib (i :: accu)))
    | ']' -> accu
    | _ -> failwith "scan_rest")
;;

let scan_int_list ib = List.rev (scan_elems ib []);;

let test18 () =
  scan_int_list (Scanning.from_string "[]") = [] &&
  scan_int_list (Scanning.from_string "[ ]") = [] &&
  scan_int_list (Scanning.from_string "[1;2;3;4]") = [1;2;3;4] &&
  scan_int_list (Scanning.from_string "[1;2;3;4; ]") = [1;2;3;4];;

test (test18 ())
;;

(* Those properly fail *)

let test19 () =
  failure_test
    scan_int_list (Scanning.from_string "[1;2;3;4 5]")
    "scan_rest"
;;

(test19 ())
;;

let test20 () =
  scan_failure_test
    scan_int_list (Scanning.from_string "[1;2;3;4;; 5]");;

(test20 ())
;;

let test21 () =
  scan_failure_test
    scan_int_list (Scanning.from_string "[1;2;3;4;;");;

(test21 ())
;;

let rec scan_elems ib accu =
  bscanf ib "%1[];]" (function
  | "]" -> accu
  | ";" -> scan_rest ib accu
  | _ ->
    failwith
      (Printf.sprintf "scan_int_list" (*
        "scan_int_list: char %i waiting for ']' or ';' but found %c"
        (Scanning.char_count ib) (Scanning.peek_char ib)*)))

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

let test22 () =
  scan_int_list (Scanning.from_string "[]") = [] &&
  scan_int_list (Scanning.from_string "[ ]") = [] &&
  scan_int_list (Scanning.from_string "[1]") = [1] &&
  scan_int_list (Scanning.from_string "[1;2;3;4]") = [1;2;3;4] &&
  scan_int_list (Scanning.from_string "[1;2;3;4;]") = [1;2;3;4];;

test (test22 ())
;;

(* Should work but does not with this version of scan_int_list!
scan_int_list (Scanning.from_string "[1;2;3;4; ]");;
(* Should lead to a bad input error. *)
scan_int_list (Scanning.from_string "[1;2;3;4 5]");;
scan_int_list (Scanning.from_string "[1;2;3;4;;");;
scan_int_list (Scanning.from_string "[1;2;3;4;; 5]");;
scan_int_list (Scanning.from_string "[1;2;3;4;; 23]");;
*)

let rec scan_elems ib accu =
  try bscanf ib " %i %1[;]" (fun i s ->
   if s = "" then i :: accu else scan_elems ib (i :: accu)) with
  | Scan_failure _ -> accu
;;

(* The general int list scanner. *)
let rec scan_int_list ib =
  bscanf ib "[ " ();
  let accu = scan_elems ib [] in
  bscanf ib " ]" ();
  List.rev accu
;;

(* The general HO list scanner.
   This version does not fix the separator, nor the spacing before and after
   the separator (it uses the functional argument [scan_elem] to parse the
   separator, its spacing, and the item).
 *)
let rec scan_elems ib scan_elem accu =
  try scan_elem ib (fun i s ->
    let accu = i :: accu in
    if s = "" then accu else scan_elems ib scan_elem accu) with
  | Scan_failure _ -> accu
;;

let scan_list scan_elem ib =
  bscanf ib "[ " ();
  let accu = scan_elems ib scan_elem [] in
  bscanf ib " ]" ();
  List.rev accu
;;

(* Deriving particular list scanners from the HO list scanner. *)
let scan_int_elem ib = bscanf ib " %i %1[;]";;
let scan_int_list = scan_list scan_int_elem;;

let test23 () =
  scan_int_list (Scanning.from_string "[]") = [] &&
  scan_int_list (Scanning.from_string "[ ]") = [] &&
  scan_int_list (Scanning.from_string "[1]") = [1] &&
  scan_int_list (Scanning.from_string "[1;2;3;4]") = [1;2;3;4] &&
  scan_int_list (Scanning.from_string "[1;2;3;4;]") = [1;2;3;4];;

test (test23 ())
;;

let test24 () =
  scan_failure_test scan_int_list (Scanning.from_string "[1;2;3;4 5]")
and test25 () =
  scan_failure_test scan_int_list (Scanning.from_string "[1;2;3;4;;")
and test26 () =
  scan_failure_test scan_int_list (Scanning.from_string "[1;2;3;4;; 5]")
and test27 () =
  scan_failure_test scan_int_list (Scanning.from_string "[1;2;3;4;; 23]");;

 (test24 ()) &&
 (test25 ()) &&
 (test26 ()) &&
 (test27 ())
;;

(* To scan an OCaml string:
   the format is "\"%s@\"".
   A better way would be to add a %S (String.escaped), a %C (Char.escaped).
   This is now available. *)
let scan_string_elem ib = bscanf ib " \"%s@\" %1[;]";;
let scan_string_list = scan_list scan_string_elem;;

let scan_String_elem ib = bscanf ib " %S %1[;]";;
let scan_String_list = scan_list scan_String_elem;;

let test28 () =
  scan_string_list (Scanning.from_string "[]") = [] &&
  scan_string_list (Scanning.from_string "[\"Le\"]") = ["Le"] &&
  scan_string_list
    (Scanning.from_string "[\"Le\";\"langage\";\"Objective\";\"Caml\"]") =
    ["Le"; "langage"; "Objective"; "Caml"] &&
  scan_string_list
    (Scanning.from_string "[\"Le\";\"langage\";\"Objective\";\"Caml\"; ]") =
    ["Le"; "langage"; "Objective"; "Caml"] &&

  scan_String_list (Scanning.from_string "[]") = [] &&
  scan_String_list (Scanning.from_string "[\"Le\"]") = ["Le"] &&
  scan_String_list
    (Scanning.from_string "[\"Le\";\"langage\";\"Objective\";\"Caml\"]") =
    ["Le"; "langage"; "Objective"; "Caml"] &&
  scan_String_list
    (Scanning.from_string "[\"Le\";\"langage\";\"Objective\";\"Caml\"; ]") =
    ["Le"; "langage"; "Objective"; "Caml"];;

test (test28 ())
;;

(* The general HO list scanner with continuations. *)
let rec scan_elems ib scan_elem accu =
  scan_elem ib
    (fun i s ->
     let accu = i :: accu in
     if s = "" then accu else scan_elems ib scan_elem accu)
    (fun ib exc -> accu)
;;

let scan_list scan_elem ib =
  bscanf ib "[ " ();
  let accu = scan_elems ib scan_elem [] in
  bscanf ib " ]" ();
  List.rev accu
;;

(* Deriving particular list scanners from the HO list scanner. *)
let scan_int_elem ib f ek = kscanf ib ek " %i %1[;]" f;;
let scan_int_list = scan_list scan_int_elem;;

let test29 () =
  scan_int_list (Scanning.from_string "[]") = [] &&
  scan_int_list (Scanning.from_string "[ ]") = [] &&
  scan_int_list (Scanning.from_string "[1]") = [1] &&
  scan_int_list (Scanning.from_string "[1;2;3;4]") = [1;2;3;4] &&
  scan_int_list (Scanning.from_string "[1;2;3;4;]") = [1;2;3;4];;

test (test29 ())
;;

let scan_string_elem ib f ek = kscanf ib ek " %S %1[;]" f;;
let scan_string_list = scan_list scan_string_elem;;

let test30 () =
  scan_string_list (Scanning.from_string "[]") = [] &&
  scan_string_list (Scanning.from_string "[ ]") = [] &&
  scan_string_list (Scanning.from_string "[ \"1\" ]") = ["1"] &&
  scan_string_list
    (Scanning.from_string "[\"1\"; \"2\"; \"3\"; \"4\"]") =
    ["1"; "2"; "3"; "4"] &&
  scan_string_list
    (Scanning.from_string "[\"1\"; \"2\"; \"3\"; \"4\";]") =
    ["1"; "2"; "3"; "4"];;

test (test30 ())
;;

(* A generic polymorphic item scanner, *)
let scan_elem fmt ib f ek = kscanf ib ek fmt f;;

(* Derivation of list scanners from the generic polymorphic item scanner
   applications. *)
let scan_int_list = scan_list (scan_elem " %i %1[;]");;
let scan_string_list = scan_list (scan_elem " %S %1[;]");;
let scan_bool_list = scan_list (scan_elem " %B %1[;]");;
let scan_char_list = scan_list (scan_elem " %C %1[;]");;
let scan_float_list = scan_list (scan_elem " %f %1[;]");;

(* In this version the [scan_elem] function should be a [kscanf] like
   scanning function: we give it an error continuation.

   The [scan_elem] argument, probably use some partial application of the
   following generic [scan_elem]:

   let scan_elem fmt ib f ek = kscanf ib ek fmt f;;

   For instance, a suitable [scan_elem] for integers could be:

   let scan_integer_elem = scan_elem " %i";;

*)
let rec scan_elems ib scan_elem accu =
  scan_elem ib
    (fun i ->
     let accu = i :: accu in
     kscanf ib
      (fun ib exc -> accu)
      " %1[;]"
      (fun s -> if s = "" then accu else scan_elems ib scan_elem accu))
  (fun ib exc -> accu)
;;

let scan_list scan_elem ib =
  bscanf ib "[ " ();
  let accu = scan_elems ib scan_elem [] in
  bscanf ib " ]" ();
  List.rev accu
;;

let scan_int_list = scan_list (scan_elem " %i");;
let scan_string_list = scan_list (scan_elem " %S");;
let scan_bool_list = scan_list (scan_elem " %B");;
let scan_char_list = scan_list (scan_elem " %C");;
let scan_float_list = scan_list (scan_elem " %f");;

let test31 () =
  scan_int_list (Scanning.from_string "[]") = [] &&
  scan_int_list (Scanning.from_string "[ ]") = [] &&
  scan_int_list (Scanning.from_string "[1]") = [1] &&
  scan_int_list (Scanning.from_string "[1;2;3;4]") = [1;2;3;4] &&
  scan_int_list (Scanning.from_string "[1;2;3;4;]") = [1;2;3;4];;

test (test31 ())
;;

let test32 () =
  scan_string_list (Scanning.from_string "[]") = [] &&
  scan_string_list (Scanning.from_string "[ ]") = [] &&
  scan_string_list (Scanning.from_string "[ \"1\" ]") = ["1"] &&
  scan_string_list
    (Scanning.from_string "[\"1\"; \"2\"; \"3\"; \"4\"]") =
    ["1"; "2"; "3"; "4"] &&
  scan_string_list
    (Scanning.from_string "[\"1\"; \"2\"; \"3\"; \"4\";]") =
    ["1"; "2"; "3"; "4"];;

test (test32 ())
;;

(* Using [kscanf] only.

   We use format values to stand for ``functional'' specifications to scan
   the elements of lists.

   The list item separator and the separator spacing are builtin into the
   [scan_elems] iterator and thus are conveniently omitted from the
   definitional format for item scanning.
*)
let rec scan_elems ib scan_elem_fmt accu =
  kscanf ib (fun ib exc -> accu)
    scan_elem_fmt
    (fun i ->
     let accu = i :: accu in
     bscanf ib
       " %1[;] "
       (function
        | "" -> accu
        | _ -> scan_elems ib scan_elem_fmt accu)
    )
;;

let scan_list scan_elem_fmt ib =
  bscanf ib "[ " ();
  let accu = scan_elems ib scan_elem_fmt [] in
  bscanf ib " ]" ();
  List.rev accu
;;

let scan_int_list = scan_list "%i";;
let scan_string_list = scan_list "%S";;
let scan_bool_list = scan_list "%B";;
let scan_char_list = scan_list "%C";;
let scan_float_list = scan_list "%f";;

let test33 () =
  scan_int_list (Scanning.from_string "[]") = [] &&
  scan_int_list (Scanning.from_string "[ ]") = [] &&
  scan_int_list (Scanning.from_string "[ 1 ]") = [1] &&
  scan_int_list (Scanning.from_string "[ 1; 2; 3; 4 ]") = [1; 2; 3; 4] &&
  scan_int_list (Scanning.from_string "[1;2;3;4;]") = [1; 2; 3; 4];;

test (test33 ())
;;

let test34 () =
  scan_string_list (Scanning.from_string "[]") = [] &&
  scan_string_list (Scanning.from_string "[ ]") = [] &&
  scan_string_list (Scanning.from_string "[ \"1\" ]") = ["1"] &&
  scan_string_list
    (Scanning.from_string "[\"1\"; \"2\"; \"3\"; \"4\"]") =
    ["1"; "2"; "3"; "4"] &&
  scan_string_list
    (Scanning.from_string "[\"1\"; \"2\"; \"3\"; \"4\";]") =
    ["1"; "2"; "3"; "4"];;

test (test34 ())
;;

(* Using kscanf only.

   Same as the preceding functional, except that we no more use format values
   to scan items: we use scanners that scan elements of the list on the
   fly.
*)
(* This version cannot handle empty lists!
let rec scan_elems ib scan_elem accu =
  scan_elem ib
    (fun elem ->
     let accu = elem :: accu in
     kscanf ib (fun ib exc -> accu)
       " %1[;] "
       (function
        | "" -> accu
        | _ -> scan_elems ib scan_elem accu))
;;
*)

(* We use [kscanf] with a [%r] format ! *)
let rec scan_elems scan_elem accu ib =
  kscanf ib (fun ib exc -> accu)
    "%r"
    (function ib ->
     scan_elem ib
       (function elem ->
        let accu = elem :: accu in
        bscanf ib
          " %1[;] "
          (function
           | "" -> accu
           | _ -> scan_elems scan_elem accu ib)))
    (function l -> l)
;;

let scan_list scan_elem ib =
  bscanf ib "[ " ();
  let accu = scan_elems scan_elem [] ib in
  bscanf ib " ]" ();
  List.rev accu
;;

(* We may also try a version with only one format:
   We also changed the type of [scan_elem] to partially apply it to its
   ``natural'' continuation.
let rec scan_elems scan_elem accu ib =
  (* We use [kscanf], so that:
     if the element reader fails, we can return the list of elements read so
     far. *)
  kscanf ib (fun ib exc -> accu)
    (* The format string for [kscanf]: we read an element using [scan_elem],
       then find a semi-colon if any, in order to decide if we stop reading
       or go on with other elements. *)
    "%r %1[;] "
    (* The reader: once an element has been read it returns the new accu. *)
    (scan_elem (function elem -> elem :: accu))
    (fun accu s ->
     (* Cannot find a semi-colon: no more elements to read. *)
     if s = "" then accu
     (* We found a semi-colon: go on with the new accu. *)
     else scan_elems scan_elem accu ib)
;;

let scan_list scan_elem ib =
  bscanf ib "[ %r ]" (scan_elems scan_elem []) List.rev
;;

(* For instance:
let scan_float f ib = Scanf.bscanf ib "%f" f;;
# scan_list scan_float;;
- : Scanf.Scanning.scanbuf -> float list = <fun>
*)

(* The element scanner builder. *)
let make_scan_elem fmt f ib = Scanf.bscanf ib fmt f;;

(* Promote an element reader format to an element list reader. *)
let list_scanner fmt = scan_list (make_scan_elem fmt);;

let scan_float = make_scan_elem "%f";;

scan_list scan_float;;

list_scanner "%f";;
- : Scanf.Scanning.scanbuf -> float list = <fun>
*)

(* The prototype of a [scan_elem] function for the generic [scan_list]
   functional.
   This [scan_elem] scans a floating point number. *)
let scan_float ib = Scanf.bscanf ib "%f";;
let scan_float_list = scan_list scan_float;;

(* In the following list scanners, we directly give the [scan_elem] function
   as an immediate function value argument to the polymorphic
   [scan_list]. *)
let scan_int_list = scan_list (fun ib -> Scanf.bscanf ib "%i");;
let scan_string_list = scan_list (fun ib -> Scanf.bscanf ib "%S");;
let scan_bool_list = scan_list (fun ib -> Scanf.bscanf ib "%B");;
let scan_char_list = scan_list (fun ib -> Scanf.bscanf ib "%C");;

(* [scan_list] is truely polymorphic: scanning a list of lists of items
   is a one liner!

   Here we scan list of lists of floats. *)
let scan_float_list_list =
  scan_list
    (fun ib k -> k (scan_list (fun ib -> Scanf.bscanf ib "%f") ib))
;;

let scan_float_list_list =
  scan_list
    (fun ib k -> k (scan_list scan_float ib))
;;

let scan_float_list_list =
  scan_list
    (fun ib k -> k (scan_float_list ib))
;;

(* The killer way to define [scan_float_list_list]. *)
(* let scan_float_list_list = scan_list scan_float_list;; *)

let test340 () =
  scan_float_list_list
   (Scanning.from_string "[[1.0] ; []; [2.0; 3; 5.0; 6.];]") =
  [[1.]; []; [2.; 3.; 5.; 6.]]
;;

(* A general scan_list_list functional. *)
let scan_list_list scan_elems ib =
  scan_list
    (fun ib k -> k (scan_elems ib)) ib
;;

let scan_float_list_list = scan_list_list scan_float_list;;

(* Programming with continuations :) *)
let scan_float_item ib k = k (scan_float ib (fun x -> x));;
let scan_float_list ib k = k (scan_list scan_float_item ib);;
let scan_float_list_list ib k = k (scan_list scan_float_list ib);;

(* Testing the %N format. *)
let test35 () =
  sscanf "" "%N" (fun x -> x) = 0 &&
  sscanf "456" "%N" (fun x -> x) = 0 &&
  sscanf "456" "%d%N" (fun x y -> x, y) = (456, 1) &&
  sscanf " " "%N%s%N" (fun x s y -> x, s, y) = (0, "", 1)
;;

test (test340 () && test35 ())
;;

(* The prefered reader functionnals. *)

(* To read a list as in OCaml (elements are ``blank + semicolon + blank''
   separated, and the list is enclosed in brackets). *)
let rec read_elems read_elem accu ib =
  kscanf ib (fun ib exc -> accu)
    "%r %1[;] "
    (read_elem (function elem -> elem :: accu))
    (fun accu s -> if s = "" then accu else read_elems read_elem accu ib)
;;

let read_list read_elem ib =
  bscanf ib "[ %r ]" (read_elems read_elem []) List.rev
;;

(* The element reader builder. *)
let make_read_elem fmt f ib = Scanf.bscanf ib fmt f;;

(* Promote an element reader format to an element list reader. *)
let scan_List fmt = read_list (make_read_elem fmt);;

(* Example for list of floatting point numbers. *)
(*
scan_List "%f";;
- : Scanf.Scanning.scanbuf -> float list = <fun>

(* To read a list as a succession of elements separated by a blank. *)
let rec read_elems read_elem accu ib =
  kscanf ib (fun ib exc -> accu)
    "%r "
    (read_elem (function elem -> elem :: accu))
    (fun accu -> read_elems read_elem accu ib)
;;

let read_list read_elem ib =
   List.rev (read_elems read_elem [] ib)
;;

(* Promote an element reader format to an element list reader. *)
let scan_list fmt = read_list (make_read_elem fmt);;

scan_list "%f";;
*)

(* Testing the %n format. *)
let test36 () =
  sscanf "" "%n" (fun x -> x) = 0 &&
  sscanf "456" "%n" (fun x -> x) = 0 &&
  sscanf "456" "%d%n" (fun x y -> x, y) = (456, 3) &&
  sscanf " " "%n%s%n" (fun x s y -> x, s, y) = (0, "", 0)
;;

test (test36 ())
;;

(* Weird tests to empty strings or formats. *)
let test37 () =
  sscanf "" "" true &&
  sscanf "" "" (fun x -> x) 1 = 1 &&
  sscanf "123" "" (fun x -> x) 1 = 1
;;

test (test37 ())
;;

(* Testing end of input condition. *)
let test38 () =
  sscanf "a" "a%!" true &&
  sscanf "a" "a%!%!" true &&
  sscanf " a" " a%!" true &&
  sscanf "a " "a %!" true &&
  sscanf "" "%!" true &&
  sscanf " " " %!" true &&
  sscanf "" " %!" true &&
  sscanf "" " %!%!" true
;;

test (test38 ())
;;

(* Weird tests on empty buffers. *)
let test39 () =
  let is_empty_buff ib =
    Scanning.beginning_of_input ib &&
    Scanning.end_of_input ib in

  let ib = Scanning.from_string "" in
  is_empty_buff ib &&
  (* Do it twice since testing empty buff could incorrectly
     thraw an exception or wrongly change the beginning_of_input condition. *)
  is_empty_buff ib
;;

test (test39 ())
;;

(* Testing ranges. *)
let test40 () =
 let s = "cba" in
 let ib = Scanning.from_string s in
 bscanf ib "%[^ab]%s%!" (fun s1 s2 -> s1 = "c" && s2 = "ba")
;;

test (test40 ())
;;

let test41 () =
 let s = "cba" in
 let ib = Scanning.from_string s in
 bscanf ib "%[^abc]%[cba]%!" (fun s1 s2 -> s1 = "" && s2 = "cba")
;;

test (test41 ())
;;

let test42 () =
 let s = "defcbaaghi" in
 let ib = Scanning.from_string s in
 bscanf ib "%[^abc]%[abc]%s%!" (fun s1 s2 s3 ->
   s1 = "def" && s2 = "cbaa" && s3 = "ghi") &&
 let ib = Scanning.from_string s in
 bscanf ib "%s@\t" (fun s -> s = "defcbaaghi")
;;

test (test42 ())
;;

(* Testing end of file condition (bug found). *)
let test43, test44 =
 let s = "" in
 let ib = Scanning.from_string s in
 (fun () -> bscanf ib "%i%!" (fun i -> i)),
 (fun () -> bscanf ib "%!%i" (fun i -> i))
;;

test_raises_this_exc End_of_file test43 () &&
test_raises_this_exc End_of_file test44 ()
;;

(* Testing small range scanning (bug found once). *)
let test45 () =
 let s = "12.2" in
 let ib = Scanning.from_string s in
 bscanf ib "%[0-9].%[0-9]%s%!" (fun s1 s2 s3 ->
   s1 = "12" && s2 = "2" && s3 = "")
;;

test (test45 ())
;;

(* Testing printing of meta formats. *)

let test46, test47 =
  (fun () ->
     Printf.sprintf "%i %(%s%)."
       1 "spells one, %s" "in english"),
  (fun () ->
     Printf.sprintf "%i %{%s%}, %s."
       1 "spells one %s" "in english")
;;

test (test46 () = "1 spells one, in english.")
;;
test (test47 () = "1 %s, in english.")
;;

(* Testing scanning of meta formats. *)
let test48 () =
  (* Testing format_from_string. *)
  let test_meta_read s fmt efmt = format_from_string s fmt = efmt in
  (* Test if format %i is indeed read as %i. *)
  let s, fmt = "%i", format_of_string "%i" in
  test_meta_read s fmt fmt &&
  (* Test if format %i is compatible with %d and indeed read as %i. *)
  let s, fmt = "%i", format_of_string "%d" in
  test_meta_read s fmt "%i" &&
  (* Complex test of scanning a meta format specified in the scanner input
     format string and extraction of its specification from a string. *)
  sscanf "12 \"%i\"89 " "%i %{%d%}%s %!"
    (fun i f s -> i = 12 && f = "%i" && s = "89") &&
  (* Testing scanf format string replacement *)
  let k s =
    Scanf.sscanf s
      "%(%f%)" (fun _fmt i -> i) in
  k "\" : %1f\": 987654321" = 9.0  &&
  k "\" : %2f\": 987654321" = 98.0 &&
  k "\" : %3f\": 9.87654321" = 9.8 &&
  k "\" : %4f\": 9.87654321" = 9.87 &&

  let h s =
    Scanf.sscanf s
      "Read integers with %(%i%)" (fun _fmt i -> i) in
  h "Read integers with \"%1d\"987654321" = 9  &&
  h "Read integers with \"%2d\"987654321" = 98 &&
  h "Read integers with \"%3u\"987654321" = 987 &&
  h "Read integers with \"%4x\"987654321" = 39030 &&

  let i s =
    Scanf.sscanf s
      "with %(%i %s%)" (fun _fmt amount currency -> amount, currency) in
  i "with \" : %d %s\" :        21 euros" = (21, "euros")  &&
  i "with \" : %d %s\" : 987654321 dollars" = (987654321, "dollars") &&
  i "with \" : %u %s\" :     54321 pounds" = (54321, "pounds") &&
  i "with \" : %x %s\" :       321 yens" = (801, "yens") &&

  let j s =
    Scanf.sscanf s
      "with %(%i %_s %s%)" (fun _fmt amount currency -> amount, currency) in
  j "with \" : %1d %_s %s\" : 987654321 euros" = (9, "euros")  &&
  j "with \" : %2d %_s %s\" : 987654321 dollars" = (98, "dollars") &&
  j "with \" : %3u %_s %s\" : 987654321 pounds" = (987, "pounds") &&
  j "with \" : %4x %_s %s\" : 987654321 yens" = (39030, "yens")
;;

test (test48 ())
;;

(* Testing stoppers after ranges. *)
let test49 () =
  sscanf "as" "%[\\]" (fun s -> s = "") &&
  sscanf "as" "%[\\]%s" (fun s t -> s = "" && t = "as") &&
  sscanf "as" "%[\\]%s%!" (fun s t -> s = "" && t = "as") &&
  sscanf "as" "%[a..z]" (fun s -> s = "a") &&
  sscanf "as" "%[a-z]" (fun s -> s = "as") &&
  sscanf "as" "%[a..z]%s" (fun s t -> s = "a" && t = "s") &&
  sscanf "as" "%[a-z]%s" (fun s t -> s = "as" && t = "") &&
  sscanf "-as" "%[-a-z]" (fun s -> s = "-as") &&
  sscanf "-as" "%[-a-z]@s" (fun s -> s = "-a") &&
  sscanf "-as" "-%[a]@s" (fun s -> s = "a") &&
  sscanf "-asb" "-%[a]@sb%!" (fun s -> s = "a") &&
  sscanf "-asb" "-%[a]@s%s" (fun s t -> s = "a" && t = "b")
;;

test (test49 ())
;;

(* Testing buffers defined via functions
   + co-routines that read and write from the same buffers
   + range chars and proper handling of \n
   + the end of file condition. *)
let next_char ob () =
  let s = Buffer.contents ob in
  let len = String.length s in
  if len = 0 then raise End_of_file else
  let c = s.[0] in
  Buffer.clear ob;
  Buffer.add_string ob (String.sub s 1 (len - 1));
  c
;;

let send_string ob s =
  Buffer.add_string ob s; Buffer.add_char ob '\n';;
let send_int ob i = send_string ob (string_of_int i);;

let rec reader =
  let count = ref 0 in
  (fun ib ob ->
    if Scanf.Scanning.beginning_of_input ib then begin
      count := 0; send_string ob "start"; writer ib ob end else
    Scanf.bscanf ib "%[^\n]\n" (function
    | "stop" -> send_string ob "stop"; writer ib ob
    | s ->
      let l = String.length s in
      count := l + !count;
      if !count >= 100 then begin
        send_string ob "stop";
        send_int ob !count
        end else
      send_int ob l;
      writer ib ob))

and writer ib ob =
  Scanf.bscanf ib "%s\n" (function
    | "start" -> send_string ob "Hello World!"; reader ib ob
    | "stop" -> Scanf.bscanf ib "%i" (function i -> i)
    | s -> send_int ob (int_of_string s); reader ib ob);;

let go () =
  let ob = Buffer.create 17 in
  let ib = Scanf.Scanning.from_function (next_char ob) in
  reader ib ob
;;

let test50 () = go () = 100;;

test (test50 ())
;;

(* Simple tests may also fail!
   Ensure this is not the case with the current version for module [Scanf]. *)
let test51 () =
 sscanf "Hello" "%s" id = "Hello" &&
 sscanf "Hello\n" "%s\n" id = "Hello" &&
 sscanf "Hello\n" "%s%s\n" (fun s1 s2 ->
   s1 = "Hello" && s2 = "") &&
 sscanf "Hello\nWorld" "%s\n%s%!" (fun s1 s2 ->
   s1 = "Hello" && s2 = "World") &&
 sscanf "Hello\nWorld!" "%s\n%s" (fun s1 s2 ->
   s1 = "Hello" && s2 = "World!") &&
 sscanf "Hello\n" "%s@\n%s" (fun s1 s2 ->
   s1 = "Hello" && s2 = "") &&
 sscanf "Hello \n" "%s@\n%s" (fun s1 s2 ->
   s1 = "Hello " && s2 = "")
;;

test (test51 ())
;;

(* Tests that indeed the [%s@c] format works properly.
   Also tests the difference between [\n] and [@\n] is correctly handled.
   In particular, tests that if no [c] character can be found in the
   input, then the token obtained for [%s@c] spreads to the end of
   input. *)
let test52 () =
 sscanf "Hello\n" "%s@\n" id = "Hello" &&
 sscanf "Hello" "%s@\n" id = "Hello" &&
 sscanf "Hello" "%s%s@\n" (fun s1 s2 ->
   s1 = "Hello" && s2 = "") &&
 sscanf "Hello\nWorld" "%s@\n%s%!" (fun s1 s2 ->
   s1 = "Hello" && s2 = "World") &&
 sscanf "Hello\nWorld!" "%s@\n%s@\n" (fun s1 s2 ->
   s1 = "Hello" && s2 = "World!") &&
 sscanf "Hello\n" "%s@\n%s" (fun s1 s2 ->
   s1 = "Hello" && s2 = "") &&
 sscanf "Hello \n" "%s%s@\n" (fun s1 s2 ->
   s1 = "Hello" && s2 = " ") &&
sscanf "Hello \n" "%s%s%_1[ ]\n" (fun s1 s2 ->
         s1 = "Hello" && s2 = "") &&
 sscanf "Hello \n" "%s%_1[ ]%s\n" (fun s1 s2 ->
   s1 = "Hello" && s2 = "") &&
 sscanf "Hello\nWorld" "%s\n%s%!" (fun s1 s2 ->
      s1 = "Hello" && s2 = "World") &&
 sscanf "Hello\nWorld!" "%s\n%s%!" (fun s1 s2 ->
      s1 = "Hello" && s2 = "World!") &&
 sscanf "Hello\nWorld!" "%s\n%s@!%!" (fun s1 s2 ->
      s1 = "Hello" && s2 = "World") &&
 (* PR#6791 *)
 sscanf "Hello{foo}" "%s@{%s" (fun s1 s2 ->
   s1 = "Hello" && s2 = "foo}") &&
 sscanf "Hello[foo]" "%s@[%s" (fun s1 s2 ->
   s1 = "Hello" && s2 = "foo]")
;;

test (test52 ())
;;

(* Reading native, int32 and int64 numbers. *)
let test53 () =
 sscanf "123" "%nd" id = 123n &&
 sscanf "124" "%nd" (fun i -> Nativeint.pred i = 123n) &&

 sscanf "123" "%ld" id = 123l &&
 sscanf "124" "%ld" (fun i -> Int32.succ i = 125l) &&

 sscanf "123" "%Ld" id = 123L &&
 sscanf "124" "%Ld" (fun i -> Int64.pred i = 123L)
;;

test (test53 ())
;;

(* Routines to create the file that tscanf uses as a testbed case. *)
let create_tscanf_data ob lines =
  let add_line (p, e) =
    Buffer.add_string ob (Printf.sprintf "%S" p);
    Buffer.add_string ob " -> ";
    Buffer.add_string ob (Printf.sprintf "%S" e);
    Buffer.add_string ob ";\n" in
  List.iter add_line lines
;;

let write_tscanf_data_file fname lines =
  let oc = open_out fname in
  let ob = Buffer.create 42 in
  create_tscanf_data ob lines;
  Buffer.output_buffer oc ob;
  close_out oc
;;

(* The tscanf testbed case file name. *)
let tscanf_data_file = "tscanf_data";;
(* The contents of the tscanf testbed case file. *)
let tscanf_data_file_lines = [
    "Objective", "Caml";
]
;;
(* We write the tscanf testbed case file. *)
write_tscanf_data_file tscanf_data_file tscanf_data_file_lines
;;

(* Then we verify that its contents is indeed correct:
   the lines written into the [tscanf_data] file should be the same as the
   lines read from it. *)

(* Reading back tscanf_data_file_lines (hence, testing data file reading as
   well). *)
let get_lines fname =
  let ib = Scanf.Scanning.from_file fname in
  let l = ref [] in
  try
    while not (Scanf.Scanning.end_of_input ib) do
      Scanf.bscanf ib " %S -> %S; " (fun x y ->
        l := (x, y) :: !l)
    done;
    List.rev !l
  with
  | Scanf.Scan_failure s ->
    failwith (Printf.sprintf "in file %s, %s" fname s)
  | End_of_file ->
    failwith (Printf.sprintf "in file %s, unexpected end of file" fname)
;;

(* Simply test that the list of lines read from the file is the list of lines
   written to it!. *)
let test54 () =
  get_lines tscanf_data_file = tscanf_data_file_lines
;;

test (test54 ())
;;

(* Creating digests for files. *)
let add_digest_ib ob ib =
  let digest s = String.uppercase (Digest.to_hex (Digest.string s)) in
  let scan_line ib f = Scanf.bscanf ib "%[^\n\r]\n" f in
  let output_line_digest s =
    Buffer.add_string ob s;
    Buffer.add_char ob '#'; Buffer.add_string ob (digest s);
    Buffer.add_char ob '\n' in
  try while true do scan_line ib output_line_digest done; with
  | End_of_file -> ()
;;

let digest_file fname =
  let ib = Scanf.Scanning.from_file fname in
  let ob = Buffer.create 42 in
  add_digest_ib ob ib;
  Buffer.contents ob
;;

let test55 () =
  let ob = Buffer.create 42 in
  let ib =
    create_tscanf_data ob tscanf_data_file_lines;
    let s = Buffer.contents ob in
    Buffer.clear ob;
    Scanning.from_string s in
  let tscanf_data_file_lines_digest = add_digest_ib ob ib; Buffer.contents ob in
  digest_file tscanf_data_file = tscanf_data_file_lines_digest
;;

test (test55 ())
;;

(* Testing the number of characters read. *)
let test56 () =
   let g s = Scanf.sscanf s "%d%n" (fun i n -> (i, n)) in
   g "99" = (99, 2) &&
   g "99 syntaxes all in a row" = (99, 2) &&
   g "-20 degrees Celsius" = (-20, 3)
;;

test (test56 ())
;;

(* Testing the scanning of formats. *)
let test57 () =
  (* Testing format_from_string. *)
  let test_format_scan s fmt efmt =
    format_from_string s fmt = efmt in
  (* Test if format %i is indeed read as %i. *)
  let s, fmt = " %i ", format_of_string "%i" in
  test_format_scan s fmt " %i " &&
  (* Test if format %i is compatible with %d and indeed read as %i. *)
  let s, fmt = "%i", format_of_string "%d" in
  test_format_scan s fmt "%i" &&

  let s, fmt =
    "Read an int %i then a string %s.",
    format_of_string "Spec%difi%scation" in
  test_format_scan s fmt "Read an int %i then a string %s." &&

  let s, fmt =
    "Read an int %i then a string \"%s\".",
    format_of_string "Spec%difi%Scation" in
  test_format_scan s fmt "Read an int %i then a string \"%s\"." &&

  let s, fmt =
    "Read an int %i then a string \"%s\".",
    format_of_string "Spec%difi%scation" in
  test_format_scan s fmt "Read an int %i then a string \"%s\"." &&

  (* Complex test of scanning a meta format specified in the scanner input
     format string and extraction of its specification from a string. *)
  sscanf "12 \"%i\"89 " "%i %{%d%}%s %!"
    (fun i f s -> i = 12 && f = "%i" && s = "89")
;;

test (test57 ())
;;

let test58 () =
     sscanf "string1%string2" "%s@%%s" id = "string1"
  && sscanf "string1%string2" "%s@%%%s" (^) = "string1string2"
  && sscanf "string1@string2" "%[a-z0-9]@%s" (^) = "string1string2"
  && sscanf "string1@%string2" "%[a-z0-9]%@%%%s" (^) = "string1string2"
;;

test (test58 ())
;;

(* skip test number "59" which is commented below *)
let () = test (true);;
(*
let test59 () =
;;

test (test59 ())
;;
*)

(* To be continued ...
(* Trying to scan records. *)
let rec scan_fields ib scan_field accu =
  kscanf ib (fun ib exc -> accu)
   scan_field
   (fun i ->
      let accu = i :: accu in
      kscanf ib (fun ib exc -> accu)
       " %1[;] "
       (fun s ->
          if s = "" then accu else scan_fields ib scan_field accu))
;;

let scan_record scan_field ib =
  bscanf ib "{ " ();
  let accu = scan_fields ib scan_field [] in
  bscanf ib " }" ();
  List.rev accu
;;

let scan_field ib =
  bscanf ib "%s = %[^;]" (fun finame ficont -> finame, ficont);;
*)

(* testing formats that do not consume their input *)
let test60 () =
  sscanf "abc" "%0c%0c%c%n" (fun c1 c2 c3 n ->
    c1 = 'a' && c2 = 'a' && c3 = 'a' && n = 1)
  &&
  sscanf "abc" "%0s%s" (fun s1 s2 -> s1 = "" && s2 = "abc")
  &&
  sscanf "abc" "%1s%s" (fun s1 s2 -> s1 = "a" && s2 = "bc")
;;

test (test60 ());
