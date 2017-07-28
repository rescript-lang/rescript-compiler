(* these are not valid under -strict-formats, but we test them here
   for backward-compatibility *)
open Printf

let () =
  printf "1 [%.5s]\n" "foo";
  printf "2 [%.*s]\n" 5 "foo";
  printf "3 [%.-5s]\n" "foo";
  printf "4 [%-.5s]\n" "foo";
  printf "5 [%-.*s]\n" 5 "foo";
  printf "6 [%.*s]\n" (-5) "foo";

  printf "1 [%.7S]\n" "foo";
  printf "2 [%.*S]\n" 7 "foo";
  printf "3 [%.-7S]\n" "foo";
  printf "4 [%-.7S]\n" "foo";
  printf "5 [%-.*S]\n" 7 "foo";
  printf "6 [%.*S]\n" (-7) "foo";
  ()
