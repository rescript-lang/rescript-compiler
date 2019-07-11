(* let ((>::), (>:::)) = OUnit.((>::),(>:::))

   let (=~) = OUnit.assert_equal

   let suites = __FILE__ >::: [ __LOC__ >:: begin fun _ -> Ext_sexp.from_string
   "( a)" =~ [ List [ Atom "a"]] end; __LOC__ >:: begin fun _ ->
   Ext_sexp.from_string "( a ())" =~ [ List [ Atom "a"; List []]] end; __LOC__
   >:: begin fun _ -> Ext_sexp.from_string "( a (b))" =~ [ List [ Atom "a";
   List [Atom "b"]]] end; __LOC__ >:: begin fun _ -> Ext_sexp.from_string "( a
   (b)) (c d)" =~ [ List [ Atom "a"; List [Atom "b"]]; List [ Atom "c"; Atom
   "d"] ] end; __LOC__ >:: begin fun _ -> Ext_sexp.from_string "( a (b 1 2 3) c
   d) (c d)" =~ [ List [ Atom "a"; List [Atom "b"; Atom "1"; Atom "2"; Atom
   "3"] ; Atom "c"; Atom "d"]; List [ Atom "c"; Atom "d"] ]; end; __LOC__ ^
   "raise" >:: begin fun _ -> (try ignore @@ Ext_sexp.from_string {| (1 2 3 ( a
   (b) ) |}; false with e -> true) =~ true ;

   match Ext_sexp.from_string {| (setq bsc "../bin/bsc.exe"
   bs-external-includes '( "../runtime" "../stdlib" "./others") bs-package-name
   "bs-platform")

   (setq bsc-flags '("-w" "-40" "-bs-no-version-header " "-bs-diagnose"
   "-bs-cross-module-opt"))

   |} with | _ -> () | exception _ -> OUnit.assert_failure __LOC__ end; ] *)
