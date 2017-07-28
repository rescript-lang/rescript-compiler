#use "internal_test_header.ml";;
#use "findlibonly_test_header.ml";;

let () = test "camlp4.opt"
  ~description:"Fixes PR#5652"
  ~options:[`package "camlp4.macro";`tags ["camlp4o.opt"; "syntax\\(camp4o\\)"];
            `ppflag "camlp4o.opt"; `ppflag "-parser"; `ppflag "macro";
            `ppflag "-DTEST"]
  ~tree:[T.f "dummy.ml"
            ~content:"IFDEF TEST THEN\nprint_endline \"Hello\";;\nENDIF;;"]
  ~matching:[M.x "dummy.native" ~output:"Hello"]
  ~targets:("dummy.native",[]) ();;

let () = test "ThreadAndArchive"
  ~description:"Fixes PR#6058"
  ~options:[`use_ocamlfind; `package "threads"; `tag "thread"]
  ~tree:[T.f "t.ml" ~content:""]
  ~matching:[M.f "_build/t.cma"]
  ~targets:("t.cma",[]) ();;

let () = test "SyntaxFlag"
  ~options:[`use_ocamlfind; `package "camlp4.macro"; `syntax "camlp4o"]
  ~description:"-syntax for ocamlbuild"
  ~tree:[T.f "dummy.ml" ~content:"IFDEF TEST THEN\nprint_endline \"Hello\";;\nENDIF;;"]
  ~matching:[M.f "dummy.native"]
  ~targets:("dummy.native",[]) ();;

let () = test "PredicateFlag"
  ~description:"ocamlfind ocamldep does not support the -predicate option"
  ~options:[`use_ocamlfind; `tag "\"predicate(byte)\""]
  ~tree:[T.f "test.ml" ~content:"let x = List.map"]
  ~matching:[_build [M.f "test.ml.depends"]]
  ~targets:("test.ml.depends", []) ();;

let () = test "ToolsFlagsConflict"
  ~description:"PR#6300: conflicts between -ocamlc and -use-ocamlfind options"
  ~options:[`use_ocamlfind; `ocamlc "\"ocamlc -annot\""]
  ~tree:[T.f "test.ml" ~content:"let x = 1"]
  ~matching:[_build [M.f "test.annot"; M.f "test.byte"]]
  ~targets:("test.byte", []) ();;

run ~root:"_test_findlibonly";;
