#use "internal_test_header.ml";;
#use "findlibonly_test_header.ml";;
#use "external_test_header.ml";;

let () = test "SubtoolOptions"
  ~description:"Options that come from tags that needs to be spliced \
                to the subtool invocation (PR#5763)"
  (* testing for the 'menhir' executable directly
     is too hard to do in a portable way; test the ocamlfind package instead *)
  ~requirements:(package_exists "menhirLib")
  ~options:[`use_ocamlfind; `use_menhir; `tags["package\\(camlp4.fulllib\\)"]]
  ~tree:[T.f "parser.mly"
            ~content:"%{ %}
                      %token DUMMY
                      %start<Camlp4.PreCast.Syntax.Ast.expr option> test
                      %%
                      test: {None}"]
  ~matching:[M.f "parser.native"; M.f "parser.byte"]
  ~targets:("parser.native",["parser.byte"])
  ();;

run ~root:"_test_external";;
