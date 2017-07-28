(* Fullfilled and Missing are defined in ocamlbuild_test.ml
   Findlib was loaded in findlibonly_test_header.ml *)
let package_exists package =
  let open Findlib in
  try ignore (package_directory package); Fullfilled
  with No_such_package _ ->
    Missing (Printf.sprintf "the ocamlfind package %s" package)
