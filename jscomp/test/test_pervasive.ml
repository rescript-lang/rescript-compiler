

module Pervasives = struct include List  include Pervasives end

let f = Pervasives.(@)

(* local variables: *)
(* compile-command: "ocamlc -dlambda -c test_pervasive.ml" *)
(* end: *)
