(***********************************************************************)
(*                                                                     *)
(*                             ocamlbuild                              *)
(*                                                                     *)
(*  Nicolas Pouillard, Berke Durak, projet Gallium, INRIA Rocquencourt *)
(*                                                                     *)
(*  Copyright 2007 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Ocamlbuild_plugin;;
dispatch begin function
  | After_rules ->
      rule "copy foo"
        ~prod:"bar"
        ~dep:"foo.otarget"
        begin fun _env _build ->
          cp "foo" "bar"
        end
  | _ -> ()
end
