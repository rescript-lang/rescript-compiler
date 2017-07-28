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

let i = Tutu.tutu + 10
let s = Tata.tata ^ ".ml"
let l = 3 :: Titi.titi
let () = Format.printf "toto.native: %s: Hello world!!!@." Sys.argv.(0)
let () = Format.printf "Tutu.tutu => %d@.Tata.tata => %S@." Tutu.tutu Tata.tata
