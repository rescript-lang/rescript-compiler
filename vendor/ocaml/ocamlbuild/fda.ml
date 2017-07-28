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


(* Original author: Berke Durak *)
(* FDA *)

open Log
open Hygiene
;;

exception Exit_hygiene_failed
;;

let laws =
  [
    { law_name = "Leftover OCaml compilation files";
      law_rules = [Not ".cmo"; Not ".cmi"; Not ".cmx"; Not ".cma"; Not ".cmxa"];
      law_penalty = Fail };
    { law_name = "Leftover OCaml type annotation files";
      law_rules = [Not ".annot"];
      law_penalty = Warn };
    { law_name = "Leftover object files";
      law_rules = [Not ".o"; Not ".a"; Not ".so"; Not ".obj"; Not ".lib"; Not ".dll"];
      law_penalty = Fail };
    { law_name = "Leftover ocamlyacc-generated files";
      law_rules = [Implies_not(".mly",".ml"); Implies_not(".mly",".mli")];
      law_penalty = Fail };
    { law_name = "Leftover ocamllex-generated files";
      law_rules = [Implies_not(".mll",".ml")];
      law_penalty = Fail };
    { law_name = "Leftover dependency files";
      law_rules = [Not ".ml.depends"; Not ".mli.depends"];
      law_penalty = Fail }
  ]

let inspect entry =
  dprintf 5 "Doing sanity checks";
  let evil = ref false in
  match Hygiene.check
    ?sanitize:
      begin
        if !Options.sanitize then
          Some(Pathname.concat !Options.build_dir !Options.sanitization_script)
        else
          None
      end
      laws entry
  with
  | [] -> ()
  | stuff ->
    List.iter
      begin fun (law, msgs) ->
        Printf.printf "%s: %s:\n"
          (match law.law_penalty with
           | Warn -> "Warning"
           | Fail ->
               if not !evil then
                 begin
                   Printf.printf "IMPORTANT: I cannot work with leftover compiled files.\n%!";
                   evil := true
                 end;
              "ERROR")
          law.law_name;
        List.iter
          begin fun msg ->
            Printf.printf "  %s\n" msg
          end
          msgs
      end
      stuff;
    if !evil then raise Exit_hygiene_failed;
;;
