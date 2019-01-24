(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Sebastien Hinderer, projet Gallium, INRIA Paris            *)
(*                                                                        *)
(*   Copyright 2016 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Description of ocamltest's command-line options *)

let show_objects title string_of_object objects =
  let print_object o = print_endline ("  " ^ (string_of_object o)) in
  print_endline title;
  List.iter print_object objects;
  exit 0

let string_of_action action = action.Actions.action_name

let string_of_test test =
  if test.Tests.test_run_by_default
  then (test.Tests.test_name ^ " (run by default)")
  else test.Tests.test_name

let show_actions () =
  let actions = Actions.get_registered_actions () in
  show_objects "Available actions are:" string_of_action actions

let show_tests () =
  let tests = Tests.get_registered_tests () in
  show_objects "Available tests are:" string_of_test tests

let commandline_options =
[
  ("-show-actions", Arg.Unit show_actions, "Show available actions.");
  ("-show-tests", Arg.Unit show_tests, "Show available tests.");
]

let testfile = ref ""

let set_testfile name =
  if !testfile<> "" then
  begin
    Printf.eprintf "Can't deal with more than one test file at the moment\n%!";
    exit 1
  end else testfile := name

let usage = "Usage: " ^ Sys.argv.(0) ^ " options testfile"

let _ =
  Arg.parse commandline_options set_testfile usage
