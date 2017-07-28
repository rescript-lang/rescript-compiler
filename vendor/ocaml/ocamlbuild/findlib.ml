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


(* Original author: Romain Bardou *)

open My_std
open My_unix
open Command

type command_spec = Command.spec

type error =
  | Cannot_run_ocamlfind
  | Dependency_not_found of string * string (* package, dependency *)
  | Package_not_found of string
  | Cannot_parse_query of string * string (* package, explaination *)

exception Findlib_error of error

let error x = raise (Findlib_error x)

let string_of_error = function
  | Cannot_run_ocamlfind ->
      "Cannot run Ocamlfind."
  | Dependency_not_found(p, d) ->
      Printf.sprintf
        "Ocamlfind returned \"%s\" as a dependency for package \"%s\" but does \
not know this dependency." d p
  | Package_not_found p ->
      Printf.sprintf "Findlib package not found: \"%s\"." p
  | Cannot_parse_query(p, e) ->
      Printf.sprintf "Cannot parse Ocamlfind query for package \"%s\": %s" p e

let report_error e =
  prerr_endline (string_of_error e);
  exit 2

let ocamlfind = "ocamlfind"

type package = {
  name: string;
  description: string;
  version: string;
  archives_byte: string;
  archives_native: string;
  link_options: string;
  location: string;
  dependencies: package list;
}

let packages = Hashtbl.create 42

let run_and_parse lexer command =
  Printf.ksprintf
    (fun command -> lexer & Lexing.from_string & run_and_read command)
    command

let run_and_read command =
  Printf.ksprintf run_and_read command

let rec query name =
  try
    Hashtbl.find packages name
  with Not_found ->
    try
      let n, d, v, a_byte, lo, l =
        run_and_parse
          (Lexers.ocamlfind_query Const.Source.ocamlfind_query)
          "%s query -l -predicates byte %s" ocamlfind name
      in
      let a_native =
        run_and_parse
          (Lexers.trim_blanks Const.Source.ocamlfind_query)
          "%s query -a-format -predicates native %s" ocamlfind name
      in
      let deps =
        run_and_parse
          (Lexers.blank_sep_strings Const.Source.ocamlfind_query)
          "%s query -r -p-format %s" ocamlfind name
      in
      let deps = List.filter ((<>) n) deps in
      let deps =
        try
          List.map query deps
        with Findlib_error (Package_not_found dep_name) ->
          (* Ocamlfind cannot find a package which it returned as a dependency.
             This should not happen. *)
          error (Dependency_not_found (name, dep_name))
      in
      let package = {
        name = n;
        description = d;
        version = v;
        archives_byte = a_byte;
        archives_native = a_native;
        link_options = lo;
        location = l;
        dependencies = deps;
      } in
      Hashtbl.add packages n package;
      package
    with
      | Failure _ ->
          (* TODO: Improve to differenciate whether ocamlfind cannot be
             run or is not installed *)
          error Cannot_run_ocamlfind
      | Lexers.Error (s,_) ->
          error (Cannot_parse_query (name, s))

let split_nl s =
  let x = ref [] in
  let rec go s =
    let pos = String.index s '\n' in
    x := (String.before s pos)::!x;
    go (String.after s (pos + 1))
  in
  try
    go s
  with Not_found -> !x

let before_space s =
  try
    String.before s (String.index s ' ')
  with Not_found -> s

let list () =
  List.map before_space (split_nl & run_and_read "%s list" ocamlfind)

(* The closure algorithm is easy because the dependencies are already closed
and sorted for each package. We only have to make the union. We could also
make another ocamlfind query such as:
  ocamlfind query -p-format -r package1 package2 ... *)
let topological_closure l =
  let add l x = if List.mem x l then l else x :: l in
  let l = List.fold_left begin fun acc p ->
    add (List.fold_left add acc p.dependencies) p
  end [] l in
  List.rev l

module SSet = Set.Make(String)

let add_atom a l = match a, l with
  | A "", _ -> l
  | _ -> a :: l

let compile_flags l =
  let pkgs = topological_closure l in
  let locations = List.fold_left begin fun acc p ->
    SSet.add p.location acc
  end SSet.empty pkgs in
  let flags = [] in
  (* includes *)
  let flags =
    List.fold_left begin fun acc l ->
      add_atom (P l) (add_atom (A "-I") acc)
    end flags (SSet.elements locations)
  in
  S (List.rev flags)
let compile_flags_byte = compile_flags
let compile_flags_native = compile_flags

let link_flags f l =
  let pkgs = topological_closure l in
  let locations = List.fold_left begin fun acc p ->
    SSet.add p.location acc
  end SSet.empty pkgs in
  let flags = [] in
  (* includes *)
  let flags =
    List.fold_left begin fun acc l ->
      add_atom (P l) (add_atom (A "-I") acc)
    end flags (SSet.elements locations)
  in
  (* special link options *)
  let flags =
    List.fold_left begin fun acc x ->
      add_atom (A x.link_options) acc
    end flags pkgs
  in
  (* archives *)
  let flags =
    List.fold_left begin fun acc x ->
      add_atom (A (f x)) acc
    end flags pkgs
  in
  S (List.rev flags)
let link_flags_byte = link_flags (fun x -> x.archives_byte)
let link_flags_native = link_flags (fun x -> x.archives_native)
