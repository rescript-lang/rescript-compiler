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


(* Original author: Nicolas Pouillard *)
open My_std
open Format
open Log
open Pathname.Operators
open Tags.Operators
open Tools
open Flags
open Command;;


module S = Set.Make(String)

let flag_and_dep tags cmd_spec =
  flag tags cmd_spec;
  let ps = Command.fold_pathnames (fun p ps -> p :: ps) (Cmd cmd_spec) [] in
  dep tags ps

let stdlib_dir = lazy begin
  let ocamlc_where = !Options.build_dir / (Pathname.mk "ocamlc.where") in
  let () = Command.execute ~quiet:true (Cmd(S[!Options.ocamlc; A"-where"; Sh">"; P ocamlc_where])) in
  String.chomp (read_file ocamlc_where)
end

let pflag_and_dep tags ptag cmd_spec =
  Param_tags.declare ptag
    (fun param ->
       flag_and_dep (Param_tags.make ptag param :: tags) (cmd_spec param))

let module_name_of_filename f = String.capitalize (Pathname.remove_extensions f)
let module_name_of_pathname x =
  module_name_of_filename (Pathname.to_string (Pathname.basename x))

let ignore_stdlib x =
  if !Options.nostdlib then false
  else
    let x' = !*stdlib_dir/((String.uncapitalize x)-.-"cmi") in
    Pathname.exists x'

let non_dependencies = ref []
let non_dependency m1 m2 =
  (* non_dependency was not supposed to accept pathnames without extension. *)
  if String.length (Pathname.get_extensions m1) = 0 then
    invalid_arg "non_dependency: no extension";
  non_dependencies := (m1, m2) :: !non_dependencies

let path_importance path x =
  if List.mem (path, x) !non_dependencies
  || (List.mem x !Options.ignore_list) then begin
    let () = dprintf 3 "This module (%s) is ignored by %s" x path in
    `ignored
  end
  else if ignore_stdlib x then `just_try else `mandatory

let expand_module =
  memo3 (fun include_dirs module_name exts ->
    let dirname = Pathname.dirname module_name in
    let basename = Pathname.basename module_name in
    let module_name_cap = dirname/(String.capitalize basename) in
    let module_name_uncap = dirname/(String.uncapitalize basename) in
    List.fold_right begin fun include_dir ->
      List.fold_right begin fun ext acc ->
        include_dir/(module_name_uncap-.-ext) ::
          include_dir/(module_name_cap-.-ext) :: acc
      end exts
    end include_dirs [])

let string_list_of_file file =
  with_input_file file begin fun ic ->
    Lexers.blank_sep_strings
      Const.Source.file (Lexing.from_channel ic)
  end
let print_path_list = Pathname.print_path_list

let ocaml_ppflags tags =
  let flags = Flags.of_tags (tags++"ocaml"++"pp") in
  let reduced = Command.reduce flags in
  if reduced = N then N else S[A"-pp"; Quote reduced]

let ocaml_add_include_flag x acc =
  if x = Pathname.current_dir_name then acc else A"-I" :: A x :: acc

let ocaml_include_flags path =
  S (List.fold_right ocaml_add_include_flag (Pathname.include_dirs_of (Pathname.dirname path)) [])

let info_libraries = Hashtbl.create 103

let libraries = Hashtbl.create 103
let libraries_of m =
  try Hashtbl.find libraries m with Not_found -> []
let use_lib m lib = Hashtbl.replace libraries m (lib :: libraries_of m)

let ocaml_lib ?(extern=false) ?(byte=true) ?(native=true) ?dir ?tag_name libpath =
  let add_dir x =
    match dir with
    | Some dir -> S[A"-I"; P dir; x]
    | None -> x
  in
  let tag_name =
    match tag_name with
    | Some x -> x
    | None -> "use_" ^ Pathname.basename libpath
  in
  let flag_and_dep tags lib =
    flag tags (add_dir (A lib));
    if not extern then dep tags [lib] (* cannot happen? *)
  in
  Hashtbl.replace info_libraries tag_name (libpath, extern);
  (* adding [tag_name] to [info_libraries] will make this tag
     affect include-dir lookups, so it is used even if not
     mentioned explicitly in any rule. *)
  Flags.mark_tag_used tag_name;
  if extern then begin
    if byte then
      flag_and_dep ["ocaml"; tag_name; "link"; "byte"] (libpath^".cma");
    if native then
      flag_and_dep ["ocaml"; tag_name; "link"; "native"] (libpath^".cmxa");
  end else begin
    if not byte && not native then
      invalid_arg "ocaml_lib: ~byte:false or ~native:false only works with ~extern:true";
  end;
  match dir with
  | None -> ()
  | Some dir ->
      List.iter
        (fun x -> flag ["ocaml"; tag_name; x] (S[A"-I"; P dir]))
        ["compile"; "doc"; "infer_interface"]

let cmi_of = Pathname.update_extensions "cmi"

exception Ocamldep_error of string

let read_path_dependencies =
  let path_dependencies = Hashtbl.create 103 in
  let read path =
    let module_name = module_name_of_pathname path in
    let depends = path-.-"depends" in
    with_input_file depends begin fun ic ->
      let ocamldep_output =
        try Lexers.ocamldep_output
              Const.Source.ocamldep (Lexing.from_channel ic)
        with Lexers.Error (msg,_) -> raise (Ocamldep_error(Printf.sprintf "Ocamldep.ocamldep: bad output (%s)" msg)) in
      let deps =
        List.fold_right begin fun (path, deps) acc ->
          let module_name' = module_name_of_pathname path in
          if module_name' = module_name
          then List.union deps acc
          else raise (Ocamldep_error(Printf.sprintf "Ocamldep.ocamldep: multiple files in ocamldep output (%s not expected)" path))
        end ocamldep_output [] in
      let deps =
        if !Options.nostdlib && not (Tags.mem "nopervasives" (tags_of_pathname path)) then
          "Pervasives" :: deps
        else deps in
      let deps' = List.fold_right begin fun dep acc ->
        match path_importance path dep with
        | `ignored -> acc
        | (`just_try | `mandatory) as importance -> (importance, dep) :: acc
      end deps [] in
      Hashtbl.replace path_dependencies path
        (List.union (try Hashtbl.find path_dependencies path with Not_found -> []) deps');
      deps'
    end
  in read

let path_dependencies_of = memo read_path_dependencies
