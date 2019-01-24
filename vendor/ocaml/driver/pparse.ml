(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*         Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt       *)
(*                                                                        *)
(*   Copyright 2002 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Format

type error =
  | CannotRun of string
  | WrongMagic of string

exception Error of error

(* Optionally preprocess a source file *)

let call_external_preprocessor sourcefile pp =
      let tmpfile = Filename.temp_file "ocamlpp" "" in
      let comm = Printf.sprintf "%s %s > %s"
                                pp (Filename.quote sourcefile) tmpfile
      in
      if Ccomp.command comm <> 0 then begin
        Misc.remove_file tmpfile;
        raise (Error (CannotRun comm));
      end;
      tmpfile

let preprocess sourcefile =
  match !Clflags.preprocessor with
    None -> sourcefile
  | Some pp ->
      Profile.record "-pp"
        (call_external_preprocessor sourcefile) pp


let remove_preprocessed inputfile =
  match !Clflags.preprocessor with
    None -> ()
  | Some _ -> Misc.remove_file inputfile

type 'a ast_kind =
| Structure : Parsetree.structure ast_kind
| Signature : Parsetree.signature ast_kind

let magic_of_kind : type a . a ast_kind -> string = function
  | Structure -> Config.ast_impl_magic_number
  | Signature -> Config.ast_intf_magic_number

(* Note: some of the functions here should go to Ast_mapper instead,
   which would encapsulate the "binary AST" protocol. *)

let write_ast (type a) (kind : a ast_kind) fn (ast : a) =
  let oc = open_out_bin fn in
  output_string oc (magic_of_kind kind);
  output_value oc (!Location.input_name : string);
  output_value oc (ast : a);
  close_out oc

let apply_rewriter kind fn_in ppx =
  let magic = magic_of_kind kind in
  let fn_out = Filename.temp_file "camlppx" "" in
  let comm =
    Printf.sprintf "%s %s %s" ppx (Filename.quote fn_in) (Filename.quote fn_out)
  in
  let ok = Ccomp.command comm = 0 in
  Misc.remove_file fn_in;
  if not ok then begin
    Misc.remove_file fn_out;
    raise (Error (CannotRun comm));
  end;
  if not (Sys.file_exists fn_out) then
    raise (Error (WrongMagic comm));
  (* check magic before passing to the next ppx *)
  let ic = open_in_bin fn_out in
  let buffer =
    try really_input_string ic (String.length magic) with End_of_file -> "" in
  close_in ic;
  if buffer <> magic then begin
    Misc.remove_file fn_out;
    raise (Error (WrongMagic comm));
  end;
  fn_out

let read_ast (type a) (kind : a ast_kind) fn : a =
  let ic = open_in_bin fn in
  try
    let magic = magic_of_kind kind in
    let buffer = really_input_string ic (String.length magic) in
    assert(buffer = magic); (* already checked by apply_rewriter *)
    Location.input_name := (input_value ic : string);
    let ast = (input_value ic : a) in
    close_in ic;
    Misc.remove_file fn;
    ast
  with exn ->
    close_in ic;
    Misc.remove_file fn;
    raise exn

let rewrite kind ppxs ast =
  let fn = Filename.temp_file "camlppx" "" in
  write_ast kind fn ast;
  let fn = List.fold_left (apply_rewriter kind) fn (List.rev ppxs) in
  read_ast kind fn

let apply_rewriters_str ?(restore = true) ~tool_name ast =
  match !Clflags.all_ppx with
  | [] -> ast
  | ppxs ->
      ast
      |> Ast_mapper.add_ppx_context_str ~tool_name
      |> rewrite Structure ppxs
      |> Ast_mapper.drop_ppx_context_str ~restore

let apply_rewriters_sig ?(restore = true) ~tool_name ast =
  match !Clflags.all_ppx with
  | [] -> ast
  | ppxs ->
      ast
      |> Ast_mapper.add_ppx_context_sig ~tool_name
      |> rewrite Signature ppxs
      |> Ast_mapper.drop_ppx_context_sig ~restore

let apply_rewriters ?restore ~tool_name
    (type a) (kind : a ast_kind) (ast : a) : a =
  match kind with
  | Structure ->
      apply_rewriters_str ?restore ~tool_name ast
  | Signature ->
      apply_rewriters_sig ?restore ~tool_name ast

(* Parse a file or get a dumped syntax tree from it *)

exception Outdated_version

let open_and_check_magic inputfile ast_magic =
  let ic = open_in_bin inputfile in
  let is_ast_file =
    try
      let buffer = really_input_string ic (String.length ast_magic) in
      if buffer = ast_magic then true
      else if String.sub buffer 0 9 = String.sub ast_magic 0 9 then
        raise Outdated_version
      else false
    with
      Outdated_version ->
        Misc.fatal_error "OCaml and preprocessor have incompatible versions"
    | _ -> false
  in
  (ic, is_ast_file)

let parse (type a) (kind : a ast_kind) lexbuf : a =
  match kind with
  | Structure -> Parse.implementation lexbuf
  | Signature -> Parse.interface lexbuf

let file_aux ppf ~tool_name inputfile (type a) parse_fun invariant_fun
             (kind : a ast_kind) =
  let ast_magic = magic_of_kind kind in
  let (ic, is_ast_file) = open_and_check_magic inputfile ast_magic in
  let ast =
    try
      if is_ast_file then begin
        if !Clflags.fast then
          (* FIXME make this a proper warning *)
          fprintf ppf "@[Warning: %s@]@."
            "option -unsafe used with a preprocessor returning a syntax tree";
        Location.input_name := (input_value ic : string);
        (input_value ic : a)
      end else begin
        seek_in ic 0;
        let lexbuf = Lexing.from_channel ic in
        Location.init lexbuf inputfile;
        Profile.record_call "parser" (fun () -> parse_fun lexbuf)
      end
    with x -> close_in ic; raise x
  in
  close_in ic;
  let ast =
    Profile.record_call "-ppx" (fun () ->
      apply_rewriters ~restore:false ~tool_name kind ast) in
  if is_ast_file || !Clflags.all_ppx <> [] then invariant_fun ast;
  ast

let file ppf ~tool_name inputfile parse_fun ast_kind =
  file_aux ppf ~tool_name inputfile parse_fun ignore ast_kind

let report_error ppf = function
  | CannotRun cmd ->
      fprintf ppf "Error while running external preprocessor@.\
                   Command line: %s@." cmd
  | WrongMagic cmd ->
      fprintf ppf "External preprocessor does not produce a valid file@.\
                   Command line: %s@." cmd

let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (Location.error_of_printer_file report_error err)
      | _ -> None
    )

let parse_file ~tool_name invariant_fun apply_hooks kind ppf sourcefile =
  Location.input_name := sourcefile;
  let inputfile = preprocess sourcefile in
  let ast =
    try file_aux ppf ~tool_name inputfile (parse kind) invariant_fun kind
    with exn ->
      remove_preprocessed inputfile;
      raise exn
  in
  remove_preprocessed inputfile;
  let ast = apply_hooks { Misc.sourcefile } ast in
  ast

module ImplementationHooks = Misc.MakeHooks(struct
    type t = Parsetree.structure
  end)
module InterfaceHooks = Misc.MakeHooks(struct
    type t = Parsetree.signature
  end)

let parse_implementation ppf ~tool_name sourcefile =
  Profile.record_call "parsing" (fun () ->
    parse_file ~tool_name Ast_invariants.structure
      ImplementationHooks.apply_hooks Structure ppf sourcefile)
let parse_interface ppf ~tool_name sourcefile =
  Profile.record_call "parsing" (fun () ->
    parse_file ~tool_name Ast_invariants.signature
      InterfaceHooks.apply_hooks Signature ppf sourcefile)
