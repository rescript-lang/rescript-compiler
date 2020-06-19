

(* Note: some of the functions here should go to Ast_mapper instead,
   which would encapsulate the "binary AST" protocol. *)

let write_ast (type a) (kind : a Ml_binary.kind) fn (ast : a) =
  let oc = open_out_bin fn in
  output_string oc (Ml_binary.magic_of_kind kind);
  output_value oc (!Location.input_name : string);
  output_value oc (ast : a);
  close_out oc

let apply_rewriter kind fn_in ppx =
  let magic = Ml_binary.magic_of_kind kind in
  let fn_out = Filename.temp_file "camlppx" "" in
  let comm =
    Printf.sprintf "%s %s %s" ppx (Filename.quote fn_in) (Filename.quote fn_out)
  in
  let ok = Ccomp.command comm = 0 in
  Misc.remove_file fn_in;
  if not ok then begin
    Misc.remove_file fn_out;
    Cmd_ast_exception.cannot_run comm
  end;
  if not (Sys.file_exists fn_out) then
    Cmd_ast_exception.cannot_run comm;
  (* check magic before passing to the next ppx *)
  let ic = open_in_bin fn_out in
  let buffer =
    try really_input_string ic (String.length magic) with End_of_file -> "" in
  close_in ic;
  if buffer <> magic then begin
    Misc.remove_file fn_out;
    Cmd_ast_exception.wrong_magic buffer;
  end;
  fn_out

let read_ast (type a) (kind : a Ml_binary.kind) fn : a =
  let ic = open_in_bin fn in
  try
    let magic = Ml_binary.magic_of_kind kind in
    let buffer = really_input_string ic (String.length magic) in
    assert(buffer = magic); (* already checked by apply_rewriter *)
    Location.set_input_name @@ (input_value ic : string);
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
    |> rewrite Ml ppxs
    |> Ast_mapper.drop_ppx_context_str ~restore

let apply_rewriters_sig ?(restore = true) ~tool_name ast =
  match !Clflags.all_ppx with
  | [] -> ast
  | ppxs ->
    ast
    |> Ast_mapper.add_ppx_context_sig ~tool_name
    |> rewrite Mli ppxs
    |> Ast_mapper.drop_ppx_context_sig ~restore

let apply_rewriters ?restore ~tool_name
    (type a) (kind : a Ml_binary.kind) (ast : a) : a =
  match kind with
  | Ml_binary.Ml ->
    apply_rewriters_str ?restore ~tool_name ast
  | Ml_binary.Mli ->
    apply_rewriters_sig ?restore ~tool_name ast
