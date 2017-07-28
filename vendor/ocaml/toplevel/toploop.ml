(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* The interactive toplevel loop *)

open Path
open Format
open Config
open Misc
open Parsetree
open Types
open Typedtree
open Outcometree
open Ast_helper

type directive_fun =
   | Directive_none of (unit -> unit)
   | Directive_string of (string -> unit)
   | Directive_int of (int -> unit)
   | Directive_ident of (Longident.t -> unit)
   | Directive_bool of (bool -> unit)

(* The table of toplevel value bindings and its accessors *)

module StringMap = Map.Make(String)

let toplevel_value_bindings : Obj.t StringMap.t ref = ref StringMap.empty

let getvalue name =
  try
    StringMap.find name !toplevel_value_bindings
  with Not_found ->
    fatal_error (name ^ " unbound at toplevel")

let setvalue name v =
  toplevel_value_bindings := StringMap.add name v !toplevel_value_bindings

(* Return the value referred to by a path *)

let rec eval_path = function
  | Pident id ->
      if Ident.persistent id || Ident.global id then
        Symtable.get_global_value id
      else begin
        let name = Translmod.toplevel_name id in
        try
          StringMap.find name !toplevel_value_bindings
        with Not_found ->
          raise (Symtable.Error(Symtable.Undefined_global name))
      end
  | Pdot(p, s, pos) ->
      Obj.field (eval_path p) pos
  | Papply(p1, p2) ->
      fatal_error "Toploop.eval_path"

let eval_path env path =
  eval_path (Env.normalize_path (Some Location.none) env path)

(* To print values *)

module EvalPath = struct
  type valu = Obj.t
  exception Error
  let eval_path env p = try eval_path env p with Symtable.Error _ -> raise Error
  let same_value v1 v2 = (v1 == v2)
end

module Printer = Genprintval.Make(Obj)(EvalPath)

let max_printer_depth = ref 100
let max_printer_steps = ref 300

let print_out_value = Oprint.out_value
let print_out_type = Oprint.out_type
let print_out_class_type = Oprint.out_class_type
let print_out_module_type = Oprint.out_module_type
let print_out_type_extension = Oprint.out_type_extension
let print_out_sig_item = Oprint.out_sig_item
let print_out_signature = Oprint.out_signature
let print_out_phrase = Oprint.out_phrase

let print_untyped_exception ppf obj =
  !print_out_value ppf (Printer.outval_of_untyped_exception obj)
let outval_of_value env obj ty =
  Printer.outval_of_value !max_printer_steps !max_printer_depth
    (fun _ _ _ -> None) env obj ty
let print_value env obj ppf ty =
  !print_out_value ppf (outval_of_value env obj ty)

type ('a, 'b) gen_printer = ('a, 'b) Genprintval.gen_printer =
  | Zero of 'b
  | Succ of ('a -> ('a, 'b) gen_printer)

let install_printer = Printer.install_printer
let install_generic_printer = Printer.install_generic_printer
let install_generic_printer' = Printer.install_generic_printer'
let remove_printer = Printer.remove_printer

(* Hooks for parsing functions *)

let parse_toplevel_phrase = ref Parse.toplevel_phrase
let parse_use_file = ref Parse.use_file
let print_location = Location.print_error (* FIXME change back to print *)
let print_error = Location.print_error
let print_warning = Location.print_warning
let input_name = Location.input_name

let parse_mod_use_file name lb =
  let modname =
    String.capitalize (Filename.chop_extension (Filename.basename name))
  in
  let items =
    List.concat
      (List.map
         (function Ptop_def s -> s | Ptop_dir _ -> [])
         (!parse_use_file lb))
  in
  [ Ptop_def
      [ Str.module_
          (Mb.mk
             (Location.mknoloc modname)
             (Mod.structure items)
          )
       ]
   ]

(* Hooks for initialization *)

let toplevel_startup_hook = ref (fun () -> ())

(* Load in-core and execute a lambda term *)

let may_trace = ref false (* Global lock on tracing *)
type evaluation_outcome = Result of Obj.t | Exception of exn

let load_lambda ppf lam =
  if !Clflags.dump_rawlambda then fprintf ppf "%a@." Printlambda.lambda lam;
  let slam = Simplif.simplify_lambda lam in
  if !Clflags.dump_lambda then fprintf ppf "%a@." Printlambda.lambda slam;
  let (init_code, fun_code) = Bytegen.compile_phrase slam in
  if !Clflags.dump_instr then
    fprintf ppf "%a%a@."
    Printinstr.instrlist init_code
    Printinstr.instrlist fun_code;
  let (code, code_size, reloc) = Emitcode.to_memory init_code fun_code in
  let can_free = (fun_code = []) in
  let initial_symtable = Symtable.current_state() in
  Symtable.patch_object code reloc;
  Symtable.check_global_initialized reloc;
  Symtable.update_global_table();
  let initial_bindings = !toplevel_value_bindings in
  try
    may_trace := true;
    let retval = (Meta.reify_bytecode code code_size) () in
    may_trace := false;
    if can_free then begin
      Meta.static_release_bytecode code code_size;
      Meta.static_free code;
    end;
    Result retval
  with x ->
    may_trace := false;
    if can_free then begin
      Meta.static_release_bytecode code code_size;
      Meta.static_free code;
    end;
    toplevel_value_bindings := initial_bindings; (* PR#6211 *)
    Symtable.restore_state initial_symtable;
    Exception x

(* Print the outcome of an evaluation *)

let rec pr_item env items =
  Printtyp.hide_rec_items items;
  match items with
  | Sig_value(id, decl) :: rem ->
      let tree = Printtyp.tree_of_value_description id decl in
      let valopt =
        match decl.val_kind with
        | Val_prim _ -> None
        | _ ->
            let v =
              outval_of_value env (getvalue (Translmod.toplevel_name id))
                decl.val_type
            in
            Some v
      in
      Some (tree, valopt, rem)
  | Sig_type(id, _, _) :: rem when Btype.is_row_name (Ident.name id) ->
      pr_item env rem
  | Sig_type(id, decl, rs) :: rem ->
      let tree = Printtyp.tree_of_type_declaration id decl rs in
      Some (tree, None, rem)
  | Sig_typext(id, ext, es) :: rem ->
      let tree = Printtyp.tree_of_extension_constructor id ext es in
      Some (tree, None, rem)
  | Sig_module(id, md, rs) :: rem ->
      let tree = Printtyp.tree_of_module id md.md_type rs in
      Some (tree, None, rem)
  | Sig_modtype(id, decl) :: rem ->
      let tree = Printtyp.tree_of_modtype_declaration id decl in
      Some (tree, None, rem)
  | Sig_class(id, decl, rs) :: cltydecl :: tydecl1 :: tydecl2 :: rem ->
      let tree = Printtyp.tree_of_class_declaration id decl rs in
      Some (tree, None, rem)
  | Sig_class_type(id, decl, rs) :: tydecl1 :: tydecl2 :: rem ->
      let tree = Printtyp.tree_of_cltype_declaration id decl rs in
      Some (tree, None, rem)
  | _ -> None

let rec item_list env = function
  | [] -> []
  | items ->
     match pr_item env items with
     | None -> []
     | Some (tree, valopt, items) -> (tree, valopt) :: item_list env items

(* The current typing environment for the toplevel *)

let toplevel_env = ref Env.empty

(* Print an exception produced by an evaluation *)

let print_out_exception ppf exn outv =
  !print_out_phrase ppf (Ophr_exception (exn, outv))

let print_exception_outcome ppf exn =
  if exn = Out_of_memory then Gc.full_major ();
  let outv = outval_of_value !toplevel_env (Obj.repr exn) Predef.type_exn in
  print_out_exception ppf exn outv

(* The table of toplevel directives.
   Filled by functions from module topdirs. *)

let directive_table = (Hashtbl.create 13 : (string, directive_fun) Hashtbl.t)

(* Execute a toplevel phrase *)

let execute_phrase print_outcome ppf phr =
  match phr with
  | Ptop_def sstr ->
      let oldenv = !toplevel_env in
      Typecore.reset_delayed_checks ();
      let (str, sg, newenv) = Typemod.type_toplevel_phrase oldenv sstr in
      if !Clflags.dump_typedtree then Printtyped.implementation ppf str;
      let sg' = Typemod.simplify_signature sg in
      ignore (Includemod.signatures oldenv sg sg');
      Typecore.force_delayed_checks ();
      let lam = Translmod.transl_toplevel_definition str in
      Warnings.check_fatal ();
      begin try
        toplevel_env := newenv;
        let res = load_lambda ppf lam in
        let out_phr =
          match res with
          | Result v ->
              if print_outcome then
                Printtyp.wrap_printing_env oldenv (fun () ->
                  match str.str_items with
                  | [ { str_desc = Tstr_eval (exp, _attrs) }] ->
                      let outv = outval_of_value newenv v exp.exp_type in
                      let ty = Printtyp.tree_of_type_scheme exp.exp_type in
                      Ophr_eval (outv, ty)
                  | [] -> Ophr_signature []
                  | _ -> Ophr_signature (item_list newenv sg'))
              else Ophr_signature []
          | Exception exn ->
              toplevel_env := oldenv;
              if exn = Out_of_memory then Gc.full_major();
              let outv =
                outval_of_value !toplevel_env (Obj.repr exn) Predef.type_exn
              in
              Ophr_exception (exn, outv)
        in
        !print_out_phrase ppf out_phr;
        begin match out_phr with
        | Ophr_eval (_, _) | Ophr_signature _ -> true
        | Ophr_exception _ -> false
        end
      with x ->
        toplevel_env := oldenv; raise x
      end
  | Ptop_dir(dir_name, dir_arg) ->
      let d =
        try Some (Hashtbl.find directive_table dir_name)
        with Not_found -> None
      in
      begin match d with
      | None ->
          fprintf ppf "Unknown directive `%s'.@." dir_name;
          false
      | Some d ->
          match d, dir_arg with
          | Directive_none f, Pdir_none -> f (); true
          | Directive_string f, Pdir_string s -> f s; true
          | Directive_int f, Pdir_int n -> f n; true
          | Directive_ident f, Pdir_ident lid -> f lid; true
          | Directive_bool f, Pdir_bool b -> f b; true
          | _ ->
              fprintf ppf "Wrong type of argument for directive `%s'.@."
                dir_name;
              false
      end


(* Temporary assignment to a reference *)

let protect r newval body =
  let oldval = !r in
  try
    r := newval;
    let res = body() in
    r := oldval;
    res
  with x ->
    r := oldval;
    raise x

(* Read and execute commands from a file, or from stdin if [name] is "". *)

let use_print_results = ref true

let preprocess_phrase ppf phr =
  let phr =
    match phr with
    | Ptop_def str ->
        let str =
          Pparse.apply_rewriters_str ~restore:true ~tool_name:"ocaml" str
        in
        Ptop_def str
    | phr -> phr
  in
  if !Clflags.dump_parsetree then Printast.top_phrase ppf phr;
  if !Clflags.dump_source then Pprintast.top_phrase ppf phr;
  phr

let use_file ppf wrap_mod name =
  try
    let (filename, ic, must_close) =
      if name = "" then
        ("(stdin)", stdin, false)
      else begin
        let filename = find_in_path !Config.load_path name in
        let ic = open_in_bin filename in
        (filename, ic, true)
      end
    in
    let lb = Lexing.from_channel ic in
    Location.init lb filename;
    (* Skip initial #! line if any *)
    Lexer.skip_sharp_bang lb;
    let success =
      protect Location.input_name filename (fun () ->
        try
          List.iter
            (fun ph ->
              let ph = preprocess_phrase ppf ph in
              if not (execute_phrase !use_print_results ppf ph) then raise Exit)
            (if wrap_mod then
               parse_mod_use_file name lb
             else
               !parse_use_file lb);
          true
        with
        | Exit -> false
        | Sys.Break -> fprintf ppf "Interrupted.@."; false
        | x -> Location.report_exception ppf x; false) in
    if must_close then close_in ic;
    success
  with Not_found -> fprintf ppf "Cannot find file %s.@." name; false

let mod_use_file ppf name = use_file ppf true name
let use_file ppf name = use_file ppf false name

let use_silently ppf name =
  protect use_print_results false (fun () -> use_file ppf name)

(* Reading function for interactive use *)

let first_line = ref true
let got_eof = ref false;;

let read_input_default prompt buffer len =
  output_string Pervasives.stdout prompt; flush Pervasives.stdout;
  let i = ref 0 in
  try
    while true do
      if !i >= len then raise Exit;
      let c = input_char Pervasives.stdin in
      Bytes.set buffer !i c;
      incr i;
      if c = '\n' then raise Exit;
    done;
    (!i, false)
  with
  | End_of_file ->
      (!i, true)
  | Exit ->
      (!i, false)

let read_interactive_input = ref read_input_default

let refill_lexbuf buffer len =
  if !got_eof then (got_eof := false; 0) else begin
    let prompt =
      if !Clflags.noprompt then ""
      else if !first_line then "# "
      else if !Clflags.nopromptcont then ""
      else if Lexer.in_comment () then "* "
      else "  "
    in
    first_line := false;
    let (len, eof) = !read_interactive_input prompt buffer len in
    if eof then begin
      Location.echo_eof ();
      if len > 0 then got_eof := true;
      len
    end else
      len
  end

(* Toplevel initialization. Performed here instead of at the
   beginning of loop() so that user code linked in with ocamlmktop
   can call directives from Topdirs. *)

let _ =
  Sys.interactive := true;
  let crc_intfs = Symtable.init_toplevel() in
  Compmisc.init_path false;
  List.iter
    (fun (name, crco) ->
      Env.add_import name;
      match crco with
        None -> ()
      | Some crc->
          Consistbl.set Env.crc_units name crc Sys.executable_name)
    crc_intfs

let load_ocamlinit ppf =
  if !Clflags.noinit then ()
  else match !Clflags.init_file with
  | Some f -> if Sys.file_exists f then ignore (use_silently ppf f)
              else fprintf ppf "Init file not found: \"%s\".@." f
  | None ->
     if Sys.file_exists ".ocamlinit" then ignore (use_silently ppf ".ocamlinit")
     else try
       let home_init = Filename.concat (Sys.getenv "HOME") ".ocamlinit" in
       if Sys.file_exists home_init then ignore (use_silently ppf home_init)
     with Not_found -> ()
;;

let set_paths () =
  (* Add whatever -I options have been specified on the command line,
     but keep the directories that user code linked in with ocamlmktop
     may have added to load_path. *)
  load_path := !load_path @ [Filename.concat Config.standard_library "camlp4"];
  load_path := "" :: (List.rev !Clflags.include_dirs @ !load_path);
  Dll.add_path !load_path

let initialize_toplevel_env () =
  toplevel_env := Compmisc.initial_env()

(* The interactive loop *)

exception PPerror

let loop ppf =
  Location.formatter_for_warnings := ppf;
  fprintf ppf "        OCaml version %s@.@." Config.version;
  initialize_toplevel_env ();
  let lb = Lexing.from_function refill_lexbuf in
  Location.init lb "//toplevel//";
  Location.input_name := "//toplevel//";
  Location.input_lexbuf := Some lb;
  Sys.catch_break true;
  load_ocamlinit ppf;
  while true do
    let snap = Btype.snapshot () in
    try
      Lexing.flush_input lb;
      Location.reset();
      first_line := true;
      let phr = try !parse_toplevel_phrase lb with Exit -> raise PPerror in
      let phr = preprocess_phrase ppf phr  in
      Env.reset_cache_toplevel ();
      ignore(execute_phrase true ppf phr)
    with
    | End_of_file -> exit 0
    | Sys.Break -> fprintf ppf "Interrupted.@."; Btype.backtrack snap
    | PPerror -> ()
    | x -> Location.report_exception ppf x; Btype.backtrack snap
  done

(* Execute a script.  If [name] is "", read the script from stdin. *)

let run_script ppf name args =
  let len = Array.length args in
  if Array.length Sys.argv < len then invalid_arg "Toploop.run_script";
  Array.blit args 0 Sys.argv 0 len;
  Obj.truncate (Obj.repr Sys.argv) len;
  Arg.current := 0;
  Compmisc.init_path false;
  toplevel_env := Compmisc.initial_env();
  Sys.interactive := false;
  use_silently ppf name
