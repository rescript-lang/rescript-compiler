(***********************************************************************)
(*                                                                     *)
(*                             OCamldoc                                *)
(*                                                                     *)
(*            Maxence Guesdon, projet Cristal, INRIA Rocquencourt      *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(** Analysis of source files. This module is strongly inspired from
    driver/main.ml :-) *)

let print_DEBUG s = print_string s ; print_newline ()

open Config
open Clflags
open Misc
open Format
open Typedtree


(** Initialize the search path.
   The current directory is always searched first,
   then the directories specified with the -I option (in command-line order),
   then the standard library directory. *)
let init_path () =
#if undefined BS_NO_COMPILER_PATCH then 
  load_path :=
    (if !Clflags.no_implicit_current_dir then 
      List.rev (Config.standard_library :: !Clflags.include_dirs)
    else 
      "" ::   List.rev (Config.standard_library :: !Clflags.include_dirs)) ; 
#else
  load_path :=
    "" :: List.rev (Config.standard_library :: !Clflags.include_dirs);
#end
  Env.reset_cache ()

(** Return the initial environment in which compilation proceeds. *)
let initial_env () =
  let initial =
    if !Clflags.unsafe_string then Env.initial_unsafe_string
    else Env.initial_safe_string
  in
  try
    if !Clflags.nopervasives then initial else
    Env.open_pers_signature "Pervasives" initial
  with Not_found ->
    fatal_error "cannot open pervasives.cmi"

(** Optionally preprocess a source file *)
let preprocess sourcefile =
  try
    Pparse.preprocess sourcefile
  with Pparse.Error err ->
    Format.eprintf "Preprocessing error@.%a@."
      Pparse.report_error err;
    exit 2

let (++) x f = f x

(** Analysis of an implementation file. Returns (Some typedtree) if
   no error occured, else None and an error message is printed.*)

let tool_name = "ocamldoc"

let process_implementation_file ppf sourcefile =
  init_path ();
  let prefixname = Filename.chop_extension sourcefile in
  let modulename = String.capitalize(Filename.basename prefixname) in
  Env.set_unit_name modulename;
  let inputfile = preprocess sourcefile in
  let env = initial_env () in
  try
    let parsetree =
      Pparse.file ~tool_name Format.err_formatter inputfile
        Parse.implementation ast_impl_magic_number
    in
    let typedtree =
      Typemod.type_implementation
        sourcefile prefixname modulename env parsetree
    in
    (Some (parsetree, typedtree), inputfile)
  with
    e ->
      match e with
        Syntaxerr.Error err ->
          fprintf Format.err_formatter "@[%a@]@."
            Syntaxerr.report_error err;
          None, inputfile
      | Failure s ->
          prerr_endline s;
          incr Odoc_global.errors ;
          None, inputfile
      | e ->
          raise e

(** Analysis of an interface file. Returns (Some signature) if
   no error occured, else None and an error message is printed.*)
let process_interface_file ppf sourcefile =
  init_path ();
  let prefixname = Filename.chop_extension sourcefile in
  let modulename = String.capitalize(Filename.basename prefixname) in
  Env.set_unit_name modulename;
  let inputfile = preprocess sourcefile in
  let ast =
    Pparse.file ~tool_name Format.err_formatter inputfile
      Parse.interface ast_intf_magic_number
  in
  let sg = Typemod.type_interface (initial_env()) ast in
  Warnings.check_fatal ();
  (ast, sg, inputfile)

(** The module used to analyse the parsetree and signature of an implementation file.*)
module Ast_analyser = Odoc_ast.Analyser (Odoc_comments.Basic_info_retriever)

(** The module used to analyse the parse tree and typed tree of an interface file.*)
module Sig_analyser = Odoc_sig.Analyser (Odoc_comments.Basic_info_retriever)

(** Handle an error. *)

let process_error exn =
  match Location.error_of_exn exn with
  | Some err ->
      fprintf Format.err_formatter "@[%a@]@." Location.report_error err
  | None ->
      fprintf Format.err_formatter
        "Compilation error(%s). Use the OCaml compiler to get more details.@."
        (Printexc.to_string exn)

(** Process the given file, according to its extension. Return the Module.t created, if any.*)
let process_file ppf sourcefile =
  if !Odoc_global.verbose then
    (
     let f = match sourcefile with
       Odoc_global.Impl_file f
     | Odoc_global.Intf_file f -> f
     | Odoc_global.Text_file f -> f
     in
     print_string (Odoc_messages.analysing f) ;
     print_newline ();
    );
  match sourcefile with
    Odoc_global.Impl_file file ->
      (
       Location.input_name := file;
       try
         let (parsetree_typedtree_opt, input_file) = process_implementation_file ppf file in
         match parsetree_typedtree_opt with
           None ->
             None
         | Some (parsetree, typedtree) ->
             let file_module = Ast_analyser.analyse_typed_tree file
                 !Location.input_name parsetree typedtree
             in
             file_module.Odoc_module.m_top_deps <- Odoc_dep.impl_dependencies parsetree ;

             if !Odoc_global.verbose then
               (
                print_string Odoc_messages.ok;
                print_newline ()
               );
             Pparse.remove_preprocessed input_file;
             Some file_module
       with
       | Sys_error s
       | Failure s ->
           prerr_endline s ;
           incr Odoc_global.errors ;
           None
       | e ->
           process_error e ;
           incr Odoc_global.errors ;
           None
      )
  | Odoc_global.Intf_file file ->
      (
       Location.input_name := file;
       try
         let (ast, signat, input_file) = process_interface_file ppf file in
         let file_module = Sig_analyser.analyse_signature file
             !Location.input_name ast signat.sig_type
         in

         file_module.Odoc_module.m_top_deps <- Odoc_dep.intf_dependencies ast ;

         if !Odoc_global.verbose then
           (
            print_string Odoc_messages.ok;
            print_newline ()
           );
         Pparse.remove_preprocessed input_file;
         Some file_module
       with
       | Sys_error s
       | Failure s ->
           prerr_endline s;
           incr Odoc_global.errors ;
           None
       | e ->
           process_error e ;
           incr Odoc_global.errors ;
           None
      )
  | Odoc_global.Text_file file ->
      Location.input_name := file;
      try
        let mod_name =
          let s =
            try Filename.chop_extension file
            with _ -> file
          in
          String.capitalize (Filename.basename s)
        in
        let txt =
          try Odoc_text.Texter.text_of_string (Odoc_misc.input_file_as_string file)
          with Odoc_text.Text_syntax (l, c, s) ->
            raise (Failure (Odoc_messages.text_parse_error l c s))
        in
        let m =
          {
            Odoc_module.m_name = mod_name ;
            Odoc_module.m_type = Types.Mty_signature [] ;
            Odoc_module.m_info = None ;
            Odoc_module.m_is_interface = true ;
            Odoc_module.m_file = file ;
            Odoc_module.m_kind = Odoc_module.Module_struct
              [Odoc_module.Element_module_comment txt] ;
            Odoc_module.m_loc =
              { Odoc_types.loc_impl = None ;
                Odoc_types.loc_inter = Some (Location.in_file file) } ;
            Odoc_module.m_top_deps = [] ;
            Odoc_module.m_code = None ;
            Odoc_module.m_code_intf = None ;
            Odoc_module.m_text_only = true ;
          }
        in
        Some m
      with
       | Sys_error s
       | Failure s ->
           prerr_endline s;
           incr Odoc_global.errors ;
           None
       | e ->
           process_error e ;
           incr Odoc_global.errors ;
           None

(** Remove the class elements between the stop special comments. *)
let rec remove_class_elements_between_stop keep eles =
  match eles with
    [] -> []
  | ele :: q ->
      match ele with
        Odoc_class.Class_comment [ Odoc_types.Raw "/*" ] ->
          remove_class_elements_between_stop (not keep) q
      | Odoc_class.Class_attribute _
      | Odoc_class.Class_method _
      | Odoc_class.Class_comment _ ->
          if keep then
            ele :: (remove_class_elements_between_stop keep q)
          else
            remove_class_elements_between_stop keep q

(** Remove the class elements between the stop special comments in a class kind. *)
let rec remove_class_elements_between_stop_in_class_kind k =
  match k with
    Odoc_class.Class_structure (inher, l) ->
      Odoc_class.Class_structure (inher, remove_class_elements_between_stop true l)
  | Odoc_class.Class_apply _ -> k
  | Odoc_class.Class_constr _ -> k
  | Odoc_class.Class_constraint (k1, ctk) ->
      Odoc_class.Class_constraint (remove_class_elements_between_stop_in_class_kind k1,
                        remove_class_elements_between_stop_in_class_type_kind ctk)

(** Remove the class elements beetween the stop special comments in a class type kind. *)
and remove_class_elements_between_stop_in_class_type_kind tk =
  match tk with
    Odoc_class.Class_signature (inher, l) ->
      Odoc_class.Class_signature (inher, remove_class_elements_between_stop true l)
  | Odoc_class.Class_type _ -> tk


(** Remove the module elements between the stop special comments. *)
let rec remove_module_elements_between_stop keep eles =
  let f = remove_module_elements_between_stop in
  match eles with
    [] -> []
  | ele :: q ->
      match ele with
        Odoc_module.Element_module_comment [ Odoc_types.Raw "/*" ] ->
          f (not keep) q
      | Odoc_module.Element_module_comment _ ->
          if keep then
            ele :: (f keep q)
          else
            f keep q
      | Odoc_module.Element_module m ->
          if keep then
            (
             m.Odoc_module.m_kind <- remove_module_elements_between_stop_in_module_kind m.Odoc_module.m_kind ;
             (Odoc_module.Element_module m) :: (f keep q)
            )
          else
            f keep q
      | Odoc_module.Element_module_type mt ->
          if keep then
            (
             mt.Odoc_module.mt_kind <- Odoc_misc.apply_opt
                 remove_module_elements_between_stop_in_module_type_kind mt.Odoc_module.mt_kind ;
             (Odoc_module.Element_module_type mt) :: (f keep q)
            )
          else
            f keep q
      | Odoc_module.Element_included_module _ ->
          if keep then
            ele :: (f keep q)
          else
            f keep q
      | Odoc_module.Element_class c ->
          if keep then
            (
             c.Odoc_class.cl_kind <- remove_class_elements_between_stop_in_class_kind c.Odoc_class.cl_kind ;
             (Odoc_module.Element_class c) :: (f keep q)
            )
          else
            f keep q
      | Odoc_module.Element_class_type ct ->
          if keep then
            (
             ct.Odoc_class.clt_kind <- remove_class_elements_between_stop_in_class_type_kind ct.Odoc_class.clt_kind ;
             (Odoc_module.Element_class_type ct) :: (f keep q)
            )
          else
            f keep q
      | Odoc_module.Element_value _
      | Odoc_module.Element_type_extension _
      | Odoc_module.Element_exception _
      | Odoc_module.Element_type _ ->
          if keep then
            ele :: (f keep q)
          else
            f keep q


(** Remove the module elements between the stop special comments, in the given module kind. *)
and remove_module_elements_between_stop_in_module_kind k =
  match k with
  | Odoc_module.Module_struct l -> Odoc_module.Module_struct (remove_module_elements_between_stop true l)
  | Odoc_module.Module_alias _ -> k
  | Odoc_module.Module_functor (params, k2)  ->
      Odoc_module.Module_functor (params, remove_module_elements_between_stop_in_module_kind k2)
  | Odoc_module.Module_apply (k1, k2) ->
      Odoc_module.Module_apply (remove_module_elements_between_stop_in_module_kind k1,
                    remove_module_elements_between_stop_in_module_kind k2)
  | Odoc_module.Module_with (mtkind, s) ->
      Odoc_module.Module_with (remove_module_elements_between_stop_in_module_type_kind mtkind, s)
  | Odoc_module.Module_constraint (k2, mtkind) ->
      Odoc_module.Module_constraint (remove_module_elements_between_stop_in_module_kind k2,
                         remove_module_elements_between_stop_in_module_type_kind mtkind)
  | Odoc_module.Module_typeof _ -> k
  | Odoc_module.Module_unpack _ -> k

(** Remove the module elements between the stop special comment, in the given module type kind. *)
and remove_module_elements_between_stop_in_module_type_kind tk =
  match tk with
  | Odoc_module.Module_type_struct l -> Odoc_module.Module_type_struct (remove_module_elements_between_stop true l)
  | Odoc_module.Module_type_functor (params, tk2) ->
      Odoc_module.Module_type_functor (params, remove_module_elements_between_stop_in_module_type_kind tk2)
  | Odoc_module.Module_type_alias _ -> tk
  | Odoc_module.Module_type_with (tk2, s) ->
      Odoc_module.Module_type_with (remove_module_elements_between_stop_in_module_type_kind tk2, s)
  | Odoc_module.Module_type_typeof _ -> tk

(** Remove elements between the stop special comment. *)
let remove_elements_between_stop module_list =
  List.map
    (fun m ->
      m.Odoc_module.m_kind <- remove_module_elements_between_stop_in_module_kind m.Odoc_module.m_kind;
      m
    )
    module_list

(** This function builds the modules from the given list of source files. *)
let analyse_files ?(init=[]) files =
  let modules_pre =
    init @
    (List.fold_left
       (fun acc -> fun file ->
         try
           match process_file Format.err_formatter file with
             None ->
               acc
           | Some m ->
               acc @ [ m ]
         with
           Failure s ->
             prerr_endline s ;
             incr Odoc_global.errors ;
             acc
       )
       []
       files
    )
  in
  (* Remove elements between the stop special comments, if needed. *)
  let modules =
    if !Odoc_global.no_stop then
      modules_pre
    else
      remove_elements_between_stop modules_pre
  in


  if !Odoc_global.verbose then
    (
     print_string Odoc_messages.merging;
     print_newline ()
    );
  let merged_modules = Odoc_merge.merge !Odoc_global.merge_options modules in
  if !Odoc_global.verbose then
    (
     print_string Odoc_messages.ok;
     print_newline ();
    );
  let modules_list =
    (List.fold_left
       (fun acc -> fun m -> acc @ (Odoc_module.module_all_submodules ~trans: false m))
       merged_modules
       merged_modules
    )
  in
  if !Odoc_global.verbose then
    (
     print_string Odoc_messages.cross_referencing;
     print_newline ()
    );
  let _ = Odoc_cross.associate modules_list in

  if !Odoc_global.verbose then
    (
     print_string Odoc_messages.ok;
     print_newline ();
    );

  if !Odoc_global.sort_modules then
    List.sort (fun m1 m2 -> compare m1.Odoc_module.m_name m2.Odoc_module.m_name) merged_modules
  else
    merged_modules

let dump_modules file (modules : Odoc_module.t_module list) =
  try
    let chanout = open_out_bin file in
    let dump = Odoc_types.make_dump modules in
    output_value chanout dump;
    close_out chanout
  with
    Sys_error s ->
      raise (Failure s)

let load_modules file =
  try
    let chanin = open_in_bin file in
    let dump = input_value chanin in
    close_in chanin ;
    let (l : Odoc_module.t_module list) = Odoc_types.open_dump dump in
    l
  with
    Sys_error s ->
      raise (Failure s)
