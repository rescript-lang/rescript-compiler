(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* adapted by bucklescript from [driver/compile.ml] for convenience    *)

open Format
open Typedtree
open Compenv



let fprintf = Format.fprintf



let print_if_pipe ppf flag printer arg =
  if !flag then fprintf ppf "%a@." printer arg;
  arg

let print_if ppf flag printer arg =
  if !flag then fprintf ppf "%a@." printer arg
  


let process_with_gentype filename =    
  match !Clflags.bs_gentype with
  | None -> ()
  | Some cmd -> 
    let comm = (cmd ^ 
                " -bs-version " ^ Bs_version.version ^
                " -cmt-add " ^ 
                filename ^ 
                ( ":" ^ !Location.input_name)) in 
    if !Clflags.verbose then begin 
      prerr_string "+ ";
      prerr_endline comm;
      prerr_newline ()
    end ;                        
    ignore 
      (Sys.command comm
      )

let after_parsing_sig ppf  outputprefix ast  =
  if !Js_config.binary_ast then
    begin 
      Binary_ast.write_ast
        Mli
        ~sourcefile:!Location.input_name
        ~output:(outputprefix ^ if !Js_config.is_reason  then Literals.suffix_reiast else Literals.suffix_mliast)
        (* to support relocate to another directory *)
        ast 

    end;
  if !Js_config.syntax_only then 
    Warnings.check_fatal()
  else 
    begin 
      let modulename = module_of_filename ppf !Location.input_name outputprefix in
      Lam_compile_env.reset () ;
      let initial_env = Compmisc.initial_env () in
      Env.set_unit_name modulename;

      let tsg = Typemod.type_interface 
#if OCAML_VERSION =~ ">4.03.0" then
          !Location.input_name
#end
          initial_env ast in
      if !Clflags.dump_typedtree then fprintf ppf "%a@." Printtyped.interface tsg;
      let sg = tsg.sig_type in
      if !Clflags.print_types then
        Printtyp.wrap_printing_env initial_env (fun () ->
            fprintf std_formatter "%a@."
              Printtyp.signature (Typemod.simplify_signature sg));
      ignore (Includemod.signatures initial_env sg sg);
      Typecore.force_delayed_checks ();
      Warnings.check_fatal ();
      if not !Clflags.print_types then begin
#if OCAML_VERSION =~ ">4.03.0" then
        let deprecated = Builtin_attributes.deprecated_of_sig ast in
        let sg =
          Env.save_signature ~deprecated sg modulename (outputprefix ^ ".cmi")
        in
#else
        let sg = Env.save_signature ?check_exists:(if !Js_config.force_cmi then None else Some ()) sg modulename (outputprefix ^ ".cmi") in
#end        
        Typemod.save_signature modulename tsg outputprefix !Location.input_name
          initial_env sg ;
        process_with_gentype (outputprefix ^ ".cmti");  
      end
    end



let interface ppf fname outputprefix =
  Compmisc.init_path false;
  Pparse.parse_interface ~tool_name:Js_config.tool_name ppf fname
  |> Ppx_entry.rewrite_signature
  |> print_if_pipe ppf Clflags.dump_parsetree Printast.interface
  |> print_if_pipe ppf Clflags.dump_source Pprintast.signature 
  |> after_parsing_sig ppf  outputprefix 

let interface_mliast ppf fname outputprefix  = 
  Compmisc.init_path false;
  Binary_ast.read_ast Mli fname 
  |> print_if_pipe ppf Clflags.dump_parsetree Printast.interface
  |> print_if_pipe ppf Clflags.dump_source Pprintast.signature 
  |> after_parsing_sig ppf  outputprefix 


let get_lambda = fun   
#if OCAML_VERSION =~ ">4.03.0" then
              {Lambda.code = lambda}
#else
              lambda
#end              -> lambda 


let all_module_alias (ast : Parsetree.structure)= 
  Ext_list.for_all ast (fun {pstr_desc} -> 
    match pstr_desc with 
    | Pstr_module {pmb_expr = {pmod_desc = Pmod_ident _ }} 
      -> true 
    | Pstr_attribute _ -> true   
    | Pstr_eval _ 
    | Pstr_value _ 
    | Pstr_primitive _ 
    | Pstr_type _ 
    | Pstr_typext  _ 
    | Pstr_exception _ 
    | Pstr_module _ 
    | Pstr_recmodule _  
    | Pstr_modtype _
    | Pstr_open _
    | Pstr_class _ 
    | Pstr_class_type _ 
    | Pstr_include _
    | Pstr_extension _ -> false 
   )

let after_parsing_impl ppf  outputprefix ast =
  Js_config.all_module_aliases := 
    !Clflags.assume_no_mli =  Mli_non_exists &&
    all_module_alias ast 
    ;
  if !Js_config.binary_ast then
    Binary_ast.write_ast ~sourcefile:!Location.input_name 
      Ml ~output:(outputprefix ^ 
        if !Js_config.is_reason then  Literals.suffix_reast else Literals.suffix_mlast
        )
      ast ;
  if !Js_config.syntax_only then 
    Warnings.check_fatal ()
  else 
    begin
      let modulename = Ext_filename.module_name outputprefix in
      Lam_compile_env.reset () ;
      let env = Compmisc.initial_env() in
      Env.set_unit_name modulename;
      let (typedtree, coercion, _, _) =
        Typemod.type_implementation_more 
          ?check_exists:(if !Js_config.force_cmi then None else Some ()) 
          !Location.input_name outputprefix modulename env ast  in 
      let typedtree_coercion = (typedtree, coercion) in        
      print_if ppf Clflags.dump_typedtree
        Printtyped.implementation_with_coercion  typedtree_coercion ;
      if !Clflags.print_types || !Js_config.cmi_only then begin
        Warnings.check_fatal ();
      end else begin
        let lambda = Translmod.transl_implementation modulename typedtree_coercion in 
        let js_program =  
          print_if_pipe ppf Clflags.dump_rawlambda Printlambda.lambda (get_lambda lambda)
          |> Lam_compile_main.compile outputprefix in 
        if not !Js_config.cmj_only then 
          Lam_compile_main.lambda_as_module
            js_program
            outputprefix 
        ;
      end;
      process_with_gentype (outputprefix ^ ".cmt")        
    end
let implementation ppf fname outputprefix =
  Compmisc.init_path false;
  Pparse.parse_implementation ~tool_name:Js_config.tool_name ppf fname
  |> Ppx_entry.rewrite_implementation
  |> print_if_pipe ppf Clflags.dump_parsetree Printast.implementation
  |> print_if_pipe ppf Clflags.dump_source Pprintast.structure
  |> after_parsing_impl ppf outputprefix 

let implementation_mlast ppf fname outputprefix = 
  Compmisc.init_path false;
  Binary_ast.read_ast Ml fname
  |> print_if_pipe ppf Clflags.dump_parsetree Printast.implementation
  |> print_if_pipe ppf Clflags.dump_source Pprintast.structure
  |> after_parsing_impl ppf  outputprefix 







let make_structure_item ~ns cunit : Parsetree.structure_item =
  let open Ast_helper in 
  let loc = Location.none in 
  Str.module_ 
    (Mb.mk {txt = cunit; loc  }
       (Mod.ident 
          {txt = Lident 
               ( Ext_namespace.make ~ns cunit)
          ; loc}))


(** decoding [.mlmap]
  keep in sync {!Bsb_namespace_map_gen.output}
*)
let implementation_map ppf sourcefile outputprefix = 
  let () = Js_config.cmj_only := true in 
  let ichan = open_in_bin sourcefile in 
  seek_in ichan (Ext_digest.length +1);
  let list_of_modules = Ext_io.rev_lines_of_chann ichan in 
  close_in ichan;
  let ns = Ext_filename.module_name sourcefile in
  let ml_ast = Ext_list.fold_left list_of_modules [] (fun acc line -> 
      if Ext_string.is_empty line then acc 
      else make_structure_item ~ns line :: acc 
    )  in 
  Compmisc.init_path false;
  ml_ast
  |> print_if_pipe ppf Clflags.dump_parsetree Printast.implementation
  |> print_if_pipe ppf Clflags.dump_source Pprintast.structure
  |> after_parsing_impl ppf outputprefix 

