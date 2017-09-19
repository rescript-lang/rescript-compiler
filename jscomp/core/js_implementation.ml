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



let print_if ppf flag printer arg =
  if !flag then fprintf ppf "%a@." printer arg;
  arg



let after_parsing_sig ppf sourcefile outputprefix ast  =
  if !Js_config.binary_ast then
    begin 
      Binary_ast.write_ast
        Mli
        ~fname:sourcefile
        ~output:(outputprefix ^ Literals.suffix_mliast)
        (* to support relocate to another directory *)
        ast 

    end;
  if !Js_config.syntax_only then () else 
    begin 
      if not @@ !Js_config.no_warn_unused_bs_attribute then 
        Bs_ast_invariant.emit_external_warnings.signature Bs_ast_invariant.emit_external_warnings ast ;
      if Js_config.get_diagnose () then
        Format.fprintf Format.err_formatter "Building %s@." sourcefile;    
      let modulename = module_of_filename ppf sourcefile outputprefix in
      Lam_compile_env.reset () ;
      let initial_env = Compmisc.initial_env () in
      Env.set_unit_name modulename;
      let tsg = Typemod.type_interface initial_env ast in
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
        let sg = Env.save_signature sg modulename (outputprefix ^ ".cmi") in
        Typemod.save_signature modulename tsg outputprefix sourcefile
          initial_env sg ;
      end
    end
let interface ppf sourcefile outputprefix =
  Compmisc.init_path false;
  Ocaml_parse.parse_interface ppf sourcefile
  |> print_if ppf Clflags.dump_parsetree Printast.interface
  |> print_if ppf Clflags.dump_source Pprintast.signature 
  |> after_parsing_sig ppf sourcefile outputprefix 

let interface_mliast ppf sourcefile outputprefix  = 
  Compmisc.init_path false;
  Binary_ast.read_ast Mli sourcefile 
  |> print_if ppf Clflags.dump_parsetree Printast.interface
  |> print_if ppf Clflags.dump_source Pprintast.signature 
  |> after_parsing_sig ppf sourcefile outputprefix 

let after_parsing_impl ppf sourcefile outputprefix ast =
  if !Js_config.binary_ast then
    Binary_ast.write_ast ~fname:sourcefile 
      Ml ~output:(outputprefix ^ Literals.suffix_mlast)
      ast ;
  if !Js_config.syntax_only then () else 
    begin

      if not @@ !Js_config.no_warn_unused_bs_attribute then 
        Bs_ast_invariant.emit_external_warnings.structure Bs_ast_invariant.emit_external_warnings ast ;
      if Js_config.get_diagnose () then
        Format.fprintf Format.err_formatter "Building %s@." sourcefile;      
      let modulename = Compenv.module_of_filename ppf sourcefile outputprefix in
      Lam_compile_env.reset () ;
      let env = Compmisc.initial_env() in
      Env.set_unit_name modulename;
      try
        let (typedtree, coercion, finalenv, current_signature) =
          ast 
          |> Typemod.type_implementation_more sourcefile outputprefix modulename env 
          |> print_if ppf Clflags.dump_typedtree
            (fun fmt (ty,co,_,_) -> Printtyped.implementation_with_coercion fmt  (ty,co))
        in
        if !Clflags.print_types then begin
          Warnings.check_fatal ();
        end else begin
          (typedtree, coercion)
          |> Translmod.transl_implementation modulename
          |> print_if ppf Clflags.dump_rawlambda Printlambda.lambda
          |> (fun lambda -> 
              match           
                Lam_compile_main.lambda_as_module
                  finalenv current_signature 
                  sourcefile  outputprefix lambda  with
              | e -> e 
              | exception e -> 
                (* Save to a file instead so that it will not scare user *)
                if Js_config.get_diagnose () then
                  begin              
                    let file = "bsc.dump" in
                    Ext_pervasives.with_file_as_chan file
                      (fun ch -> output_string ch @@             
                        Printexc.raw_backtrace_to_string (Printexc.get_raw_backtrace ()));
                    Ext_log.err __LOC__
                      "Compilation fatal error, stacktrace saved into %s when compiling %s"
                      file sourcefile;
                  end;            
                raise e             
            );
          Warnings.check_fatal ()
        end;
        Stypes.dump (Some (outputprefix ^ ".annot"));
      with x ->
        Stypes.dump (Some (outputprefix ^ ".annot"));
        raise x
    end
let implementation ppf sourcefile outputprefix =
  Compmisc.init_path false;
  Ocaml_parse.parse_implementation ppf sourcefile
  |> print_if ppf Clflags.dump_parsetree Printast.implementation
  |> print_if ppf Clflags.dump_source Pprintast.structure
  |> after_parsing_impl ppf sourcefile outputprefix 

let implementation_mlast ppf sourcefile outputprefix = 
  Compmisc.init_path false;
  Binary_ast.read_ast Ml sourcefile
  |> print_if ppf Clflags.dump_parsetree Printast.implementation
  |> print_if ppf Clflags.dump_source Pprintast.structure
  |> after_parsing_impl ppf sourcefile outputprefix 







let make_structure_item ~ns cunit : Parsetree.structure_item =
  let open Ast_helper in 
  let loc = Location.none in 
  Str.module_ 
    (Mb.mk {txt = cunit; loc  }
       (Mod.ident 
          {txt = Lident 
               ( Ext_namespace.make ~ns cunit)
          ; loc}))



let implementation_map ppf sourcefile outputprefix = 
  let list_of_modules = Ext_io.rev_lines_of_file sourcefile 
  in 
  let ns = 
    Ext_string.capitalize_ascii
      (Filename.chop_extension (Filename.basename sourcefile)) in
  let ml_ast = List.fold_left (fun acc module_name -> 
      if Ext_string.is_empty module_name then acc 
      else make_structure_item ~ns module_name :: acc 
    ) [] list_of_modules in 
  Compmisc.init_path false;
  ml_ast
  |> print_if ppf Clflags.dump_parsetree Printast.implementation
  |> print_if ppf Clflags.dump_source Pprintast.structure
  |> after_parsing_impl ppf sourcefile outputprefix 

