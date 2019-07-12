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
  if !Js_config.syntax_only then 
    Warnings.check_fatal()
  else 
    begin 

      if Js_config.get_diagnose () then
        Format.fprintf Format.err_formatter "Building %s@." sourcefile;    
      let modulename = module_of_filename ppf sourcefile outputprefix in
      Lam_compile_env.reset () ;
      let initial_env = Compmisc.initial_env () in
      Env.set_unit_name modulename;

      let tsg = Typemod.type_interface 
#if OCAML_VERSION =~ ">4.03.0" then
          sourcefile
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
        Typemod.save_signature modulename tsg outputprefix sourcefile
          initial_env sg ;
      end
    end
let interface ppf sourcefile outputprefix =
  Js_config.set_current_file sourcefile ; 
  Compmisc.init_path false;
  Ocaml_parse.parse_interface ppf sourcefile
  |> print_if ppf Clflags.dump_parsetree Printast.interface
  |> print_if ppf Clflags.dump_source Pprintast.signature 
  |> after_parsing_sig ppf sourcefile outputprefix 

let interface_mliast ppf sourcefile outputprefix  = 
  Js_config.set_current_file sourcefile ; 
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
  if !Js_config.syntax_only then 
    Warnings.check_fatal ()
  else 
    begin

      if Js_config.get_diagnose () then
        Format.fprintf Format.err_formatter "Building %s@." sourcefile;      
      let modulename = Compenv.module_of_filename ppf sourcefile outputprefix in
      Lam_compile_env.reset () ;
      let env = Compmisc.initial_env() in
      Env.set_unit_name modulename;
      try
        let (typedtree, coercion, finalenv, current_signature) =
          ast 
          |> Typemod.type_implementation_more ?check_exists:(if !Js_config.force_cmi then None else Some ()) sourcefile outputprefix modulename env 
          |> print_if ppf Clflags.dump_typedtree
            (fun fmt (ty,co,_,_) -> Printtyped.implementation_with_coercion fmt  (ty,co))
        in
        if !Clflags.print_types || !Js_config.cmi_only then begin
          Warnings.check_fatal ();
        end else begin
          (typedtree, coercion)
          |> Translmod.transl_implementation modulename

          |> (fun 
#if OCAML_VERSION =~ ">4.03.0" then
              {code = lambda}
#else
              lambda
#end              
               -> 
              ignore (print_if ppf Clflags.dump_rawlambda Printlambda.lambda lambda);
              try
                Lam_compile_main.lambda_as_module
                  finalenv  
                  sourcefile  outputprefix lambda  with
              | e -> 
                (* Save to a file instead so that it will not scare user *)
                (if Js_config.get_diagnose () then
                   begin              
                     let file = "bsc.dump" in
                     Ext_pervasives.with_file_as_chan file
                       (fun ch -> output_string ch @@             
                         Printexc.raw_backtrace_to_string (Printexc.get_raw_backtrace ()));
                     Ext_log.err __LOC__
                       "Compilation fatal error, stacktrace saved into %s when compiling %s"
                       file sourcefile;
                   end;            
                 raise e)             
            );

        end;
        Stypes.dump (Some (outputprefix ^ ".annot"));
      with x ->
        Stypes.dump (Some (outputprefix ^ ".annot"));
        raise x
    end
let implementation ppf sourcefile outputprefix =
  Compmisc.init_path false;
  Js_config.set_current_file sourcefile ; 
  Ocaml_parse.parse_implementation ppf sourcefile
  |> print_if ppf Clflags.dump_parsetree Printast.implementation
  |> print_if ppf Clflags.dump_source Pprintast.structure
  |> after_parsing_impl ppf sourcefile outputprefix 

let implementation_mlast ppf sourcefile outputprefix = 
  Compmisc.init_path false;
  Js_config.set_current_file sourcefile ; 
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


(** decoding [.mlmap]
  keep in sync {!Bsb_namespace_map_gen.output}
*)
let implementation_map ppf sourcefile outputprefix = 
  let ichan = open_in_bin sourcefile in 
  seek_in ichan (Ext_digest.length +1);
  let list_of_modules = Ext_io.rev_lines_of_chann ichan in 
  close_in ichan;
  let ns = 
    Ext_string.capitalize_ascii
      (Ext_filename.chop_extension_maybe (Filename.basename sourcefile)) in
  let ml_ast = Ext_list.fold_left list_of_modules [] (fun acc line -> 
      if Ext_string.is_empty line then acc 
      else make_structure_item ~ns line :: acc 
    )  in 
  Compmisc.init_path false;
  ml_ast
  |> print_if ppf Clflags.dump_parsetree Printast.implementation
  |> print_if ppf Clflags.dump_source Pprintast.structure
  |> after_parsing_impl ppf sourcefile outputprefix 

