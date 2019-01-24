(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2002 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* The batch compiler *)

open Misc
open Config
open Format
open Typedtree
open Compenv

(* Compile a .mli file *)

(* Keep in sync with the copy in compile.ml *)

let tool_name = "ocamlopt"

let interface ppf sourcefile outputprefix =
  Profile.record_call sourcefile (fun () ->
    Compmisc.init_path false;
    let modulename = module_of_filename ppf sourcefile outputprefix in
    Env.set_unit_name modulename;
    let initial_env = Compmisc.initial_env () in
    let ast = Pparse.parse_interface ~tool_name ppf sourcefile in
    if !Clflags.dump_parsetree then fprintf ppf "%a@." Printast.interface ast;
    if !Clflags.dump_source then fprintf ppf "%a@." Pprintast.signature ast;
    Profile.(record_call typing) (fun () ->
      let tsg = Typemod.type_interface sourcefile initial_env ast in
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
        let deprecated = Builtin_attributes.deprecated_of_sig ast in
        let sg =
          Env.save_signature ~deprecated sg modulename (outputprefix ^ ".cmi")
        in
        Typemod.save_signature modulename tsg outputprefix sourcefile
          initial_env sg ;
      end
    )
  )

(* Compile a .ml file *)

let print_if ppf flag printer arg =
  if !flag then fprintf ppf "%a@." printer arg;
  arg

let (++) x f = f x
let (+++) (x, y) f = (x, f y)

let implementation ~backend ppf sourcefile outputprefix =
  Profile.record_call sourcefile (fun () ->
    Compmisc.init_path true;
    let modulename = module_of_filename ppf sourcefile outputprefix in
    Env.set_unit_name modulename;
    let env = Compmisc.initial_env() in
    Compilenv.reset ?packname:!Clflags.for_package modulename;
    let cmxfile = outputprefix ^ ".cmx" in
    let objfile = outputprefix ^ ext_obj in
    let comp ast =
      let (typedtree, coercion) =
        ast
        ++ print_if ppf Clflags.dump_parsetree Printast.implementation
        ++ print_if ppf Clflags.dump_source Pprintast.structure
        ++ Profile.(record typing)
            (Typemod.type_implementation sourcefile outputprefix modulename env)
        ++ print_if ppf Clflags.dump_typedtree
            Printtyped.implementation_with_coercion
      in
      if not !Clflags.print_types then begin
        if Config.flambda then begin
          if !Clflags.classic_inlining then begin
            Clflags.default_simplify_rounds := 1;
            Clflags.use_inlining_arguments_set Clflags.classic_arguments;
            Clflags.unbox_free_vars_of_closures := false;
            Clflags.unbox_specialised_args := false
          end;
          (typedtree, coercion)
          ++ Profile.(record transl)
              (Translmod.transl_implementation_flambda modulename)
          ++ Profile.(record generate)
            (fun { Lambda.module_ident; main_module_block_size;
                   required_globals; code } ->
            ((module_ident, main_module_block_size), code)
            +++ print_if ppf Clflags.dump_rawlambda Printlambda.lambda
            +++ Simplif.simplify_lambda sourcefile
            +++ print_if ppf Clflags.dump_lambda Printlambda.lambda
            ++ (fun ((module_ident, size), lam) ->
                Middle_end.middle_end ppf
                  ~prefixname:outputprefix
                  ~size
                  ~filename:sourcefile
                  ~module_ident
                  ~backend
                  ~module_initializer:lam)
            ++ Asmgen.compile_implementation_flambda
              outputprefix ~required_globals ~backend ppf;
            Compilenv.save_unit_info cmxfile)
        end
        else begin
          Clflags.use_inlining_arguments_set Clflags.classic_arguments;
          (typedtree, coercion)
          ++ Profile.(record transl)
              (Translmod.transl_store_implementation modulename)
          ++ print_if ppf Clflags.dump_rawlambda Printlambda.program
          ++ Profile.(record generate)
              (fun program ->
                { program with
                  Lambda.code = Simplif.simplify_lambda sourcefile
                    program.Lambda.code }
                ++ print_if ppf Clflags.dump_lambda Printlambda.program
                ++ Asmgen.compile_implementation_clambda
                  outputprefix ppf;
                Compilenv.save_unit_info cmxfile)
        end
      end;
      Warnings.check_fatal ();
      Stypes.dump (Some (outputprefix ^ ".annot"))
    in
    try comp (Pparse.parse_implementation ~tool_name ppf sourcefile)
    with x ->
      Stypes.dump (Some (outputprefix ^ ".annot"));
      remove_file objfile;
      remove_file cmxfile;
      raise x
  )
