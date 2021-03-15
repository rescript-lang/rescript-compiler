(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

(* Clflags.keep_docs := false; *)
(* default to false -check later*)
(* Clflags.keep_locs := false; *)
let setup_env () =
  Env.Persistent_signature.load := Bs_cmi_load.load_cmi;    
  Translcore.wrap_single_field_record := Transl_single_field_record.wrap_single_field_record;
  Translmod.eval_rec_bindings := Compile_rec_module.eval_rec_bindings;
  Typemod.should_hide := Typemod_hide.should_hide;
  Matching.make_test_sequence_variant_constant := Polyvar_pattern_match.make_test_sequence_variant_constant;
  Matching.call_switcher_variant_constant := Polyvar_pattern_match.call_switcher_variant_constant;
  Matching.call_switcher_variant_constr := Polyvar_pattern_match.call_switcher_variant_constr;
  Clflags.no_std_include := true;
  Warnings.parse_options false Bsc_warnings.defaults_w;
  Warnings.parse_options true Bsc_warnings.defaults_warn_error;
  Clflags.dump_location := false;  
  Clflags.compile_only := true;
  Config.bs_only := true;  
  Clflags.no_implicit_current_dir := true; 
  Clflags.color := Some Always;
  (* default true
     otherwise [bsc -I sc src/hello.ml ] will include current directory to search path
  *)
  Clflags.assume_no_mli := Clflags.Mli_non_exists;
  Clflags.unsafe_string := false;
  Clflags.debug := true;
  Clflags.record_event_when_debug := false;
  Clflags.binary_annotations := true;
  Clflags.strict_sequence := true;
  Clflags.strict_formats := true;
  (* Turn on [-no-alias-deps] by default -- double check *)
  Oprint.out_ident := Outcome_printer_ns.out_ident;
  Builtin_attributes.check_bs_attributes_inclusion := Record_attributes_check.check_bs_attributes_inclusion;
  Builtin_attributes.check_duplicated_labels :=
    Record_attributes_check.check_duplicated_labels;
  Lambda.fld_record := Record_attributes_check.fld_record;
  Lambda.fld_record_set := Record_attributes_check.fld_record_set;
  Lambda.blk_record := Record_attributes_check.blk_record;
  Matching.names_from_construct_pattern := 
    Matching_polyfill.names_from_construct_pattern;
#if undefined BS_RELEASE_BUILD 
    Printexc.record_backtrace true;
    (let root_dir = 
        Filename.dirname 
          (Filename.dirname Sys.executable_name) in 
    let (//) = Filename.concat in       
    Clflags.include_dirs :=
      (root_dir//"jscomp"//"others") ::
      (root_dir//"jscomp"//"stdlib-406") ::
      (root_dir//"jscomp"//"runtime") ::
      !Clflags.include_dirs);
#end
  Lexer.replace_directive_bool "BS" true;
  Lexer.replace_directive_bool "JS" true;
  Lexer.replace_directive_string "BS_VERSION"  Bs_version.version
#if false
  ; Switch.cut := 100 (* tweakable but not very useful *)
#end  

let () = 
    at_exit (fun _ -> Format.pp_print_flush Format.err_formatter ())