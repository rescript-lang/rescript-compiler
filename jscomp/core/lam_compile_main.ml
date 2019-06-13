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








module E = Js_exp_make 
module S = Js_stmt_make  

let get_cmj_case output_prefix : Ext_namespace.file_kind = 
  let little = 
    Ext_char.is_lower_case (Filename.basename output_prefix).[0] 
  in 
  match little, !Js_config.bs_suffix with 
  | true, true -> Little_bs
  | true, false -> Little_js
  | false, true -> Upper_bs 
  | false, false -> Upper_js
  

let compile_group ({filename = file_name; env;} as meta : Lam_stats.t) 
    (x : Lam_group.t) : Js_output.t  = 
  match x, file_name with 
  (* 
        We need

        2. [E.builtin_dot] for javascript builtin
        3. [E.mldot]
     *)
  (* ATTENTION: check {!Lam_compile_global} for consistency  *)      
  (** Special handling for values in [Pervasives] *)  
  (* 
         we delegate [stdout, stderr, and stdin] into [caml_io] module, 
         the motivation is to help dead code eliminatiion, it's helpful 
         to make those parts pure (not a function call), then it can be removed 
         if unused 
      *)                     

  (* QUICK hack to make hello world example nicer,
     Note the arity of [print_endline] is already analyzed before, 
     so it should be safe
  *)

  | Single (kind, id, lam), _ -> 
    (* let lam = Optimizer.simplify_lets [] lam in  *)
    (* can not apply again, it's wrong USE it with care*)
    (* ([Js_stmt_make.comment (Gen_of_env.query_type id  env )], None)  ++ *)
    Lam_compile.compile_lambda { continuation = Declare (kind, id);
                                    jmp_table = Lam_compile_context.empty_handler_map;
                                    meta
                                  } lam

  | Recursive id_lams, _   -> 
    Lam_compile.compile_recursive_lets 
      { continuation = EffectCall ReturnFalse; 
        jmp_table = Lam_compile_context.empty_handler_map;
        meta
      } 
      id_lams
  | Nop lam, _ -> (* TODO: Side effect callls, log and see statistics *)
    Lam_compile.compile_lambda {continuation = EffectCall ReturnFalse;
                                jmp_table = Lam_compile_context.empty_handler_map;
                                meta
                               } lam

;;

 (** Also need analyze its depenency is pure or not *)
let no_side_effects (rest : Lam_group.t list) : string option = 
  Ext_list.find_opt rest (fun x -> 
      match x with 
      | Single(kind,id,body) -> 
        begin 
          match kind with 
          | Strict | Variable -> 
            if not @@ Lam_analysis.no_side_effects body 
            then Some  (Printf.sprintf "%s" id.name)
            else None
          | _ -> None
        end
      | Recursive bindings -> 
        Ext_list.find_opt  bindings (fun (id,lam) -> 
            if not @@ Lam_analysis.no_side_effects lam 
            then Some (Printf.sprintf "%s" id.Ident.name )
            else None
          )
      | Nop lam -> 
        if not @@ Lam_analysis.no_side_effects lam 
        then 
          (*  (Lam_util.string_of_lambda lam) *)
          Some ""
        else None (* TODO :*))


let _d  = fun env s lam -> 
#if undefined BS_RELEASE_BUILD then 
    Lam_util.dump env s lam ;
    Ext_log.dwarn ~__POS__ "START CHECKING PASS %s@." s;
    ignore @@ Lam_check.check (Js_config.get_current_file ()) lam;
    Ext_log.dwarn ~__POS__ "FINISH CHECKING PASS %s@." s;
#end
    lam

(** Actually simplify_lets is kind of global optimization since it requires you to know whether 
    it's used or not 
*)
let compile  
    ~filename (output_prefix : string) 
    (env : Env.t) 
    (lam : Lambda.lambda)   = 
  let export_idents = Translmod.get_export_identifiers() in
  let export_ident_sets = Ident_set.of_list export_idents in 
  (* To make toplevel happy - reentrant for js-demo *)
  let () = 
#if undefined BS_RELEASE_BUILD then     
    Ext_list.iter export_idents 
      (fun id -> Ext_log.dwarn ~__POS__ "export idents: %s/%d"  id.name id.stamp) ;
#end      
    Lam_compile_env.reset () ;
  in 
  let lam, may_required_modules = Lam_convert.convert export_ident_sets lam in 

  let _j = Js_pass_debug.dump in
  let lam = _d env "initial"  lam in
  let lam  = Lam_pass_deep_flatten.deep_flatten lam in
  let lam = _d  env "flatten0" lam in
  let meta = 
    Lam_pass_collect.count_alias_globals env filename
      export_idents export_ident_sets lam in
  let lam = 
    let lam =  
      lam
      |> _d env "flattern1"
      |>  Lam_pass_exits.simplify_exits
      |> _d env "simplyf_exits"
      |> (fun lam -> Lam_pass_collect.collect_helper meta lam; lam)
      |>  Lam_pass_remove_alias.simplify_alias  meta
      |> _d env "simplify_alias"
      |> Lam_pass_deep_flatten.deep_flatten
      |> _d env "flatten2"
    in  (* Inling happens*)

    let ()  = Lam_pass_collect.collect_helper meta lam in
    let lam = Lam_pass_remove_alias.simplify_alias meta lam  in
    let lam = Lam_pass_deep_flatten.deep_flatten lam in
    let ()  = Lam_pass_collect.collect_helper meta lam in
    let lam = 
      lam
      |> _d env "alpha_before"
      |> Lam_pass_alpha_conversion.alpha_conversion meta
      |> _d env "alpha_after"
      |> Lam_pass_exits.simplify_exits in    
    let () = Lam_pass_collect.collect_helper meta lam in


    lam
    |> _d env "simplify_alias_before"
    |>  Lam_pass_remove_alias.simplify_alias meta 
    |> _d env "alpha_conversion"
    |>  Lam_pass_alpha_conversion.alpha_conversion meta
    |> _d env "before-simplify_lets"
    (* we should investigate a better way to put different passes : )*)
    |> Lam_pass_lets_dce.simplify_lets 

    |> _d env "before-simplify-exits"
    (* |> (fun lam -> Lam_pass_collect.collect_helper meta lam 
       ; Lam_pass_remove_alias.simplify_alias meta lam) *)
    (* |> Lam_group_pass.scc_pass
       |> _d "scc" *)
    |> Lam_pass_exits.simplify_exits
    |> _d env "simplify_lets"
#if undefined BS_RELEASE_BUILD then    
    |> (fun lam -> 
       let () = 
        Ext_log.dwarn ~__POS__ "Before coercion: %a@." Lam_stats.print meta in 
      Lam_check.check (Js_config.get_current_file ()) lam
    ) 
#end    
  in

  let ({Lam_coercion.groups = groups } as coerced_input , meta) = 
    Lam_coercion.coerce_and_group_big_lambda  meta lam
  in 

#if undefined BS_RELEASE_BUILD then   
  let () =
    Ext_log.dwarn ~__POS__ "After coercion: %a@." Lam_stats.print meta ;
    if Js_config.is_same_file () then
      let f =
        Ext_path.chop_extension ~loc:__LOC__ filename ^ ".lambda" in
      Ext_pervasives.with_file_as_pp f begin fun fmt ->
        Format.pp_print_list ~pp_sep:Format.pp_print_newline
          (Lam_group.pp_group env) fmt (coerced_input.groups) 
      end;
  in
#end  
  let maybe_pure = no_side_effects groups in
#if undefined BS_RELEASE_BUILD then 
  let () = Ext_log.dwarn ~__POS__ "\n@[[TIME:]Pre-compile: %f@]@."  (Sys.time () *. 1000.) in      
#end  
  let body  =     
    Ext_list.map groups (fun group -> compile_group meta group)
    |> Js_output.concat
    |> Js_output.output_as_block
  in
#if undefined BS_RELEASE_BUILD then 
  let () = Ext_log.dwarn ~__POS__ "\n@[[TIME:]Post-compile: %f@]@."  (Sys.time () *. 1000.) in      
#end    
  (* The file is not big at all compared with [cmo] *)
  (* Ext_marshal.to_file (Ext_path.chop_extension filename ^ ".mj")  js; *)
  let meta_exports = meta.exports in 
  let export_set = Ident_set.of_list meta_exports in 
  let js : J.program = 
      { J.name = filename ; 
        exports = meta_exports ; 
        export_set; 
        block = body}
  in
  js 
  |> _j "initial"
  |> Js_pass_flatten.program
  |> _j "flattern"
  |> Js_pass_tailcall_inline.tailcall_inline
  |> _j "inline_and_shake"
  |> Js_pass_flatten_and_mark_dead.program
  |> _j "flatten_and_mark_dead"
  (* |> Js_inline_and_eliminate.inline_and_shake *)
  (* |> _j "inline_and_shake" *)
  |> (fun js -> ignore @@ Js_pass_scope.program  js ; js )
  |> Js_shake.shake_program
  |> _j "shake"
  |> ( fun (program:  J.program) -> 
      let external_module_ids = 
        Lam_compile_env.get_required_modules  
          may_required_modules  
          (Js_fold_basic.calculate_hard_dependencies program.block)
        |>
        (fun x ->
           if !Js_config.sort_imports then
             Ext_list.sort_via_array x
               (fun id1 id2 ->
                  Ext_string.compare (Lam_module_ident.name id1) (Lam_module_ident.name id2)
               ) 
           else
             x
        )
      in
      Warnings.check_fatal ();  
      let effect = 
        Lam_stats_export.get_dependent_module_effect
        meta maybe_pure external_module_ids in 
      let v : Js_cmj_format.t = 
        Lam_stats_export.export_to_cmj 
          meta  
          effect 
          coerced_input.export_map
          (get_cmj_case output_prefix)
      in
      (if not @@ !Clflags.dont_write_files then
         Js_cmj_format.to_file 
          ~check_exists:(not !Js_config.force_cmj)
           (output_prefix ^ Literals.suffix_cmj) v);
      {J.program = program ; side_effect = effect ; modules = external_module_ids }      
    )
;;

let (//) = Filename.concat  

let lambda_as_module 
    finalenv 
    (filename : string) 
    (output_prefix : string)
    (lam : Lambda.lambda) = 
  let lambda_output = 
    compile ~filename output_prefix finalenv lam in
  let basename =  
    Ext_namespace.js_name_of_basename !Js_config.bs_suffix 
      (Filename.basename
         output_prefix) in
  let package_info = Js_packages_state.get_packages_info () in 
  if Js_packages_info.is_empty package_info  then 
    begin 
      let output_chan chan =         
        Js_dump_program.dump_deps_program ~output_prefix NodeJS lambda_output chan in
      (if !Js_config.dump_js then output_chan stdout);
      if not !Clflags.dont_write_files then 
        Ext_pervasives.with_file_as_chan 
          (Filename.dirname filename //  basename)
          output_chan
    end
  else
    Js_packages_info.iter package_info (fun {module_system; path = _path} -> 
        let output_chan chan  = 
          Js_dump_program.dump_deps_program ~output_prefix
            module_system 
            lambda_output
            chan in
        (if !Js_config.dump_js then 
           output_chan  stdout);
        if not @@ !Clflags.dont_write_files then 
          Ext_pervasives.with_file_as_chan
#if BS_NATIVE then
            (if Filename.is_relative _path then Lazy.force Ext_filename.package_dir // _path // basename
             (* #913 only generate little-case js file *)
            else _path // basename) output_chan )
#else
            (Lazy.force Ext_filename.package_dir //
             _path //
             basename
             (* #913 only generate little-case js file *)
            ) output_chan )
  
#end

(* We can use {!Env.current_unit = "Pervasives"} to tell if it is some specific module, 
    We need handle some definitions in standard libraries in a special way, most are io specific, 
    includes {!Pervasives.stdin, Pervasives.stdout, Pervasives.stderr}

    However, use filename instead of {!Env.current_unit} is more honest, since node-js module system is coupled with the file name 
*)
