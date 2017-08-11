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

open Js_output.Ops

exception Not_a_module

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
  | Single(_, ({name="stdout"|"stderr"|"stdin";_} as id),_ ),
    "pervasives.ml" -> 
    Js_output.of_stmt @@ S.alias_variable id
      ~exp:(E.runtime_ref  Js_runtime_modules.io id.name)
  (* 
         we delegate [stdout, stderr, and stdin] into [caml_io] module, 
         the motivation is to help dead code eliminatiion, it's helpful 
         to make those parts pure (not a function call), then it can be removed 
         if unused 
      *)                     
  | Single(_, ({name="infinity";_} as id),_ ),  "pervasives.ml" 
    -> (* TODO: check relative path to compiler*)
    Js_output.of_stmt @@ S.alias_variable id ~exp:(E.js_global "Infinity")
  | Single(_, ({name="neg_infinity";_} as id),_ ), "pervasives.ml" ->
    Js_output.of_stmt @@ S.alias_variable id ~exp:(E.js_global "-Infinity")
  | Single(_, ({name="nan";_} as id),_ ),  "pervasives.ml" ->
    Js_output.of_stmt @@ S.alias_variable id ~exp:(E.js_global "NaN")

  (* TODO: 
      Make it more safe, we should rewrite the last one...
       checkout [E.mldot], it would make sense that cross module inlining done there
       In general, we would like to do such specialization on primitive specialization
        [Lam_dispatch_primitive], here it makes an exception since this function is not a primitive
  *) 
  | Single(_, ({name="^";_} as id),_ ),  "pervasives.ml" ->
    Js_output.of_stmt @@ S.alias_variable id 
      ~exp:(let a = Ext_ident.create "a" in 
            let b = Ext_ident.create "b" in
            E.ocaml_fun [a;b] [S.return (E.string_append (E.var a) (E.var b))]
           )

  (* QUICK hack to make hello world example nicer,
     Note the arity of [print_endline] is already analyzed before, 
     so it should be safe
  *)
  | Single(_, ({name="print_endline";_} as id),_ ),  "pervasives.ml" ->
    Js_output.of_stmt @@ S.alias_variable id 
      ~exp:(let param = Ext_ident.create "param" in 
            E.ocaml_fun [param] [S.return 
                                   (E.seq (E.call ~info:{arity=Full; call_info = Call_na} 
                                             (E.js_global "console.log") [E.var param]) 
                                      E.zero_int_literal )] )
  | Single(_, ({name="prerr_endline";_} as id),_ ),  "pervasives.ml" ->
    Js_output.of_stmt @@ S.alias_variable id 
      ~exp:(let param = Ext_ident.create "param" in 
            E.ocaml_fun [param] [S.return 
                                   (E.seq (E.call ~info:{arity=Full; call_info = Call_na} 
                                             (E.js_global "console.error") [E.var param]) 
                                      E.zero_int_literal )] )


  | Single(_, ({name="string_of_int";_} as id),_ ),  "pervasives.ml" ->
    Js_output.of_stmt @@ S.alias_variable id
      ~exp:( 
        let arg = Ext_ident.create "param" in
        E.ocaml_fun [arg] [S.return (E.anything_to_string (E.var arg))]
      )

  | Single(_, ({name="max_float";_} as id),_ ),  "pervasives.ml" ->

    Js_output.of_stmt @@ S.alias_variable id 
      ~exp:(E.js_global_dot "Number" "MAX_VALUE")
  | Single(_, ({name="min_float";_} as id) ,_ ), "pervasives.ml" ->
    Js_output.of_stmt @@  S.alias_variable id
      ~exp:(E.js_global_dot  "Number" "MIN_VALUE")
  | Single(_, ({name="epsilon_float";_} as id) ,_ ),  "pervasives.ml" ->
    Js_output.of_stmt @@ S.alias_variable id 
      ~exp:(E.float "2.220446049250313e-16")
  | Single(_, ({name="cat";_} as id) ,_ ),  "bytes.ml" ->
    Js_output.of_stmt @@ S.alias_variable id
      ~exp:(let a = Ext_ident.create "a" in 
            let b = Ext_ident.create "b" in
            E.ocaml_fun [a;b] [S.return (E.array_append (E.var a) (E.var b))]
           )

  (** Special handling for values in [Sys] *)
  | Single(_, ({name="max_array_length" | "max_string_length";_} as id) ,_ ),  "sys.ml" ->
    (* See [js_knowledge] Array size section, can not be expressed by OCaml int,
       note that casual handling of {!Sys.max_string_length} could result into 
       negative value which could cause wrong behavior of {!Buffer.create}
    *)
    Js_output.of_stmt @@ S.alias_variable id ~exp:(E.float "2147483647")  (*2 ^ 31 - 1*)

  | Single(_, ({name="max_int";_} as id) ,_ ),  ("sys.ml" | "nativeint.ml") ->
    (* See [js_knowledge] Max int section, (2. ** 53. -. 1.;;)
       can not be expressed by OCaml int 
       FIXME: we need handle {!Nativeint} and {!Sys} differently
    *)
    Js_output.of_stmt @@ S.alias_variable id 
      ~exp:(E.float "9007199254740991.") 

  | Single(_, ({name="min_int";_} as id) ,_ ),  ("sys.ml" | "nativeint.ml") ->
    (* See [js_knowledge] Max int section, -. (2. ** 53. -. 1.);;
       can not be expressed by OCaml int 
       FIXME: we need handle {!Nativeint} and {!Sys} differently
    *)
    Js_output.of_stmt @@ S.alias_variable id
      ~exp:(E.float ("-9007199254740991.")) 

  | Single (kind, id, lam), _ -> 
    (* let lam = Optimizer.simplify_lets [] lam in  *)
    (* can not apply again, it's wrong USE it with care*)
    (* ([Js_stmt_make.comment (Gen_of_env.query_type id  env )], None)  ++ *)
    Lam_compile.compile_let  kind { st = Declare (kind, id);
                                    should_return = ReturnFalse;
                                    jmp_table = Lam_compile_defs.empty_handler_map;
                                    meta
                                  } id  lam

  | Recursive id_lams, _   -> 
    Lam_compile.compile_recursive_lets 
      { st = EffectCall ;
        should_return = ReturnFalse; 
        jmp_table = Lam_compile_defs.empty_handler_map;
        meta
      } 
      id_lams
  | Nop lam, _ -> (* TODO: Side effect callls, log and see statistics *)
    Lam_compile.compile_lambda {st = EffectCall;
                                should_return = ReturnFalse;
                                jmp_table = Lam_compile_defs.empty_handler_map;
                                meta
                               } lam

;;

 (** Also need analyze its depenency is pure or not *)
let no_side_effects (rest : Lam_group.t list) : string option = 
    Ext_list.for_all_opt (fun (x : Lam_group.t) -> 
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
          Ext_list.for_all_opt (fun (id,lam) -> 
              if not @@ Lam_analysis.no_side_effects lam 
              then Some (Printf.sprintf "%s" id.Ident.name )
              else None
            ) bindings
        | Nop lam -> 
          if not @@ Lam_analysis.no_side_effects lam 
          then 
            (*  (Lam_util.string_of_lambda lam) *)
            Some ""
          else None (* TODO :*))
      rest



(** Actually simplify_lets is kind of global optimization since it requires you to know whether 
    it's used or not 
*)
let compile  ~filename output_prefix env _sigs 
    (lam : Lambda.lambda)   = 
  let export_idents = Translmod.get_export_identifiers() in
  let export_ident_sets = Ident_set.of_list export_idents in 
  (* To make toplevel happy - reentrant for js-demo *)
  let () = 
#if BS_DEBUG then     
    export_idents |> List.iter 
      (fun (id : Ident.t) -> Ext_log.dwarn __LOC__ "export: %s/%d"  id.name id.stamp) ;
#end      
    Lam_compile_env.reset () ;
  in 
  let lam, may_required_modules = Lam.convert export_ident_sets lam in 
  let _d  = fun s lam -> 
    let result = Lam_util.dump env s lam  in
#if BS_DEBUG then 
    Ext_log.dwarn __LOC__ "START CHECKING PASS %s@." s;
    ignore @@ Lam.check (Js_config.get_current_file ()) lam;
    Ext_log.dwarn __LOC__ "FINISH CHECKING PASS %s@." s;
#end
    result 
  in
  let _j = Js_pass_debug.dump in
  let lam = _d "initial"  lam in
  let lam  = Lam_pass_deep_flatten.deep_flatten lam in
  let lam = _d  "flatten" lam in
  let meta = 
    Lam_pass_collect.count_alias_globals env filename
      export_idents export_ident_sets lam in
  let lam = 
    let lam =  
      lam
      |> _d "flattern"
      |>  Lam_pass_exits.simplify_exits
      |> _d "simplyf_exits"
      |> (fun lam -> Lam_pass_collect.collect_helper meta lam; lam)
      |>  Lam_pass_remove_alias.simplify_alias  meta
      |> _d "simplify_alias"
      |> Lam_pass_deep_flatten.deep_flatten
      |> _d "flatten"
    in  (* Inling happens*)

    let ()  = Lam_pass_collect.collect_helper meta lam in
    let lam = Lam_pass_remove_alias.simplify_alias meta lam  in
    let lam = Lam_pass_deep_flatten.deep_flatten lam in
    let ()  = Lam_pass_collect.collect_helper meta lam in
    let lam = 
      lam
      |> _d "alpha_before"
      |> Lam_pass_alpha_conversion.alpha_conversion meta
      |> Lam_pass_exits.simplify_exits in    
    let () = Lam_pass_collect.collect_helper meta lam in


    lam
    |> _d "simplify_alias_before"
    |>  Lam_pass_remove_alias.simplify_alias meta 
    |> _d "alpha_conversion"
    |>  Lam_pass_alpha_conversion.alpha_conversion meta
    |> _d "before-simplify_lets"
    (* we should investigate a better way to put different passes : )*)
    |> Lam_pass_lets_dce.simplify_lets 

    |> _d "before-simplify-exits"
    (* |> (fun lam -> Lam_pass_collect.collect_helper meta lam 
       ; Lam_pass_remove_alias.simplify_alias meta lam) *)
    (* |> Lam_group_pass.scc_pass
       |> _d "scc" *)
    |> Lam_pass_exits.simplify_exits
    |> _d "simplify_lets"
#if BS_DEBUG then    
    |> (fun lam -> 
       let () = 
        Ext_log.dwarn __LOC__ "Before coercion: %a@." Lam_stats.print meta in 
      Lam.check (Js_config.get_current_file ()) lam
    ) 
#end    
  in

  let ({Lam_coercion.groups = groups } as coerced_input , meta) = 
    Lam_coercion.coerce_and_group_big_lambda  meta lam
  in 

#if BS_DEBUG then   
  let () =
    Ext_log.dwarn __LOC__ "After coercion: %a@." Lam_stats.print meta ;
    if Js_config.is_same_file () then
      let f =
        Ext_filename.chop_extension ~loc:__LOC__ filename ^ ".lambda" in
      Ext_pervasives.with_file_as_pp f begin fun fmt ->
        Format.pp_print_list ~pp_sep:Format.pp_print_newline
          (Lam_group.pp_group env) fmt (coerced_input.groups) 
      end;
  in
#end  
  let maybe_pure = no_side_effects groups in
#if BS_DEBUG then 
  let () = Ext_log.dwarn __LOC__ "\n@[[TIME:]Pre-compile: %f@]@."  (Sys.time () *. 1000.) in      
#end  
  let body  = 
    groups
    |> List.map (fun group -> compile_group meta group)
    |> Js_output.concat
    |> Js_output.to_block
  in
#if BS_DEBUG then 
  let () = Ext_log.dwarn __LOC__ "\n@[[TIME:]Post-compile: %f@]@."  (Sys.time () *. 1000.) in      
#end    
  (* The file is not big at all compared with [cmo] *)
  (* Ext_marshal.to_file (Ext_filename.chop_extension filename ^ ".mj")  js; *)
  let js = 
    Js_program_loader.make_program filename meta.exports
      body 
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
  |> ( fun (js:  J.program) -> 
      let external_module_ids = 
        Lam_compile_env.get_required_modules  
          may_required_modules  
          (Js_fold_basic.calculate_hard_dependencies js.block)
        |>
        (fun x ->
           if !Js_config.sort_imports then
             Ext_list.sort_via_array
               (fun (id1 : Lam_module_ident.t) (id2 : Lam_module_ident.t) ->
                  Ext_string.compare (Lam_module_ident.name id1) (Lam_module_ident.name id2)
               ) x
           else
             x
        )
      in

      let v = 
        Lam_stats_export.export_to_cmj 
          meta  
          maybe_pure 
          external_module_ids
          coerced_input.export_map
      in
      (if not @@ !Clflags.dont_write_files then
         Js_cmj_format.to_file 
           (output_prefix ^ Literals.suffix_cmj) v);
      Js_program_loader.decorate_deps external_module_ids v.effect js
    )
;;



let lambda_as_module 
    env 
    (sigs : Types.signature)
    (filename : string) 
    (output_prefix : string)
    (lam : Lambda.lambda) = 
  begin 
    Js_config.set_current_file filename ;  
#if BS_DEBUG then    
    Js_config.set_debug_file "inner_define.ml";
#end    
    let lambda_output = compile ~filename output_prefix env sigs lam in
    let (//) = Filename.concat in 
    let basename =  
      (* #758, output_prefix is already chopped *)
      Ext_filename.output_js_basename (Filename.basename
         output_prefix (* -o *)
         (* filename *) (* see #757  *)
      ) in
    (* Not re-entrant *)
    match Js_packages_state.get_packages_info () with 
    | Empty 
    | NonBrowser (_, []) -> 
      (* script mode *)
      let output_chan chan =         
        Js_dump.dump_deps_program ~output_prefix NodeJS lambda_output chan in
      (if !Js_config.dump_js then output_chan stdout);
      if not @@ !Clflags.dont_write_files then 
        Ext_pervasives.with_file_as_chan 
          ((if Filename.is_relative filename then 
              Lazy.force Ext_filename.cwd // 
              Filename.dirname filename 
            else 
              Filename.dirname filename) //  basename
          (* #913
             only generate little-case js file
          *)
          ) output_chan
    | NonBrowser (_package_name, module_systems) ->
      module_systems |> List.iter begin fun (module_system, _path) -> 
        let output_chan chan  = 
          Js_dump.dump_deps_program ~output_prefix
            module_system 
            lambda_output
            chan in
        (if !Js_config.dump_js then 
           output_chan  stdout);
        if not @@ !Clflags.dont_write_files then 
          Ext_pervasives.with_file_as_chan
            (Lazy.force Ext_filename.package_dir //
             _path //
              basename
             (* #913 only generate little-case js file *)
            ) output_chan

      end
  end
(* We can use {!Env.current_unit = "Pervasives"} to tell if it is some specific module, 
    We need handle some definitions in standard libraries in a special way, most are io specific, 
    includes {!Pervasives.stdin, Pervasives.stdout, Pervasives.stderr}

    However, use filename instead of {!Env.current_unit} is more honest, since node-js module system is coupled with the file name 
*)
