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

let compile_group ({filename = file_name; env;} as meta : Lam_stats.meta) 
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
      ~exp:(E.runtime_ref  Js_config.io id.name)
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
    Js_output.of_stmt @@ S.alias_variable id ~exp:(E.float "4_294_967_295.") 
                           
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
                                    should_return = False;
                                    jmp_table = Lam_compile_defs.empty_handler_map;
                                    meta
                                  } id  lam

  | Recursive id_lams, _   -> 
    Lam_compile.compile_recursive_lets 
      { st = EffectCall ;
        should_return = False; 
        jmp_table = Lam_compile_defs.empty_handler_map;
        meta
      } 
      id_lams
  | Nop lam, _ -> (* TODO: Side effect callls, log and see statistics *)
    Lam_compile.compile_lambda {st = EffectCall;
                                should_return = False;
                                jmp_table = Lam_compile_defs.empty_handler_map;
                                meta
                               } lam

;;

(** Actually simplify_lets is kind of global optimization since it requires you to know whether 
    it's used or not 
    [no_export] is only used in playground
*)
let compile  ~filename output_prefix no_export env _sigs 
    (lam : Lambda.lambda)   = 
  let export_idents = 
    if no_export then
      []    
    else  Translmod.get_export_identifiers()  
  in
  let () = 
    export_idents |> List.iter 
      (fun (id : Ident.t) -> Ext_log.dwarn __LOC__ "export: %s/%d"  id.name id.stamp) 
  in
  (* To make toplevel happy - reentrant for js-demo *)
  let ()   = 
    Translmod.reset () ; 
    Lam_compile_env.reset () ;
  in 
  let lam = Lam.convert lam in 
  let _d  = Lam_util.dump env  in
  let _j = Js_pass_debug.dump in
  let lam = _d "initial"  lam in
  let lam  = Lam_group.deep_flatten lam in
  let lam = _d  "flatten" lam in
  let meta = 
    Lam_pass_collect.count_alias_globals env filename  export_idents lam in
  let lam = 
    let lam =  
      lam
      |> _d "flattern"
      |>  Lam_pass_exits.simplify_exits
      |> _d "simplyf_exits"
      |>  Lam_pass_remove_alias.simplify_alias  meta 
      |> _d "simplify_alias"
      |> Lam_group.deep_flatten
      |> _d "flatten"
    in  (* Inling happens*)
  
    let ()  = Lam_pass_collect.collect_helper meta lam in
    let lam = Lam_pass_remove_alias.simplify_alias meta lam  in
    let lam = Lam_group.deep_flatten lam in
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
    |> _d "simplify_lets"
    (* we should investigate a better way to put different passes : )*)
    |> Lam_pass_lets_dce.simplify_lets 
    |> _d "simplify_lets"
    (* |> (fun lam -> Lam_pass_collect.collect_helper meta lam 
       ; Lam_pass_remove_alias.simplify_alias meta lam) *)
    |> Lam_pass_exits.simplify_exits
    |> _d "simplify_lets"


  in

  (* Debug identifier table *)
  (* Lam_stats_util.pp_alias_tbl Format.err_formatter meta.alias_tbl; *)
  (* Lam_stats_util.dump_exports_arities meta ; *)
  (* Lam_stats_util.pp_arities_tbl Format.err_formatter meta.arities_tbl; *)

  (* Dump for debugger *)

  begin 
    match (lam : Lam.t) with
    | Lprim{primitive = Psetglobal id; args =  [biglambda]; _}
      -> 
      (* Invariant: The last one is always [exports]
         Compile definitions
         Compile exports
         Assume Pmakeblock(_,_),
         lambda_exports are pure
         compile each binding with a return value
         This might be wrong in toplevel
      *)

      begin 
        match Lam_group.flatten [] biglambda with 
        | Lprim {primitive = Pmakeblock (_,_,_); args =  lambda_exports},
          rest ->
          let coercion_groups, new_exports, new_export_set,  export_map = 
            if no_export then 
              [], [], Ident_set.empty, Ident_map.empty
            else
              List.fold_right2 
                (fun  eid lam (coercions, new_exports, new_export_set,  export_map) ->
                   match (lam : Lam.t) with 
                   | Lvar id 
                     when Ident.name id = Ident.name eid -> 
                     (* {[ Ident.same id eid]} is more  correct, 
                        however, it will introduce 
                        a coercion, which is not necessary, 
                        as long as its name is the same, we want to avoid 
                        another coercion                        
                     *)
                     (coercions, 
                      id :: new_exports, 
                      Ident_set.add id new_export_set,
                      export_map)
                   | _ -> (** TODO : bug 
                              check [map.ml] here coercion, we introduced 
                              rebound which is not corrrect 
                              {[
                                let Make/identifier = function (funarg){
                                    var $$let = Make/identifier(funarg);
                                            return [0, ..... ]
                                  }
                              ]}
                              Possible fix ? 
                              change export identifier, we should do this in the very 
                              beginning since lots of optimizations depend on this
                              however
                          *)
                     (Lam_group.Single(Strict ,eid,  lam) :: coercions, 
                      eid :: new_exports,
                      Ident_set.add eid new_export_set, 
                      Ident_map.add eid lam export_map))
                meta.exports lambda_exports 
                ([],[], Ident_set.empty, Ident_map.empty)
          in
          let () = 
            new_exports |> List.iter 
              (fun (id : Ident.t) -> Ext_log.dwarn __LOC__ "export: %s/%d"  id.name id.stamp) 
          in
          let meta = { meta with 
                       export_idents = new_export_set;
                       exports = new_exports
                     } in 
          let (export_map, rest) = 
            List.fold_left 
              (fun (export_map, acc) x ->
                 (match (x : Lam_group.t)  with 
                 | Single (_,id,lam) when Ident_set.mem id new_export_set 
                   -> Ident_map.add id lam export_map
                 | _ -> export_map), x :: acc ) (export_map, coercion_groups) rest in

          (* TODO: turn in on debug mode later*)
          let () =
            if Js_config.is_same_file () then
              let f =
                Ext_filename.chop_extension ~loc:__LOC__ filename ^ ".lambda" in
              Ext_pervasives.with_file_as_pp f @@ fun fmt ->
              Format.pp_print_list ~pp_sep:Format.pp_print_newline
                (Lam_group.pp_group env) fmt rest ;
          in
          let rest = Lam_dce.remove meta.exports rest 
          in
          let module  E = struct exception  Not_pure of string end in
          (** Also need analyze its depenency is pure or not *)
          let no_side_effects rest = 
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
          in
          let maybe_pure = no_side_effects rest
          in
          let body  = 
            rest
            |> List.map (fun group -> compile_group meta group)
            |> Js_output.concat
            |> Js_output.to_block
          in
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
              Lam_compile_env.get_requried_modules  
                meta.env
                meta.required_modules  
                (Js_fold_basic.calculate_hard_dependencies js.block)
            in
            (* Exporting ... *)
            let v = 
              Lam_stats_export.export_to_cmj meta  maybe_pure external_module_ids
                (if no_export then Ident_map.empty else export_map) 
            in


            (if not @@ Ext_string.is_empty filename then
               Js_cmj_format.to_file 
                  (output_prefix ^ Js_config.cmj_ext) v);
            Js_program_loader.decorate_deps external_module_ids v.effect js
          )
        | _ -> raise Not_a_module
      end
    | _ -> raise Not_a_module end
;;



let lambda_as_module 
    env 
    (sigs : Types.signature)
    (filename : string) 
    (output_prefix : string)
    (lam : Lambda.lambda) = 
  begin 
    Js_config.set_current_file filename ;  
    Js_config.iset_debug_file "optional_ffi_test.ml";
    let lambda_output = compile ~filename output_prefix false env sigs lam in
    let (//) = Filename.concat in 
    let basename =  
      Ext_filename.chop_extension ~loc:__LOC__ 
        (Filename.basename filename) ^  Js_config.get_ext() in
    (* Not re-entrant *)
    match Js_config.get_packages_info () with 
    | Browser -> ()
    | Empty 
    | NonBrowser (_, []) -> 
      (* script mode *)
      let output_filename = 
        (if Filename.is_relative filename then 
           Lazy.force Ext_filename.cwd // 
           Filename.dirname filename 
         else 
           Filename.dirname filename) // basename         
      in 
      Ext_pervasives.with_file_as_chan 
        output_filename 
        (fun chan -> 
           Js_dump.dump_deps_program `NodeJS lambda_output chan)

    | NonBrowser (_package_name, module_systems) -> 
      module_systems |> List.iter begin fun (module_system, _path) -> 
        let output_filename = 
          Lazy.force Ext_filename.package_dir //
          _path //
          basename 
        in
        Ext_pervasives.with_file_as_chan 
          output_filename 
          (fun chan -> 
             Js_dump.dump_deps_program 
               (module_system :> [Js_config.module_system | `Browser])
               lambda_output
               chan)
      end
  end
(* We can use {!Env.current_unit = "Pervasives"} to tell if it is some specific module, 
    We need handle some definitions in standard libraries in a special way, most are io specific, 
    includes {!Pervasives.stdin, Pervasives.stdout, Pervasives.stderr}

    However, use filename instead of {!Env.current_unit} is more honest, since node-js module system is coupled with the file name 
*)
