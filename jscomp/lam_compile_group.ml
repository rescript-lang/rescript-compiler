(* OCamlScript compiler
 * Copyright (C) 2015-2016 Bloomberg Finance L.P.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

(* Author: Hongbo Zhang  *)



module E = Js_helper.Exp 
module S = Js_helper.Stmt  

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
            E.fun_ [a;b] [S.return (E.string_append (E.var a) (E.var b))]
            )

  (* QUICK hack to make hello world example nicer,
     Note the arity of [print_endline] is already analyzed before, 
     so it should be safe
  *)
  | Single(_, ({name="print_endline";_} as id),_ ),  "pervasives.ml" ->
    Js_output.of_stmt @@ S.alias_variable id 
      ~exp:(E.js_global "console.log")
  | Single(_, ({name="prerr_endline";_} as id),_ ),  "pervasives.ml" ->
    Js_output.of_stmt @@ S.alias_variable id 
      ~exp:(E.js_global "console.error")


  | Single(_, ({name="string_of_int";_} as id),_ ),  "pervasives.ml" ->
    Js_output.of_stmt @@ S.alias_variable id
      ~exp:( 
        let arg = Ext_ident.create "param" in
        E.fun_ [arg] [S.return (E.anything_to_string (E.var arg))]
      )

  | Single(_, ({name="max_float";_} as id),_ ),  "pervasives.ml" ->

    Js_output.of_stmt @@ S.alias_variable id 
      ~exp:(E.js_global_dot "Number" "MAX_VALUE")
  | Single(_, ({name="min_float";_} as id) ,_ ), "pervasives.ml" ->
    Js_output.of_stmt @@  S.alias_variable id
      ~exp:(E.js_global_dot  "Number" "MIN_VALUE")
  | Single(_, ({name="epsilon_float";_} as id) ,_ ),  "pervasives.ml" ->
    Js_output.of_stmt @@ S.alias_variable id 
      ~exp:(E.js_global_dot  "Number" "EPSILON")
  | Single(_, ({name="cat";_} as id) ,_ ),  "bytes.ml" ->
    Js_output.of_stmt @@ S.alias_variable id
      ~exp:(let a = Ext_ident.create "a" in 
            let b = Ext_ident.create "b" in
            E.fun_ [a;b] [S.return (E.array_append (E.var a) (E.var b))]
           )

  (** Special handling for values in [Sys] *)
  | Single(_, ({name="max_array_length" | "max_string_length";_} as id) ,_ ),  "sys.ml" ->
    (* See [js_knowledge] Array size section, can not be expressed by OCaml int,
       note that casual handling of {!Sys.max_string_length} could result into 
       negative value which could cause wrong behavior of {!Buffer.create}
     *)
    Js_output.of_stmt @@ S.alias_variable id ~exp:(E.float "4_294_967_295.") 
                           
  | Single(_, ({name="max_int";_} as id) ,_ ),  ("sys.ml" | "nativeint.ml") ->
    (* See [js_knowledge] Max int section, (2. ** 53. -. 1.;;) can not be expressed by OCaml int *)
    Js_output.of_stmt @@ S.alias_variable id ~exp:(E.float "9007199254740991.") 

  | Single(_, ({name="min_int";_} as id) ,_ ),  ("sys.ml" | "nativeint.ml") ->
    (* See [js_knowledge] Max int section, -. (2. ** 53. -. 1.);; can not be expressed by OCaml int *)
    Js_output.of_stmt @@ S.alias_variable id ~exp:(E.float ("-9007199254740991.")) 

  | Single (kind, id, lam), _ -> 
    (* let lam = Optimizer.simplify_lets [] lam in  *)
    (* can not apply again, it's wrong USE it with care*)
    (* ([Js_helper.Stmt.comment (Gen_of_env.query_type id  env )], None)  ++ *)
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
    [non_export] is only used in playground
*)
let compile  ~filename non_export env _sigs lam   = 
  let export_idents = 
    if non_export then
      []    
    else  Translmod.get_export_identifiers()  
  in
  let ()   = Translmod.reset () in (* To make toplevel happy - reentrant for js-demo *)
  let ()   = Lam_compile_env.reset ()  in
  let _d   = Lam_util.dump env filename in
  let lam = _d  lam in
  let lam  = Lam_group.deep_flatten lam in
  let lam = _d  lam in
  let meta = 
    Lam_pass_collect.count_alias_globals env filename  export_idents lam in
  let lam = 
    let lam =  
      lam
      |> _d
      |>  Lam_pass_exits.simplify_exits
      |> _d
      |>  Lam_pass_remove_alias.simplify_alias  meta in  (* Inling happens*)
    (* TODO: research how to combine those passes efficiently *)
    let () = ignore @@ _d lam in
    let lam = Lam_group.deep_flatten lam in
    let () = ignore @@ _d lam  in
    let ()  = Lam_pass_collect.collect_helper meta lam in
    let lam = Lam_pass_remove_alias.simplify_alias meta lam  in
    let lam = Lam_group.deep_flatten lam in
    let ()  = Lam_pass_collect.collect_helper meta lam in
    let () = ignore @@ _d lam  in

    let lam = 
      lam
      |> Lam_pass_alpha_conversion.alpha_conversion meta
      |> Lam_pass_exits.simplify_exits in    
    let () = Lam_pass_collect.collect_helper meta lam in


    lam
    |> _d 
    |>  Lam_pass_remove_alias.simplify_alias meta 
    |> _d 
    |>  Lam_pass_alpha_conversion.alpha_conversion meta
    (* we should investigate a better way to put different passes : )*)
    |> Lam_pass_lets_dce.simplify_lets 
    (* |> (fun lam -> Lam_pass_collect.collect_helper meta lam 
       ; Lam_pass_remove_alias.simplify_alias meta lam) *)
    |> Lam_pass_exits.simplify_exits



  in

  (* Debug identifier table *)
  (* Lam_stats_util.pp_alias_tbl Format.err_formatter meta.alias_tbl; *)
  (* Lam_stats_util.dump_exports_arities meta ; *)
  (* Lam_stats_util.pp_arities_tbl Format.err_formatter meta.arities_tbl; *)

  (* Dump for debugger *)

  begin 
    match (lam : Lambda.lambda) with
    | Lprim(Psetglobal id, [biglambda])  (* ATT: might be wrong in toplevel *) ->
      begin 
        match Lam_group.flatten [] biglambda with 
        | Lprim( (Pmakeblock (_,_,_), lambda_exports)),  rest ->
          let coercion_groups, new_exports = 
            if non_export then 
              [], []
            else
              List.fold_right2 
                (fun  eid lam (coercions, new_exports) ->
                   match (lam : Lambda.lambda) with 
                   | Lvar id when Ident.name id = Ident.name eid -> 
                     (coercions, id :: new_exports)
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
                      eid :: new_exports))
                meta.exports lambda_exports ([],[])
          in

          let meta = { meta with 
                       export_idents = Lam_util.ident_set_of_list new_exports;
                       exports = new_exports
                     } in 
          let rest = List.rev_append rest coercion_groups in
          let () =
            if not @@ Ext_string.is_empty filename 
            then
              let f = 
                Ext_filename.chop_extension ~loc:__LOC__ filename ^ ".lambda" in
              Ext_pervasives.with_file_as_pp f @@ fun fmt ->
              Format.pp_print_list ~pp_sep:Format.pp_print_newline
                (Lam_group.pp_group env) fmt rest ;
          in
          (* Invariant: The last one is always [exports]
             Compile definitions
             Compile exports
             Assume Pmakeblock(_,_),
             lambda_exports are pure
             compile each binding with a return value
          *)
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
          |> Js_pass_flatten.program
          |> Js_inline_and_eliminate.inline_and_shake

          |> Js_pass_flatten_and_mark_dead.program
          |> (fun js -> ignore @@ Js_pass_scope.program  js ; js )
          |> Js_shake.shake_program
          |> ( fun (js:  J.program) -> 
            let external_module_ids = 
              Lam_compile_env.get_requried_modules  
                meta.env
                meta.required_modules  
                (Js_fold_basic.calculate_hard_dependencies js.block)
            in
            let required_modules =
              List.map 
                (fun id -> Lam_module_ident.id id, Js_program_loader.string_of_module_id id )
                external_module_ids in

            (* Exporting ... *)
            let v = 
              Lam_stats_util.export_to_cmj meta  maybe_pure external_module_ids
                (if non_export then [] else lambda_exports) 
            in
            (if not @@ Ext_string.is_empty filename then
               Js_cmj_format.to_file 
                 (Ext_filename.chop_extension ~loc:__LOC__  filename ^ ".cmj") v);
            Js_program_loader.decorate_deps required_modules v.pure js
          )
        | _ -> raise Not_a_module
      end
    | _ -> raise Not_a_module end
;;



let lambda_as_module 
    env 
    (sigs : Types.signature)
    (filename : string) 
    (lam : Lambda.lambda) = 
  begin 
    Lam_current_unit.set_file filename ;  
    Lam_current_unit.iset_debug_file "ari_regress_test.ml";
    Ext_pervasives.with_file_as_chan 
      (Ext_filename.chop_extension ~loc:__LOC__ filename ^  ".js")
      (fun chan -> Js_dump.dump_program (compile ~filename false env sigs lam) chan)
  end
(* We can use {!Env.current_unit = "Pervasives"} to tell if it is some specific module, 
    We need handle some definitions in standard libraries in a special way, most are io specific, 
    includes {!Pervasives.stdin, Pervasives.stdout, Pervasives.stderr}

    However, use filename instead of {!Env.current_unit} is more honest, since node-js module system is coupled with the file name 
*)
