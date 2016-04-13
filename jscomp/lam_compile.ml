(* BuckleScript compiler
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



open Js_output.Ops 

module E = Js_exp_make 

module S = Js_stmt_make  

let method_cache_id = ref 1 (*TODO: move to js runtime for re-entrant *)


(* assume outer is [Lstaticcatch] *)
let rec flat_catches acc (x : Lambda.lambda)
  : (int * Lambda.lambda * Ident.t  list ) list * Lambda.lambda = 
  match x with 
  | Lstaticcatch( Lstaticcatch(l, (code,bindings), handler), (code1, bindings1),handler1) 
    when 
      not @@ Lam_exit_code.has_exit_code 
        (fun exit -> exit = code1 || List.exists (fun (c, _, _) -> c = exit ) acc ) handler
    -> 
    (* when handler does not have [exit code] which [code] belongs to collected,
       it is okay to merge
       *)
    flat_catches ( (code, handler,bindings) :: (code1,handler1,bindings1)  :: acc)  l
  | Lstaticcatch(l, (code, bindings), handler) ->
    (code,handler,bindings)::acc, l
    (* flat_catches ((code,handler,bindings)::acc) l  *)
  | _ -> acc, x

let flatten_caches  x = flat_catches [] x 

(* exception Not_an_expression *)

(* TODO:
    for expression generation, 
    name, should_return  is not needed,
    only jmp_table and env needed
*)
let translate_dispatch = ref (fun _ -> assert false)

type default_case = 
  | Default of Lambda.lambda
  | Complete
  | NonComplete


let rec  
  get_exp_with_index (cxt : Lam_compile_defs.cxt) lam 
    ((id : Ident.t), (pos : int),env) : Js_output.t = 
  let f =   Js_output.handle_name_tail cxt.st cxt.should_return lam in    
  Lam_compile_env.find_and_add_if_not_exist (id,pos) env 
    ~not_found:(fun id -> 
        f (E.str ~pure:false (Printf.sprintf "Err %s %d %d" id.name id.flags pos))
        (* E.index m (pos + 1) *) (** shift by one *)
        (** This can not happen since this id should be already consulted by type checker *)
      )
    ~found:(fun {id; name; closed_lambda } ->
        match id, name, closed_lambda with 
        | {name = "Sys"; _}, "os_type" , _
          (** We drop the ability of cross-compiling
              the compiler has to be the same running 
          *)
          ->  f (E.str Sys.os_type)
        | _, _, Some lam when Lam_util.not_function lam
          (* since it's only for alias, there is no arguments, 
             we should not inline function definition here, even though
             it is very small             
             TODO: add comment here, we should try to add comment for 
             cross module inlining             
          
             if we do too agressive inlining here: 

             if we inline {!List.length} which will call {!A_list.length}, 
             then we if we try inline {!A_list.length}, this means if {!A_list} 
             is rebuilt, this module should also be rebuilt,

             But if the build system is content-based, suppose {!A_list} 
             is changed, cmj files in {!List} is unchnaged, however, 
             {!List.length} call {!A_list.length} which is changed, since
             [ocamldep] only detect that we depend on {!List}, it will not 
             get re-built, then we are screwed.                   

             This is okay for stamp based build system.

             Another solution is that we add dependencies in the compiler

             -: we should not do functor application inlining in a 
                non-toplevel, it will explode code very quickly              
          *)               
          ->  
          compile_lambda cxt lam
        | _ -> 
          f (E.ml_var_dot id name)
      ) 
(* TODO: how nested module call would behave,
   In the future, we should keep in track  of if 
   it is fully applied from [Lapply]
   Seems that the module dependency is tricky..
   should we depend on [Pervasives] or not?

   we can not do this correctly for the return value, 
   however we can inline the definition in Pervasives
   TODO:
   [Pervasives.print_endline]
   [Pervasives.prerr_endline]
   @param id external module id 
   @param number the index of the external function 
   @param env typing environment
   @param args arguments 
 *)

and get_exp_with_args (cxt : Lam_compile_defs.cxt)  lam args_lambda
    (id : Ident.t) (pos : int) env : Js_output.t = 
  Lam_compile_env.find_and_add_if_not_exist (id,pos) env ~not_found:(fun id -> 
      (** This can not happen since this id should be already consulted by type checker 
          Worst case 
          {[
            E.index m (pos + 1)
          ]}
          shift by one (due to module encoding)
      *)
      (* Js_output.handle_block_return cxt.st cxt.should_return lam args_code @@  *)
      (* E.str ~pure:false  (Printf.sprintf "Err %s %d %d" *)
      (*                       id.name *)
      (*                       id.flags *)
      (*                       pos *)
      (*                    ) *)
      assert false 
    )

    ~found:(fun {id; name;arity; closed_lambda ; _} -> 
        let args_code, args = 
          List.fold_right 
            (fun (x : Lambda.lambda) (args_code, args)  ->
               match x with 
               | Lprim (Pgetglobal i, [] ) -> 
                 (* when module is passed as an argument - unpack to an array
                     for the function, generative module or functor can be a function,
                     however it can not be global -- global can only module
                 *)

                 args_code, (Lam_compile_global.get_exp (i, env, true) :: args)
               | _ -> 
                 begin match compile_lambda {cxt with st = NeedValue; should_return = False} x with
                   | {block = a; value = Some b} -> 
                     (a @ args_code), (b :: args )
                   | _ -> assert false
                 end
            ) args_lambda ([], []) in


        match closed_lambda with 
        | Some (Lfunction (_, params, body)) 
          when Ext_list.same_length params args_lambda -> 
          (* TODO: serialize it when exporting to save compile time *)
          let (_, param_map)  = 
            Lam_analysis.is_closed_with_map Ident_set.empty params body in
          compile_lambda cxt 
            (Lam_beta_reduce.propogate_beta_reduce_with_map cxt.meta param_map
               params body args_lambda)
        | _ ->  
          Js_output.handle_block_return cxt.st cxt.should_return lam args_code @@ 
          (match id, name,  args with 
           | {name = "Pervasives"; _}, "^", [ e0 ; e1] ->  
             E.string_append e0 e1 
           | {name = "Pervasives"; _}, "print_endline", ([ _ ] as args) ->  
             E.seq (E.dump Log args) E.unit
           | {name = "Pervasives"; _}, "prerr_endline", ([ _ ] as args) ->  
             E.seq (E.dump Error args) E.unit
           | {name = "CamlinternalMod"; _}, "update_mod" ,
             [  shape  ;
                _module ; 
                _ ] when Js_of_lam_module.is_empty_shape shape
             ->
             E.unit
           | {name = "CamlinternalMod"; _}, "init_mod" ,
             [ 
                _ ;
                shape  ;
                (* Module []
                   TODO: add a function [empty_shape]
                   This pattern match is fragile, since it depends 
                   on how we compile [Lconst]
                *)
             ] when Js_of_lam_module.is_empty_shape shape
             ->
             E.dummy_obj () (* purely type definition*)

           | _ -> 


             let rec aux (acc : J.expression)
                 (arity : Lam_stats.function_arities) args (len : int)  =
               match arity, len with
               | _, 0 -> 
                 acc (** All arguments consumed so far *)
               | Determin (a, (x,_) :: rest, b), len   ->
                 let x = 
                   if x = 0 
                   then 1 
                   else x in (* Relax when x = 0 *)
                 if  len >= x 
                 then
                   let first_part, continue =  (Ext_list.take x args) in
                   aux
                     (E.call ~info:{arity=Full; call_info = Call_ml} acc first_part)
                     (Determin (a, rest, b))
                     continue (len - x)
                 else  acc 
               (* alpha conversion now? --
                  Since we did an alpha conversion before so it is not here
               *)
               | Determin (a, [], b ), _ ->
                 (* can not happen, unless it's an exception ? *)
                 E.call ~info:Js_call_info.dummy acc args
               | NA, _ ->
                 E.call ~info:Js_call_info.dummy acc args
             in
             aux (E.ml_var_dot id name) arity args (List.length args ))
      )

and  compile_let flag (cxt : Lam_compile_defs.cxt) id (arg : Lambda.lambda) : Js_output.t =


  match flag, arg  with 
  |  let_kind, _  -> 
    compile_lambda {cxt with st = Declare (let_kind, id); should_return = False } arg 

and compile_recursive_let (cxt : Lam_compile_defs.cxt) (id : Ident.t) (arg : Lambda.lambda)   = 
  match arg with 
  |  Lfunction (kind, params, body)  -> 
    (* Invariant:  jmp_table can not across function boundary,
       here we share env *)

    let continue_label = Lam_util.generate_label ~name:id.name () in
    (* TODO: Think about recursive value 
       {[
         let rec v = ref (fun _ ... 
                         )
       ]}
        [Alias] may not be exact 
    *)
    Js_output.handle_name_tail (Declare (Alias, id)) False arg
      (
        let ret : Lam_compile_defs.return_label = 
          {id; 
           label = continue_label; 
           params;
           immutable_mask = Array.make (List.length params) true;
           new_params = Ident_map.empty;
           triggered = false} in
        let output = 
          compile_lambda
            { cxt with 
              st = EffectCall;  
              should_return = True (Some ret );
              jmp_table = Lam_compile_defs.empty_handler_map}  body in
        if ret.triggered then 
          let body_block = Js_output.to_block output in
          E.fun_ (* TODO:  save computation of length several times *)
            ~immutable_mask:ret.immutable_mask
            (List.map (fun x -> 
                 try Ident_map.find x ret.new_params with  Not_found -> x)
                params)
            [
              S.while_ (* ~label:continue_label *)
                E.caml_true   
                (
                  Ident_map.fold
                    (fun old new_param  acc ->
                       S.define ~kind:Alias old (E.var new_param) :: acc) 
                    ret.new_params body_block
                )
            ]

        else            (* TODO:  save computation of length several times *)
          E.fun_ params (Js_output.to_block output )
      ), [] 
  | (Lprim(Pmakeblock _ , _) )  -> 
    (* Lconst should not appear here if we do [scc]
       optimization, since it's faked recursive value,
       however it would affect scope issues, we have to declare it first 
    *)
    (* Ext_log.err "@[recursive value %s/%d@]@." id.name id.stamp; *)
    begin
      match compile_lambda {cxt with st = NeedValue; should_return = False } arg with
      | { block = b; value = Some v} -> 
        (* TODO: check recursive value .. 
            could be improved for simple cases
        *)
        Js_output.of_block  (
          b  @ [S.exp(E.runtime_call Js_config.obj_runtime "caml_update_dummy" [ E.var id;  v])]),
        [id]
      (* S.define ~kind:Variable id (E.arr Mutable [])::  *)
      | _ -> assert false 
    end
  | Lvar _   ->
    compile_lambda {cxt with st = Declare (Alias ,id); should_return = False } arg, []
  | _ -> 
    (* pathological case:
        fail to capture taill call?
       {[ let rec a = 
         if  g > 30 then .. fun () -> a ()
       ]}

        Neither  below is not allowed in ocaml:
       {[
         let rec v = 
           if sum 0 10 > 20 then 
             1::v 
           else 2:: v
       ]}
       {[
         let rec v = 
           if sum 0 10 > 20 then 
             fun _ -> print_endline "hi"; v ()
           else 
             fun _-> print_endline "hey"; v ()
       ]}
    *)
    compile_lambda {cxt with st = Declare (Alias ,id); should_return = False } arg, []

and compile_recursive_lets cxt id_args : Js_output.t = 
  let output_code, ids  = List.fold_right
      (fun (ident,arg) (acc, ids) -> 
         let code, declare_ids  = compile_recursive_let cxt ident arg in
         (code ++ acc, declare_ids @ ids )
      )  id_args (Js_output.dummy, [])
  in
  match ids with 
  | [] -> output_code
  | _ ->  
    (Js_output.of_block  @@
     List.map (fun id -> S.define ~kind:Variable id (E.dummy_obj ())) ids ) 
    ++  output_code

and compile_general_cases : 
  'a . 
  ('a -> J.expression) ->
  (J.expression -> J.expression -> J.expression) -> 
  Lam_compile_defs.cxt -> 
  (?default:J.block ->
   ?declaration:Lambda.let_kind * Ident.t  -> 
   _ -> 'a J.case_clause list ->  J.statement) -> 
  _ -> 
  ('a * Lambda.lambda) list -> default_case -> J.block 
  = fun f eq cxt switch v table default -> 
    let wrap (cxt : Lam_compile_defs.cxt) k =
      let cxt, define =
        match cxt.st with 
        | Declare (kind, did)
          -> 
          {cxt with st = Assign did}, Some (kind,did)
        | _ -> cxt, None
      in
      k cxt  define 
    in
    match table, default with 
    | [], Default lam ->  
      Js_output.to_block  (compile_lambda cxt lam)
    | [], (Complete | NonComplete) ->  []
    | [(id,lam)],Complete -> 
      (* To take advantage of such optimizations, 
          when we generate code using switch, 
          we should always have a default,
          otherwise the compiler engine would think that 
          it's also complete
      *)
      Js_output.to_block @@ compile_lambda cxt lam 
    | [(id,lam)], NonComplete 
      ->
      wrap cxt @@ fun cxt define  ->
      [S.if_ ?declaration:define (eq v (f id) )
         (Js_output.to_block @@ compile_lambda cxt lam )]

    | ([(id,lam)], Default x) | ([(id,lam); (_,x)], Complete)
      ->
      wrap cxt  @@ fun cxt define -> 
      let else_block = Js_output.to_block (compile_lambda cxt x) in
      let then_block = Js_output.to_block (compile_lambda cxt lam)  in
      [ S.if_ ?declaration:define (eq v (f id) )
          then_block
          ~else_:else_block
      ]
    | _ , _ -> 
      (* TODO: this is not relevant to switch case
          however, in a subset of switch-case if we can analysis 
          its branch are the same, we can propogate which 
          might encourage better inlining strategey
          ---
          TODO: grouping can be delayed untile JS IR
      *)
      (*TOOD: disabled temporarily since it's not perfect yet *)
      wrap cxt @@ fun cxt declaration  ->
      let default =
        match default with
        | Complete -> None
        | NonComplete -> None
        | Default lam -> Some (Js_output.to_block  (compile_lambda cxt lam))
      in
      let body = 
        table 
        |> Ext_list.stable_group (fun (_,lam) (_,lam1) -> Lam_analysis.eq_lambda lam lam1)
        |> Ext_list.flat_map 
          (fun group -> 
             group 
             |> Ext_list.map_last 
               (fun last (x,lam) -> 
                  if last 
                  then {J.case =  x; body = Js_output.to_break_block (compile_lambda cxt lam) }
                  else { case = x; body = [],false }))
          (* TODO: we should also group default *)
          (* The last clause does not need [break]
              common break through, *)

      in
      [switch ?default ?declaration v body] 

and compile_cases cxt = compile_general_cases (fun x -> E.small_int  x) E.int_equal cxt 
    (fun  ?default ?declaration e clauses    -> S.int_switch ?default  ?declaration e clauses)

and compile_string_cases cxt = compile_general_cases E.str E.string_equal cxt 
    (fun  ?default ?declaration e clauses    -> S.string_switch ?default  ?declaration e clauses)
(* TODO: optional arguments are not good 
    for high order currying *)
and
  compile_lambda
    ({st ; should_return; jmp_table; meta = {env ; _} } as cxt : Lam_compile_defs.cxt)
    (lam : Lambda.lambda)  : Js_output.t  =
  begin
    match lam with 
    | Lfunction(kind, params, body) ->
      Js_output.handle_name_tail st should_return lam 
        (E.fun_
           params
           (* Invariant:  jmp_table can not across function boundary,
              here we share env
           *)
           (Js_output.to_block 
              ( compile_lambda
                  { cxt with st = EffectCall;  
                             should_return = True None; (* Refine*)
                             jmp_table = Lam_compile_defs.empty_handler_map}  body)))


    | Lapply(
        Lapply(an, args', ({apply_status = NA} as _info1)),
        args, 
        ({apply_status = NA} as _info2))
      ->    
      (* After inlining we can generate such code, 
         see {!Ari_regress_test}         
      *)      
      compile_lambda  cxt  
        (Lapply (an, (args' @ args), (Lam_util.mk_apply_info NA)))
    (* External function calll *)
    | Lapply(Lprim(Pfield (n,_), [ Lprim(Pgetglobal id,[])]), args_lambda,_info) ->

      get_exp_with_args cxt lam  args_lambda id n  env


    | Lapply(fn,args_lambda,  info) -> 
      (* TODO: --- 
         1. check arity, can be simplified for pure expression
         2. no need create names
      *)
      begin 
        let [@warning "-8" (* non-exhaustive pattern*)] (args_code, fn_code:: args) = 
          List.fold_right (fun (x : Lambda.lambda) (args_code, fn_code )-> 
              match x with             
              | Lprim (Pgetglobal ident, []) -> 
                (* when module is passed as an argument - unpack to an array
                    for the function, generative module or functor can be a function, 
                    however it can not be global -- global can only module 
                *)
                args_code, Lam_compile_global.get_exp  (ident, env,true) :: fn_code
              | _ ->
                begin
                  match compile_lambda 
                          {cxt with st = NeedValue ; should_return =  False} x with
                  | {block = a; value =  Some b} -> a @ args_code , b:: fn_code 
                  | _ -> assert false
                end
            ) (fn::args_lambda) ([],[]) in


        begin
          match fn, should_return with
          | (Lvar id',
             True (Some ({id;label; params; _} as ret))) when Ident.same id id' ->


            (* Ext_log.err "@[ %s : %a tailcall @]@."  cxt.meta.filename Ident.print id; *)
            ret.triggered <- true;
            (* Here we mark [finished] true, since the continuation 
                does not make sense any more (due to that we have [continue])
                TODO: [finished] is not a meaningful name, we should use [truncate] 
                to mean the following statement should be truncated
            *)
            (* 
                actually, there is no easy way to determin 
                if the argument depends on an expresion, since 
                it can be a function, then it may depend on anything
                http://caml.inria.fr/pub/ml-archives/caml-list/2005/02/5727b4ecaaef6a7a350c9d98f5f68432.en.html
                http://caml.inria.fr/pub/ml-archives/caml-list/2005/02/fe9bc4e23e6dc8c932c8ab34240ff195.en.html

            *)
            (* TODO: use [fold]*)            
            let block =  args_code @
                        (
                          let (_,assigned_params,new_params) = 
                            List.fold_left2 (fun (i,assigns,new_params) param (arg : J.expression) ->
                                match arg with
                                | {expression_desc = Var (Id x); _} when Ident.same x param ->
                                  (i + 1, assigns, new_params)
                                | _ ->
                                  let new_param, m  = 
                                    match Ident_map.find  param ret.new_params with 
                                    | exception Not_found -> 
                                      ret.immutable_mask.(i)<- false;
                                      let v = Ext_ident.create ("_"^param.Ident.name) in
                                      v, (Ident_map.add param v new_params) 
                                    | v -> v, new_params  in
                                  (i+1, (new_param, arg) :: assigns, m)
                              ) (0, [], Ident_map.empty) params args  in 
                          let () = ret.new_params <- Ident_map.(merge_disjoint new_params ret.new_params) in
                          assigned_params |> List.map (fun (param, arg) -> S.assign param arg))
                         @
                        [S.continue ()(* label *)]
                         (* Note true and continue needed to be handled together*)
            in
            begin
              (* Ext_log.dwarn __LOC__ "size : %d" (List.length block); *)
              Js_output.of_block  ~finished:True block 
            end

 

          (* match assigned_params with *)
          (* | [] ->  [] *)
          (* | [param,arg] -> [S.assign param arg ] *)
          (* | _ -> *)
          (*    let arg_map = Ident_map.of_list assigned_params in *)
          (*    match Lam_util.sort_dag_args arg_map with *)
          (*    | Some args -> *)
          (*        List.map (fun a -> S.assign a (Ident_map.find a arg_map )) args *)
          (*    | None -> *)
          (*        let renamed_params_args = *)
          (*          assigned_params |> *)
          (*          List.map (fun (param, arg) -> (param, Ident.rename param, arg )) *)
          (*        in *)
          (*        List.map (fun (param, param2, arg) -> *)
          (*          S.declare param2 arg *)
          (*                 ) renamed_params_args *)
          (*        @ *)
          (*          List.map (fun (param, param2, _) -> *)
          (*            S.assign param (E.var param2) *)
          (*                   )  renamed_params_args *)
          (*  Js_output.handle_block_return st should_return lam   *)
          (* (E.call fn_code args)  *)
          | _ -> 

            Js_output.handle_block_return st should_return lam args_code 
              (E.call ~info:(match fn, info with 
                   | _, { apply_status = Full} -> {arity = Full ; call_info = Call_ml}
                   | _,  { apply_status = NA} -> {arity = NA; call_info = Call_ml } ) fn_code args) 
        end;
      end


    | Llet (let_kind,id,arg, body) ->
      (* Order matters..  see comment below in [Lletrec] *)
      let args_code =
        compile_let  let_kind cxt id arg  in 
      args_code ++
      compile_lambda  cxt  body

    | Lletrec (id_args, body) -> 
      (* There is a bug in our current design, 
         it requires compile args first (register that some objects are jsidentifiers)
         and compile body wiht such effect.
         So here we should compile [id_args] first, then [body] later.
         Note it has some side effect over cache number as well, mostly the value of
         [Caml_primitive["caml_get_public_method"](x,hash_tab, number)]

         To fix this, 
         1. scan the lambda layer first, register js identifier before proceeding
         2. delay the method call into javascript ast
      *)
      let v =  compile_recursive_lets cxt  id_args in v ++ compile_lambda cxt  body

    | Lvar id -> Js_output.handle_name_tail st  should_return lam (E.var id )
    | Lconst c -> 
      Js_output.handle_name_tail st should_return lam (Lam_compile_const.translate c)

    | Lprim(Pfield (n,_), [ Lprim(Pgetglobal id,[])]) -> (* should be before Pgetglobal *)
        get_exp_with_index cxt lam  (id,n, env)

    | Lprim(Praise _raise_kind, [ e ]) -> 
      begin
        match compile_lambda {
            cxt with should_return = False; st = NeedValue} e with 
        | {block = b; value =  Some v} -> 

          Js_output.make (b @ [S.throw v])
            ~value:E.undefined ~finished:True
        (* FIXME -- breaks invariant when NeedValue, reason is that js [throw] is statement 
           while ocaml it's an expression, we should remove such things in lambda optimizations
        *)
        | {value =  None; _} -> assert false 
      end
    | Lprim(Psequand , [l;r] )
      ->
      begin match cxt with 
        | {should_return = True _ } 
          (* Invariant: if [should_return], then [st] will not be [NeedValue] *)
          ->
          compile_lambda cxt (Lifthenelse (l, r, Lam_util.lam_false))
        | _ -> 
          let l_block,l_expr = 
            match compile_lambda {cxt with st = NeedValue; should_return = False} l with 
            | {block = a; value = Some b} -> a, b
            | _ -> assert false 
          in
          let r_block, r_expr = 
            match compile_lambda {cxt with st = NeedValue; should_return = False} r with
            | {block = a; value = Some b} -> a, b
            | _ -> assert false 
          in
          let args_code =  l_block @ r_block  in
          let exp =  E.and_ l_expr r_expr  in
          Js_output.handle_block_return st should_return lam args_code exp           
      end

    | Lprim(Psequor, [l;r])
      ->
      begin match cxt with
        | {should_return = True _ }
          (* Invariant: if [should_return], then [st] will not be [NeedValue] *)
          ->
          compile_lambda cxt (Lifthenelse (l, Lam_util.lam_true, r))
        | _ ->
          let l_block,l_expr =
            match compile_lambda {cxt with st = NeedValue; should_return = False} l with
            | {block = a; value = Some b} -> a, b
            | _ -> assert false
          in
          let r_block, r_expr =
            match compile_lambda {cxt with st = NeedValue; should_return = False} r with
            | {block = a; value = Some b} -> a, b
            | _ -> assert false
          in
          let args_code =  l_block @ r_block  in
          let exp =  E.or_ l_expr r_expr  in
          Js_output.handle_block_return st should_return lam args_code exp
      end
    | Lprim (Pccall {prim_name = 
                       (
                         "js_fn_mk_00" 
                       | "js_fn_mk_01"
                       | "js_fn_mk_02"
                       | "js_fn_mk_03"
                       | "js_fn_mk_04"
                       | "js_fn_mk_05"
                       | "js_fn_mk_06"
                       | "js_fn_mk_07"
                       | "js_fn_mk_08"
                       | "js_fn_mk_09"
                         as name )
                    }, [fn])
      -> 
      let arity = Ext_string.digits_of_str ~offset:9 (* String.length "js_fn_mk_" *) name 2   in
      begin match fn with
      | Lambda.Lfunction(kind,args, body) 
        ->
        let len =  List.length args in 
        if len = arity then
          compile_lambda cxt fn 
        else if len > arity then 
          let first, rest  = Ext_list.take arity args  in 
          compile_lambda cxt (Lambda.Lfunction (kind, first, Lambda.Lfunction (kind, rest, body)))
        else 
          compile_lambda cxt (Lam_util.eta_conversion arity Lam_util.default_apply_info  fn  [] )
          (* let extra_args = Ext_list.init (arity - len) (fun _ ->   (Ident.create Literals.param)) in *)
          (* let extra_lambdas = List.map (fun x -> Lambda.Lvar x) extra_args in *)
          (* Lambda.Lfunction (kind, extra_args @ args , body ) *)
      (*TODO: can be optimized ?
        {[\ x y -> (\u -> body x) x y]}
        {[\u x -> body x]}        
        rewrite rules 
        {[
          \x -> body 
                --
                \y (\x -> body ) y 
        ]}
        {[\ x y -> (\a b c -> g a b c) x y]}
        {[ \a b -> \c -> g a b c ]}
      *)
      | _ -> 
        compile_lambda cxt (Lam_util.eta_conversion arity Lam_util.default_apply_info  fn  [] )
      end
    (* TODO: 
       check the arity of fn before wrapping it 
       we need mark something that such eta-conversion can not be simplified in some cases 
    *)
    | Lprim (Pccall{prim_name = "js_debugger"; _}, 
             _) 
      -> 
      (* [%bs.debugger] guarantees that the expression does not matter 
         TODO: make it even safer
         *)
      Js_output.handle_block_return st should_return lam  [S.debugger] E.unit 
    | Lprim (prim, args_lambda)  ->
      begin
        let args_block, args_expr =
          args_lambda 
          |> List.map (fun (x : Lambda.lambda) ->
              match compile_lambda {cxt with st = NeedValue; should_return = False} x 
              with 
              | {block = a; value = Some b} -> a,b
              | _ -> assert false )
          |> List.split 
        in
        let args_code  = List.concat args_block in
        let exp  =  (* TODO: all can be done in [compile_primitive] *)
          Lam_compile_primitive.translate cxt prim args_expr in
        Js_output.handle_block_return st should_return lam args_code exp 
      end
    | Lsequence (l1,l2) ->
      let output_l1 = 
        compile_lambda {cxt with st = EffectCall; should_return =  False} l1 in
      let output_l2 = 
        compile_lambda cxt l2  in
      let result = output_l1 ++ output_l2  in
      (* let () = *)
      (*   Ext_log.dwarn __LOC__ *)
      (*     "@ @[l1:%a@ js-l1(%d):%s@ l2:@ %a@ js-l2(%d):%s@ js-l:@ %s@]" *)
      (*     Printlambda.lambda l1 (List.length output_l1.block) (Js_output.to_string output_l1) *)
      (*     Printlambda.lambda l2 (List.length output_l2.block) (Js_output.to_string output_l2) *)
      (*     (Js_output.to_string result ) in  *)
      result      

    (* begin
       match cxt.st, cxt.should_return with  *)
    (* | NeedValue, False  ->  *)
    (*     Js_output.append_need_value output_l1 output_l2  *)
    (* | _ -> output_l1 ++ output_l2  *)
    (* end *)

    | Lifthenelse(p,t_br,f_br) ->
      (*
         This should be optimized in lambda layer 
         (let (match/1038 = (apply g/1027 x/1028))
         (catch
         (stringswitch match/1038
         case "aabb": 0
         case "bbc": 1
         default: (exit 1))
         with (1) 2))
      *)
      begin 
        match compile_lambda {cxt with st = NeedValue ; should_return = False } p with 
        | {block = b; value =  Some e} ->
          (match st, should_return, 
                 compile_lambda {cxt with st= NeedValue}  t_br, 
                 compile_lambda {cxt with st= NeedValue}  f_br with 
          | NeedValue, _, 
            {block = []; value =  Some out1}, 
            {block = []; value =  Some out2} -> (* speical optimization *)
            Js_output.make b ~value:(E.econd e out1 out2)
          | NeedValue, _, _, _  -> 
            (* we can not reuse -- here we need they have the same name, 
                   TODO: could be optimized by inspecting assigment statement *)
            let id = Ext_ident.gen_js () in
            (match
               compile_lambda  {cxt with st = Assign id} t_br,
               compile_lambda {cxt with st = Assign id} f_br
             with
             | out1 , out2 -> 
               Js_output.make 
                 (S.declare_variable ~kind:Variable id :: b @ [
                     S.if_ e 
                       (Js_output.to_block out1) 
                       ~else_:(Js_output.to_block out2 )
                   ])
                 ~value:(E.var id)
            )

          | Declare (kind,id), _, 
            {block = []; value =  Some out1},
            {block = []; value =  Some out2} ->  
            (* Invariant: should_return is false*)
            Js_output.make [
              S.define ~kind id (E.econd e out1 out2) ]
          | Declare (kind, id), _, _, _ ->
            Js_output.make 
              ( b @ [
                   S.if_ ~declaration:(kind,id) e 
                     (Js_output.to_block @@ 
                      compile_lambda {cxt with st = Assign id}  t_br)
                     ~else_:(Js_output.to_block @@  
                             (compile_lambda {cxt with st = Assign id} f_br))
                 ])

          | Assign id, _ , 
            {block = []; value =  Some out1}, 
            {block = []; value =  Some out2} ->  
            (* Invariant:  should_return is false *)
            Js_output.make [S.assign id (E.econd e out1 out2)]
          | EffectCall, True _ , 
            {block = []; value =  Some out1}, 
            {block = []; value =  Some out2} ->
            Js_output.make [S.return  (E.econd e  out1 out2)] ~finished:True
              (* see PR#83 *)
          | EffectCall, False , {block = []; value =  Some out1}, 
            {block = []; value =  Some out2} ->
            begin
              match Js_exp_make.extract_non_pure out1 ,
                    Js_exp_make.extract_non_pure out2 with
              | None, None -> Js_output.make b
              | Some out1, Some out2 -> 
                Js_output.make b  ~value:(E.econd e  out1 out2)
              | Some out1, None -> 
                Js_output.make (b @ [S.if_ e  [S.exp out1]])
              | None, Some out2 -> 
                Js_output.make @@
                b @ [S.if_ (E.not e)
                       [S.exp out2]
                    ]
            end
          | EffectCall , False , {block = []; value = Some out1}, _ -> 
            (* assert branch 
                TODO: here we re-compile two branches since
                its context is different -- could be improved
            *)
            if Js_analyzer.no_side_effect_expression out1 then 
              Js_output.make
                (b @[ S.if_ (E.not e)
                        (Js_output.to_block @@
                         (compile_lambda cxt f_br))])
            else 
              Js_output.make 
                (b @[S.if_ e 
                       (Js_output.to_block 
                        @@ compile_lambda cxt t_br)
                       ~else_:(Js_output.to_block @@  
                               (compile_lambda cxt f_br))]
                )

          | EffectCall , False , _, {block = []; value = Some out2} -> 
            let else_ = 
              if  Js_analyzer.no_side_effect_expression out2 then  
                None 
              else 
                Some (
                  Js_output.to_block @@
                  compile_lambda cxt f_br) in 
            Js_output.make 
              (b @[S.if_ e 
                     (Js_output.to_block @@
                      compile_lambda cxt t_br)
                     ?else_])


          | (Assign _ | EffectCall), _, _, _  ->
            let then_output = 
              Js_output.to_block @@ 
              (compile_lambda cxt  t_br) in
            let else_output = 
              Js_output.to_block @@ 
              (compile_lambda cxt f_br) in
            Js_output.make (b @ [
                S.if_ e 
                  then_output
                  ~else_:else_output
              ]))
        | _ -> assert false 
      end
    | Lstringswitch(l, cases, default) -> 

      (* TODO might better optimization according to the number of cases  
          Be careful: we should avoid multiple evaluation of l,
          The [gen] can be elimiated when number of [cases] is less than 3
      *)
      begin
        match compile_lambda {cxt with should_return = False ; st = NeedValue} l 
        with
        | {block ; value =  Some e}  -> 
          (* when should_return is true -- it's passed down 
             otherwise it's ok *)
          let default = 
            match default with 
            | Some x -> Default x 
            | None -> Complete in
          begin
            match st with 
            (* TODO: can be avoided when cases are less than 3 *)
            | NeedValue -> 
              let v = Ext_ident.gen_js () in 
              Js_output.make (block @ 
                              compile_string_cases 
                                {cxt with st = Declare (Variable, v)}
                                e cases default) ~value:(E.var v)
            | _ -> 
              Js_output.make (block @ compile_string_cases  cxt e cases default)  end

        | _ -> assert false 
      end
    | Lswitch(lam,
              {sw_numconsts; 
               sw_consts;
               sw_numblocks;
               sw_blocks;
               sw_failaction = default }) 
      -> 
      (* TODO: if default is None, we can do some optimizations
          Use switch vs if/then/else

          TODO: switch based optimiztion - hash, group, or using array,
                also if last statement is throw -- should we drop remaining
                statement?
      *)
      let default : default_case  = 
        match default with 
        | None -> Complete 
        | Some x -> Default x in
      let compile_whole  ({st; _} as cxt  : Lam_compile_defs.cxt ) =
        begin
          match sw_numconsts, sw_numblocks, 
                compile_lambda {cxt with should_return = False; st = NeedValue}
                  lam with 
          | 0 , _ , {block; value =  Some e}  ->
            compile_cases cxt (E.tag e )  sw_blocks default
          | _, 0, {block; value =  Some e} ->  
            compile_cases cxt e  sw_consts default
          | _, _,  { block; value =  Some e} -> (* [e] will be used twice  *)
            let dispatch e = 
              [
                S.if_ 
                  (E.is_type_number e )
                  (compile_cases cxt e sw_consts default)
                  (* default still needed, could simplified*)
                  ~else_:(
                    (compile_cases  cxt (E.tag e ) sw_blocks default ))] in 
            begin
              match e.expression_desc with 
              | J.Var _  -> dispatch e  
              | _ -> 
                let v = Ext_ident.gen_js () in  
                (* Necessary avoid duplicated computation*)
                (S.define ~kind:Variable v e ) ::  dispatch (E.var v)
            end
          | _, _, {value =  None; _}  -> assert false 
        end in
      begin
        match st with  (* Needs declare first *)
        | NeedValue -> 
          (* Necessary since switch is a statement, we need they return 
             the same value for different branches -- can be optmized 
             when branches are minimial (less than 2)
          *)
          let v = Ext_ident.gen_js () in
          Js_output.make (S.declare_variable ~kind:Variable v   :: compile_whole {cxt with st = Assign v})
            ~value:(E.var  v)

        | Declare (kind,id) -> 
          Js_output.make (S.declare_variable ~kind id
                          :: compile_whole {cxt with st = Assign id} )
        | EffectCall | Assign _  -> Js_output.make (compile_whole cxt)
      end

    | Lstaticraise(i, largs) ->  (* TODO handlding *largs*)
      (* [i] is the jump table, [largs] is the arguments passed to [Lstaticcatch]*)
      begin
        match Lam_compile_defs.HandlerMap.find i cxt.jmp_table  with 
        | {exit_id; args ; order_id} -> 
          let args_code  =
            (Js_output.concat @@ List.map2 (
                fun (x : Lambda.lambda) (arg : Ident.t) ->
                  match x with
                  | Lvar id -> 
                    Js_output.make [S.assign arg (E.var id)]

                  | _ -> (* TODO: should be Assign -- Assign is an optimization *)
                    compile_lambda {cxt with st = Assign arg ; should_return =  False} x 
              ) largs (args : Ident.t list)) 
          in
          args_code ++ (* Declared in [Lstaticraise ]*)
          Js_output.make [S.assign exit_id (E.small_int  order_id)]
            ~value:E.undefined
        | exception Not_found ->
          Js_output.make [S.unknown_lambda ~comment:"error" lam]
          (* staticraise is always enclosed by catch  *)
      end
    (* Invariant: code can not be reused 
        (catch l with (32)
        (handler))
        32 should not be used in another catch
        Assumption: 
        This is true in current ocaml compiler
        currently exit only appears in should_return position relative to staticcatch
        if not we should use ``javascript break`` or ``continue``
    *)
    | Lstaticcatch _  -> 
      let code_table, body =  flatten_caches lam in

      let exit_id =   Ext_ident.gen_js ~name:"exit" () in
      let exit_expr = E.var exit_id in
      let bindings = Ext_list.flat_map (fun (_,_,bindings) -> bindings) code_table in

      (* compile_list name l false (\*\) *)
      (* if exit_code_id == code 
         handler -- ids are not useful, since 
         when compiling `largs` we will do the binding there
         - when exit_code is undefined internally, 
           it should PRESERVE  ``tail`` property
         - if it uses `staticraise` only once 
           or handler is minimal, we can inline
         - always inline also seems to be ok, but it might bloat the code
         - another common scenario is that we have nested catch
           (catch (catch (catch ..))
      *)
      (*
        checkout example {!Digest.file}, you can not inline handler there, 
        we can spot such patten and use finally there?
        {[
        let file filename =
           let ic = open_in_bin filename in
           match channel ic (-1) with
           | d -> close_in ic; d
           | exception e -> close_in ic; raise e

        ]}
       *)
      (* TODO: handle NeedValue *)
      let jmp_table, handlers =  Lam_compile_defs.add_jmps (exit_id, code_table) jmp_table in

      (* Declaration First, body and handler have the same value *)
      (* There is a bug in google closure compiler:
            https://github.com/google/closure-compiler/issues/1234#issuecomment-151976340 
            TODO: wait for a bug fix
        *)
      let declares = 
        S.define ~kind:Variable exit_id
          E.zero_int_literal :: 
        (* we should always make it zero here, since [zero] is reserved in our mapping*)
        List.map (fun x -> S.declare_variable ~kind:Variable x ) bindings in

      begin match  st with 
         (* could be optimized when cases are less than 3 *)
         | NeedValue -> 
           let v = Ext_ident.gen_js  () in 
           let lbody = compile_lambda {cxt with 
                                       jmp_table = jmp_table;
                                       st = Assign v
                                      } body in
           Js_output.make  (S.declare_variable ~kind:Variable v  :: declares) ++ 
           lbody ++ Js_output.make (
             compile_cases 
               {cxt with st = Assign v;
                         jmp_table = jmp_table} 
               exit_expr handlers  NonComplete)  ~value:(E.var v )
         | Declare (kind, id)
         (* declare first this we will do branching*) ->
           let declares = 
             S.declare_variable ~kind id  :: declares in   
           let lbody = compile_lambda {cxt with jmp_table = jmp_table; st = Assign id } body in
           Js_output.make  declares ++ 
           lbody ++ 
           Js_output.make (compile_cases 
                             {cxt with jmp_table = jmp_table; st = Assign id} 
                             exit_expr 
                             handlers
                             NonComplete
                             (* place holder -- tell the compiler that 
                                we don't know if it's complete
                             *)
                          )
         | EffectCall | Assign _  -> 
           let lbody = compile_lambda {cxt with jmp_table = jmp_table } body in
           Js_output.make declares ++
           lbody ++
           Js_output.make (compile_cases
                             {cxt with jmp_table = jmp_table}
                             exit_expr
                             handlers
                             NonComplete)
      end
    | Lwhile(p,body) ->  
      (* Note that ``J.While(expression * statement )``
            idealy if ocaml expression does not need fresh variables, we can generate
            while expression, here we generate for statement, leave optimization later. 
            (Sine OCaml expression can be really complex..)
      *)
      (match compile_lambda {cxt with st = NeedValue; should_return = False } p 
       with 
       | {block; value =  Some e} -> 
         (* st = NeedValue -- this should be optimized and never happen *)
         let e = 
           match block with
           | [] -> e 
           | _ -> E.of_block block e  in
         let block = 
           [
             S.while_
               e
               (Js_output.to_block @@ 
                compile_lambda 
                  {cxt with st = EffectCall; should_return = False}
                  body)
           ] in

         begin
           match st, should_return  with 
           | Declare (_kind, x), _  ->  (* FIXME _kind not used *)
             Js_output.make (block @ [S.declare_unit x ])
           | Assign x, _  ->
             Js_output.make (block @ [S.assign_unit x ])
           | EffectCall, True _  -> 
             Js_output.make (block @ [S.return_unit ()]) ~finished:True
           | EffectCall, _ -> Js_output.make block
           | NeedValue, _ -> Js_output.make block ~value:E.unit end
       | _ -> assert false )

    | Lfor (id,start,finish,direction,body) -> 
      (* all non-tail *)
      (* TODO: check semantics should start, finish be executed each time in both 
           ocaml and js?, also check evaluation order..
           in ocaml id is not in the scope of finish, so it should be safe here

           for i  = 0 to (print_int 3; 10) do print_int i done;;
           3012345678910- : unit = ()

         for(var i =  0 ; i < (console.log(i),10); ++i){console.log('hi')}
         print i each time, so they are different semantics...
      *)

      let block =
        begin
          match compile_lambda {cxt with st = NeedValue; should_return = False} start,
                compile_lambda {cxt with st = NeedValue; should_return = False} finish with 
          | {block = b1; value =  Some e1}, {block = b2; value =  Some e2} -> 

            (* order b1 -- (e1 -- b2 -- e2) 
                in most cases we can shift it into such scenarios
                b1, b2, [e1, e2]
                - b2 is Empty
                - e1 is pure
                we can guarantee e1 is pure, if it literally contains a side effect call,
                put it in the beginning


            *)
            begin 
              match b1,b2 with
              | _,[] -> 
                b1 @  [S.for_ (Some e1) e2  id direction 
                         (Js_output.to_block @@ 
                          compile_lambda {cxt with should_return = False ; st = EffectCall}
                            body) ]
              | _, _ when Js_analyzer.no_side_effect_expression e1 
                (* 
                     e1 > b2 > e2
                     re-order 
                     b2 > e1 > e2
                   *)
                -> 
                b1 @ b2 @ [S.for_ (Some e1) e2  id direction 
                             (Js_output.to_block @@ 
                              compile_lambda {cxt with should_return = False ; st = EffectCall}
                                body) ]
              | _ , _
                -> 
                (*       let b2, e2 =  *)
                (*   (\* e2 is of type [int]*\) *)
                (*   match e2.expression_desc with *)
                (*   | Number v  -> b2, J.Const v *)
                (*   | Var v -> b2, J.Finish v *)

                (*   | Array_length e  *)
                (*   | Bytes_length e  *)
                (*   | Function_length e  *)
                (*   | String_length e  *)
                (*     ->  *)
                (*       let len = Ext_ident.create "_length" in *)
                (*       b2 @ [ S.alias_variable len ~exp:e2 ],  J.Finish (Id len ) *)
                (*   | _ ->  *)
                (*       (\* TODO: guess a better name when possible*\) *)
                (*       let len = Ext_ident.create "_finish" in *)
                (*       b2 @ [S.alias_variable len ~exp:e2],  J.Finish (Id len) *)
                (* in  *)

                b1 @ (S.define ~kind:Variable id e1 :: b2 ) @ ([
                    S.for_ None e2 id direction 
                      (Js_output.to_block @@ 
                       compile_lambda {cxt with should_return = False ; st = EffectCall}
                         body) 
                  ])

            end


          | _ -> assert false end in
      begin
        match st, should_return with 
        | EffectCall, False  -> Js_output.make block
        | EffectCall, True _  -> 
          Js_output.make (block @ [S.return_unit()]) ~finished:True
        (* unit -> 0, order does not matter *)
        | (Declare _ | Assign _), True _ -> Js_output.make [S.unknown_lambda lam]
        | Declare (_kind, x), False  ->   
          (* FIXME _kind unused *)
          Js_output.make (block @   [S.declare_unit x ])
        | Assign x, False  -> Js_output.make (block @ [S.assign_unit x ])
        | NeedValue, _ 
          ->  
          Js_output.make block ~value:E.unit
          (* TODO: fixme, here it's ok*)
      end
    | Lassign(id,lambda) -> 
      let block = 
        match lambda with
        | Lprim(Poffsetint  v, [Lvar id']) when Ident.same id id' ->
          [ S.exp (E.assign (E.var id) 
                     (E.int32_add (E.var id) (E.small_int  v)))
          ]
        | _ ->
          begin 
            match compile_lambda {cxt with st = NeedValue; should_return = False} lambda with 
            | {block = b; value =  Some v}  -> 
              (b @ [S.assign id v ])
            | _ -> assert false  
          end
      in
      begin
        match st, should_return with 
        | EffectCall, False -> Js_output.make block
        | EffectCall, True _ -> 
          Js_output.make (block @ [S.return_unit ()]) ~finished:True
        | (Declare _ | Assign _ ) , True _ -> 
          Js_output.make [S.unknown_lambda lam]
        (* bound by a name, while in a tail position, this can not happen  *)
        | Declare (_kind, x) , False ->
          (* FIXME: unused *)
          Js_output.make (block @ [ S.declare_unit x ])
        | Assign x, False  -> Js_output.make (block @ [S.assign_unit x ])
        | NeedValue, _ -> 
          Js_output.make block ~value:E.unit
      end
    | Ltrywith(lam,id, catch) ->  (* generate documentation *)
      (* 
         tail --> should be renamed to `shouldReturn`  
          in most cases ``shouldReturn`` == ``tail``, however, here is not, 
          should return, but it is not a tail call in js
          (* could be optimized using javascript style exceptions *)
         {[
           {try
              {var $js=g(x);}
                catch(exn){if(exn=Not_found){var $js=0;}else{throw exn;}}
           return h($js);
         }
         ]}
      *)
      let aux st = 
        (* should_return is passed down *)
        [ S.try_ 
            (Js_output.to_block (compile_lambda {cxt with st = st} lam))
            ~with_:(id, 
                    Js_output.to_block @@ 
                    compile_lambda {cxt with st = st} catch )

        ] in 

      begin
        match st with 
        | NeedValue -> 
          let v = Ext_ident.gen_js () in
          Js_output.make (S.declare_variable ~kind:Variable v :: aux (Assign v))  ~value:(E.var v )
        | Declare (kind,  id) -> 
          Js_output.make (S.declare_variable ~kind
                            id :: aux (Assign id))
        | Assign _ | EffectCall -> Js_output.make (aux st)
      end


    | Lsend(meth_kind,met, obj, args,loc) -> 
      (* Note that in [Texp_apply] for [%sendcache] the cache might not be used 
         see {!CamlinternalOO.send_meth} and {!Translcore.transl_exp0} the branch
         [Texp_apply] when [public_send ], args are simply dropped

         reference 
         [js_of_ocaml] 
         1. GETPUBMET
         2. GETDYNMET
         3. GETMETHOD
         [ocaml]
         Lsend (bytegen.ml)
         For the object layout refer to [camlinternalOO/create_object]
         {[
           let create_object table =
             (* XXX Appel de [obj_block] *)
             let obj = mark_ocaml_object @@ Obj.new_block Obj.object_tag table.size in
             (* XXX Appel de [caml_modify] *)
             Obj.set_field obj 0 (Obj.repr table.methods);
             Obj.obj (set_id obj)

           let create_object_opt obj_0 table =
             if (Obj.magic obj_0 : bool) then obj_0 else begin
               (* XXX Appel de [obj_block] *)
               let obj = mark_ocaml_object @@ Obj.new_block Obj.object_tag table.size in
               (* XXX Appel de [caml_modify] *)
               Obj.set_field obj 0 (Obj.repr table.methods);
               Obj.obj (set_id obj)
             end
         ]}
         it's a block with tag [248], the first field is [table.methods] which is an array 
         {[
           type table =
             { mutable size: int;
               mutable methods: closure array;
               mutable methods_by_name: meths;
               mutable methods_by_label: labs;
               mutable previous_states:
                 (meths * labs * (label * item) list * vars *
                  label list * string list) list;
               mutable hidden_meths: (label * item) list;
               mutable vars: vars;
               mutable initializers: (obj -> unit) list }
         ]}
      *)


      begin match 
        (met :: obj :: args) 
        |> Ext_list.split_map (fun (x : Lambda.lambda) -> 
            match x with 
            | Lprim (Pgetglobal i, []) -> 
              [], Lam_compile_global.get_exp  (i, env, true)
            | Lprim (Pccall {prim_name ; _}, []) (* nullary external call*)
              -> 
              [], E.var (Ext_ident.create_js prim_name)
            | _ -> 
              begin
                match compile_lambda
                        {cxt with st = NeedValue; should_return = False}
                        x with
                | {block = a; value = Some b} -> a, b 
                | _ -> assert false
              end
          ) with  
      | _, ([] | [_]) -> assert false
      | (args_code, label::obj'::args) 
        -> 
        let cont =
          Js_output.handle_block_return 
            st should_return lam (List.concat args_code)
        in
        let cont2 obj_code v = 
          Js_output.handle_block_return 
            st should_return lam 
            (obj_code :: List.concat args_code) v in 
        let cont3 obj' k = 
          match Js_ast_util.named_expression obj' with 
          | None -> cont (k obj')
          | Some (obj_code, v) -> 
            let obj' = E.var v in 
            cont2 obj_code (k obj') 
        in
        begin
          match meth_kind with 
          | Self -> 
            (* TODO: horrible hack -- fixed later *)
            cont3 obj' (fun obj' -> E.call ~info:Js_call_info.dummy 
                   (Js_of_lam_array.ref_array 
                    (Js_of_lam_record.field Fld_na obj' 0l) label )
                 (obj' :: args))
          (* [E.small_int 1] is because we use array, 
              when we change the runtime represenation, it needs to be adapted 
          *)

          | Cached | Public None
            (* TODO: check -- 1. js object propagate 2. js object create  *)
            -> 
            let get = E.runtime_ref  Js_config.oo "caml_get_public_method" in
            let cache = !method_cache_id in
            let () = incr method_cache_id  in
            cont3 obj' (fun obj' -> 
              E.call ~info:Js_call_info.dummy 
                 (E.call ~info:Js_call_info.dummy get 
                    [obj'; label; E.small_int cache]) (obj'::args)
              ) (* avoid duplicated compuattion *)


          | Public (Some name) -> 


            let js_no_arity obj' name = 
              match args with 
                | [] -> cont @@  E.dot obj' name 
                | _ -> cont3 obj' (fun obj' -> E.bind_call obj' name args)
            in 
            begin
              match Lam_methname.process name, obj with
              | (Js_read_index, _name), _  ->
                begin match args with
                  | [] ->
                    let i = Ext_ident.create "i" in
                    cont3 obj' @@ fun obj' ->  
                    E.fun_ [i]
                      S.[return 
                           (Js_array.ref_array obj' (E.var i)) ]
                  | [x] -> 
                    cont @@ Js_array.ref_array obj' x 
                  |  x :: rest  ->
                    cont @@
                    E.call ~info:Js_call_info.dummy 
                      (Js_array.ref_array obj' x ) 
                      rest 
                end
              | (Js_write_index, _name), _  ->
                  begin match args with
                  | [] ->
                    let i = Ext_ident.create "i" in
                    let v = Ext_ident.create "v" in 
                    cont3 obj' @@ fun obj' -> E.fun_ [i; v ]
                      S.[return (E.seq 
                                   (Js_array.set_array
                                      obj' 
                                      (E.var i) 
                                      (E.var v))
                                   E.unit) 
                        ]
                  | [ i ]
                    -> 
                    let v = Ext_ident.create "v" in 
                    cont3 obj' @@ fun obj' -> E.fun_ [v]
                      S.[return 
                           (E.seq (Js_array.set_array obj' i (E.var v))  E.unit )]
                  | [x; y] -> 
                    cont @@ Js_array.set_array obj' x y 
                  |  x :: y:: rest  ->
                    cont @@  E.call ~info:Js_call_info.dummy
                      (Js_array.set_array obj' x y ) rest 
                  end
              | (Js_write, name), _ -> 

                  begin match args with
                  | [] ->
                    let v = Ext_ident.create "v" in
                    cont3 obj' @@ fun obj' -> E.fun_ [v]
                      S.[return (E.assign (E.dot obj' name) (E.var v)) ]
                  | [v] ->
                    cont @@ E.assign (E.dot obj' name)  v
                  |  _ :: _  -> 
                    (* TODO: better error message *)
                    assert false
                  end
              | (Js_read, name), _ -> 
                cont @@ E.dot obj' name              
              | (Js (Some arity), name), _  -> 

                let args, n, rest = 
                  Ext_list.try_take arity args  in
                if n = arity then 
                  match rest with
                  | [] -> 
                    cont @@ E.call ~info:{arity=Full; call_info = Call_na}
                      (E.dot obj' name) args 
                  | _ ->  
                    cont @@ E.call ~info:Js_call_info.dummy 
                      (E.call 
                         ~info:{arity=Full; call_info = Call_na}
                         (E.dot obj' name) args )
                      rest 
                else 
                  let rest = Ext_list.init 
                      (arity - n) (fun i -> Ext_ident.create Literals.prim) in
                  cont3 obj' @@ fun obj' -> E.fun_ rest 
                    S.[return 
                         (E.call 
                            ~info:{arity=Full; call_info = Call_na }
                            (E.dot obj' name)
                            (args @ List.map E.var rest )                               
                         ) ]                  
              | (Js None, p_name), _  -> 
                js_no_arity obj' p_name 
              | _ , Lprim (Pccall {prim_name = _; _}, []) ->
                (* we know obj is a truly js object,
                    shall it always be global?
                    shall it introduce dependency?
                *)
                js_no_arity obj' name 

              (*TODO: if js has such variable -- all ocaml variables should be aliased *)
              | _, Lvar id 
                when Ext_ident.is_js_object id
              (*TODO#11: check alias table as well,
                here we do flow analysis
                TODO#22: we can also track whether it's an ocaml object
              *)->
                js_no_arity obj' name 
              | ((Ml _ | Unknown _), _), _  ->
                (* For [Ml] or [Unknown]  the name is untouched *)
                let cache = !method_cache_id in
                incr method_cache_id ;
                cont3 obj' 
                  (fun obj' -> E.public_method_call name obj' label 
                      (Int32.of_int cache) args )


            end
        end
      end
    (* TODO: object layer *)
    | Levent (lam,_lam_event) -> compile_lambda cxt lam
    (* [J.Empty,J.N] *)  (* TODO debugging, sourcemap, ignore lambda_event currently *)
    (* 
        seems to be an optimization trick for [translclass]
        | Lifused(v, l) ->
        if count_var v > 0 then simplif l else lambda_unit
    *)
    | Lifused(_,lam) -> compile_lambda cxt lam
  end
