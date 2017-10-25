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



[@@@warning "+9"]

module E = Js_exp_make


(** 
   [bind_name] is a hint to the compiler to generate 
   better names for external module 
*)
let handle_external 
    ({bundle ; bind_name} : External_ffi_types.external_module_name)
  : Ident.t * string 
  =
  match bind_name with 
  | None -> 
    Lam_compile_env.add_js_module ~hint_name:None bundle , bundle
  | Some bind_name -> 
    Lam_compile_env.add_js_module 
      ~hint_name:(Some bind_name)
      bundle,
    bundle

let handle_external_opt 
    (module_name : External_ffi_types.external_module_name option) 
  : (Ident.t * string) option = 
  match module_name with 
  | Some module_name -> Some (handle_external module_name) 
  | None -> None 



(** The first return value is value, the second argument is side effect expressions 
    Only the [unit] with no label will be ignored
    When  we are passing a boxed value to external(optional), we need
    unbox it in the first place.

    Note when optional value is not passed, the unboxed value would be 
    [undefined], with the combination of `[@bs.int]` it would be still be 
    [undefined], this by default is still correct..  
    {[
      (function () {
           switch (undefined) {
             case 97 : 
               return "a";
             case 98 : 
               return "b";

           }
         }()) === undefined
    ]} 

     This would not work with [NonNullString]
*)
let ocaml_to_js_eff 
    ({ External_arg_spec.arg_label;  arg_type })
    (raw_arg : J.expression)
  : E.t list * E.t list  =
  let arg =
    match arg_label with
    | Optional label -> Js_of_lam_option.get_default_undefined raw_arg
    | Label (_, None) | Empty None -> raw_arg
    | Label (_, Some _) 
    | Empty ( Some _)
      -> assert false in 
  match arg_type with
  | Arg_cst _ -> assert false 
  | Fn_uncurry_arity _ -> assert false  
  (* has to be preprocessed by {!Lam} module first *)
  | Extern_unit ->  
    (if arg_label = External_arg_spec.empty_label then [] else [E.unit]), 
    (if Js_analyzer.no_side_effect_expression arg then 
       []
     else 
       [arg]) (* leave up later to decide *)
  | Ignore -> 
    [], 
    (if Js_analyzer.no_side_effect_expression arg then 
       []
     else 
       [arg])
  | NullString dispatches -> 
    [Js_of_lam_variant.eval arg dispatches],[]
  | NonNullString dispatches -> 
    Js_of_lam_variant.eval_as_event arg dispatches,[]
  | Int dispatches -> 
    [Js_of_lam_variant.eval_as_int arg dispatches],[]
  | Unwrap ->
    let single_arg =
      match arg_label with
      | Optional label ->
        (**
           If this is an optional arg (like `?arg`), we have to potentially do
           2 levels of unwrapping:
           - if ocaml arg is `None`, let js arg be `undefined` (no unwrapping)
           - if ocaml arg is `Some x`, unwrap the arg to get the `x`, then
             unwrap the `x` itself
        *)
        Js_of_lam_option.get_default_undefined
          ~map:(fun opt_unwrapping exp ->
              match opt_unwrapping with
              | Static_unwrapped ->
                (* If we can unwrap the option statically, do `arg[1]` *)
                E.index exp 1l
              | Runtime_maybe_unwrapped ->
                (* If we can't, do Js_primitive.option_get_unwrap(arg) *)
                E.runtime_call Js_runtime_modules.js_primitive "option_get_unwrap" [raw_arg]
            )
          raw_arg
      | _ ->
        Js_of_lam_variant.eval_as_unwrap raw_arg
    in
    [single_arg],[]
  | Nothing  | Array ->  [arg], []



let empty_pair = [],[]       

let add_eff eff e =
  match eff with
  | None -> e 
  | Some v -> E.seq v e 



(* TODO: fix splice, 
   we need a static guarantee that it is static array construct
   otherwise, we should provide a good error message here, 
   no compiler failure here 
   Invariant : Array encoding
   @return arguments and effect
*)
let assemble_args call_loc ffi  js_splice arg_types args : E.t list * E.t option = 
  let rec aux (labels : External_arg_spec.t list) args = 
    match labels, args with 
    | [] , [] -> empty_pair
    | { arg_label =  Empty (Some cst) ; _} :: labels  , args 
    | { arg_label =  Label (_, Some cst); _} :: labels  , args -> 
      let accs, eff = aux labels args in
      Lam_compile_const.translate_arg_cst cst :: accs, eff 
    | ({arg_label = Empty None | Label (_,None) | Optional _ ;_ } as arg_kind) ::labels, arg :: args
      ->  
      if js_splice && args = [] then 
        let accs, eff = aux labels [] in 
        begin match arg_kind.arg_type with 
          | Array -> 
            begin match (arg : E.t) with 
              | {expression_desc = Array (ls,_mutable_flag) ;_ } -> 
                Ext_list.append ls accs, eff 
              | _ -> 
                Location.raise_errorf ~loc:call_loc
                  {|@{<error>Error:@} function call with %s  is a primitive with [@@bs.splice], it expects its `bs.splice` argument to be a syntactic array in the call site and  all arguments to be supplied|}
                  (External_ffi_types.name_of_ffi ffi)
            end
          | _ -> assert false 
        end
      else 
        let accs, eff = aux labels args in 
        let acc, new_eff = ocaml_to_js_eff arg_kind arg in 
        Ext_list.append acc  accs, Ext_list.append new_eff  eff
    | { arg_label = Empty None | Label (_,None) | Optional _  ; _ } :: _ , [] 
      -> assert false 
    | [],  _ :: _  -> assert false      

  in 
  let args, eff = aux arg_types args  in 
  args,
  begin  match eff with
    | [] -> None 
    | x::xs ->  
      Some (E.fuse_to_seq x xs) 
  end

let translate_scoped_module_val module_name fn  scopes = 
  match handle_external_opt module_name with 
  | Some (id,external_name) ->
    begin match scopes with 
      | [] -> 
        E.external_var_dot ~external_name ~dot:fn id 
        (* E.dot (E.var id) fn *)
      | x :: rest -> 
        (* let start = E.dot (E.var id )  x in  *)
        let start = E.external_var_dot ~external_name ~dot:x id in 
        List.fold_left (fun acc x -> E.dot  acc x) start (Ext_list.append rest  [fn])
    end
  | None ->  
    (*  no [@@bs.module], assume it's global *)
    begin match scopes with 
      | [] -> 
        (* E.external_var_dot ~external_name ~dot:fn id  *)
        E.js_var fn
      | x::rest -> 
        let start = E.js_var x  in 
        List.fold_left (fun acc x -> E.dot acc x) start (Ext_list.append rest  [fn])
    end



let translate_scoped_access scopes obj =
  match scopes with 
  | [] ->  obj
  | x::xs -> 
    List.fold_left (fun acc x -> E.dot acc x) (E.dot obj x) xs 
  
let translate_ffi 
    call_loc (ffi : External_ffi_types.ffi ) 
    (* prim_name *)
    (cxt  : Lam_compile_context.t)
    arg_types 
    (args : J.expression list) = 
  match ffi with 

  | Js_call{ external_module_name = module_name; 
             name = fn; splice = js_splice ; 
             scopes

           } -> 
    let fn =  translate_scoped_module_val module_name fn scopes in 
    let args, eff  = assemble_args   call_loc ffi js_splice arg_types args in 
    add_eff eff @@              
    E.call ~info:{arity=Full; call_info = Call_na} fn args

  | Js_module_as_var module_name -> 
    let (id, name) =  handle_external  module_name  in
    E.external_var_dot id ~external_name:name 

  | Js_module_as_fn {external_module_name = module_name; splice} ->
    let fn =
      let (id, name) = handle_external  module_name  in
      E.external_var_dot id ~external_name:name 
    in           
    let args, eff = assemble_args   call_loc ffi splice arg_types args in 
    (* TODO: fix in rest calling convention *)          
    add_eff eff @@
    E.call ~info:{arity=Full; call_info = Call_na} fn args

  | Js_module_as_class module_name ->
    let fn =
      let (id,name) = handle_external  module_name in
      E.external_var_dot id ~external_name:name  in           
    let args,eff = assemble_args call_loc  ffi false  arg_types args in 
    (* TODO: fix in rest calling convention *)   
    add_eff eff        
      begin 
        (match cxt.st with 
         | Declare (_, id) | Assign id  ->
           (* Format.fprintf Format.err_formatter "%a@."Ident.print  id; *)
           Ext_ident.make_js_object id 
         | EffectCall | NeedValue -> ())
        ;
        E.new_ fn args
      end            

  | Js_new { external_module_name = module_name; 
             name = fn;
             splice ;
             scopes
           } -> 
    (* This has some side effect, it will 
       mark its identifier (If it has) as an object,
       ATTENTION: 
       order also matters here, since we mark its jsobject property, 
       it  will affect the code gen later
       TODO: we should propagate this property 
       as much as we can(in alias table)
    *)
    let args, eff = assemble_args  call_loc  ffi splice arg_types args in
    let fn =  translate_scoped_module_val module_name fn scopes in 
    add_eff eff 
      begin 
        (match cxt.st with 
         | Declare (_, id) | Assign id  ->
           (* Format.fprintf Format.err_formatter "%a@."Ident.print  id; *)
           Ext_ident.make_js_object id 
         | EffectCall | NeedValue -> ())
        ;
        E.new_ fn args
      end            



  | Js_global {name; external_module_name; scopes} -> 

    (* TODO #11
       1. check args -- error checking 
       2. support [@@bs.scope "window"]
       we need know whether we should call [add_js_module] or not 
    *)
    begin match name, handle_external_opt external_module_name , scopes with 
      | "true", None, []  -> E.js_bool true
      | "false", None, [] -> E.js_bool false
      | "null", None, [] -> E.nil 
      | "undefined", None, [] -> E.undefined
      | _, _, _ -> 
        translate_scoped_module_val external_module_name name scopes
    end
  | Js_send {splice  = js_splice ; name ; pipe = false; js_send_scopes = scopes } -> 
    begin match args  with
      | self :: args -> 
        (* PR2162 [self_type] more checks in syntax:
          - should not be [bs.as] *)
        let [@warning"-8"] ( _self_type::arg_types )
          = arg_types in
        let args, eff = assemble_args  call_loc ffi  js_splice arg_types args in
        add_eff eff @@ 
          let self = translate_scoped_access scopes self in 
          E.call ~info:{arity=Full; call_info = Call_na}  (E.dot self name) args
      | _ -> 
        assert false 
    end
  | Js_send { name ; pipe = true ; splice = js_splice; js_send_scopes = scopes  }
    -> (* splice should not happen *)
    (* assert (js_splice = false) ;  *)
    let args, self = Ext_list.split_at_last args in
    let arg_types, self_type = Ext_list.split_at_last arg_types in
    let args, eff = assemble_args call_loc ffi  js_splice arg_types args in
    add_eff eff @@
    let self = translate_scoped_access scopes self in 
    E.call ~info:{arity=Full; call_info = Call_na}  (E.dot self name) args

  | Js_get {js_get_name = name; js_get_scopes = scopes } -> 
    let args,cur_eff = assemble_args call_loc ffi false arg_types args in 
    add_eff cur_eff @@ 
    begin match args with 
      | [obj] ->
        let obj = translate_scoped_access scopes obj in 
        E.dot obj name        
      | _ -> assert false  (* Note these assertion happens in call site *)
    end  
  | Js_set {js_set_name = name; js_set_scopes = scopes  } -> 
    (* assert (js_splice = false) ;  *)
    let args,cur_eff = assemble_args call_loc ffi false arg_types args in 
    add_eff cur_eff @@
    begin match args, arg_types with 
      | [obj; v], _ -> 
        let obj = translate_scoped_access scopes obj in
        E.assign (E.dot obj name) v         
      | _ -> 
        assert false 
    end
  | Js_get_index { js_get_index_scopes = scopes }
    -> 
    let args,cur_eff = assemble_args call_loc ffi false arg_types args in 
    add_eff cur_eff @@ 
    begin match args with
      | [obj; v ] -> 
        Js_arr.ref_array (translate_scoped_access scopes obj) v 
      | _ -> assert false 
    end
  | Js_set_index { js_set_index_scopes = scopes }
    -> 
    let args,cur_eff = assemble_args call_loc ffi false arg_types args in 
    add_eff cur_eff @@ 
    begin match args with 
      | [obj; v ; value] -> 
          Js_arr.set_array (translate_scoped_access scopes obj) v value
      | _ -> assert false
    end

