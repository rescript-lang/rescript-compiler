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


(** 
   [bind_name] is a hint to the compiler to generate 
   better names for external module 
*)
let handle_external 
    ({bundle ; bind_name} : Ast_ffi_types.external_module_name)
  : Ident.t * string 
  =
  match bind_name with 
  | None -> 
    Lam_compile_env.add_js_module bundle , bundle
  | Some bind_name -> 
    Lam_compile_env.add_js_module 
      ~hint_name:bind_name
      bundle,
    bundle

let handle_external_opt 
    (module_name : Ast_ffi_types.external_module_name option) 
  : (Ident.t * string) option = 
  match module_name with 
  | Some module_name -> Some (handle_external module_name) 
  | None -> None 

type typ = Ast_core_type.t

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
    ({ Ast_ffi_types.arg_label;  arg_type })
    (arg : J.expression) 
  : E.t list * E.t list  =
  let arg =
    match arg_label with
    | Optional label -> Js_of_lam_option.get_default_undefined arg 
    | Label _ | Empty -> arg in 
  match arg_type with
  | Extern_unit ->  
    (if arg_label = Empty then [] else [E.unit]), 
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
  | Nothing  | Array ->  [arg], []

let fuse x xs =
    if xs = [] then x  
    else List.fold_left E.seq x xs 

let assemble_args (arg_types : Ast_ffi_types.arg_kind list) args : E.t list * E.t option  = 
  let args, eff = 
    List.fold_right2 
      (fun arg_type arg (accs, effs) -> 
         match ocaml_to_js_eff arg_type arg with
         | acc, eff  -> 
           acc @ accs , eff @ effs 
      ) arg_types args ([],[]) in
  args, begin match eff with 
    | [] -> None
    | x::xs -> Some (fuse x xs)
  end

let add_eff eff e =
  match eff with
  | None -> e 
  | Some v -> E.seq v e 

(* Note: can potentially be inconsistent, sometimes 
   {[
     { x : 3 , y : undefined}
   ]}
   and 
   {[
     {x : 3 }
   ]}
   But the default to be undefined  seems reasonable 
*)
let assemble_args_obj (labels : Ast_ffi_types.arg_kind list) (args : J.expression list) = 

  let map, eff  = 
    List.fold_right2
      (fun ({arg_label; arg_type} as arg_kind : Ast_ffi_types.arg_kind) ( arg : J.expression) (accs, eff ) -> 
         match arg_label with 
         | Empty ->  
           accs , 
           if Js_analyzer.no_side_effect_expression arg then eff 
           else arg :: eff 
         | Label label -> 
           let acc, new_eff = ocaml_to_js_eff arg_kind arg in 
           begin match acc with 
             | [ ] -> assert false
             | x::xs -> 
               (Js_op.Key label, fuse x xs ) :: accs , new_eff @ eff 
           end (* evaluation order is undefined *)
         | Optional label -> 
           begin match arg.expression_desc with 
             | Number _ -> (*Invariant: None encoding*)
               accs, eff 
             | _ ->                 
               let acc, new_eff = ocaml_to_js_eff arg_kind arg in 
               begin match acc with 
                 | [] -> assert false 
                 | x::xs -> 
                   (Js_op.Key label, fuse x xs)::accs , 
                   new_eff @ eff 
               end 
           end
      ) labels args ([], []) in
  match eff with
  | [] -> 
    E.obj map 
  | x::xs -> E.seq (fuse x xs) (E.obj map)


(* TODO: fix splice, 
   we need a static guarantee that it is static array construct
   otherwise, we should provide a good error message here, 
   no compiler failure here 
   Invariant : Array encoding
*)

let ocaml_to_js ~js_splice:(js_splice : bool) call_loc ffi
    last ({ Ast_ffi_types.arg_label;  arg_type = ty } as arg_ty)
    (arg : J.expression) 
  = 
  if last && js_splice then
    match ty with 
    | Array -> 
      begin match arg with 
        | {expression_desc = Array (ls,_mutable_flag) } -> 
          ls, [] 
        | _ -> 
          Location.raise_errorf ~loc:call_loc
            "function call with %s  is a primitive with [@@bs.splice], it expects its arguments to be a syntactic array in the call site" 
            (Ast_ffi_types.name_of_ffi ffi)
      end
    | _ -> assert  false
  else 
    ocaml_to_js_eff arg_ty arg 

let assemble_args_splice call_loc ffi  js_splice arg_types args : E.t list * E.t option = 
  let args, eff = 
    Ext_list.fold_right2_last (fun last arg_ty arg (accs, effs)  -> 
        let (acc,eff) = ocaml_to_js call_loc ffi  ~js_splice last arg_ty arg  in acc @ accs, eff @ effs
      ) arg_types args ([], []) in
  args,
  begin  match eff with
    | [] -> None 
    | x::xs ->  
      Some (fuse x xs) 
  end


let translate_ffi call_loc (ffi : Ast_ffi_types.ffi ) prim_name
    (cxt  : Lam_compile_defs.cxt)
    arg_types result_type
    (args : J.expression list) = 
  match ffi with 

  | Js_call{ external_module_name = module_name; 
             name = fn; splice = js_splice ; 

           } -> 
    let fn =  
      match handle_external_opt module_name with 
      | Some (id,_) -> 
        E.dot (E.var id) fn
      | None ->  E.js_var fn
    in
    let args, eff  = assemble_args_splice   call_loc ffi js_splice arg_types args in 
    add_eff eff 
      (if result_type then 

         E.seq (E.call ~info:{arity=Full; call_info = Call_na} fn args) E.unit
       else 
         E.call ~info:{arity=Full; call_info = Call_na} fn args)

  | Js_module_as_var module_name -> 
    let (id, name) =  handle_external  module_name  in
    E.external_var_dot id ~external_name:name 

  | Js_module_as_fn {external_module_name = module_name; splice} ->
    let fn =
      let (id, name) = handle_external  module_name  in
      E.external_var_dot id ~external_name:name 
    in           
    let args, eff = assemble_args_splice   call_loc ffi splice arg_types args in 
    (* TODO: fix in rest calling convention *)          
    add_eff eff 
      ( if result_type then
          E.seq (E.call ~info:{arity=Full; call_info = Call_na} fn args) E.unit
        else
          E.call ~info:{arity=Full; call_info = Call_na} fn args
      )
  | Js_module_as_class module_name ->
    let fn =
      let (id,name) = handle_external  module_name in
      E.external_var_dot id ~external_name:name  in           
    let args,eff = assemble_args arg_types args in 
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
             splice 
           } -> 
    (* This has some side effect, it will 
       mark its identifier (If it has) as an object,
       ATTENTION: 
       order also matters here, since we mark its jsobject property, 
       it  will affect the code gen later
       TODO: we should propagate this property 
       as much as we can(in alias table)
    *)
    let args, eff = assemble_args_splice  call_loc  ffi splice arg_types args in
    let fn =  
      match handle_external_opt module_name with 
      | Some (id,name) ->  
        E.external_var_dot id ~external_name:name ~dot:fn

      | None -> 
        (** TODO: check, no [@@bs.module], 
            assume it's global *)
        E.js_var fn

    in
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



  | Js_global {name; external_module_name} -> 

    (* TODO #11
       1. check args -- error checking 
       2. support [@@bs.scope "window"]
       we need know whether we should call [add_js_module] or not 
    *)
    begin match name, handle_external_opt external_module_name with 
      | "true", None -> E.js_bool true
      | "false", None -> E.js_bool false
      | "null", None -> E.nil 
      | "undefined", None -> E.undefined
      | _, Some(id,mod_name)
        -> E.external_var_dot id ~external_name:mod_name ~dot:name
      | _, None -> 

        E.var (Ext_ident.create_js name)
    end
  | Js_send {splice  = js_splice ; name ; pipe = false} -> 
    begin match args  with
      | self :: args -> 
        let [@warning"-8"] ( self_type::arg_types )
          = arg_types in
        let args, eff = assemble_args_splice  call_loc ffi  js_splice arg_types args in
        add_eff eff @@ 
        E.call ~info:{arity=Full; call_info = Call_na}  (E.dot self name) args
      | _ -> 
        assert false 
    end
  | Js_send { name ; pipe = true ; splice = js_splice}
    -> (* splice should not happen *)
    (* assert (js_splice = false) ;  *)
    let self, args = Ext_list.exclude_tail args in
    let self_type, arg_types = Ext_list.exclude_tail arg_types in
    let args, eff = assemble_args_splice call_loc ffi  js_splice arg_types args in
    add_eff eff @@
    E.call ~info:{arity=Full; call_info = Call_na}  (E.dot self name) args

  | Js_get name -> 
    begin match args with 
      | [obj] ->
        E.dot obj name        
      | _ -> assert false 
    end  
  | Js_set name -> 
    begin match args with 
      | [obj; v] -> 
        E.assign (E.dot obj name) v         
      | _ -> 
        assert false 
    end
  | Js_get_index 
    -> 
    begin match args with
      | [obj; v ] -> 
        Js_arr.ref_array obj v
      | _ -> assert false 
    end
  | Js_set_index 
    -> 
    begin match args with 
      | [obj; v ; value] -> 
        Js_arr.set_array obj v value
      | _ -> assert false
    end



let translate loc cxt 
    ({prim_name ;  prim_native_name} 
     : Primitive.description) args  = 

  match Ast_ffi_types.from_string prim_native_name with 
  | Ffi_normal ->    
    Lam_dispatch_primitive.translate prim_name args 
  | Ffi_obj_create labels -> assemble_args_obj labels args 
  | Ffi_bs (arg_types, result_type, ffi) -> 
    translate_ffi loc  ffi prim_name cxt arg_types result_type args 
