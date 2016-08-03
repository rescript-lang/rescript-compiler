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


let handle_external 
    (module_name : Ast_external_attributes.external_module_name option) = 
  match module_name with 
  | Some {bundle ; bind_name} -> 
    let id  = 
      match bind_name with 
      | None -> 
        Lam_compile_env.add_js_module bundle , bundle
      | Some bind_name -> 
        Lam_compile_env.add_js_module 
          ~id:(Ext_ident.create_js_module bind_name) bundle,
        bundle
    in Some id 
  | None -> None 

type typ = Ast_core_type.t

let ocaml_to_js last
    (js_splice : bool)
    ({ Ast_external_attributes.arg_label;  arg_type = ty })
    (arg : J.expression) 
  : E.t list = 
  if last && js_splice 
  then
    match ty with 
    | Array -> 
      begin match arg with 
        | {expression_desc = Array (ls,_mutable_flag) } -> 
          ls (* Invariant : Array encoding *)
        | _ -> 
          assert false  
          (* TODO: fix splice, 
             we need a static guarantee that it is static array construct
             otherwise, we should provide a good error message here
          *)
      end
    | _ -> assert  false
  else 
    match ty with
    | Unit ->  [] (* ignore unit *)
    | NullString dispatches -> 
      [Js_of_lam_variant.eval arg dispatches]
    | NonNullString dispatches -> 
      Js_of_lam_variant.eval_as_event arg dispatches
    | Int dispatches -> 
      [Js_of_lam_variant.eval_as_int arg dispatches]
    | Nothing  | Array -> 
      begin match arg_label with 
      | Optional label -> [Js_of_lam_option.get_default_undefined arg]
      | Label _ | Empty ->  [arg]  
      end

          


let translate_ffi (ffi : Ast_external_attributes.ffi ) prim_name
    (cxt  : Lam_compile_defs.cxt)
    arg_types result_type
    (args : J.expression list) = 
    match ffi with 
    | Obj_create labels -> 
      E.obj @@ Ext_list.filter_map2 
          (fun label ( arg : J.expression) -> 
            match (label : Ast_core_type.arg_label) with 
            | Empty ->  None 
            | Label label -> 
              Some ( Js_op.Key label, arg)
            | Optional label -> 
              begin match arg.expression_desc with 
                | Number _ -> (*Invariant: None encoding*)
                  None
                | _ ->  
                  (* FIXME: can potentially be inconsistent, sometimes 
                     {[
                       { x : 3 , y : undefined}
                     ]}
                     and 
                     {[
                       {x : 3 }
                     ]}
                  *)
                  Some ( Js_op.Key label, 
                         Js_of_lam_option.get_default_undefined arg)
              end
          ) labels args 
    | Js_call{ external_module_name = module_name; 
               txt = { name = fn; splice = js_splice ; 

                     }} -> 
      let fn =  
        match handle_external module_name with 
        | Some (id,_) -> 
          E.dot (E.var id) fn
        | None ->  E.js_var fn
      in
      let args = 
        Ext_list.flat_map2_last (ocaml_to_js js_splice) arg_types args  in 
      begin match (result_type : Ast_core_type.arg_type) with 
      | Unit -> 
        E.seq (E.call ~info:{arity=Full; call_info = Call_na} fn args) E.unit
      | _ -> 
        E.call ~info:{arity=Full; call_info = Call_na} fn args
      end
    | Js_module_as_var module_name -> 
      begin match handle_external (Some module_name) with 
        | Some (id, name) -> 
          E.external_var_dot id name None
        | None -> assert false 
      end
    | Js_module_as_fn module_name ->
      let fn =
        match handle_external (Some module_name) with
        | Some (id,name) ->
          E.external_var_dot id name None           
        | None -> assert false in           
      let args = 
        Ext_list.flat_map2_last (ocaml_to_js false) arg_types args
        (* TODO: fix in rest calling convention *)          
      in 
      begin match (result_type : Ast_core_type.arg_type) with 
        | Unit -> 
          E.seq (E.call ~info:{arity=Full; call_info = Call_na} fn args) E.unit
        | _ -> 
          E.call ~info:{arity=Full; call_info = Call_na} fn args
      end

    | Js_new { external_module_name = module_name; 
               txt = fn;
             } -> 

      let args = 
        Ext_list.flat_map2_last (ocaml_to_js false) arg_types args  in 
      let fn =  
        match handle_external module_name with 
        | Some (id,name) ->  
          E.external_var_dot id name (Some fn)

        | None -> 
          (** TODO: check, no [@@bs.module], 
              assume it's global *)
          E.js_var fn

      in
      (* This has some side effect, it will 
         mark its identifier (If it has) as an object,
         ATTENTION: 
         order also matters here, since we mark its jsobject property, 
         it  will affect the code gen later
         TODO: we should propagate this property 
         as much as we can(in alias table)
      *)
      (
        match cxt.st with 
        | Declare (_, id) | Assign id  ->
          (* Format.fprintf Format.err_formatter "%a@."Ident.print  id; *)
          Ext_ident.make_js_object id 
        | EffectCall | NeedValue -> ()
      );
      E.new_ fn args



    | Js_global {txt = name; external_module_name} -> 

      (* TODO #11
         1. check args -- error checking 
         2. support [@@bs.scope "window"]
         we need know whether we should call [add_js_module] or not 
      *)
      begin match name, handle_external external_module_name with 
        | "true", None -> E.js_bool true
        | "false", None -> E.js_bool false
        | "null", None -> E.nil 
        | "undefined", None -> E.undefined
        | _, Some(id,mod_name)
          -> E.external_var_dot id mod_name (Some name)
        | _, None -> 

          E.var (Ext_ident.create_js name)
      end
    | Js_send {splice  = js_splice ; name } -> 
      begin match args  with
        | self :: args -> 
          let [@warning"-8"] ( self_type::arg_types )
            = arg_types in
          let args = Ext_list.flat_map2_last (ocaml_to_js js_splice) arg_types args in
          E.call ~info:{arity=Full; call_info = Call_na}  (E.dot self name) args
        | _ -> 
          assert false 
      end
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
          Js_array.ref_array obj v
        | _ -> assert false 
      end
    | Js_set_index 
      -> 
      begin match args with 
      | [obj; v ; value] -> 
        Js_array.set_array obj v value
      | _ -> assert false
      end
    


let translate cxt 
    ({prim_name ;  prim_native_name} 
     : Ast_external_attributes.prim) args  = 
  if Ast_external_attributes.is_bs_external_prefix prim_native_name then 
    begin 
      match Ast_external_attributes.unsafe_from_string prim_native_name with 
      | Normal -> 
        Lam_dispatch_primitive.translate prim_name args 
      | Bs (arg_types, result_type, ffi) -> 
        translate_ffi  ffi prim_name cxt arg_types result_type args 
    end
  else 
    begin 
      Lam_dispatch_primitive.translate prim_name args 
    end

